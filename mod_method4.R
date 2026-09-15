# app_method_4.R
# ===========================================================================
# Method 4 (Bracis et al., 2018) — "How often does she come back?"
# Revisit-hotspot analysis for the Kaudulla elephants
#
# This file is written as a SHINY MODULE (mod_method4_ui / mod_method4_server)
# so it can be dropped straight into the group's combined app:
#
#   In the big app's ui:      mod_method4_ui("method4")
#   In the big app's server:  mod_method4_server("method4", data = shared_df)
#
# A small standalone runner is included at the very bottom so this file also
# works on its own for testing — delete that section when merging.
# ===========================================================================

library(shiny)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(lubridate)
library(geosphere)   # kept only for reference/back-compat; not used in the fast path
library(leaflet)
library(htmltools)
library(scales)
library(DT)

# ---------------------------------------------------------------------------
# 1. Fast revisit calculator
# ---------------------------------------------------------------------------
# The original version computed a full Haversine distance matrix for every
# point against every other point (O(n^2)) — for ~15,000 fixes that's ~250
# million distance calculations, which will hang a live app.
#
# This version bins points into a spatial grid (cell size = radius_m) and,
# for each point, only checks candidates from its own cell and the 8
# neighbouring cells. That drops the cost to roughly O(n) for the point
# densities we see here, while producing identical results (same radius,
# same episode-splitting rule).
calc_revisits_fast <- function(df, radius_m = 150, gap_hours = 24) {
  
  df <- df %>% arrange(datetime)
  n <- nrow(df)
  
  if (n == 0) {
    return(df %>% mutate(
      revisit = integer(0),
      visit_dates = character(0),
      first_visit = as.POSIXct(character(0), tz = "UTC"),
      last_visit  = as.POSIXct(character(0), tz = "UTC")
    ))
  }
  
  # Local equirectangular projection (metres), centred on this elephant's
  # own mean position — accurate enough at the scale of a single park and
  # much cheaper than repeated Haversine calls.
  lat0 <- mean(df$lat)
  m_per_deg_lat <- 111132.92 - 559.82 * cos(2 * lat0 * pi / 180) + 1.175 * cos(4 * lat0 * pi / 180)
  m_per_deg_lon <- 111412.84 * cos(lat0 * pi / 180) - 93.5 * cos(3 * lat0 * pi / 180)
  
  x  <- (df$lon - mean(df$lon)) * m_per_deg_lon
  y  <- (df$lat - mean(df$lat)) * m_per_deg_lat
  dt <- df$datetime
  
  cx <- floor(x / radius_m)
  cy <- floor(y / radius_m)
  key <- paste(cx, cy, sep = "_")
  cell_index <- split(seq_len(n), key)
  
  revisit      <- integer(n)
  visit_dates  <- character(n)
  first_visit  <- as.POSIXct(rep(NA, n), tz = "UTC")
  last_visit   <- as.POSIXct(rep(NA, n), tz = "UTC")
  
  for (i in seq_len(n)) {
    
    ci <- cx[i]; cj <- cy[i]
    cand <- integer(0)
    for (dx in -1:1) {
      for (dy in -1:1) {
        k <- paste(ci + dx, cj + dy, sep = "_")
        idx <- cell_index[[k]]
        if (!is.null(idx)) cand <- c(cand, idx)
      }
    }
    
    d <- sqrt((x[cand] - x[i])^2 + (y[cand] - y[i])^2)
    inside <- cand[d <= radius_m]
    
    tt <- sort(dt[inside])
    gaps <- c(TRUE, diff(tt) > hours(gap_hours))
    episode_id <- cumsum(gaps)
    
    starts_num <- sort(tapply(as.numeric(tt), episode_id, min))
    episode_dates <- format(as.POSIXct(starts_num, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
    
    revisit[i]     <- length(episode_dates)
    visit_dates[i] <- paste(episode_dates, collapse = ", ")
    first_visit[i] <- min(tt)
    last_visit[i]  <- max(tt)
  }
  
  df$revisit     <- revisit
  df$visit_dates <- visit_dates
  df$first_visit <- first_visit
  df$last_visit  <- last_visit
  df
}

# ---------------------------------------------------------------------------
# 1b. Data-quality check: longest continuous tracking stretch
# ---------------------------------------------------------------------------
# Total calendar span and total fix count can both look fine while the
# actual data is chopped into short, disconnected bursts (collar dropout,
# battery failure, etc.) with nothing usable in between. Since the
# revisit method needs real continuous time to observe a second visit,
# what matters is the *longest single unbroken stretch* of tracking, not
# the total span. A new burst starts whenever the gap since the previous
# fix exceeds `gap_threshold_h` (default 72h / 3 days).
.burst_stats <- function(df, gap_threshold_h = 72) {
  df <- df %>% arrange(datetime)
  n <- nrow(df)
  if (n <= 1) {
    return(tibble(n_bursts = n, max_burst_days = 0))
  }
  gaps_h <- as.numeric(difftime(df$datetime[-1], df$datetime[-n], units = "hours"))
  burst_id <- cumsum(c(TRUE, gaps_h > gap_threshold_h))
  burst_summary <- tibble(datetime = df$datetime, burst = burst_id) %>%
    group_by(burst) %>%
    summarise(days = as.numeric(difftime(max(datetime), min(datetime), units = "days")), .groups = "drop")
  tibble(n_bursts = nrow(burst_summary), max_burst_days = max(burst_summary$days))
}

# ---------------------------------------------------------------------------
# 2. Small helpers for the KPI cards and the plain-English interpretation
# ---------------------------------------------------------------------------

.kpi_card <- function(name, n_total, pct_once, max_revisit, css_class) {
  div(
    class = paste("kpi-card", css_class),
    div("\U0001F418", class = "kpi-icon"),
    div(toupper(name), class = "kpi-name"),
    div(class = "kpi-stat",
        div(format(n_total, big.mark = ","), class = "kpi-value"),
        div("Total Tracked Locations", class = "kpi-label")),
    div(class = "kpi-stat",
        div(paste0(pct_once, "%"), class = "kpi-value"),
        div("Visited Only Once", class = "kpi-label")),
    div(class = "kpi-stat",
        div(max_revisit, class = "kpi-value"),
        div("Most-Revisited Spot (visits)", class = "kpi-label"))
  )
}

.interpret_text <- function(name, pct_once, max_revisit, span_days, max_burst_days, n_bursts, min_burst_days = 10) {
  
  # Longest unbroken stretch of tracking is what actually determines
  # whether "never revisited" can mean anything — an elephant can have a
  # long total calendar span and still never have had enough continuous
  # time for a real second visit to be observed (see Tara Devi / Talatha).
  if (max_burst_days < min_burst_days) {
    return(paste0(
      "<b>", name, "</b> \u2014 not enough continuous data for a reliable read. Its longest unbroken ",
      "stretch of tracking was only ", round(max_burst_days, 1), " day", if (round(max_burst_days,1) != 1) "s" else "",
      " (spread across ", n_bursts, " separate tracking bursts totalling ", round(span_days), " calendar days). ",
      "That's too short a continuous window to tell whether it revisits places or not \u2014 any percentage ",
      "shown for it mostly reflects gaps in collar data, not real behaviour."
    ))
  }
  
  body <- if (pct_once >= 30) {
    paste0(
      "<b>", name, "</b> — wide-ranging pattern: ", pct_once, "% of its tracked locations were ",
      "never revisited during the monitoring period. This can reflect long-distance ranging, ",
      "exploratory forays, or dispersal-like movement. Worth a closer look at which one-time ",
      "locations sit outside its main cluster or close to the park boundary \u2014 those are the ",
      "ones most relevant to human\u2013elephant conflict early warning."
    )
  } else if (pct_once <= 10) {
    paste0(
      "<b>", name, "</b> — strong site fidelity: only ", pct_once, "% of its locations were one-time-only, ",
      "meaning it kept returning to a fairly consistent set of places. Its most-revisited spot was used ",
      "on ", max_revisit, " separate occasions \u2014 a strong candidate for a core resource (water, shade, ",
      "or forage) worth prioritising for protection."
    )
  } else {
    paste0(
      "<b>", name, "</b> — mixed pattern: ", pct_once, "% of locations were one-time-only, alongside a ",
      "well-used core area (top spot revisited ", max_revisit, " times). Consistent with an elephant that ",
      "has a defined home range but still makes periodic exploratory movements."
    )
  }
  
  # Short tracking windows make "never revisited" much easier to see by
  # chance alone — there simply wasn't much time for a second visit to
  # happen. Flag this so a high percentage isn't over-read as behaviour.
  if (span_days < 60) {
    body <- paste0(
      body, " <i>Caveat: only tracked for ", round(span_days), " days, versus a year or more for most ",
      "other elephants here \u2014 treat this percentage as a short snapshot, not a settled home-range pattern.</i>"
    )
  }
  
  body
}

# ---------------------------------------------------------------------------
# 3. Module UI
# ---------------------------------------------------------------------------

mod_method4_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .m4-wrap { padding: 24px 30px; }
        .app-title { font-weight: 700; color: #2c3e50; margin-bottom: 4px; }
        .app-subtitle { color: #7f8c8d; font-size: 15px; margin-bottom: 4px; }
        .app-methodnote { color: #95a5a6; font-size: 12.5px; margin-bottom: 22px; }
        .kpi-row { display: flex; gap: 18px; margin-bottom: 20px; flex-wrap: wrap; }
        .kpi-card {
          flex: 1 1 0; min-width: 260px; border-radius: 14px; padding: 20px 22px;
          color: #ffffff; box-shadow: 0 6px 16px rgba(0,0,0,0.12); position: relative; overflow: hidden;
        }
        .kpi-card .kpi-icon { position: absolute; right: 14px; top: 12px; font-size: 34px; opacity: 0.35; }
        .kpi-card .kpi-name { font-size: 17px; font-weight: 700; letter-spacing: 0.3px; margin-bottom: 14px; }
        .kpi-stat { margin-bottom: 8px; }
        .kpi-stat .kpi-value { font-size: 26px; font-weight: 800; line-height: 1.1; }
        .kpi-stat .kpi-label { font-size: 12.5px; opacity: 0.9; text-transform: uppercase; letter-spacing: 0.4px; }
        .kpi-c1 { background: linear-gradient(135deg, #2E86AB, #1B4F72); }
        .kpi-c2 { background: linear-gradient(135deg, #27AE60, #145A32); }
        .kpi-c3 { background: linear-gradient(135deg, #C0392B, #7B241C); }
        .kpi-c4 { background: linear-gradient(135deg, #8E44AD, #4A235A); }
        .kpi-c5 { background: linear-gradient(135deg, #D68910, #7E5109); }
        .kpi-c6 { background: linear-gradient(135deg, #16A085, #0B5345); }
        .interp-box {
          background: #f8f9fa; border-left: 4px solid #2E86AB; border-radius: 6px;
          padding: 14px 18px; margin-bottom: 20px; font-size: 14px; color: #2c3e50; line-height: 1.55;
        }
        .interp-box p { margin-bottom: 8px; }
        .interp-caveat { color: #95a5a6; font-size: 12.5px; margin-top: 10px; }
        .map-card { border-radius: 14px; overflow: hidden; box-shadow: 0 6px 16px rgba(0,0,0,0.12); background: #ffffff; }
        .nav-tabs { border-bottom: none; padding: 0 6px; }
        .nav-tabs > li > a { border-radius: 10px 10px 0 0; font-weight: 600; color: #7f8c8d; border: none; padding: 12px 22px; }
        .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
          color: #2c3e50; background-color: #ffffff; border: none; box-shadow: 0 -2px 0 #2E86AB inset;
        }
        .tab-content { background: #ffffff; border-radius: 0 14px 14px 14px; padding: 18px; }
        .chart-caption { color: #7f8c8d; font-size: 13.5px; margin-top: 6px; text-align: center; }
        .dl-btn { margin-bottom: 14px; }
        .dl-btn .btn { background-color: #2E86AB; color: #ffffff; border: none; border-radius: 8px; font-weight: 600; padding: 8px 18px; }
        .dl-btn .btn:hover { background-color: #1B4F72; color: #ffffff; }
        table.dataTable thead th { color: #2c3e50; font-weight: 700; }
      "))
    ),
    
    div(
      class = "m4-wrap",
      
      h2("Elephant Revisit Hotspot Map", class = "app-title"),
      div("Method 4 (Bracis et al., 2018) \u2014 how often does each elephant come back to the same place?",
          class = "app-subtitle"),
      div("Radius = 150 m \u00b7 Gap threshold = 24 h. Treating a never-revisited location as \u201cunusual\u201d ",
          "is our reading of the method, not a claim made in the original paper.",
          class = "app-methodnote"),
      
      uiOutput(ns("kpi_cards")),
      
      div(
        class = "map-card",
        tabsetPanel(
          type = "tabs",
          
          tabPanel(
            "Revisited Locations",
            br(),
            leafletOutput(ns("map"), height = "680px")
          ),
          
          tabPanel(
            "Visit-Once Locations",
            br(),
            leafletOutput(ns("visit_once_map"), height = "680px"),
            div(
              "Showing only locations visited exactly once. Use the layer control to toggle each elephant.",
              class = "chart-caption"
            )
          ),
          
          tabPanel(
            "Location Data",
            br(),
            div(class = "dl-btn", downloadButton(ns("download_data"), "Download CSV")),
            DTOutput(ns("location_table"))
          ),
          
          tabPanel(
            "Insights",
            br(),
            uiOutput(ns("interpretation"))
          )
        )
      )
    )
  )
}


# ---------------------------------------------------------------------------
# 4. Module server
# ---------------------------------------------------------------------------
# `data` can be:
#   - NULL (default)      -> reads `data_path` itself (useful when run standalone)
#   - a plain data.frame   -> used as-is (useful when the group leader loads the
#                             shared CSV once and passes it to every member's module)
#   - a reactive expression -> called each time it's needed
#
# `elephants` controls which individuals this tab's map/table/interpretation
# cover — defaults to all 14 tracked elephants.
# `kpi_elephants` controls which of those get a headline KPI card at the top;
# defaults to the original three so the summary strip stays uncluttered while
# the fuller set still appears in the map, table, and interpretation panel.
# `min_burst_days` sets how many days of *unbroken* tracking (no gap longer
# than 72h) an elephant needs before the Insights tab will attempt a real
# behavioural read for it; below that, it gets an honest "not enough
# continuous data" note instead. Default of 10 comes from a natural gap in
# this dataset (longest bursts cluster below ~7 days or above ~10 days,
# nothing in between) — worth re-checking if the underlying data changes.

mod_method4_server <- function(id,
                               data = NULL,
                               data_path = "kaudulla_elephants_clean.csv",
                               elephants = c("Gothami", "recollared female", "female_1",
                                             "Mina", "Talatha", "Rahu", "Dona", "Dewmi",
                                             "Tara Devi", "Kasun", "Wilmini", "Illuk",
                                             "Pazhani", "Damien"),
                               kpi_elephants = c("Gothami", "Rahu", "recollared female"),
                               radius_m = 150,
                               gap_hours = 24,
                               min_burst_days = 10) {
  
  moduleServer(id, function(input, output, session) {
    
    # -- 4.1 Load & prepare data -------------------------------------------
    eleph_raw <- reactive({
      if (!is.null(data)) {
        if (is.reactive(data)) data() else data
      } else {
        validate(need(file.exists(data_path),
                      paste("Can't find", data_path, "- check the working directory.")))
        # datetime is forced to stay as raw text here — otherwise readr's
        # own column-type guesser can auto-parse the ISO-8601 "...Z" string
        # into a POSIXct on its own, and then re-parsing that already-parsed
        # value with ymd_hms() below becomes ambiguous (it round-trips
        # through as.character() first, which is sensitive to the R
        # session's locale/timezone and can silently nudge some
        # timestamps). Keeping it as character here means there is only
        # one explicit, unambiguous parse step, in eleph() below.
        read_csv(data_path, show_col_types = FALSE, col_types = cols(datetime = col_character()))
      }
    })
    
    eleph <- reactive({
      df <- eleph_raw()
      validate(need(all(c("name", "lat", "lon", "datetime") %in% names(df)),
                    "Data is missing one of: name, lat, lon, datetime"))
      df %>%
        mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
        filter(!is.na(lat), !is.na(lon), !is.na(datetime)) %>%
        mutate(hour_bin = floor_date(datetime, "hour")) %>%
        arrange(name, datetime) %>%
        # Collapse to at most one fix per elephant per hour — matches the
        # group's Quarto report pipeline, so revisit counts are computed on
        # the same de-duplicated point set rather than raw sub-hourly fixes.
        distinct(name, hour_bin, .keep_all = TRUE) %>%
        select(-hour_bin) %>%
        filter(name %in% elephants)
    })
    
    # -- 4.2 Compute revisits (once per session, with a progress bar) ------
    hotspots <- reactive({
      df <- eleph()
      validate(need(nrow(df) > 0, "No data available for the selected elephants."))
      
      pieces <- df %>% group_split(name)
      n_pieces <- length(pieces)
      
      withProgress(message = "Calculating revisit patterns\u2026", value = 0, {
        out <- vector("list", n_pieces)
        for (i in seq_len(n_pieces)) {
          out[[i]] <- calc_revisits_fast(pieces[[i]], radius_m, gap_hours)
          incProgress(1 / n_pieces, detail = unique(pieces[[i]]$name))
        }
        bind_rows(out)
      })
    })
    
    # -- 4.3 Per-elephant summary (drives KPI cards + interpretation) -----
    burst_quality <- reactive({
      eleph() %>%
        group_by(name) %>%
        group_modify(~ .burst_stats(.x)) %>%
        ungroup()
    })
    
    hotspot_summary <- reactive({
      hotspots() %>%
        group_by(name) %>%
        summarise(
          n_total     = n(),
          pct_once    = round(100 * mean(revisit == 1), 1),
          max_revisit = max(revisit),
          span_days   = round(as.numeric(difftime(max(datetime), min(datetime), units = "days")), 1),
          .groups = "drop"
        ) %>%
        left_join(burst_quality(), by = "name")
    })
    
    # -- 4.4 KPI cards (fully reactive — no hardcoded numbers) ------------
    # Only the headline elephants get a card here; the rest still show up
    # in the map, table, and interpretation panel below.
    output$kpi_cards <- renderUI({
      s <- hotspot_summary() %>%
        filter(name %in% kpi_elephants) %>%
        arrange(match(name, kpi_elephants))
      
      css_classes <- rep(c("kpi-c1", "kpi-c2", "kpi-c3", "kpi-c4", "kpi-c5", "kpi-c6"), length.out = nrow(s))
      
      cards <- lapply(seq_len(nrow(s)), function(i) {
        .kpi_card(s$name[i], s$n_total[i], s$pct_once[i], s$max_revisit[i], css_classes[i])
      })
      
      div(class = "kpi-row", cards)
    })
    
    # -- 4.5 Plain-English interpretation panel ----------------------------
    output$interpretation <- renderUI({
      s <- hotspot_summary() %>% arrange(desc(max_burst_days))
      
      paragraphs <- lapply(seq_len(nrow(s)), function(i) {
        HTML(paste0(
          "<p>",
          .interpret_text(s$name[i], s$pct_once[i], s$max_revisit[i], s$span_days[i],
                          s$max_burst_days[i], s$n_bursts[i], min_burst_days),
          "</p>"
        ))
      })
      
      n_flagged <- sum(s$max_burst_days < min_burst_days)
      
      div(
        class = "interp-box",
        if (n_flagged > 0) {
          div(
            paste0(
              n_flagged, " of ", nrow(s), " elephants below don't have a long enough unbroken tracking ",
              "stretch (at least ", min_burst_days, " continuous days) for a reliable read, and are ",
              "flagged as such rather than given a behavioural interpretation."
            ),
            class = "interp-caveat",
            style = "margin-bottom: 12px;"
          )
        },
        paragraphs,
        div(
          "Note: these are exploratory read-outs meant to flag places worth a closer look, not confirmed diagnoses.",
          class = "interp-caveat"
        )
      )
    })
    
    # -- 4.6 Colour palette --------------------------------------------------
    pal <- reactive({
      d <- hotspots()
      colorNumeric("YlOrRd", domain = d$revisit)
    })
    
    # -- 4.7 Map: all locations, coloured by revisit count -------------------
    output$map <- renderLeaflet({
      d <- hotspots()
      pal_fun <- pal()
      
      m <- leaflet() %>%
        addProviderTiles("OpenStreetMap", group = "Street Map") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite")
      
      for (sp in unique(d$name)) {
        d_sp <- d %>% filter(name == sp)
        m <- m %>%
          addCircleMarkers(
            data = d_sp,
            lng = ~lon, lat = ~lat,
            group = sp,
            radius = ~ rescale(revisit, to = c(4, 10)),
            stroke = FALSE,
            fillOpacity = 0.85,
            color = ~ pal_fun(revisit),
            popup = ~ paste0(
              "<b>", name, "</b><br>",
              "Revisits: ", revisit, "<br>",
              format(datetime, "%Y-%m-%d %H:%M"), "<br>",
              "<b>Lat:</b> ", round(lat, 5), ", <b>Lon:</b> ", round(lon, 5)
            )
          )
      }
      
      m %>%
        addLegend(position = "bottomright", pal = pal_fun, values = d$revisit,
                  title = "Revisit Count", opacity = 1) %>%
        addLayersControl(baseGroups = c("Street Map", "Satellite"),
                         overlayGroups = unique(d$name),
                         options = layersControlOptions(collapsed = FALSE))
    })
    
    # -- 4.8 Map: locations visited exactly once -----------------------------
    output$visit_once_map <- renderLeaflet({
      d <- hotspots()
      visit_once <- d %>% filter(revisit == 1)
      
      m <- leaflet() %>%
        addProviderTiles("OpenStreetMap", group = "Street Map") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite")
      
      if (nrow(visit_once) == 0) {
        return(m %>% addControl("No one-time-only locations found", position = "topright"))
      }
      
      for (sp in unique(visit_once$name)) {
        dd <- visit_once %>% filter(name == sp)
        m <- m %>%
          addCircleMarkers(
            data = dd,
            lng = ~lon, lat = ~lat,
            group = sp,
            radius = 6, stroke = TRUE, weight = 1,
            color = "black", fillColor = "#2C7FB8", fillOpacity = 0.85,
            popup = ~ paste0(
              "<b>Elephant:</b> ", name,
              "<br><b>Date:</b> ", format(datetime, "%Y-%m-%d %H:%M"),
              "<br><b>Visit Count:</b> ", revisit,
              "<br><b>Status:</b> Visited only once",
              "<br><b>Lat:</b> ", round(lat, 5), ", <b>Lon:</b> ", round(lon, 5)
            )
          )
      }
      
      m %>%
        addLayersControl(baseGroups = c("Street Map", "Satellite"),
                         overlayGroups = unique(visit_once$name),
                         options = layersControlOptions(collapsed = FALSE))
    })
    
    # -- 4.9 Data table + download -------------------------------------------
    location_data <- reactive({
      hotspots() %>%
        transmute(
          Elephant = name,
          Latitude = round(lat, 5),
          Longitude = round(lon, 5),
          `Revisit Count` = revisit,
          `Visit Dates` = visit_dates,
          `First Visit` = format(first_visit, "%Y-%m-%d"),
          `Last Visit`  = format(last_visit, "%Y-%m-%d"),
          `Days Span`   = round(as.numeric(difftime(last_visit, first_visit, units = "days")), 1),
          Status = ifelse(revisit == 1, "Visited once", "Revisited")
        ) %>%
        arrange(Elephant, desc(`Revisit Count`))
    })
    
    output$location_table <- renderDT({
      datatable(
        location_data(),
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 15,
          lengthMenu = c(15, 30, 50, 100),
          dom = "ltip",
          order = list(list(3, "desc"))
        )
      )
    })
    
    output$download_data <- downloadHandler(
      filename = function() paste0("elephant_locations_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(location_data(), file, row.names = FALSE)
    )
  })
}

# ===========================================================================
# STANDALONE RUNNER
# Only needed for testing this tab on its own. When merging into the group's
# combined app, delete everything below this line — the group leader just
# needs to call mod_method4_ui("method4") / mod_method4_server("method4", data)
# from their own ui/server.
# ===========================================================================

if (sys.nframe() == 0 || interactive()) {
  
  ui <- fluidPage(
    mod_method4_ui("method4")
  )
  
  server <- function(input, output, session) {
    mod_method4_server("method4")
  }
  
  shinyApp(ui, server)
}