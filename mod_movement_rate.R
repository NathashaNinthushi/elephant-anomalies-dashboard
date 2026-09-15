# =============================================================================
# mod_movement_rate.R
# Method 2 — Movement rate (Wall et al. 2014), as a self-contained Shiny module.
#
# Merge target: this file exposes ONE UI function and ONE server function that
# drop straight into the team's final combined app:
#
#   nav_panel("Movement rate", mod_movement_rate_ui("movement_rate"))
#   mod_movement_rate_server("movement_rate", shared_data)
#
# where `shared_data` is a reactive() returning the cleaned elephant data frame
# (columns: name, datetime, lat, lon; extra columns are ignored). Everything
# below is namespaced so it will not collide with your teammates' modules.
#
# Method, verbatim from the paper + your report:
#   For a trailing 24-hour window (here: one calendar day), sum the haversine
#   distance between consecutive fixes. Compare each day's total against the
#   elephant's OWN historical distribution and flag days at/below the 1st
#   percentile. Full-history (in-sample) baseline, since we have no confirmed
#   healthy reference period. Cutoff percentile is Wall et al.'s stated 1%.
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(leaflet)
library(DT)

# ---- shared helper ----------------------------------------------------------
# Same haversine as the report. Guarded so that if another module already
# defined it in the combined app, we don't clobber it.
if (!exists("hav_m")) {
  hav_m <- function(lat1, lon1, lat2, lon2) {
    R <- 6371000
    p1 <- lat1 * pi / 180; p2 <- lat2 * pi / 180
    dphi    <- (lat2 - lat1) * pi / 180
    dlambda <- (lon2 - lon1) * pi / 180
    a <- sin(dphi / 2)^2 + cos(p1) * cos(p2) * sin(dlambda / 2)^2
    2 * R * asin(pmin(1, sqrt(a)))
  }
}

# Okabe-Ito orange used for "flagged" in the report, kept for consistency.
MR_FLAG   <- "#D55E00"
MR_NORMAL <- "grey55"

# Don't draw a connecting line across a gap longer than this many days.
# 1 = connect only truly adjacent days; raise it to tolerate short dropouts.
MR_GAP_DAYS <- 1

# ---- core computation (pure functions, no Shiny) ----------------------------

# One elephant -> daily distance table (days with enough coverage only).
mr_daily_distance <- function(sub, min_fixes_per_day = 20) {
  sub %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    mutate(day = as_date(datetime)) %>%
    arrange(datetime) %>%
    group_by(day) %>%
    filter(n() >= min_fixes_per_day) %>%
    summarise(
      n_fix        = n(),
      daily_dist_m = sum(hav_m(lag(lat), lag(lon), lat, lon), na.rm = TRUE),
      .groups = "drop"
    )
}

# All elephants -> daily table with per-elephant percentile threshold + flag.
mr_compute <- function(eleph, min_fixes_per_day = 20, pct = 0.01) {
  names_vec <- unique(eleph$name)
  purrr_map <- lapply(names_vec, function(nm) {
    dd <- mr_daily_distance(dplyr::filter(eleph, name == nm), min_fixes_per_day)
    if (nrow(dd) == 0) return(NULL)
    dd$name <- nm
    dd
  })
  out <- dplyr::bind_rows(purrr_map)
  if (nrow(out) == 0) return(out)
  out %>%
    group_by(name) %>%
    mutate(
      threshold_m = quantile(daily_dist_m, probs = pct, names = FALSE),
      flagged     = daily_dist_m <= threshold_m
    ) %>%
    ungroup() %>%
    arrange(name, day)
}

# =============================================================================
# UI
# =============================================================================
mod_movement_rate_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # --- KPI gradient styling (self-contained, namespaced classes) ----------
    tags$style(HTML("
      .mr-kpi { border: none !important; border-radius: 14px !important;
                box-shadow: 0 4px 14px rgba(0,0,0,.12); overflow: hidden; }
      .mr-kpi, .mr-kpi * { color: #fff !important; }
      .mr-kpi .value-box-title { opacity:.9; font-weight:500; letter-spacing:.02em; }
      .mr-kpi .value-box-value { font-weight:700; }
      .mr-kpi .value-box-showcase { opacity:.85; }
      .mr-kpi-days   { background: linear-gradient(135deg,#2a9d8f,#21807a); }
      .mr-kpi-flag   { background: linear-gradient(135deg,#f3722c,#d1495b); }
      .mr-kpi-thresh { background: linear-gradient(135deg,#3a7ca5,#2c6284); }
      .mr-kpi-median { background: linear-gradient(135deg,#5c6bc0,#3f51b5); }
    ")),

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      title = "Movement rate",
      selectInput(ns("elephant"), "Elephant", choices = NULL),
      sliderInput(ns("pct"), "Alert percentile (%)",
                  min = 0.5, max = 10, value = 1, step = 0.5),
      helpText("Wall et al. use the 1st percentile. Higher = more days flagged."),
      sliderInput(ns("min_fixes"), "Min fixes required per day",
                  min = 5, max = 24, value = 20, step = 1),
      helpText("A 24h distance is only trustworthy if most of the day's hourly",
               "fixes are present."),
      sliderInput(ns("min_days"), "Min qualifying days to list an elephant",
                  min = 10, max = 100, value = 30, step = 5),
      hr(),
      div(class = "text-muted small",
          "Baseline = each elephant's own full history (in-sample). A flagged",
          "day is part of what defines 'low' for that animal, so this is a",
          "weaker, retrospective comparison than Wall et al.'s prospective one.")
    ),

    # top row: summary value boxes
    layout_columns(
      fill = FALSE,
      value_box("Qualifying days", textOutput(ns("vb_days")),
                showcase = bsicons::bs_icon("calendar-check"),
                class = "mr-kpi mr-kpi-days"),
      value_box("Flagged days", textOutput(ns("vb_flagged")),
                showcase = bsicons::bs_icon("exclamation-triangle"),
                class = "mr-kpi mr-kpi-flag"),
      value_box("Alert threshold", textOutput(ns("vb_thresh")),
                showcase = bsicons::bs_icon("rulers"),
                class = "mr-kpi mr-kpi-thresh"),
      value_box("Median daily distance", textOutput(ns("vb_median")),
                showcase = bsicons::bs_icon("graph-up"),
                class = "mr-kpi mr-kpi-median")
    ),

    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Daily distance vs. this elephant's own history"),
        plotlyOutput(ns("ts_plot"), height = "340px")
      ),
      card(
        full_screen = TRUE,
        card_header("Where the bottom slice sits"),
        plotlyOutput(ns("dist_plot"), height = "340px")
      )
    ),

    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("A flagged day vs. her normal range"),
        layout_columns(
          col_widths = c(7, 5),
          uiOutput(ns("flag_day_ui")),
          div(class = "pt-4",
              checkboxInput(ns("show_normal"),
                            "Overlay a typical day for scale", TRUE))
        ),
        leafletOutput(ns("map"), height = "360px")
      ),
      card(
        full_screen = TRUE,
        card_header("Flagged days"),
        DTOutput(ns("tbl"))
      )
    )
  )
  )
}

# =============================================================================
# Server
#   data_r : a reactive() returning the cleaned elephant data frame.
# =============================================================================
mod_movement_rate_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {

    # Defensive prep: coerce, drop NA coords, one fix per hour per elephant.
    eleph <- reactive({
      df <- data_r()
      req(df)
      df %>%
        mutate(
          lat      = suppressWarnings(as.numeric(lat)),
          lon      = suppressWarnings(as.numeric(lon)),
          datetime = if (is.character(datetime)) ymd_hms(datetime, tz = "UTC") else datetime
        ) %>%
        filter(!is.na(lat), !is.na(lon)) %>%
        mutate(hour_bin = floor_date(datetime, "hour")) %>%
        arrange(name, datetime) %>%
        distinct(name, hour_bin, .keep_all = TRUE)
    })

    # Full daily table across all elephants (recomputed when knobs change).
    daily_all <- reactive({
      mr_compute(eleph(), min_fixes_per_day = input$min_fixes, pct = input$pct / 100)
    })

    # Which elephants have enough qualifying days to be worth listing.
    eligible <- reactive({
      da <- daily_all()
      if (nrow(da) == 0) return(character(0))
      da %>% count(name) %>% filter(n >= input$min_days) %>%
        arrange(desc(n)) %>% pull(name)
    })

    # Keep the elephant dropdown in sync with eligibility.
    observeEvent(eligible(), {
      choices <- eligible()
      sel <- if (!is.null(input$elephant) && input$elephant %in% choices) {
        input$elephant
      } else if (length(choices)) choices[1] else character(0)
      updateSelectInput(session, "elephant", choices = choices, selected = sel)
    }, ignoreNULL = FALSE)

    # Selected elephant's daily table.
    sel_daily <- reactive({
      req(input$elephant)
      daily_all() %>% filter(name == input$elephant) %>% arrange(day)
    })

    # ---- value boxes --------------------------------------------------------
    output$vb_days    <- renderText(nrow(sel_daily()))
    output$vb_flagged <- renderText(sum(sel_daily()$flagged))
    output$vb_thresh  <- renderText({
      d <- sel_daily(); if (!nrow(d)) return("--")
      sprintf("%.2f km", d$threshold_m[1] / 1000)
    })
    output$vb_median  <- renderText({
      d <- sel_daily(); if (!nrow(d)) return("--")
      sprintf("%.2f km", median(d$daily_dist_m) / 1000)
    })

    # ---- time series --------------------------------------------------------
    output$ts_plot <- renderPlotly({
      d <- sel_daily(); req(nrow(d) > 0)
      thr <- d$threshold_m[1]
      d <- d %>% arrange(day) %>% mutate(
        km   = daily_dist_m / 1000,
        col  = ifelse(flagged, MR_FLAG, MR_NORMAL),
        tip  = paste0(format(day, "%d %b %Y"),
                      "<br>", round(km, 2), " km",
                      "<br>", n_fix, " fixes",
                      ifelse(flagged, "<br><b>FLAGGED</b>", "")),
        # Start a new line segment whenever the previous qualifying day is more
        # than MR_GAP_DAYS away, so the line never bridges a real data gap.
        seg  = cumsum(c(TRUE, as.numeric(diff(day)) > MR_GAP_DAYS))
      )

      p <- plot_ly()
      # one grey line per unbroken run of days (no connecting across gaps)
      for (s in unique(d$seg)) {
        seg <- d[d$seg == s, ]
        p <- add_lines(p, data = seg, x = ~day, y = ~km,
                       line = list(color = "grey80"),
                       hoverinfo = "none", showlegend = FALSE)
      }
      p %>%
        add_markers(data = d, x = ~day, y = ~km,
                    marker = list(color = ~col, size = 7),
                    text = ~tip, hoverinfo = "text", showlegend = FALSE) %>%
        add_lines(x = range(d$day), y = c(thr, thr) / 1000,
                  line = list(color = MR_FLAG, dash = "dash"),
                  hoverinfo = "none", showlegend = FALSE) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Distance in one day (km)"),
          margin = list(t = 10)
        ) %>% config(displayModeBar = FALSE)
    })

    # ---- distribution -------------------------------------------------------
    output$dist_plot <- renderPlotly({
      d <- sel_daily(); req(nrow(d) > 0)
      thr <- d$threshold_m[1] / 1000
      km  <- d$daily_dist_m / 1000
      plot_ly() %>%
        add_histogram(x = km, nbinsx = 30, marker = list(color = "grey70"),
                      name = "Daily distances", hovertemplate = "%{x:.1f} km: %{y}<extra></extra>") %>%
        layout(
          shapes = list(list(type = "line", x0 = thr, x1 = thr, y0 = 0, y1 = 1,
                             yref = "paper",
                             line = list(color = MR_FLAG, dash = "dash"))),
          annotations = list(list(x = thr, y = 1, yref = "paper",
                                  text = paste0("cutoff ", round(thr, 2), " km"),
                                  showarrow = FALSE, xanchor = "left",
                                  font = list(color = MR_FLAG))),
          xaxis = list(title = "Distance in one day (km)"),
          yaxis = list(title = "Number of days"),
          bargap = 0.05, margin = list(t = 10), showlegend = FALSE
        ) %>% config(displayModeBar = FALSE)
    })

    # ---- map ----------------------------------------------------------------
    # Flagged days for the selected elephant, to populate the picker.
    flagged_days_r <- reactive({
      d <- sel_daily(); sort(d$day[d$flagged])
    })

    output$flag_day_ui <- renderUI({
      fd <- flagged_days_r()
      if (length(fd) == 0)
        return(helpText("No flagged days for this elephant at the current settings."))
      selectInput(session$ns("flag_day"), "Flagged day to inspect",
                  choices = setNames(as.character(fd), format(fd, "%d %b %Y")))
    })

    # A representative "normal" day = the day closest to her median distance.
    normal_day_r <- reactive({
      d <- sel_daily(); req(nrow(d) > 0)
      d$day[which.min(abs(d$daily_dist_m - median(d$daily_dist_m)))]
    })

    output$map <- renderLeaflet({
      req(nrow(sel_daily()) > 0)
      pts <- eleph() %>%
        filter(name == input$elephant) %>%
        mutate(day = as_date(datetime)) %>%
        arrange(datetime)

      # Context = all fixes as faint dots (her overall range), thinned if huge.
      ctx <- if (nrow(pts) > 2500)
        pts[round(seq(1, nrow(pts), length.out = 2500)), ] else pts

      m <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(data = ctx, lng = ~lon, lat = ~lat, radius = 2,
                         stroke = FALSE, fillColor = "grey55", fillOpacity = 0.20)

      bb_lng <- numeric(0); bb_lat <- numeric(0)
      leg_cols <- MR_FLAG; leg_lab <- "Flagged day"

      # Typical day (blue) for scale comparison.
      if (isTRUE(input$show_normal)) {
        nseg <- pts %>% filter(day == normal_day_r())
        if (nrow(nseg) >= 1) {
          m <- m %>% addPolylines(data = nseg, lng = ~lon, lat = ~lat,
                                  weight = 3, color = "#0072B2", opacity = 0.85)
          bb_lng <- c(bb_lng, nseg$lon); bb_lat <- c(bb_lat, nseg$lat)
          leg_cols <- c(leg_cols, "#0072B2"); leg_lab <- c(leg_lab, "Typical day")
        }
      }

      # The selected flagged day (orange), with per-fix time popups.
      fd <- input$flag_day
      if (!is.null(fd) && nzchar(fd)) {
        fseg <- pts %>% filter(as.character(day) == fd)
        if (nrow(fseg) >= 1) {
          m <- m %>%
            addPolylines(data = fseg, lng = ~lon, lat = ~lat,
                         weight = 4, color = MR_FLAG, opacity = 0.95) %>%
            addCircleMarkers(data = fseg, lng = ~lon, lat = ~lat, radius = 4,
                             color = MR_FLAG, weight = 1, fillOpacity = 0.85,
                             popup = ~format(datetime, "%H:%M"))
          bb_lng <- c(bb_lng, fseg$lon); bb_lat <- c(bb_lat, fseg$lat)
        }
      }

      leg_cols <- c(leg_cols, "grey55"); leg_lab <- c(leg_lab, "All fixes (range)")
      m <- m %>% addLegend(position = "bottomleft", colors = leg_cols,
                           labels = leg_lab, opacity = 0.8)

      # Zoom to the flagged + typical day so the size contrast is visible.
      if (length(bb_lng) >= 1) {
        pad <- 0.004
        m <- m %>% fitBounds(min(bb_lng) - pad, min(bb_lat) - pad,
                             max(bb_lng) + pad, max(bb_lat) + pad)
      }
      m
    })

    # ---- table --------------------------------------------------------------
    output$tbl <- renderDT({
      d <- sel_daily() %>% filter(flagged) %>%
        transmute(Date = format(day, "%Y-%m-%d"),
                  `Distance (km)` = round(daily_dist_m / 1000, 2),
                  Fixes = n_fix)
      datatable(d, rownames = FALSE, options = list(pageLength = 8, dom = "tp"),
                caption = "Days at/below this elephant's own bottom percentile.")
    })
  })
}
