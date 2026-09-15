# =============================================================================
# mod_proximity.R
# Method — Proximity anomalies (Wall et al. 2014), as a self-contained Shiny
# module, converted from a standalone app so it drops into the team's
# combined app.R exactly like the other three methods:
#
#   nav_panel("Proximity anomalies", mod_proximity_ui("proximity"))
#   mod_proximity_server("proximity", shared_data)
#
# where `shared_data` is a reactive() returning the cleaned elephant data
# frame (columns: name, datetime, lat, lon; extra columns are ignored).
# Everything below is namespaced so it will not collide with the other
# modules, and every helper function is prefixed px_ for the same reason.
#
# Method, in one line: Wall et al.'s proximity rule (haversine distance
# between two elephants, flagged below a chosen threshold) kept exactly as
# published, then a simple absolute-threshold rule decides when a real
# period of togetherness followed by a separation is worth a second look —
# see the "How this works" tab inside this module for the full write-up,
# plain-language first, references and equations in a collapsible technical
# section underneath.
#
# Three tabs: Data coverage, Relationships (a full pairwise heatmap), and
# Separation (pick any two elephants, click a diamond/square on the map or
# pick from the separation-events list to narrow in on one event). Year/month
# in the sidebar only restrict the Separation tab's calculations and map,
# Data coverage and Relationships always show every elephant/pair regardless.
# =============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(scales)

# ---- shared helper ----------------------------------------------------------
# Same haversine used by the other modules. Guarded so that if another module
# already defined it in the combined app, we don't clobber it.
if (!exists("hav_m")) {
  hav_m <- function(lat1, lon1, lat2, lon2) {
    R <- 6371000
    p1 <- lat1 * pi / 180; p2 <- lat2 * pi / 180
    dphi <- (lat2 - lat1) * pi / 180
    dlambda <- (lon2 - lon1) * pi / 180
    a <- sin(dphi / 2)^2 + cos(p1) * cos(p2) * sin(dlambda / 2)^2
    2 * R * asin(pmin(1, sqrt(a)))
  }
}

# Formats a metre distance as "412 m" or "3.2 km", whichever reads better.
px_fmt_dist_m <- function(d_m) {
  if (is.na(d_m)) return("unknown")
  if (d_m >= 1000) paste0(round(d_m / 1000, 1), " km") else paste0(round(d_m), " m")
}

## ---- Shape markers -----------------------------------------------------
## leaflet's R package can't colour a divIcon per row directly, so instead
## each distinct (colour, shape) combination is drawn once as a tiny SVG
## file (diamond = last together, square = during separation) and cached to
## disk; leaflet then base64-embeds the file into the map widget the same
## way it would any other local icon image. Cheap since there's only ever a
## handful of elephant colours times 2 shapes, never one file per point.
px_icon_dir <- file.path(tempdir(), "px_shape_icons")

px_shape_icon_file <- function(color, shape) {
  if (!dir.exists(px_icon_dir)) dir.create(px_icon_dir, recursive = TRUE)
  fname <- file.path(px_icon_dir, paste0(shape, "_", gsub("[^A-Za-z0-9]", "", color), ".svg"))
  if (!file.exists(fname)) {
    svg <- if (shape == "diamond") {
      sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18"><rect x="4" y="4" width="10" height="10" fill="%s" stroke="#222" stroke-width="1.5" transform="rotate(45 9 9)"/></svg>', color)
    } else {
      sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18"><rect x="3" y="3" width="12" height="12" fill="%s" stroke="#222" stroke-width="1.5"/></svg>', color)
    }
    writeLines(svg, fname)
  }
  fname
}

## ---- On-map legend -------------------------------------------------------
## Builds the HTML for the leaflet addControl() legend. Always shows the two
## elephant colours and the diamond/square shape key; the 4 line colours are
## added only in the narrowed (single-event) view.
px_legend_html <- function(pal_colors, both, show_lines = FALSE) {
  dot <- function(color) sprintf(
    "<span style='display:inline-block;width:10px;height:10px;background:%s;margin-right:5px;border-radius:50%%;'></span>", color)
  diamond_sw <- "<span style='display:inline-block;width:9px;height:9px;background:#888;border:1px solid #222;transform:rotate(45deg);margin-right:6px;'></span>"
  square_sw  <- "<span style='display:inline-block;width:10px;height:10px;background:#888;border:1px solid #222;margin-right:5px;'></span>"
  line_sw <- function(color, dashed) {
    style <- if (dashed) "border-top:3px dashed" else "border-top:3px solid"
    sprintf("<span style='display:inline-block;width:20px;%s %s;margin-right:5px;vertical-align:middle;'></span>", style, color)
  }
  rows <- c(
    sprintf("<div>%s%s</div>", dot(unname(pal_colors[both[1]])), both[1]),
    sprintf("<div>%s%s</div>", dot(unname(pal_colors[both[2]])), both[2]),
    sprintf("<div style='margin-top:4px;'>%sLast together</div>", diamond_sw),
    sprintf("<div>%sDuring separation</div>", square_sw)
  )
  if (show_lines) {
    rows <- c(rows,
              sprintf("<div style='margin-top:4px;'>%sDistance when together</div>", line_sw("#2ca02c", TRUE)),
              sprintf("<div>%sDistance now</div>", line_sw("#d62728", TRUE)),
              sprintf("<div>%s%s's path</div>", line_sw(unname(pal_colors[both[1]]), FALSE), both[1]),
              sprintf("<div>%s%s's path</div>", line_sw(unname(pal_colors[both[2]]), FALSE), both[2])
    )
  }
  paste0("<div style='font-size:12px; line-height:1.5;'>", paste(rows, collapse = ""), "</div>")
}

## Filters a points tibble down to rows whose local (Sri Lanka) hour-of-day
## falls within hour_range. Display-only filter, used just before points are
## handed to leaflet, never upstream of any encounter/separation math.
px_filter_hour <- function(pts, hour_range) {
  if (nrow(pts) == 0 || is.null(hour_range)) return(pts)
  local_h <- hour(with_tz(pts$datetime, "Asia/Colombo"))
  pts[local_h >= hour_range[1] & local_h <= hour_range[2], ]
}

## Adds a polyline between two single-row point tibbles, silently skipping
## if either point is missing (e.g. no GPS fix close enough in time to draw
## from) rather than erroring the whole map.
px_add_line <- function(m, p1, p2, ...) {
  if (nrow(p1) == 1 && nrow(p2) == 1) {
    addPolylines(m, lng = c(p1$lon, p2$lon), lat = c(p1$lat, p2$lat), ...)
  } else m
}

# Minimum real GPS fixes an elephant needs before it's used in the
# Relationships/Data coverage tabs' "enough data" judgement. Doesn't depend
# on the data itself, so it's safe as a plain constant.
PX_MIN_FIXES <- 200

# =============================================================================
# Core computation (pure functions, no Shiny; every name prefixed px_ so it
# can't collide with a teammate's function of the same shape)
# =============================================================================

## Wall et al.'s proximity rule, applied to every pair present in the same
## hour_bin: d(A,B) = haversine distance; close = d(A,B) < threshold.
px_build_pairwise <- function(df, threshold_m) {
  by_hour <- df %>% group_by(hour_bin) %>% filter(n() >= 2) %>% group_split()
  if (length(by_hour) == 0) {
    return(tibble(pair = character(), name_a = character(), name_b = character(),
                  hour_bin = as.POSIXct(character()), d_m = numeric(), close = logical()))
  }
  map_dfr(by_hour, function(g) {
    n <- nrow(g); idx <- combn(seq_len(n), 2)
    tibble(hour_bin = g$hour_bin[1], name_a = g$name[idx[1, ]], name_b = g$name[idx[2, ]],
           d_m = hav_m(g$lat[idx[1, ]], g$lon[idx[1, ]], g$lat[idx[2, ]], g$lon[idx[2, ]]))
  }) %>%
    mutate(pair = paste(pmin(name_a, name_b), pmax(name_a, name_b), sep = " & "),
           close = d_m < threshold_m) %>%
    select(pair, name_a, name_b, hour_bin, d_m, close) %>%
    arrange(pair, hour_bin)
}

## Groups consecutive "close" hour_bins for one pair into encounters,
## tolerating small gaps (a missed simultaneous fix) up to max_gap_h.
px_segment_encounters <- function(pair_df, max_gap_h = 2) {
  pair_df <- pair_df %>% arrange(hour_bin)
  n <- nrow(pair_df)
  if (n == 0) return(tibble())
  close <- pair_df$close; hb <- pair_df$hour_bin
  out <- list(); i <- 1
  while (i <= n) {
    if (!close[i]) { i <- i + 1; next }
    j <- i
    repeat {
      if (j + 1 > n) break
      gap_h <- as.numeric(difftime(hb[j + 1], hb[j], units = "hours"))
      if (!close[j + 1] || gap_h > max_gap_h) break
      j <- j + 1
    }
    dur_h <- as.numeric(difftime(hb[j], hb[i], units = "hours")) + 1
    out[[length(out) + 1]] <- tibble(start = hb[i], end = hb[j], duration_h = dur_h, n_bins = j - i + 1)
    i <- j + 1
  }
  bind_rows(out)
}

## Full pipeline: proximity -> pair overview. Separation-event flagging uses
## a simple, user-adjustable absolute threshold applied on demand per pair
## (see px_pair_analysis / the server code below), not computed here, so
## changing it doesn't require recomputing the whole pipeline, and it isn't
## gated by a pair having "enough" history first.
px_run_pipeline <- function(eleph_all, keep_names, threshold_m, MIN_OVERVIEW_HOURS = 20) {
  
  all_names <- sort(unique(eleph_all$name))
  fix_counts <- eleph_all %>% count(name, sort = TRUE)
  
  ## Built once across ALL elephants, not just the qualifying ones, so the
  ## overview table can show every pair honestly, including ones involving
  ## low-fix elephants.
  pw_all <- px_build_pairwise(eleph_all, threshold_m)
  
  empty <- tibble()
  if (nrow(pw_all) == 0) {
    return(list(pair_overview = empty, threshold_m = threshold_m))
  }
  
  ## ---- Full pair overview: every possible pair, every elephant, no hiding ----
  pair_grid <- as_tibble(t(combn(sort(all_names), 2)), .name_repair = "minimal")
  names(pair_grid) <- c("name_a", "name_b")
  pair_grid <- pair_grid %>% mutate(pair = paste(name_a, name_b, sep = " & "))
  
  observed <- pw_all %>%
    group_by(pair) %>%
    summarise(shared_hours = n(), close_hours = sum(close), .groups = "drop")
  
  pair_overview <- pair_grid %>%
    left_join(observed, by = "pair") %>%
    mutate(shared_hours = coalesce(shared_hours, 0L),
           close_hours = coalesce(close_hours, 0L),
           close_rate_pct = ifelse(shared_hours > 0, round(100 * close_hours / shared_hours, 1), NA_real_)) %>%
    left_join(fix_counts %>% rename(name_a = name, n_fixes_a = n), by = "name_a") %>%
    left_join(fix_counts %>% rename(name_b = name, n_fixes_b = n), by = "name_b") %>%
    mutate(
      both_qualify = name_a %in% keep_names & name_b %in% keep_names,
      data_confidence = case_when(
        shared_hours == 0 ~ "Never tracked at the same time",
        !both_qualify | shared_hours < MIN_OVERVIEW_HOURS ~ "Too little data to draw conclusions",
        TRUE ~ "Enough data to compare"
      )
    ) %>%
    arrange(desc(shared_hours)) %>%
    select(name_a, n_fixes_a, name_b, n_fixes_b, shared_hours, close_hours, close_rate_pct, data_confidence)
  
  list(pair_overview = pair_overview, threshold_m = threshold_m)
}

## Finds every qualifying "bond" (a continuous close encounter lasting at
## least min_bond_hours) and reports how long the pair then spent apart
## afterward, until they reunited or tracking ran out. No statistical
## baseline involved, this is the same simple rule for every pair.
px_compute_separation_events <- function(encounters, min_bond_hours, last_tracked_time) {
  if (nrow(encounters) == 0) return(tibble())
  encounters <- encounters %>% arrange(start)
  qualifying <- encounters %>% filter(duration_h >= min_bond_hours)
  if (nrow(qualifying) == 0) return(tibble())
  events <- vector("list", nrow(qualifying))
  for (i in seq_len(nrow(qualifying))) {
    bond_end <- qualifying$end[i]
    next_start <- encounters$start[encounters$start > bond_end]
    if (length(next_start) > 0) {
      apart_end <- min(next_start); ongoing <- FALSE
    } else {
      apart_end <- last_tracked_time; ongoing <- TRUE
    }
    events[[i]] <- tibble(bond_start = qualifying$start[i], bond_end = bond_end,
                          bond_duration_h = qualifying$duration_h[i],
                          apart_start = bond_end, apart_end = apart_end,
                          apart_duration_h = as.numeric(difftime(apart_end, bond_end, units = "hours")),
                          ongoing = ongoing)
  }
  bind_rows(events)
}

## Finds the fix of one elephant closest in time to a target moment. Used to
## answer "where was the partner at roughly this same time", since the two
## elephants are rarely tracked at the exact same second.
px_nearest_fix <- function(eleph_all, target_name, target_time) {
  sub <- eleph_all %>% filter(name == target_name)
  if (nrow(sub) == 0 || is.na(target_time)) return(NULL)
  idx <- which.min(abs(as.numeric(difftime(sub$datetime, target_time, units = "secs"))))
  sub[idx, ]
}

## The 4 reference points for a separation event, each tagged with which
## elephant it is (matching the order the two dropdowns were picked in, "A"
## = elephant_1, "B" = elephant_2) and which phase it's from ("last_together"
## or "separated"). Tagging by elephant + phase (instead of an ad-hoc role
## string) is what lets the map draw "this elephant's path" and "distance
## between them" lines without guessing which point belongs to which animal.
##
## If no specific moment was clicked (e.g. picked from the event list, or
## navigating via Next/Previous instead of clicking a marker), the first fix
## recorded in the apart window stands in for "the moment being viewed",
## which in practice means the hour right after they separated.
px_four_points_separation <- function(eleph_all, event, both, clicked_name = NULL, clicked_time = NULL) {
  if (is.null(clicked_name) || is.null(clicked_time)) {
    cand <- eleph_all %>% filter(name %in% both, datetime > event$apart_start, datetime <= event$apart_end) %>%
      arrange(datetime)
    if (nrow(cand) == 0) { clicked_name <- both[1]; clicked_time <- event$apart_end }
    else { clicked_name <- cand$name[1]; clicked_time <- cand$datetime[1] }
  }
  partner_name <- setdiff(both, clicked_name)[1]
  
  p_last_a <- px_nearest_fix(eleph_all, both[1], event$bond_end)
  p_last_b <- px_nearest_fix(eleph_all, both[2], event$bond_end)
  p_now_clicked <- px_nearest_fix(eleph_all, clicked_name, clicked_time)
  p_now_partner <- px_nearest_fix(eleph_all, partner_name, clicked_time)
  
  ## Tag each point with which elephant it is ("A"/"B", whichever it turns
  ## out to be, since the person could have clicked either one's marker) and
  ## which phase it's from. Guarded so a missing fix (NULL) never gets piped
  ## into mutate(), which would error.
  tag_pt <- function(pt, phase) {
    if (is.null(pt)) return(NULL)
    pt %>% mutate(which = ifelse(name == both[1], "A", "B"), phase = phase)
  }
  
  bind_rows(
    tag_pt(p_last_a, "last_together"),
    tag_pt(p_last_b, "last_together"),
    tag_pt(p_now_clicked, "separated"),
    tag_pt(p_now_partner, "separated")
  )
}

## Analyses any two elephants directly, with zero gating on whether they
## "qualify". The choice of pair is entirely the user's; this function only
## reports, honestly, how much data backs that choice, it never decides which
## pairs are worth offering in the first place.
##
## sel_years / sel_months restrict which GPS readings are used at all, before
## anything is calculated.
px_pair_analysis <- function(eleph_all, name_a, name_b, threshold_m,
                             sel_years = NULL, sel_months = NULL,
                             min_bond_hours = 6, max_gap_h = 2) {
  sub_full <- eleph_all %>% filter(name %in% c(name_a, name_b))
  true_last_tracked_time <- if (nrow(sub_full) > 0) max(sub_full$datetime) else as.POSIXct(NA)
  
  sub <- sub_full
  if (!is.null(sel_years))  sub <- sub %>% filter(year %in% as.integer(sel_years))
  if (!is.null(sel_months)) sub <- sub %>% filter(as.character(month) %in% sel_months)
  
  a <- sub %>% filter(name == name_a) %>% select(hour_bin, lat, lon)
  b <- sub %>% filter(name == name_b) %>% select(hour_bin, lat, lon)
  joined <- inner_join(a, b, by = "hour_bin", suffix = c("_a", "_b"))
  
  if (nrow(joined) == 0) {
    return(list(status_overall = "never_tracked_together",
                encounters = tibble(), separation_events = tibble(),
                total_shared_hours = 0))
  }
  joined <- joined %>% mutate(d_m = hav_m(lat_a, lon_a, lat_b, lon_b), close = d_m < threshold_m)
  
  encounters <- px_segment_encounters(joined %>% arrange(hour_bin), max_gap_h = max_gap_h)
  window_last_time <- max(joined$hour_bin)
  separation_events <- px_compute_separation_events(encounters, min_bond_hours, window_last_time)
  if (nrow(separation_events) > 0) {
    ## An "ongoing" separation only means something if we actually ran out of
    ## real tracking data. If it's ongoing only because the year/month filter
    ## window ended, that's the filter talking, not the elephants, so it gets
    ## labelled differently rather than implying a confirmed real separation.
    separation_events <- separation_events %>%
      mutate(window_limited = ongoing & (window_last_time < true_last_tracked_time))
  }
  
  list(status_overall = "tracked_together",
       encounters = encounters, separation_events = separation_events,
       total_shared_hours = nrow(joined))
}

# =============================================================================
# UI
# =============================================================================
mod_proximity_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        checkboxGroupInput(ns("sel_years"), "Year(s)", choices = NULL, inline = TRUE),
        checkboxGroupInput(ns("sel_months"), "Month(s)", choices = NULL, inline = TRUE),
        tags$p(style = "font-size: 12px; color: #666;",
               tags$b("Only affects the Separation tab."), " Year and month narrow down which",
               " GPS readings are used at all there, before anything is calculated (Data",
               " coverage and Relationships always show every elephant/pair). If a \"still",
               " apart\" result happens to land right at the edge of your selection, it's",
               " labelled as such rather than implied to be a confirmed, ongoing separation."),
        tags$hr(),
        numericInput(ns("threshold_m"), "How close counts as \"together\" (metres)",
                     value = 500, min = 50, step = 50),
        actionButton(ns("recalc"), "Update with new distance", class = "btn-primary"),
        tags$p(style = "font-size: 12px; color: #666; margin-top: 10px;",
               "Change the distance above and click \"Update with new distance\" to redo",
               " the Relationships heatmap at a new distance."),
        tags$hr(),
        uiOutput(ns("coverage_note")),
        tags$hr(),
        tags$p(style = "font-size: 10px; color: #bbb;",
               paste0("App started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      ". If you've just edited this file, fully stop and re-run the app",
                      " (not just refresh the browser) and check this timestamp updates."))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Data coverage", br(),
                   tags$p(style = "color:#444;",
                          "How many real GPS readings each elephant has. Elephants below the",
                          " dashed line don't have enough readings for the flagging in other tabs",
                          " to say anything reliable, that's a statement about how much data we",
                          " have, not about the elephant."),
                   plotlyOutput(ns("coverage_plot"), height = "500px"),
                   tags$details(tags$summary(style = "cursor:pointer; margin-top:10px;", "Exact numbers"),
                                DTOutput(ns("coverage_table")))),
          tabPanel("Relationships", br(),
                   tags$p(style = "color:#444;",
                          "Every pair of elephants, how close they are colour-coded from white",
                          " (never close) to dark red (close most of the time). Grey means we",
                          " don't have enough data to say anything for that pair. Hover any cell",
                          " for the exact numbers, for example, Talatha and the recollared female",
                          " were tracked together for 443 hours and were never once close enough to",
                          " count as together, a real \"no relationship\" finding, not a gap in the data."),
                   plotlyOutput(ns("relationship_heatmap"), height = "600px"),
                   tags$details(tags$summary(style = "cursor:pointer; margin-top:10px;", "Exact numbers, every pair"),
                                DTOutput(ns("overview_table")))),
          tabPanel("Separation", br(),
                   fluidRow(
                     column(4, selectInput(ns("elephant_1"), "Select elephant 1", choices = NULL)),
                     column(4, selectInput(ns("elephant_2"), "Select elephant 2", choices = NULL)),
                     column(4, numericInput(ns("min_bond_hours"),
                                            "Hours together before it counts as real time spent together",
                                            value = 6, min = 1, step = 1))
                   ),
                   tags$p(style = "color:#444;",
                          "Pick any two elephants, any combination, this list is never filtered by",
                          " how much data they have. If there isn't enough to flag anything reliably,",
                          " that's stated plainly below rather than the pair being left off the list."),
                   uiOutput(ns("pair_data_status")),
                   actionButton(ns("toggle_events_panel"), "Show separation events",
                                class = "btn-sm btn-outline-secondary", style = "margin-bottom: 10px;"),
                   uiOutput(ns("event_picker_ui")),
                   uiOutput(ns("event_nav_controls")),
                   tags$p(style = "color:#444;",
                          "Every recorded location for both elephants. ",
                          tags$span(style = "font-weight:600;", "\u25C6 Diamonds"),
                          " mark both elephants at the last moment they were together, right before a",
                          " separation. ", tags$span(style = "font-weight:600;", "\u25A0 Squares"),
                          " mark where each elephant was while they were apart afterward. Plain dots are",
                          " ordinary GPS readings with nothing flagged. Click a diamond or square (or",
                          " pick a separation from the list above) to narrow the map down to that one",
                          " event: which separation it is, how many hours after the split the point you",
                          " picked falls, and how far apart the pair was at that moment. If this pair",
                          " never spent enough time together to check, the raw points are still shown,",
                          " just as plain dots."),
                   uiOutput(ns("hour_slider_ui")),
                   div(style = "position: relative;",
                       uiOutput(ns("map_overlay")),
                       leafletOutput(ns("togetherness_map"), height = "550px")
                   ),
                   tags$details(tags$summary(style = "cursor:pointer; margin-top:10px;", "Exact numbers for this pair"),
                                tags$h5("Times they were close, then moved apart"),
                                DTOutput(ns("separation_table")))),
          tabPanel("How this works", br(), uiOutput(ns("method_text")))
        )
      )
    )
  )
}

# =============================================================================
# Server
#   data_r : a reactive() returning the cleaned elephant data frame
#            (columns: name, datetime, lat, lon at minimum).
# =============================================================================
mod_proximity_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Same further prep every module does on top of the shared data: hour
    ## bins, dedup to one fix per elephant per hour, and this module's own
    ## local-time year/month/hour-of-day columns.
    eleph_all <- reactive({
      req(data_r())
      data_r() %>%
        mutate(hour_bin = floor_date(datetime, "hour")) %>%
        arrange(name, datetime) %>%
        distinct(name, hour_bin, .keep_all = TRUE) %>%
        mutate(
          local_dt    = with_tz(datetime, "Asia/Colombo"),
          year        = year(local_dt),
          month       = month(local_dt, label = TRUE, abbr = TRUE),
          hour_of_day = hour(local_dt)
        )
    })
    
    fix_counts_r       <- reactive({ eleph_all() %>% count(name, sort = TRUE) })
    all_names_r        <- reactive({ fix_counts_r()$name })
    qualifying_names_r <- reactive({ fix_counts_r() %>% filter(n >= PX_MIN_FIXES) %>% pull(name) })
    year_choices_r      <- reactive({ sort(unique(eleph_all()$year)) })
    month_choices_r     <- reactive({ levels(eleph_all()$month) })
    ## Stable colour per elephant, independent of which are currently selected.
    elephant_colors_r  <- reactive({
      nm <- all_names_r()
      setNames(scales::hue_pal()(length(nm)), nm)
    })
    
    ## The UI can't know the elephant/year/month list when it's first built
    ## (no data available yet at that point), so the dropdowns start empty
    ## and get filled in here, once, as soon as the shared data is ready.
    ## Same pattern the Movement rate and Immobility modules use.
    observeEvent(eleph_all(), {
      nm <- all_names_r()
      updateCheckboxGroupInput(session, "sel_years", choices = year_choices_r(), selected = year_choices_r())
      updateCheckboxGroupInput(session, "sel_months", choices = month_choices_r(), selected = month_choices_r())
      updateSelectInput(session, "elephant_1", choices = nm, selected = nm[1])
      updateSelectInput(session, "elephant_2", choices = nm,
                        selected = if (length(nm) >= 2) nm[2] else nm[1])
    }, once = TRUE)
    
    output$coverage_note <- renderUI({
      req(qualifying_names_r())
      tags$p(style = "font-size: 11px; color: #999;",
             paste0("Flagging needs enough data to mean anything, so it only runs on ",
                    length(qualifying_names_r()), " of the ", length(all_names_r()),
                    " elephants and on pairs with enough shared tracking. Every elephant",
                    " and every pair are still shown, with an honest note on how much data",
                    " backs them, in the \"Data coverage\" and \"Relationships\" tabs."))
    })
    
    ## Runs once as soon as the shared data is ready (default tau = 500m) and
    ## again whenever "Update with new distance" is clicked with a new value.
    pipeline <- eventReactive(list(input$recalc, eleph_all()), {
      req(eleph_all(), qualifying_names_r(), input$threshold_m)
      px_run_pipeline(eleph_all(), qualifying_names_r(), threshold_m = input$threshold_m)
    }, ignoreNULL = FALSE)
    
    ## ---- Data coverage: visual ----
    output$coverage_plot <- renderPlotly({
      df <- fix_counts_r() %>% mutate(qualifies = ifelse(n >= PX_MIN_FIXES, "Enough data", "Too little data"))
      p <- ggplot(df, aes(x = reorder(name, n), y = n, fill = qualifies,
                          text = paste0(name, ": ", n, " real GPS fixes"))) +
        geom_col() +
        geom_hline(yintercept = PX_MIN_FIXES, linetype = "dashed", color = "grey40") +
        coord_flip() +
        scale_fill_manual(values = c("Enough data" = "#0072B2", "Too little data" = "grey70")) +
        labs(x = NULL, y = "Real GPS fixes", fill = NULL) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = "text")
    })
    
    output$coverage_table <- renderDT({
      fix_counts_r() %>%
        mutate(qualifies = ifelse(n >= PX_MIN_FIXES, "Yes", "No")) %>%
        rename(elephant = name, `real GPS fixes` = n, `enough for anomaly detection?` = qualifies) %>%
        datatable(rownames = FALSE, options = list(pageLength = 14, order = list(1, "desc")))
    })
    
    ## ---- Relationships: visual heatmap ----
    output$relationship_heatmap <- renderPlotly({
      overview <- pipeline()$pair_overview
      validate(need(nrow(overview) > 0, "No pairwise data available."))
      heat_data <- bind_rows(
        overview %>% transmute(row = name_a, col = name_b, close_rate_pct, data_confidence, shared_hours),
        overview %>% transmute(row = name_b, col = name_a, close_rate_pct, data_confidence, shared_hours)
      ) %>%
        mutate(tip = paste0(row, " & ", col, "<br>", shared_hours, " shared hours<br>",
                            ifelse(is.na(close_rate_pct), data_confidence, paste0(close_rate_pct, "% close")),
                            "<br>", data_confidence))
      
      p <- ggplot(heat_data, aes(x = col, y = row, fill = close_rate_pct, text = tip)) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_gradient(low = "#fff5f0", high = "#a50f15", na.value = "grey85", name = "% close") +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p, tooltip = "text")
    })
    
    output$overview_table <- renderDT({
      d <- pipeline()$pair_overview
      validate(need(nrow(d) > 0, "No pairwise data available."))
      d %>%
        rename(`elephant A` = name_a, `A's fixes` = n_fixes_a,
               `elephant B` = name_b, `B's fixes` = n_fixes_b,
               `shared hrs` = shared_hours, `close hrs` = close_hours,
               `close rate (%)` = close_rate_pct, `data confidence` = data_confidence) %>%
        datatable(rownames = FALSE, filter = "top",
                  options = list(pageLength = 20, order = list(4, "desc")))
    })
    
    ## ---- Togetherness over time: any two elephants, chosen directly, no data-based gating ----
    pair_result <- reactive({
      req(input$elephant_1, input$elephant_2, input$min_bond_hours, eleph_all(),
          input$sel_years, input$sel_months)
      validate(need(input$elephant_1 != input$elephant_2, "Pick two different elephants."))
      validate(need(length(input$sel_years) > 0, "Select at least one year."))
      validate(need(length(input$sel_months) > 0, "Select at least one month."))
      px_pair_analysis(eleph_all(), input$elephant_1, input$elephant_2, pipeline()$threshold_m,
                       sel_years = input$sel_years, sel_months = input$sel_months,
                       min_bond_hours = input$min_bond_hours)
    })
    
    ## The ordered list of separation events for the current pair. Reused by
    ## the click handler, the event-picker table, and the Next/Previous
    ## buttons, so all three always agree on the same ordering.
    current_events_list <- reactive({
      pair_result()$separation_events %>% arrange(bond_end)
    })
    
    ## Which event is currently narrowed-into, tracked by its bond_end time
    ## rather than by row position. A position (1, 2, 3...) can silently end
    ## up pointing at a different event if the underlying list ever changes
    ## shape between when it was picked and when it's read again; bond_end is
    ## unique per event for a given pair, so looking it back up by value
    ## always finds the same event, or correctly finds none.
    selected_event_key <- reactiveVal(NULL)
    selected_click <- reactiveVal(NULL)
    
    ## Switching pair invalidates whatever was selected, the event list
    ## itself is different now.
    observeEvent(list(input$elephant_1, input$elephant_2), {
      selected_event_key(NULL)
      selected_click(NULL)
    })
    
    observeEvent(input$togetherness_map_marker_click, {
      click <- input$togetherness_map_marker_click
      req(click$id)
      parts <- strsplit(click$id, "\\|\\|")[[1]]
      if (length(parts) != 3 || parts[1] != "pt") return()
      clicked_name <- parts[2]
      clicked_time <- ymd_hm(parts[3], tz = "UTC")
      
      events <- current_events_list()
      if (nrow(events) == 0) return()
      
      idx <- which(events$apart_start <= clicked_time & events$apart_end >= clicked_time)
      if (length(idx) == 0) return()
      selected_event_key(events$bond_end[idx[1]])
      selected_click(list(name = clicked_name, time = clicked_time))
    })
    
    observeEvent(input$show_all_points, {
      selected_event_key(NULL)
      selected_click(NULL)
    })
    observeEvent(input$next_event, {
      events <- current_events_list(); key <- selected_event_key()
      req(nrow(events) > 0, !is.null(key))
      pos <- match(key, events$bond_end)
      if (is.na(pos)) return()
      selected_event_key(events$bond_end[if (pos < nrow(events)) pos + 1 else 1])
      selected_click(NULL)
    })
    observeEvent(input$prev_event, {
      events <- current_events_list(); key <- selected_event_key()
      req(nrow(events) > 0, !is.null(key))
      pos <- match(key, events$bond_end)
      if (is.na(pos)) return()
      selected_event_key(events$bond_end[if (pos > 1) pos - 1 else nrow(events)])
      selected_click(NULL)
    })
    
    ## ---- Overhaul 1: "Show separation events" toggle + clickable list ----
    show_events_panel <- reactiveVal(FALSE)
    observeEvent(input$toggle_events_panel, {
      show_events_panel(!show_events_panel())
      updateActionButton(session, "toggle_events_panel",
                         label = if (show_events_panel()) "Hide separation events" else "Show separation events")
    })
    
    output$event_picker_ui <- renderUI({
      req(show_events_panel())
      events <- current_events_list()
      if (nrow(events) == 0) return(NULL)
      DTOutput(ns("event_picker_table"))
    })
    
    output$event_picker_table <- renderDT({
      events <- current_events_list()
      validate(need(nrow(events) > 0, "No separation events for this pair yet."))
      events %>%
        mutate(event = row_number()) %>%
        transmute(event, `together ended (Sri Lanka time)` = format(with_tz(bond_end, "Asia/Colombo"), "%Y-%m-%d %H:%M"),
                  `hours together` = round(bond_duration_h, 1)) %>%
        datatable(rownames = FALSE, selection = "single",
                  options = list(dom = "t", pageLength = 15))
    })
    
    ## Paging between events belongs with the list of events, not floating on
    ## the map, so it lives here rather than in the map overlay. Only shown
    ## once something is actually narrowed-into.
    output$event_nav_controls <- renderUI({
      req(!is.null(selected_event_key()))
      tags$div(style = "margin-bottom: 10px;",
               actionButton(ns("prev_event"), "< Previous", class = "btn-sm btn-default"),
               actionButton(ns("next_event"), "Next >", class = "btn-sm btn-default"))
    })
    
    observeEvent(input$event_picker_table_rows_selected, {
      sel <- input$event_picker_table_rows_selected
      req(length(sel) == 1)
      events <- current_events_list()
      req(sel <= nrow(events))
      selected_event_key(events$bond_end[sel])
      selected_click(NULL)
    })
    
    ## ---- Single source of truth for "what's currently narrowed-into" ----
    ## Both the map and the overlay panel read from this, instead of each
    ## re-deriving the event independently, so they can never disagree.
    narrowed_view <- reactive({
      key <- selected_event_key()
      if (is.null(key)) return(NULL)
      events <- current_events_list()
      if (nrow(events) == 0) return(NULL)
      pos <- match(key, events$bond_end)
      if (is.na(pos)) return(NULL)
      ev <- events[pos, ]
      both <- c(input$elephant_1, input$elephant_2)
      click <- selected_click()
      
      pts <- px_four_points_separation(eleph_all(), ev, both,
                                       clicked_name = click$name, clicked_time = click$time)
      if (nrow(pts) == 0) return(NULL)
      
      now_a  <- pts %>% filter(which == "A", phase == "separated")
      now_b  <- pts %>% filter(which == "B", phase == "separated")
      last_a <- pts %>% filter(which == "A", phase == "last_together")
      last_b <- pts %>% filter(which == "B", phase == "last_together")
      
      now_dist_m  <- if (nrow(now_a) == 1 && nrow(now_b) == 1) hav_m(now_a$lat, now_a$lon, now_b$lat, now_b$lon) else NA_real_
      last_dist_m <- if (nrow(last_a) == 1 && nrow(last_b) == 1) hav_m(last_a$lat, last_a$lon, last_b$lat, last_b$lon) else NA_real_
      
      ## The moment being viewed: whichever point was actually clicked, or
      ## (no click, e.g. picked from the event list) the earlier of the two
      ## "separated" fixes, i.e. the hour right after they split.
      clicked_time_actual <- if (!is.null(click)) click$time else suppressWarnings(min(c(now_a$datetime, now_b$datetime)))
      hours_since <- as.numeric(difftime(clicked_time_actual, ev$bond_end, units = "hours"))
      
      list(pos = pos, n_total = nrow(events), ev = ev, pts = pts, both = both,
           last_a = last_a, last_b = last_b, now_a = now_a, now_b = now_b,
           now_dist_m = now_dist_m, last_dist_m = last_dist_m, hours_since = hours_since)
    })
    
    ## ---- Map-local hour-of-day filter (display only) ----
    ## Filters which points are DRAWN on the full-view map, nothing else.
    ## Deliberately never touches px_pair_analysis, encounters, or separation
    ## events, those keep running on the full hourly timeline regardless of
    ## this slider, same reasoning as why year/month don't touch it either
    ## for anything downstream of "which raw fixes get plotted". Rendered via
    ## renderUI (rather than a static sliderInput in the UI) so it can be
    ## disabled while narrowed into a single event, where thinning points
    ## out doesn't make sense, those 4 points are specific chosen moments,
    ## not a cloud to declutter. The current selection is fed back in as the
    ## slider's value on every re-render so toggling narrowed on/off doesn't
    ## reset it back to the full 0-23 range.
    output$hour_slider_ui <- renderUI({
      cur_val <- if (!is.null(input$hour_range)) input$hour_range else c(0, 23)
      is_narrowed <- !is.null(selected_event_key())
      slider <- sliderInput(ns("hour_range"),
                            "Hour of day, Sri Lanka time (map display only, doesn't change any numbers above)",
                            min = 0, max = 23, value = cur_val, step = 1)
      if (is_narrowed) {
        tags$div(style = "opacity: 0.45; pointer-events: none; filter: grayscale(70%);",
                 title = "Not used in the single-event view, click \"Show all points\" first.",
                 slider)
      } else {
        slider
      }
    })
    
    ## Small floating panel drawn on top of the map: navigation controls,
    ## which event this is, and (once narrowed) how long after the split the
    ## point you're looking at is and how far apart the pair were then.
    output$map_overlay <- renderUI({
      nv <- narrowed_view()
      if (is.null(nv)) return(NULL)
      ev <- nv$ev
      limited_note <- if (isTRUE(ev$window_limited)) {
        " (still apart when your selected year/month range ends, not necessarily still apart in reality)"
      } else if (isTRUE(ev$ongoing)) {
        " (still apart, tracking ran out before they reunited)"
      } else ""
      tags$div(
        style = paste("position:absolute; top:10px; left:10px; z-index:1000;",
                      "background:rgba(255,255,255,0.95); padding:10px 12px; border-radius:6px;",
                      "box-shadow:0 1px 4px rgba(0,0,0,0.35); max-width:320px; font-size:13px;"),
        tags$div(style = "margin-bottom:6px;",
                 actionButton(ns("show_all_points"), "Show all points", class = "btn-sm btn-default")),
        tags$b(paste0("Separation ", nv$pos, " of ", nv$n_total)),
        tags$p(style = "margin:4px 0 0 0;",
               "Together ended ", format(with_tz(ev$bond_end, "Asia/Colombo"), "%Y-%m-%d %H:%M"),
               " Sri Lanka time. This point is ",
               tags$b(round(nv$hours_since, 1)), " hour(s) after that.", limited_note),
        tags$p(style = "margin:4px 0 0 0;",
               "They were about ", tags$b(px_fmt_dist_m(nv$now_dist_m)), " apart at this moment.")
      )
    })
    
    output$pair_data_status <- renderUI({
      r <- pair_result()
      if (r$status_overall == "never_tracked_together") {
        return(tags$p(style = "color:#a00;", tags$b("Never tracked at the same time."),
                      " No basis to say anything about separation for this pair, within the",
                      " year(s)/month(s) currently selected."))
      }
      n_events <- nrow(r$separation_events)
      if (n_events == 0) {
        return(tags$p(style = "color:#666;",
                      "These two were never recorded together for ", input$min_bond_hours,
                      " hour(s) in a row within the year(s)/month(s) currently selected, so",
                      " there's nothing to check for a separation after. Try a smaller number",
                      " above, a wider year/month selection, or a different pair."))
      }
      n_limited <- sum(r$separation_events$window_limited)
      limited_note <- if (n_limited > 0) {
        paste0(" ", n_limited, " of these only look \"still apart\" because your year/month",
               " selection ends there, not because tracking itself ran out, widen the selection",
               " to check.")
      } else ""
      tags$p(style = "color:#080;", tags$b(n_events), " time(s) found where they were",
             " together for at least ", input$min_bond_hours, "h, then moved apart.",
             limited_note)
    })
    
    ## Map for the selected pair. Full view: every recorded fix for both
    ## elephants as plain dots, diamonds at the last-together moment of every
    ## separation, squares for fixes while apart. Diamonds and squares are
    ## clickable (layerId encodes name + time) to narrow down to that one
    ## event. Narrowed view: just the 4 reference points and the 4
    ## distance/path lines, until "Show all points" is clicked.
    output$togetherness_map <- renderLeaflet({
      r <- pair_result()
      both <- c(input$elephant_1, input$elephant_2)
      pal_colors <- unname(elephant_colors_r()[both]); names(pal_colors) <- both
      nv <- narrowed_view()
      
      if (!is.null(nv)) {
        pts <- nv$pts %>%
          mutate(color = unname(pal_colors[name]),
                 shape = ifelse(phase == "last_together", "diamond", "square"))
        validate(need(nrow(pts) > 0, "Couldn't find matching GPS points for this event."))
        
        icons_obj <- makeIcon(iconUrl = mapply(px_shape_icon_file, pts$color, pts$shape),
                              iconWidth = 18, iconHeight = 18, iconAnchorX = 9, iconAnchorY = 9)
        
        m <- leaflet(pts) %>%
          addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Map") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
          addScaleBar(position = "bottomleft") %>%
          addMarkers(lng = ~lon, lat = ~lat, icon = icons_obj,
                     popup = ~paste0("<b>", ifelse(phase == "last_together", "Last together", "During separation"),
                                     "</b><br>", name, "<br>", format(with_tz(datetime, "Asia/Colombo"), "%Y-%m-%d %H:%M"), " Sri Lanka time")) %>%
          px_add_line(nv$last_a, nv$last_b, color = "#2ca02c", weight = 3, dashArray = "6,6") %>%
          px_add_line(nv$now_a,  nv$now_b,  color = "#d62728", weight = 3, dashArray = "6,6") %>%
          px_add_line(nv$last_a, nv$now_a,  color = unname(pal_colors[both[1]]), weight = 3) %>%
          px_add_line(nv$last_b, nv$now_b,  color = unname(pal_colors[both[2]]), weight = 3) %>%
          addLayersControl(baseGroups = c("Map", "Satellite"), options = layersControlOptions(collapsed = FALSE)) %>%
          addControl(html = px_legend_html(pal_colors, both, show_lines = TRUE), position = "bottomright")
        return(m)
      }
      
      df <- eleph_all() %>% filter(name %in% both)
      if (!is.null(input$sel_years))  df <- df %>% filter(year %in% as.integer(input$sel_years))
      if (!is.null(input$sel_months)) df <- df %>% filter(as.character(month) %in% input$sel_months)
      validate(need(nrow(df) > 0, "No GPS fixes for one or both of these elephants in the selected year(s)/month(s)."))
      
      events <- r$separation_events
      
      ## Two diamonds per separation event: each elephant's fix nearest the
      ## moment they were last together.
      last_together_pts <- if (nrow(events) > 0) {
        bind_rows(lapply(seq_len(nrow(events)), function(i) {
          bind_rows(px_nearest_fix(eleph_all(), both[1], events$bond_end[i]),
                    px_nearest_fix(eleph_all(), both[2], events$bond_end[i]))
        }))
      } else tibble()
      
      ## Every fix (either elephant) inside any apart window, strictly after
      ## the bond ended (so the last-together fix itself stays a diamond, not
      ## double-counted as a square too).
      apart_flag <- rep(FALSE, nrow(df))
      if (nrow(events) > 0) {
        for (i in seq_len(nrow(events))) {
          hit <- df$hour_bin > events$apart_start[i] & df$hour_bin <= events$apart_end[i]
          apart_flag[hit] <- TRUE
        }
      }
      apart_pts <- df[apart_flag, ]
      
      flagged_keys <- unique(c(paste(last_together_pts$name, last_together_pts$hour_bin),
                               paste(apart_pts$name, apart_pts$hour_bin)))
      base_pts <- df %>% filter(!paste(name, hour_bin) %in% flagged_keys)
      
      ## Display-only hour-of-day filter, applied last, after every point has
      ## already been correctly categorized above. Doesn't change which
      ## points exist, only which of them get drawn.
      base_pts          <- px_filter_hour(base_pts, input$hour_range)
      apart_pts         <- px_filter_hour(apart_pts, input$hour_range)
      last_together_pts <- px_filter_hour(last_together_pts, input$hour_range)
      
      m <- leaflet() %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Map") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addScaleBar(position = "bottomleft")
      for (nm in both) {
        d <- base_pts %>% filter(name == nm)
        m <- m %>% addCircleMarkers(
          data = d, lng = ~lon, lat = ~lat, group = nm, radius = 3, stroke = FALSE,
          fillOpacity = 0.7, color = unname(pal_colors[nm]),
          popup = ~paste0("<b>", name, "</b><br>", format(floor_date(with_tz(datetime, "Asia/Colombo"), "hour"), "%Y-%m-%d %H:%M"), " Sri Lanka time")
        )
      }
      
      overlay_groups <- both
      flagged <- bind_rows(
        if (nrow(last_together_pts) > 0) last_together_pts %>% mutate(shape = "diamond", group = "Last together") else NULL,
        if (nrow(apart_pts) > 0) apart_pts %>% mutate(shape = "square", group = "During separation") else NULL
      )
      if (nrow(flagged) > 0) {
        flagged <- flagged %>%
          mutate(color = unname(pal_colors[name]),
                 click_id = paste("pt", name, format(hour_bin, "%Y%m%d%H%M"), sep = "||"))
        icons_obj <- makeIcon(iconUrl = mapply(px_shape_icon_file, flagged$color, flagged$shape),
                              iconWidth = 14, iconHeight = 14, iconAnchorX = 7, iconAnchorY = 7)
        m <- m %>% addMarkers(
          data = flagged, lng = ~lon, lat = ~lat, icon = icons_obj, layerId = ~click_id, group = ~group,
          popup = ~paste0("<b>", ifelse(shape == "diamond", "Last together", "During separation"),
                          "</b><br>", name, "<br>", format(floor_date(with_tz(datetime, "Asia/Colombo"), "hour"), "%Y-%m-%d %H:%M"), " Sri Lanka time<br>",
                          "<i>Click to see the full picture for this event</i>")
        )
        overlay_groups <- c(both, "Last together", "During separation")
      }
      m %>%
        addLayersControl(baseGroups = c("Map", "Satellite"), overlayGroups = overlay_groups,
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addControl(html = px_legend_html(pal_colors, both, show_lines = FALSE), position = "bottomright")
    })
    
    output$separation_table <- renderDT({
      r <- pair_result()
      d <- r$separation_events
      validate(need(nrow(d) > 0,
                    paste0("Never together for ", input$min_bond_hours, " hour(s) in a row, so nothing to show.")))
      d %>%
        transmute(`together ended (Sri Lanka time)` = format(with_tz(bond_end, "Asia/Colombo"), "%Y-%m-%d %H:%M"),
                  `hours together` = round(bond_duration_h, 1),
                  `hours apart afterward` = case_when(
                    window_limited ~ paste0(round(apart_duration_h, 1), " (through end of selected range)"),
                    ongoing         ~ paste0(round(apart_duration_h, 1), " (still apart)"),
                    TRUE            ~ as.character(round(apart_duration_h, 1))
                  )) %>%
        datatable(rownames = FALSE, options = list(pageLength = 15, dom = "t"))
    })
    
    ## ---- How this works ----
    output$method_text <- renderUI({
      req(pipeline())
      tagList(
        tags$h4("In plain terms"),
        tags$p("Every collared elephant sends back an hourly GPS location. For every hour where",
               " two elephants both have a reading, we work out how far apart they were. If they",
               " were within ", tags$b(paste0(pipeline()$threshold_m, " metres")),
               " of each other, we count that hour as \"close\"."),
        tags$p("Just knowing two elephants were close at some point isn't useful on its own,",
               " elephants are close together all the time for perfectly normal reasons. What this",
               " tab flags instead is a specific pattern: a real period of togetherness, then what",
               " happened right after it, using a simple, fixed rule rather than comparing against a",
               " pair's own past, on purpose, so a pair with little history isn't left out."),
        tags$p(tags$b("Separation event: "), "a continuous close encounter lasting at least a set",
               " number of hours (adjustable, default 6h, counts as real time spent together),",
               " followed by however long the pair then spent apart before reuniting. That",
               " apart-time is shown directly, however long or short it turns out to be, nothing",
               " decides in advance whether it \"counts\"."),
        tags$p("On the map, ", tags$b("diamonds"), " mark both elephants at the last moment they",
               " were together before a separation, and ", tags$b("squares"), " mark where each",
               " elephant was while apart afterward. Click a diamond or square, or pick a",
               " separation from the list above the map, to narrow down to just that event: which",
               " separation it is, how many hours after the split the point you picked falls, and",
               " how far apart the pair were at that moment. That narrowed view also draws 4 lines,",
               " coloured and explained in the map's own legend: how far apart the pair were when",
               " last together, how far apart they are now, and each elephant's own path between",
               " those two moments."),
        tags$p("Why this matters for management decisions: an unexplained, sustained separation",
               " between elephants that are normally close can be an early, low-cost signal of",
               " injury, illness, a family group splitting up, or several animals converging on the",
               " same shrinking resource, worth checking, for instance, during a drought when water",
               " points are limited."),
        tags$p(tags$b("What this is not: "), "a diagnosis. GPS distance alone can't tell us why two",
               " elephants are close or apart, only that a specific pair's pattern changed. Every",
               " flagged event is a prompt to look closer, not a conclusion."),
        tags$p("The \"Data coverage\" and \"Relationships\" tabs show every elephant's data volume",
               " and every possible pair, including pairs with no relationship at all and pairs",
               " involving elephants with too little data to analyse, nothing is hidden there,",
               " only labelled honestly."),
        
        tags$details(
          tags$summary(style = "cursor: pointer; font-weight: 600; margin-top: 16px;",
                       "Technical detail (method, thresholds, references)"),
          tags$div(style = "margin-top: 10px;",
                   tags$p("Wall, Wittemyer, Klinkenberg & Douglas-Hamilton (2014) define proximity as ",
                          tags$code("d(A,B) = haversine distance"), ", alerting when ",
                          tags$code("d(A,B) < tau"), ". That's a real-time monitoring rule, in the paper's",
                          " own language, not an anomaly-detection rule; it just says two things are close",
                          " right now. Calling what this tab does with that rule \"anomaly detection\" is",
                          " this dashboard's own interpretive label, not Wall et al.'s term. This tab keeps",
                          " the underlying proximity rule exactly as published: tau = ",
                          tags$b(paste0(pipeline()$threshold_m, "m")),
                          ", chosen from the spread of distances actually observed between our own",
                          " collared elephants (adjust it in the sidebar and click \"Update with new",
                          " distance\" to test other values)."),
                   tags$p("A proximity reading only earns attention once it's compared against something.",
                          " The separation-event definition uses a simple, user-adjustable absolute",
                          " threshold rather than a statistical baseline, specifically so a pair never",
                          " needs a long track record before something can be said about it: a continuous",
                          " close encounter at least as long as the minimum set on the Separation tab",
                          " (default 6h), followed by however long the pair then spent apart before",
                          " reuniting (or the rest of tracking within the current year/month selection, if",
                          " they never did). The apart-duration is shown directly, there is no second",
                          " threshold deciding whether that apart-time \"counts\". It doesn't introduce a",
                          " new distance metric beyond Wall et al.'s own proximity rule, it only decides",
                          " when a proximity pattern is worth a second look for that specific pair."),
                   tags$p(tags$b("Year/month filtering: "), "year and month restrict which GPS readings are",
                          " used at all, before anything is calculated. A separation that appears \"still",
                          " ongoing\" purely because it sits at the edge of the current year/month",
                          " selection is labelled as such, distinct from one that reached the true end of",
                          " all available tracking."),
                   tags$p("A comparable precedent: a study of male African savanna elephants formalises",
                          " \u201cpotential social interaction\u201d using a hard proximity criterion (all",
                          " other tracked elephants within 20 km), then models what drives movement given",
                          " that definition, rather than treating the raw threshold as meaningful by itself",
                          " (Oxford Academic, Behavioral Ecology). The same shape of argument applies here:",
                          " the ", pipeline()$threshold_m, "m rule defines the candidate event, the pair's",
                          " own history is what earns it a second look."),
                   tags$h5("Data sufficiency"),
                   tags$p("The separation-event definition itself has no data-volume gate, any two",
                          " elephants can be compared directly. Only the Relationships and Data coverage",
                          " tabs' \"enough data to compare\" judgement is gated, on elephants with at least ",
                          PX_MIN_FIXES, " real GPS fixes: ", paste(qualifying_names_r(), collapse = ", "), "."),
                   tags$h5("References"),
                   tags$p(style = "font-size: 13px;",
                          "Wall, J., Wittemyer, G., Klinkenberg, B., & Douglas-Hamilton, I. (2014). Novel",
                          " opportunities for wildlife conservation and research with real-time monitoring.",
                          " Ecological Applications, 24(4), 593-601."),
                   tags$p(style = "font-size: 13px;",
                          "Interplay of physical and social drivers of movement in male African savanna",
                          " elephants. Behavioral Ecology (Oxford Academic).")
          )
        )
      )
    })
  })
}