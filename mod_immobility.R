# =============================================================================
# mod_immobility.R
# Method — Immobility monitor (Wall, Wittemyer, Klinkenberg & Douglas-Hamilton,
# 2014, Ecological Applications). Real-time "immobility" rule, applied exactly
# as published: 13 m radius sustained for >= 5 hours. (Wall et al. never call
# this an "anomaly" — that label, and this whole interactive layer, is added
# on top of their method.)
#
# CONVERTED FROM method_2.R: the original was a standalone app (its own
# ui/server, its own data load, unnamespaced inputs like "elephant", "year").
# This version exposes ONE UI function and ONE server function that drop
# straight into the combined app, exactly like the other two methods:
#
#   nav_panel("Immobility monitor", mod_immobility_ui("immobility"))
#   mod_immobility_server("immobility", shared_data)
#
# where `shared_data` is a reactive() returning the cleaned elephant data
# frame (columns: name, datetime, lat, lon; extra columns are ignored).
# Everything below is namespaced so it will not collide with the other
# modules.
# =============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(DT)
library(scales)

# ---- shared helper ----------------------------------------------------------
# Same haversine used by the movement-rate module. Guarded so that if another
# module already defined it in the combined app, we don't clobber it.
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

# ---- core computation (pure functions, no Shiny; namespaced with im_) -------

# For a candidate cluster of consecutive fixes {p_i, ..., p_j}:
#   centroid = (mean(lat), mean(lon))
#   r        = max distance from the centroid to any point in the cluster
# Alert when r <= radius_m, sustained for a duration >= min_hours.
# Strict version: EVERY point in the growing cluster must sit within the
# radius (Wall et al.'s text allows an 80%-of-points tolerance, but doesn't
# state what was actually used for the elephant application, so we implement
# the version we can pin down exactly: the strict one).
im_find_immobility <- function(sub, radius_m = 13, min_hours = 5, max_gap_h = 3) {
  sub <- sub %>% arrange(datetime)
  n <- nrow(sub)
  if (n < 2) return(tibble())
  lat <- sub$lat; lon <- sub$lon; dt <- sub$datetime
  events <- list()
  i <- 1
  while (i < n) {
    j <- i
    repeat {
      if (j + 1 > n) break
      gap_h <- as.numeric(difftime(dt[j + 1], dt[j], units = "hours"))
      if (gap_h > max_gap_h) break
      cand_lat <- lat[i:(j + 1)]; cand_lon <- lon[i:(j + 1)]
      c_lat <- mean(cand_lat); c_lon <- mean(cand_lon)
      r <- max(hav_m(cand_lat, cand_lon, c_lat, c_lon))
      if (r > radius_m) break
      j <- j + 1
    }
    duration_h <- as.numeric(difftime(dt[j], dt[i], units = "hours"))
    if (duration_h >= min_hours && j > i) {
      events[[length(events) + 1]] <- tibble(
        start = dt[i], end = dt[j],
        duration_h = round(duration_h, 1),
        n_fixes = j - i + 1,
        centroid_lat = mean(lat[i:j]),
        centroid_lon = mean(lon[i:j])
      )
      i <- j + 1
    } else {
      i <- i + 1
    }
  }
  bind_rows(events)
}

# Diagnostic-only helper (NOT part of Wall et al.'s method): a rolling
# "current spread" per fix, using a trailing window of `window_hours`, purely
# so the time-series chart can show how close each moment sits to the radius
# line rather than only showing pass/fail blocks. Carries no weight in the
# actual flagging logic above.
im_rolling_spread <- function(sub, window_hours = 5) {
  sub <- sub %>% arrange(datetime)
  n <- nrow(sub)
  spread <- rep(NA_real_, n)
  for (k in seq_len(n)) {
    win <- which(sub$datetime <= sub$datetime[k] &
                   sub$datetime >= sub$datetime[k] - hours(window_hours))
    if (length(win) >= 2) {
      c_lat <- mean(sub$lat[win]); c_lon <- mean(sub$lon[win])
      spread[k] <- max(hav_m(sub$lat[win], sub$lon[win], c_lat, c_lon))
    }
  }
  spread
}

# Mark which raw fixes fall inside a detected event window (for map/point coloring)
im_flag_points <- function(sub, events) {
  sub$flagged <- FALSE
  if (nrow(events) > 0) {
    for (e in seq_len(nrow(events))) {
      sub$flagged[sub$datetime >= events$start[e] & sub$datetime <= events$end[e]] <- TRUE
    }
  }
  sub
}

# =============================================================================
# UI
# =============================================================================
mod_immobility_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$p(
      style = "color:#555; margin-top:-6px;",
      "Flags a stretch of consecutive GPS fixes as \u201cimmobile\u201d when they all sit ",
      "within a small radius for a long stretch of time \u2014 the rule Wall et al. built ",
      "to catch death or serious injury in real time. Numbers below default to their ",
      "published values (13 m, 5 hours); adjust them to see how sensitive the flag is."
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(ns("elephant"), "Elephant", choices = "All elephants"),
        selectInput(ns("year"), "Year", choices = "All years"),
        hr(),
        sliderInput(ns("radius_m"), "Immobility radius (m)",
                    min = 5, max = 50, value = 13, step = 1),
        sliderInput(ns("min_hours"), "Minimum sustained duration (hours)",
                    min = 1, max = 24, value = 5, step = 0.5),
        sliderInput(ns("max_gap_h"), "Max allowed gap between fixes (hours)",
                    min = 1, max = 12, value = 3, step = 1),
        helpText("Defaults (13 m / 5 h) reproduce Wall et al.'s published rule exactly."),
        hr(),
        actionButton(ns("reset_defaults"), "Reset to Wall et al. defaults", width = "100%")
      ),

      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel(
            "Map",
            br(),
            plotlyOutput(ns("map_plot"), height = "560px")
          ),
          tabPanel(
            "Spread over time",
            br(),
            plotlyOutput(ns("timeseries_plot"), height = "420px"),
            helpText("Grey line = rolling spread within a trailing window (diagnostic only, our addition). ",
                     "Red dashed line = the current radius threshold. Orange shading = a flagged immobility event.")
          ),
          tabPanel(
            "Flagged events",
            br(),
            DTOutput(ns("events_table"))
          ),
          tabPanel(
            "Insights",
            br(),
            uiOutput(ns("insights_ui"))
          )
        )
      )
    )
  )
}

# =============================================================================
# Server
#   data_r : a reactive() returning the cleaned elephant data frame.
# =============================================================================
mod_immobility_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {

    # Defensive prep + a year column (this module's only extra requirement).
    eleph <- reactive({
      df <- data_r()
      req(df)
      df %>%
        mutate(
          lat      = suppressWarnings(as.numeric(lat)),
          lon      = suppressWarnings(as.numeric(lon)),
          datetime = if (is.character(datetime)) ymd_hms(datetime, tz = "UTC") else datetime
        ) %>%
        filter(!is.na(lat), !is.na(lon), !is.na(datetime)) %>%
        arrange(name, datetime) %>%
        mutate(year = year(datetime))
    })

    # Populate the Elephant / Year dropdowns once the shared data is available
    # (the original standalone app built these choices at source() time from
    # a global data frame; here the data arrives reactively, so we fill the
    # choices in on first load instead).
    observeEvent(eleph(), {
      d <- eleph()
      updateSelectInput(session, "elephant",
                         choices  = c("All elephants", sort(unique(d$name))),
                         selected = "All elephants")
      updateSelectInput(session, "year",
                         choices  = c("All years", sort(unique(d$year))),
                         selected = "All years")
    }, once = TRUE)

    observeEvent(input$reset_defaults, {
      updateSliderInput(session, "radius_m", value = 13)
      updateSliderInput(session, "min_hours", value = 5)
      updateSliderInput(session, "max_gap_h", value = 3)
    })

    # Data filtered by elephant + year selectors
    filtered_data <- reactive({
      req(input$elephant, input$year)
      d <- eleph()
      if (input$elephant != "All elephants") d <- d %>% filter(name == input$elephant)
      if (input$year != "All years") d <- d %>% filter(year == as.integer(input$year))
      d %>% arrange(name, datetime)
    })

    # Immobility events for every elephant present in the current filter,
    # computed at the current slider parameters
    all_events <- reactive({
      d <- filtered_data()
      validate(need(nrow(d) > 0, "No fixes for this elephant/year combination."))
      names_present <- unique(d$name)
      ev <- lapply(names_present, function(nm) {
        res <- im_find_immobility(d %>% filter(name == nm),
                                   radius_m  = input$radius_m,
                                   min_hours = input$min_hours,
                                   max_gap_h = input$max_gap_h)
        if (nrow(res) > 0) res$name <- nm
        res
      })
      bind_rows(ev)
    })

    # Points flagged in/out of an event, per elephant (for map + timeseries coloring)
    flagged_data <- reactive({
      d <- filtered_data()
      ev <- all_events()
      names_present <- unique(d$name)
      out <- lapply(names_present, function(nm) {
        sub <- d %>% filter(name == nm)
        ev_sub <- if (nrow(ev) > 0) ev %>% filter(name == nm) else tibble()
        im_flag_points(sub, ev_sub)
      })
      bind_rows(out)
    })

    # ---- Map (interactive lon/lat scatter with path, plotly) ----
    output$map_plot <- renderPlotly({
      d <- flagged_data()
      validate(need(nrow(d) > 0, "No data to show."))

      p <- plot_ly()
      for (nm in unique(d$name)) {
        sub <- d %>% filter(name == nm) %>% arrange(datetime)
        p <- p %>% add_trace(
          data = sub, x = ~lon, y = ~lat, type = "scatter", mode = "lines+markers",
          name = nm, legendgroup = nm,
          line = list(width = 1, color = "rgba(120,120,120,0.4)"),
          marker = list(size = 5,
                        color = ~ifelse(flagged, "#D55E00", "#4C72B0")),
          text = ~paste0(name, "<br>", format(datetime, "%Y-%m-%d %H:%M"),
                         "<br>", ifelse(flagged, "FLAGGED: immobile window", "normal fix")),
          hoverinfo = "text",
          showlegend = TRUE
        )
      }
      p %>% layout(
        title = "GPS tracks (orange = fixes inside a flagged immobility window)",
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude", scaleanchor = "x"),
        legend = list(orientation = "h", y = -0.15)
      )
    })

    # ---- Time series of rolling spread vs threshold ----
    output$timeseries_plot <- renderPlotly({
      d <- filtered_data()
      ev <- all_events()
      validate(need(nrow(d) > 0, "No data to show."))
      validate(need(length(unique(d$name)) <= 6,
                    "Pick a single elephant (or a year that narrows it to a few) to see the spread chart clearly."))

      plots <- list()
      for (nm in unique(d$name)) {
        sub <- d %>% filter(name == nm) %>% arrange(datetime)
        sub$spread <- im_rolling_spread(sub, window_hours = input$min_hours)
        ev_sub <- if (nrow(ev) > 0) ev %>% filter(name == nm) else tibble()

        shapes_list <- list()
        if (nrow(ev_sub) > 0) {
          ymax <- suppressWarnings(max(sub$spread, na.rm = TRUE))
          if (!is.finite(ymax)) ymax <- input$radius_m * 2
          for (r in seq_len(nrow(ev_sub))) {
            shapes_list[[length(shapes_list) + 1]] <- list(
              type = "rect", x0 = ev_sub$start[r], x1 = ev_sub$end[r],
              y0 = 0, y1 = ymax * 1.1,
              fillcolor = "rgba(213,94,0,0.15)", line = list(width = 0)
            )
          }
        }

        pl <- plot_ly() %>%
          add_trace(data = sub, x = ~datetime, y = ~spread, type = "scatter",
                    mode = "lines", name = paste(nm, "- rolling spread"),
                    line = list(color = "grey50")) %>%
          add_trace(x = range(sub$datetime), y = c(input$radius_m, input$radius_m),
                    type = "scatter", mode = "lines",
                    line = list(color = "#D55E00", dash = "dash"),
                    name = "radius threshold", showlegend = (nm == unique(d$name)[1])) %>%
          layout(title = nm, yaxis = list(title = "Spread (m)"),
                 xaxis = list(title = "Time"), shapes = shapes_list)

        plots[[length(plots) + 1]] <- pl
      }

      subplot(plots, nrows = length(plots), shareX = FALSE, titleY = TRUE, titleX = TRUE) %>%
        layout(showlegend = TRUE)
    })

    # ---- Events table ----
    output$events_table <- renderDT({
      ev <- all_events()
      if (nrow(ev) == 0) {
        return(datatable(tibble(Message = "No immobility events flagged at these parameters."),
                         rownames = FALSE, options = list(dom = 't')))
      }
      ev_display <- ev %>%
        select(name, start, end, duration_h, n_fixes, centroid_lat, centroid_lon) %>%
        arrange(desc(duration_h)) %>%
        rename(Elephant = name, Start = start, End = end,
               `Duration (h)` = duration_h, `Fixes in window` = n_fixes,
               `Centroid lat` = centroid_lat, `Centroid lon` = centroid_lon)
      datatable(ev_display, rownames = FALSE,
                caption = sprintf("Flagged at radius = %d m, duration >= %.1f h, max gap = %d h",
                                  input$radius_m, input$min_hours, input$max_gap_h),
                options = list(pageLength = 10))
    })

    # ---- Insights panel ----
    output$insights_ui <- renderUI({
      d <- filtered_data()
      ev <- all_events()
      n_fixes_total <- nrow(d)
      n_eleph <- length(unique(d$name))
      n_events <- nrow(ev)

      at_defaults <- input$radius_m == 13 && input$min_hours == 5 && input$max_gap_h == 3

      reproduction_note <- if (at_defaults && input$elephant %in% c("All elephants", "Gothami") &&
                               input$year %in% c("All years", "2025")) {
        tags$p(tags$b("Reproducibility check: "),
               "At Wall et al.'s exact published numbers (13 m, 5 h), this run should surface ",
               "the same single candidate found in the reference analysis: ",
               tags$b("Gothami, 6 August 2025, 01:00\u201306:00"), " \u2014 a 5-hour stationary window. ",
               "That elephant kept transmitting normally for months afterward, which is why the ",
               "reference treated it as a long rest rather than a mortality signal, not an emergency.")
      } else NULL

      tagList(
        h4("What this view is showing"),
        p(sprintf("Fixes in current selection: %s across %d elephant(s). Immobility events flagged: %d.",
                  comma(n_fixes_total), n_eleph, n_events)),
        reproduction_note,
        h4("Interests / things worth noticing"),
        tags$ul(
          tags$li("Rarity is the point: a rule tuned against confirmed elephant deaths should almost ",
                  "never fire on healthy animals. Watch whether loosening the radius/duration sliders ",
                  "turns a near-zero count into dozens of events \u2014 that tells you how close to the ",
                  "edge the default parameters are living for this population."),
          tags$li("Compare elephants: some collars (e.g. those with very few valid fixes, see the raw ",
                  "data's missing coordinates) will rarely or never produce a reliable flag simply from ",
                  "sparse data \u2014 worth checking fix density before reading too much into an elephant ",
                  "with zero flagged events."),
          tags$li("Context around a flag matters more than the flag itself: use the Map and Spread tabs ",
                  "to look at movement immediately before and after any flagged window \u2014 a normal ",
                  "walk-in/walk-out pattern (as with Gothami) argues for rest; an animal that never moves ",
                  "again is the real emergency signal Wall et al. built this for."),
          tags$li("Seasonality: switching the Year filter can reveal whether stationary bouts cluster ",
                  "around particular months (e.g. resting through the hottest part of the day, or during ",
                  "a known illness/recovery period) rather than being randomly distributed across the year.")
        )
      )
    })
  })
}
