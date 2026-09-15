# =============================================================================
# app.R — Kaudulla Elephants: Combined Anomaly-Detection Dashboard
#
# Merges four independent anomaly-detection methods into a single Shiny app.
# Each method is reachable as its own tab in the top navbar, and all four
# share ONE copy of the cleaned tracking data (loaded once at startup and
# handed to every module as a reactive()):
#
#   1. Movement rate      — Wall et al. (2014): daily-distance percentile
#                            baseline, per elephant.
#   2. Immobility monitor — Wall et al. (2014): 13 m radius sustained for
#                            >= 5 hours (published real-time mortality rule).
#   3. Revisit hotspots   — Bracis et al. (2018): spatial revisit / site-
#                            fidelity analysis.
#   4. Proximity anomalies — Wall et al. (2014): pairwise haversine distance
#                            between elephants, flagging unusually long
#                            togetherness, separations, and weekly rate shifts.
#
# Files expected in the same folder as this app.R:
#   mod_movement_rate.R   (mod_movement_rate_ui / mod_movement_rate_server)
#   mod_immobility.R      (mod_immobility_ui    / mod_immobility_server)
#   mod_method4.R         (mod_method4_ui       / mod_method4_server)
#   mod_proximity.R       (mod_proximity_ui     / mod_proximity_server)
#   kaudulla_elephants_clean.csv
#
#   library(shiny); runApp("app.R")
# =============================================================================

library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(leaflet)
library(DT)
library(scales)
library(purrr)
library(geosphere)  # used by mod_method4.R (kept for reference/back-compat)
library(htmltools)
library(ggplot2)    # used by mod_proximity.R
library(bsicons)    # icon set for the Overview tab (install.packages("bsicons") if missing)

source("mod_movement_rate.R")
source("mod_immobility.R")
source("mod_method4.R")
source("mod_proximity.R")

# ---- load the shared cleaned data ONCE, for all three methods --------------
DATA_FILE <- "kaudulla_elephants_clean.csv"

raw <- read_csv(
  DATA_FILE, show_col_types = FALSE,
  col_types = cols(datetime = col_character(),
                   lat = col_character(), lon = col_character(),
                   .default = col_guess())
)

eleph_clean <- raw %>%
  mutate(
    lat      = suppressWarnings(as.numeric(lat)),
    lon      = suppressWarnings(as.numeric(lon)),
    datetime = ymd_hms(datetime, tz = "UTC")
  ) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  arrange(name, datetime)

# ---- summary stats for the Overview stat strip (computed once, at startup) --
n_elephants   <- n_distinct(eleph_clean$name)
n_fixes       <- scales::comma(nrow(eleph_clean))
tracking_span <- paste0(
  format(min(eleph_clean$datetime), "%b %Y"), " \u2013 ",
  format(max(eleph_clean$datetime), "%b %Y")
)

# ---- shared colour palette (mirrors the project's Beamer green palette) -----
kd_darkgreen <- "#1B4332"
kd_emerald   <- "#2D6A4F"
kd_ashgray   <- "#6C757D"

# ---- combined UI -------------------------------------------------------------
ui <- page_navbar(
  title = "Kaudulla elephants — Anomaly Detection Methods",
  theme = bs_theme(version = 5, preset = "cosmo", primary = kd_emerald) %>%
    bs_add_rules(sprintf("
      /* fill the space below the navbar and spread the sections evenly, instead of stacking them at the top */
      .kd-overview {
        display: flex;
        flex-direction: column;
        justify-content: space-evenly;
        min-height: calc(100vh - 100px);
        gap: 1.5rem;
      }
      .kd-overview > * + * { margin-top: 0; }

      .kd-overview h2 { font-weight: 600; color: %s; margin-bottom: 0; font-size: 1.4rem; }

      .kd-stat .bslib-value-box { border: 1px solid rgba(0,0,0,0.08); box-shadow: none; min-height: 0; }
      .kd-stat .bslib-value-box .value-box-area { padding: 0.75rem 1rem; }

      .kd-method-card { border: 1px solid rgba(0,0,0,0.08); border-top: 3px solid %s; height: 100%%; }
      .kd-method-card .card-body { padding: 1rem 1.25rem; }
      .kd-method-icon { color: %s; font-size: 1.15rem; display: block; margin-bottom: 0.5rem; }
      .kd-method-cite { color: %s; font-size: 0.75rem; margin: 0 0 0.4rem 0; }
      .kd-method-card h5 { font-weight: 600; margin-bottom: 0.1rem; font-size: 0.95rem; }
      .kd-method-card p:last-child { margin-bottom: 0; color: #3d3d3d; font-size: 0.87rem; line-height: 1.35; }

      .kd-footnote { background: transparent; border: none; padding: 0; color: %s; font-size: 0.8rem; }
    ", kd_darkgreen, kd_emerald, kd_emerald, kd_ashgray, kd_ashgray)),
  
  nav_panel(
    "Overview",
    div(
      class = "kd-overview",
      
      div(
        h2("Four ways of asking \u201cis something wrong with this elephant?\u201d")
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        gap = "1rem",
        class = "kd-stat",
        value_box(title = "Elephants tracked", value = n_elephants,
                  showcase = bs_icon("compass"),
                  theme = value_box_theme(bg = "#ffffff", fg = kd_darkgreen)),
        value_box(title = "Tracking span", value = tracking_span,
                  showcase = bs_icon("calendar-range"),
                  theme = value_box_theme(bg = "#ffffff", fg = kd_darkgreen)),
        value_box(title = "GPS fixes analysed", value = n_fixes,
                  showcase = bs_icon("geo-alt"),
                  theme = value_box_theme(bg = "#ffffff", fg = kd_darkgreen))
      ),
      
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        gap = "1rem",
        card(class = "kd-method-card", card_body(
          bs_icon("speedometer2", class = "kd-method-icon"),
          h5("Movement rate"),
          p(class = "kd-method-cite", "Wall et al. (2014)"),
          p("Flags days where an elephant's total daily movement drops to or below its own ",
            "historical 1st percentile \u2014 a personalised, per-elephant baseline rather than ",
            "a single herd-wide threshold.")
        )),
        card(class = "kd-method-card", card_body(
          bs_icon("exclamation-triangle", class = "kd-method-icon"),
          h5("Immobility monitor"),
          p(class = "kd-method-cite", "Wall et al. (2014)"),
          p("Flags a sustained stay inside a ~13 m radius for \u2265 5 hours \u2014 the published ",
            "real-time rule for catching death or serious injury as early as possible.")
        )),
        card(class = "kd-method-card", card_body(
          bs_icon("geo-alt-fill", class = "kd-method-icon"),
          h5("Revisit hotspots"),
          p(class = "kd-method-cite", "Bracis et al. (2018)"),
          p("Looks at how often an elephant returns to the same place, separating core, ",
            "repeated-use sites from one-off, exploratory locations.")
        )),
        card(class = "kd-method-card", card_body(
          bs_icon("people-fill", class = "kd-method-icon"),
          h5("Proximity anomalies"),
          p(class = "kd-method-cite", "Wall et al. (2014)"),
          p("Tracks pairwise distance between elephants, flagging unusually long stretches ",
            "together, separations after a bond, and weeks where a pair's togetherness shifts ",
            "away from its own normal pattern.")
        ))
      ),
      
      p(class = "kd-footnote",
        "All four tabs read the same shared, cleaned dataset, so any elephant or date-range ",
        "insight can be cross-checked across methods.")
    )
  ),
  
  nav_panel("Movement rate", mod_movement_rate_ui("movement_rate")),
  nav_panel("Immobility monitor", mod_immobility_ui("immobility")),
  nav_panel("Revisit hotspots", mod_method4_ui("method4")),
  nav_panel("Proximity anomalies", mod_proximity_ui("proximity"))
)

# ---- combined server ----------------------------------------------------------
server <- function(input, output, session) {
  # One reactive, shared by every module — read the CSV once, use it four times.
  shared_data <- reactive(eleph_clean)
  
  mod_movement_rate_server("movement_rate", shared_data)
  mod_immobility_server("immobility", shared_data)
  mod_method4_server("method4", data = shared_data)
  mod_proximity_server("proximity", shared_data)
}

shinyApp(ui, server)