# ..........................................................................----
# --- SET UP ----
# ---
# IMPORTANT: install htmlwidgets >= 1.5.2 from CRAN in order to avoid CSS units issues
library(here)
library(dplyr)
library(lubridate)
library(scales)

# IMPORTANT: Data frames need to be generated via "apt_dshbd_create_df.R" before execution
source(here("R","apt_dshbd_create_df.R"), encoding = "UTF8")

source(here("R","apt_dshbd_utils.R"), encoding = "UTF8")

source(here("R", "Factsheet", "Scripts", "Factsheet_layout_setup.R"))

# ---- CALL RENDER DASHBOARDS ----

APT_DF <- APT_DF %>% arrange(AIRPORT) %>% mutate(idx = row_number())

APT_DF %>%
  filter( AIRPORT %in% c("EBBR", "EGLL", "LATI")) %>%   # for debug
  pull(AIRPORT) %>%
  purrr::walk(
    .f = function(icao) {
      cat(paste0("==>", icao, "...\n"))
      rmarkdown::render(
        input       = here("apt_dshbd_render.Rmd"),
        params      = prepare_params(icao), 
        output_file = here("docs", paste0(icao, ".html")))
      cat(paste0("==>", icao, "...end\n"))
    })


# Create factsheets

# Recreate the country maps if necessary
# source(here("R", "Factsheet", "Scripts", "Create_country_maps.R"), encoding = "UTF8")

APT_DF %>%
  filter( AIRPORT %in% c("EBBR", "EGLL", "LATI")) %>%   # for debug
  pull(AIRPORT) %>%
  purrr::walk(
    .f = function(icao) {
      cat(paste0("==>", icao, "...\n"))
      rmarkdown::render(
        input       = here("R", "Factsheet", "Scripts", "Factsheet_pdf.Rmd"),
        params      = prepare_params(icao),
        output_file = here("R", "Factsheet", "Factsheets", paste0("Factsheet_", icao, ".pdf")))
      cat(paste0("==>", icao, "...end\n"))
    })
