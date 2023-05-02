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

source(here("R", "fac_layout_setup.R"))

# ---- CALL RENDER DASHBOARDS ----

APT_DF <- APT_DF %>% arrange(AIRPORT) %>% mutate(idx = row_number())

if(BUILD_DSH == TRUE) {
  APT_DF %>%
    # only the DEBUG subset otherwise ALL
    { if(DEBUG_DSH == TRUE) filter(., AIRPORT %in% DEBUG_APTS) else .} %>%
    # process from XXXX onward    
    # filter(AIRPORT >= "LIMF") %>%
    pull(AIRPORT) %>%
    purrr::walk(
      .f = function(icao) {
        cat(paste0("==>", icao, "...\n"))
        rmarkdown::render(
          input       = here("apt_dshbd_render.Rmd"),
          params      = prepare_params(icao), 
          output_file = here("docs", paste0(icao, ".html")))
        cat(paste0("(HTML) ==>", icao, "...end\n"))
      })
}


# Create factsheets

# Recreate the country maps if necessary
# source(here("R", "fac_create_country_maps.R"), encoding = "UTF8")

# Create docs/pdf folder
dir.create(here("docs", "pdf"))




if(BUILD_FAC == TRUE) {
  APT_DF %>%
    # only the DEBUG subset otherwise ALL
    { if(DEBUG_FAC == TRUE) filter(., AIRPORT %in% DEBUG_APTS) else .} %>%
    # process from XXXX onward    
    # filter(AIRPORT >= "EHEH") %>%
    pull(AIRPORT) %>%
    purrr::walk(
      .f = function(icao) {
        cat(paste0(" (PDF) ==>", icao, "...\n"))
        rmarkdown::render(
          input       = here("factsheet_render.Rmd"),
          params      = prepare_params(icao),
          output_file = here("docs", "pdf", paste0("Factsheet_", icao, ".pdf")))
        cat(paste0("(PDF) ==>", icao, "...end\n"))
      })
}
