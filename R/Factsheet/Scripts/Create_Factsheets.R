
library("dplyr")
# library("plotly")
# library("stringr")
library("here")
# library("ggplot2")
# library("scales")

source(here("R","apt_dshbd_get_data.R"), encoding = "UTF8")
source(here("R","apt_dshbd_create_df.R"), encoding = "UTF8")
source(here("R","apt_dshbd_utils.R"), encoding = "UTF8")

APTs=APT_DF %>%
  filter( AIRPORT %in% c("EBBR", "LATI")) %>%   # for debug
  pull(AIRPORT)

# for (Airport in APTs) {
#   
#   source(here("R", "Factsheet", "Scripts", "Create_factsheet_figures.R"), encoding = "UTF8")
#   
# }

APTs %>%
  purrr::walk(
    .f = function(icao) {
      cat(paste0("==>", icao, "...\n"))
      rmarkdown::render(
        input       = here("R", "Factsheet", "Scripts", "Factsheet_pdf.Rmd"),
        params      = prepare_params(icao),
        output_file = here("R", "Factsheet", "Factsheets", paste0("Factsheet_", icao, ".pdf")))
      cat(paste0("==>", icao, "...end\n"))
    })
