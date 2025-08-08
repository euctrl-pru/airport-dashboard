library(dplyr)
library(here)
library(stringr)

shell("git pull")

source('R/plotly_download_wrapper.R')

# 0. cleanup: remove everything but (favicons and) `images/` 
fs::dir_delete(here("docs", "libs"))
fs::dir_ls(here("docs"), recurse = TRUE) %>%
  as_tibble() %>%
  filter(
    str_detect(value, pattern = "images", negate = TRUE),
    str_detect(value, pattern = "index.html", negate = TRUE)
  ) %>%
  dplyr::pull(value) %>%
  fs::file_delete()

# 1. export the data to CSV from DB
# UNCOMMENT  if data needs to be refreshed
source(here("R","apt_dshbd_get_data.R"), encoding = "UTF8")

# 2. generate airport layout
source(here("R","apt_dshbd_layout.R"), encoding = "UTF8")

# 3. generate the various airport HTML pages

# define your development/debug options (without necessarily changing this script)
# like the following in your console (build only HTML for EBBR and LFPG)
# 
# > options(
#     airport_dashboard.build_dashboard = TRUE,
#     airport_dashboard.debug_dashboard = TRUE,
#     airport_dashboard.build_factsheet = FALSE,
#     airport_dashboard.debug_dashboard = TRUE, # if above is FALSE this does not matter
#     airport_dashboard.airports = c("EBBR", "LFPG")
#   )
# 
# To reset to defaults and build everything, execute:
# > options(
#     airport_dashboard.build_dashboard = TRUE,
#     airport_dashboard.debug_dashboard = FALSE,
#     airport_dashboard.build_factsheet = TRUE,
#     airport_dashboard.debug_dashboard = FALSE)


# FALSE if you do NOT want to generate the HTML pages
BUILD_DSH <- getOption('airport_dashboard.build_dashboard', default = TRUE)
# TRUE if you are in DEBUG mode when HTML pages are built, see DEBUG_APTS
DEBUG_DSH <- getOption('airport_dashboard.debug_dashboard', default = FALSE)

# FALSE if you do NOT want to generate the PDF factsheet
BUILD_FAC <- getOption('airport_dashboard.build_factsheet', default = FALSE) 
# TRUE if you are in DEBUG mode when PDF factsheets are built, see DEBUG_APTS.
DEBUG_FAC <- getOption('airport_dashboard.debug_factsheet', default = FALSE)

# the subset of airport you are debugging
DEBUG_APTS <- getOption('airport_dashboard.airports', default = c("EBBR", "LFPG"))

source(here("R","apt_dshbd_render.R"), encoding = "UTF8")
# source(here("R","apt_dshbd_index.R"), encoding = "UTF8")

# 4. regenerate index.html
fs::file_copy(here("index.Rmd"), here("docs/"), overwrite = TRUE)
fs::file_copy(here("media","ga_script.html"), here("docs/"), overwrite = TRUE)
# fs::file_copy(here("media","_navbar.yml"), here("docs/"), overwrite = TRUE)
rmarkdown::render(here("docs", "index.Rmd"),  encoding = 'UTF-8')
fs::file_delete(path = here("docs", "index.Rmd"))
fs::file_delete(here("docs","ga_script.html"))
# fs::file_delete(here("docs","_navbar.yml"))

# 5. Push to Github
shell("git add -A")
shell("git commit -m \"Regeneration of the dashboard\"")
shell("git push")
