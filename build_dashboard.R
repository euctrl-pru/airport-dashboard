library(dplyr)
library(here)
library(stringr)

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
# UNCOMMENT  if new airports have been added
# source(here("R","apt_dshbd_layout.R"), encoding = "UTF8")

# 3. generate the various airport HTML pages

# TODO: use options so as to avoid to change this code...
BUILD_DSH <- TRUE  # FALSE if you do NOT want to generate the HTML pages
DEBUG_DSH <- FALSE   # TRUE if you are in DEBUG mode when HTML pages are built, see DEBUG_APTS

BUILD_FAC <- TRUE # FALSE if you do NOT want to generate the PDF factsheet
DEBUG_FAC <- FALSE  # TRUE if you are in DEBUG mode when PDF factsheets are built, see DEBUG_APTS.


# the subset of airport you are debugging
DEBUG_APTS <- c("EBBR")

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
