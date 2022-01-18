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
#    NOTE: it doesn't really work because it produces ugly layouts...
# UNCOMMENT  if new airports have been added
# source(here("R","generate-apt-layout.R"), encoding = "UTF8")

# 3. generate the various airport HTML pages
source(here("R","apt_dshbd_render.R"), encoding = "UTF8")
# source(here("R","apt_dshbd_index.R"), encoding = "UTF8")

# 4. regenerate index.html
fs::file_copy(here("index.Rmd"), here("docs/"), overwrite = TRUE)
rmarkdown::render(here("docs", "index.Rmd"),  encoding = 'UTF-8')
fs::file_delete(path = here("docs", "index.Rmd"))
