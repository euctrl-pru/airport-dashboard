library(here)

# export the data to CSV from DB
# UNCOMMENT  if data needs to be refreshed
source(here("R","apt_dshbd_get_data.R"), encoding = "UTF8")

# generate airport layout
# UNCOMMENT  if new airports have been added
# source(here("R","generate-apt-layout.R"), encoding = "UTF8")


source(here("R","apt_dshbd_render.R"), encoding = "UTF8")
source(here("R","apt_dshbd_index.R"), encoding = "UTF8")
