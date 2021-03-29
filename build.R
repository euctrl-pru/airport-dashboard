library(here)

# export the data to CSV from DB
# COMMENT OUT if already DONE
source(here("R","apt_dshbd_get_data.R"), encoding = "UTF8")

# generate airport layout
# COMMENT OUT if no new airports have been added
source(here("R","generate-apt-layout.R"), encoding = "UTF8")


source(here("R","apt_dshbd_render.R"), encoding = "UTF8")
source(here("R","apt_dshbd_index.R"), encoding = "UTF8")
