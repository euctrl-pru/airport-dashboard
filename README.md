# AIU Airport dashboard

Production repo for PRU airport dashboards published on ansperformance.eu.

For more information contact rainer.koelle@eurocontrol.int.

# Development

Development happens in separate branches, i.e. `YYYYMM-release` for a planned
release in month `MM` of year `YYYY`, while production is in `master`.

You can have a preview of the development branch is you create a pull request
out of changes pushed there.
The preview is published automatically via Netlify at a URL like

> `https://deploy-preview-DIGITS--aiu-airports-dashboard.netlify.app/`

for the PR of number `DIGITS`, i.e. 
https://deploy-preview-1--aiu-airports-dashboard.netlify.app/ for PR #1.


## Dashboard generation

The dashboard is generated from a templated Rmd.
As coded in `build.R`, there are potentially few steps to execute:

1. cleanup `docs/`: remove everything but (favicons and) `images/` and `index.html`
  
    ```
    fs::dir_delete(here("docs", "libs"))
    fs::dir_ls(here("docs"), recurse = TRUE) %>%
      as_tibble() %>%
      filter(str_detect(value, pattern = "images", negate = TRUE),
             str_detect(value, pattern = "index.html", negate = TRUE)) %>%
      fs::file_delete()
    ```

1. Export the data from DB to local CSV.
  You typically do this once per month.
    ```
    source(here("R","apt_dshbd_get_data.R"), encoding = "UTF8")
    ```

1. Generate airport layouts.
  This is rarely done, i.e. only when a new airport is added to the list of the
  ones in the dashboard or the code to generate the images has changed.
  
    ```
    source(here("R","generate-apt-layout.R"), encoding = "UTF8")
    ```

1. Generate the various airport HTML pages.
  This is a **MANDATORY** step to generate the dashboard.
  
    ```
    source(here("R","apt_dshbd_render.R"), encoding = "UTF8")
    ```

1. Regenerate `docs/index.html`.
  This is only needed if new airports have been added or the `index.html` has
  been removed
  
    ```
    # copy over
    fs::file_copy(here("index.Rmd"), here("docs/"), overwrite = TRUE)
    # generate HTML
    rmarkdown::render(here("docs", "index.Rmd"),  encoding = 'UTF-8')
    # remove Rmd
    fs::file_delete(path = here("docs", "index.Rmd"))
    ```
