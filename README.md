# AIU Airport dashboard

Production repo for PRU airport dashboards published on ansperformance.eu.

For more information contact rainer.koelle@eurocontrol.int.



# Production

The `master` branch is **PRODUCTION**, i.e. it contains what gets published on the
Internet.

In particular the `docs` directory contains the generated dashboard pages
that are automatically served by Netlify at

https://airport-dashboard.netlify.app/

and pointed to by the AIU Portal at

https://ansperformance.eu/dashboard/stakeholder/airport/db/

So be aware that you need to be **CAREFUL** on `master`. 


## Concrete steps to publish production

There are 2 possible scenarios:

1. merge from a development branch where `docs` has already been properly
   generated

1. change directly in master and regenerate the dashboard


I would leave 2. for sort of emergency/tiny changes not worth the overhead
of creating a new branch and develop/PR/test/merge...(even if it is a safer
way of working.)

Case 1. is more for a release preparation where different
contributions eventually from different branches have been
combined for publication in next release.


# Development

## Development of a feature

Development of a feature or change in the dashboards can happen in each
person's favorite branch.
When the changes are deemed ready for next release they can be merged in the
relevant release branch.

(The preview possibilities via Netlify described below apply to any branch.)


## Development for a release

Development for a release happens in a branch conventionally named like
`YYYYMM-release` for a planned
release in month `MM` of year `YYYY`.

You can have a preview of the development branch if you create a [pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests)
out of changes pushed on the corresponding branch.
The preview is published automatically via Netlify at a URL like

> `https://deploy-preview-DIGITS--aiu-airports-dashboard.netlify.app/`

for the PR of number `DIGITS`, i.e. 

https://deploy-preview-1--aiu-airport-dashboard.netlify.app/

for PR #1.


## Dashboard generation

The dashboard is generated from a templated Rmd.
As coded in **`build_dashboard.R`**, there are potentially few steps to execute; they are
all coded in that script, more or less like follows:

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
  You can use the DEBUG_/BUILD_ variables to reduce the amount of generation
  during development, see below for details.
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


### Generation under DEVELPMENT

1.	execute the first steps to set the stage (clean the docs/ and pre pare the CSV)
2.	set the `BUILD_`/`DEBUG_` var as best for your development
    ```
    BUILD_DSH <- TRUE       # build dashboard?
    DEBUG_DSH <- TRUE       # ... and eventually only dashboard for airports in DEBUG_APTS?
    	
    BUILD_FAC <- FALSE      # build factsheet?
    DEBUG_FAC <- TRUE       # ... and eventually only factsheet for airports in DEBUG_APTS?
    		
    DEBUG_APTS <- c("EBBR") # subset of airports to consider
    ```
3.	commit only a minimum (EBBR or more if needed) of the airport dashboard/factsheet
4.	whoever prepares for the release will set 
    ```
    BUILD_DSH <- TRUE       # build dashboard?
    DEBUG_DSH <- FALSE      # ... and eventually only dashboard for airports in DEBUG_APTS?
    	
    BUILD_FAC <- TRUE       # build factsheet?
    DEBUG_FAC <- FALSE      # ... and eventually only factsheet for airports in DEBUG_APTS?
    ```
  and build everything in docs/
