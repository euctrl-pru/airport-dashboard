# ..........................................................................----
# --- SET UP ----
# ---

library(dplyr)
library(readxl)
library(readr)
library(DBI)
library(ROracle)

# ..........................................................................----
# ORACLE DB EXPORT FUNCTION ----
# ..........................................................................----
EXPORT_QUERY <- function(schema, query) {
  USR <- Sys.getenv(paste0(schema, "_USR"))
  PWD <- Sys.getenv(paste0(schema, "_PWD"))
  DBN <- Sys.getenv(paste0(schema, "_DBNAME"))
  withr::local_envvar(c("TZ" = "UTC", "ORA_SDTZ" = "UTC"))
  withr::local_namespace("ROracle")
  con <- withr::local_db_connection(
    DBI::dbConnect(DBI::dbDriver("Oracle"),
                   USR, PWD, dbname = DBN,
                   timezone = "UTC"))
  con %>%
    dbSendQuery(query) %>%
    fetch(n = -1)
}


# ..........................................................................----
# 1 - EXTRACT DATA FROM ORACLE DATABASE  ----
# ..........................................................................----


#***********************************************************************
# ---- APT LIST ----
#***********************************************************************
EXPORT_APT_DSHBD_AIRPORT <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_AIRPORT"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_AIRPORT() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_AIRPORT.csv"))


#***********************************************************************
# ---- APT RWY CONFIG ----
#***********************************************************************
EXPORT_APT_DSHBD_RWY_CONFIG <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_RWY_CONFIG"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_RWY_CONFIG() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_RWY_CONFIG.csv"))


#***************************************************
# ---- APT TRAFFIC ----
#***************************************************
EXPORT_APT_DSHBD_TRAFFIC <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_TRAFFIC"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_TRAFFIC() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_TRAFFIC.csv"))


#***************************************************
# ---- APT TRAFFIC EVO ----
#***************************************************
EXPORT_APT_DSHBD_TRAFFIC_EVO <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_TRAFFIC_EVO"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_TRAFFIC_EVO() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_TRAFFIC_EVO.csv"))


#***************************************************
# ---- APT TRAFFIC MARKET ----
#***************************************************
EXPORT_APT_DSHBD_TRAFFIC_MARKET <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_TRAFFIC_MARKET"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_TRAFFIC_MARKET() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_TRAFFIC_MARKET.csv"))


#***************************************************
# ---- APT THROUGHPUT ----
#***************************************************
EXPORT_APT_DSHBD_THROUGHPUT <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_THROUGHPUT"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_THROUGHPUT() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_THROUGHPUT.csv"))


#***************************************************
# ---- APT ATFM ----
#***************************************************
EXPORT_APT_DSHBD_ATFM <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_ATFM"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_ATFM() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_ATFM.csv"))


#***************************************************
# ---- APT SLOT ----
#***************************************************
EXPORT_APT_DSHBD_SLOT <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_SLOT_AD"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_SLOT() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_SLOT_AD.csv"))


#***************************************************
# ---- APT PUNCTUALITY ----
#***************************************************
EXPORT_APT_DSHBD_PUNCTUALITY <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_PUNCTUALITY"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_PUNCTUALITY() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_PUNCTUALITY.csv"))


#***********************************************************************
# ---- APT APDF DATA (ASMA / TAXI OUT / TAXI IN / PREDEP DLY)  ----
#***********************************************************************
EXPORT_APT_DSHBD_APDF_DATA <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_APDF_DATA"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_APDF_DATA() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_APDF_DATA.csv"))



EXPORT_APT_DSHBD_DELAY_DATA <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_DELAY_DATA"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_DELAY_DATA() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_DELAY_DATA.csv"))


EXPORT_APT_DSHBD_TXOT_RWY <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_TXOT_RWY"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_TXOT_RWY() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_TXOT_RWY.csv"))


EXPORT_APT_DSHBD_TXIN_RWY <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_TXIN_RWY"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_TXIN_RWY() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_TXIN_RWY.csv"))


EXPORT_APT_DSHBD_ASMA_RWY <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_ASMA_RWY"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_ASMA_RWY() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_ASMA_RWY.csv"))


#***********************************************************************
# ---- APT TURNAROUND  ----
#***********************************************************************
EXPORT_APT_DSHBD_TURNAROUND <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_TURNAROUND"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_TURNAROUND() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_TURNAROUND.csv"))


#***********************************************************************
# ---- CDO/CCO  ----
#***********************************************************************
EXPORT_APT_DSHBD_CDO_CCO <- function() {
  QUERY <- "SELECT * FROM PRUTEST.CDO_CCO_MONTH_AIRPORT"
  EXPORT_QUERY("PRU_TEST", QUERY)
}

EXPORT_APT_DSHBD_CDO_CCO() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_CDO_CCO.csv"))
