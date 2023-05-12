library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(readxl)
library(here)
# ****************************----

# APT NAMES    ----

APT_DF <- read_csv2(here("data","APT_DSHBD_AIRPORT.csv"))
APT_DF <- APT_DF %>%
  select(
    AIRPORT  = AIRPORT,
    APT_NAME = APDF_NAME,
    STATE    = CTRY_ABBREVIATION,
    ICAO     = ICAO_CODE,
    IATA     = IATA_CODE,
    IS_APDF  = IS_APDF,
    LON,
    LAT
  )


# RWY CONFIGURATION ----

CONFIG_DF <- read_csv2(here("data", "APT_DSHBD_RWY_CONFIG.csv"))
CONFIG_DF <- CONFIG_DF %>%
  drop_na() %>%
  select(AIRPORT, YEAR, CONFIGURATION, SHARE_PCT) %>%
  arrange(desc(SHARE_PCT))


# TRAFFIC ----

TFC_DF <- read_csv2(here("data", "APT_DSHBD_TRAFFIC.csv"))


# TRAFFIC VARIATION ----

TFC_VAR_DF <- read_csv2(here("data", "APT_DSHBD_TRAFFIC_EVO.csv"))

TFC_VAR_DF <- TFC_VAR_DF %>%
  mutate(
    DAY        = lubridate::as_date(DAY),
    FLTS       = as.numeric(FLTS),
    FLTS_2019  = as.numeric(FLTS_2019),
    MOV_AVG_WK = as.numeric(MOV_AVG_WK)
  ) %>%
  select(APT_ICAO, ARP_NAME, DAY, FLTS, FLTS_2019, MOV_AVG_WK)

# TRAFFIC MARKET ----

TFC_MKT_DF <- read_csv2(here("data", "APT_DSHBD_TRAFFIC_MARKET.csv"))


# THROUGHPUT ----

THRU_DF <- read_csv2(here("data", "APT_DSHBD_THROUGHPUT.csv")) %>%
  mutate(APT_ICAO = AIRPORT)


# ATFM ----

ATFM_DF <- read_csv2(here("data", "APT_DSHBD_ATFM.csv")) %>%
      mutate(AIRPORT  = APT_ICAO,
             FLT_DATE = lubridate::date(FLT_DATE),
      #   
         across(starts_with("DLY_APT_ARR_"),
                ~ tidyr::replace_na(.x, 0)),
      #
      AD_DISRUPTION = DLY_APT_ARR_A_1 +
         DLY_APT_ARR_E_1 +
         DLY_APT_ARR_N_1 +
         DLY_APT_ARR_O_1 +
         DLY_APT_ARR_NA_1,
       #
       AD_CAPACITY = DLY_APT_ARR_G_1 +
         DLY_APT_ARR_M_1 +
         DLY_APT_ARR_R_1 +
         DLY_APT_ARR_V_1,
       #
       AD_WEATHER = DLY_APT_ARR_D_1 +
         DLY_APT_ARR_W_1,
       #
       AD_DISRUPTION_ATC = DLY_APT_ARR_I_1 +
         DLY_APT_ARR_T_1,
       #
       AD_CAPACITY_ATC = DLY_APT_ARR_C_1,
       #
       AD_STAFFING_ATC = DLY_APT_ARR_S_1,
       #
       AD_EVENTS = DLY_APT_ARR_P_1
) %>%
  select(
    AIRPORT,
    YEAR,
    MONTH_NUM,
    FLT_DATE,
    FLT_ARR_1,
    DLY_APT_ARR_1,
    starts_with("AD_"),
    FLT_ARR_1_DLY,
    FLT_ARR_1_DLY_15
  )


# ****************************----
# SLOT ADHERENCE ----
# ****************************----

SLOT_DF <- read_csv2(here("data", "APT_DSHBD_SLOT_AD.csv")) 

SLOT_DF <- SLOT_DF %>%
  select(YEAR, MONTH_NUM, MONTH_MON, FLT_DATE,
         APT_ICAO, APT_NAME, STATE_NAME, FLT_DEP_1, FLT_DEP_REG_1,
         FLT_DEP_OUT_EARLY_1,	FLT_DEP_IN_1,	FLT_DEP_OUT_LATE_1) %>%
  mutate(AIRPORT = APT_ICAO)


# ..SLOT YEARLY DATA ----
SLOT_YY_DF <- SLOT_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    # ..........
    FLT_DEP_1,
    FLT_DEP_REG_1,
    FLT_DEP_OUT_EARLY_1,	
    FLT_DEP_IN_1,	
    FLT_DEP_OUT_LATE_1
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR) %>%
  summarise(
    TOT_FLT_DEP_1           = sum(FLT_DEP_1,           na.rm = TRUE),
    TOT_FLT_DEP_REG_1       = sum(FLT_DEP_REG_1,       na.rm = TRUE),
    TOT_FLT_DEP_OUT_EARLY_1 = sum(FLT_DEP_OUT_EARLY_1, na.rm = TRUE),
    TOT_FLT_DEP_IN_1        = sum(FLT_DEP_IN_1,        na.rm = TRUE),
    TOT_FLT_DEP_OUT_LATE_1  = sum(FLT_DEP_OUT_LATE_1,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(AIRPORT, APT_ICAO, YEAR, TOT_FLT_DEP_1,TOT_FLT_DEP_REG_1,TOT_FLT_DEP_OUT_EARLY_1,TOT_FLT_DEP_IN_1,TOT_FLT_DEP_OUT_LATE_1)

# ..SLOT MONTLY DATA ----
SLOT_MM_DF <- SLOT_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    FLT_DEP_1,
    FLT_DEP_REG_1,
    FLT_DEP_OUT_EARLY_1,	
    FLT_DEP_IN_1,	
    FLT_DEP_OUT_LATE_1
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_FLT_DEP_1           = sum(FLT_DEP_1,           na.rm = TRUE),
    TOT_FLT_DEP_REG_1       = sum(FLT_DEP_REG_1,       na.rm = TRUE),
    TOT_FLT_DEP_OUT_EARLY_1 = sum(FLT_DEP_OUT_EARLY_1, na.rm = TRUE),
    TOT_FLT_DEP_IN_1        = sum(FLT_DEP_IN_1,        na.rm = TRUE),
    TOT_FLT_DEP_OUT_LATE_1  = sum(FLT_DEP_OUT_LATE_1,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(AIRPORT, APT_ICAO, YEAR, MONTH_NUM, TOT_FLT_DEP_1,TOT_FLT_DEP_REG_1,TOT_FLT_DEP_OUT_EARLY_1,TOT_FLT_DEP_IN_1,TOT_FLT_DEP_OUT_LATE_1)


# ****************************----
# APDF PUNCTUALITY DATA ----
# ****************************----
PUNC_DF <- read_csv2(here("data","APT_DSHBD_PUNCTUALITY.csv"))

# ..PUNC DEP YEARLY DATA ----
PUNC_DEP_YY_DF <- PUNC_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    PHASE,
    YEAR,
    PUNCT_CAT,	
    # ..........
    NB_FLT,	
    MONTHLY_TRAFIC
    # ..........
  ) %>%
  filter(PHASE == "DEP") %>%
  group_by(AIRPORT, APT_ICAO, PHASE, YEAR, PUNCT_CAT) %>%
  summarise(
    TOT_FLT    = sum(NB_FLT,         na.rm = TRUE),
    TOT_TRAFIC = sum(MONTHLY_TRAFIC, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_PER_CATEG = (TOT_FLT / TOT_TRAFIC)*100
  )


# ..PUNC DEP MONTHLY DATA ----
PUNC_DEP_MM_DF <- PUNC_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    PHASE,
    YEAR,
    MONTH_NUM,
    PUNCT_CAT,	
    # ..........
    NB_FLT,	
    MONTHLY_TRAFIC
    # ..........
  ) %>%
  filter(PHASE == "DEP") %>%
  group_by(AIRPORT, APT_ICAO, PHASE, YEAR, MONTH_NUM,PUNCT_CAT) %>%
  summarise(
    TOT_FLT    = sum(NB_FLT,         na.rm = TRUE),
    TOT_TRAFIC = sum(MONTHLY_TRAFIC, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_PER_CATEG = (TOT_FLT / TOT_TRAFIC)*100
  )

# ..PUNC ARR YEARLY DATA ----
PUNC_ARR_YY_DF <- PUNC_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    PHASE,
    YEAR,
    PUNCT_CAT,	
    # ..........
    NB_FLT,	
    MONTHLY_TRAFIC
    # ..........
  ) %>%
  filter(PHASE == "ARR") %>%
  group_by(AIRPORT, APT_ICAO, PHASE, YEAR, PUNCT_CAT) %>%
  summarise(
    TOT_FLT    = sum(NB_FLT,         na.rm = TRUE),
    TOT_TRAFIC = sum(MONTHLY_TRAFIC, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_PER_CATEG = (TOT_FLT / TOT_TRAFIC)*100
  )


# ..PUNC ARR MONTHLY DATA ----
PUNC_ARR_MM_DF <- PUNC_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    PHASE,
    YEAR,
    MONTH_NUM,
    PUNCT_CAT,	
    # ..........
    NB_FLT,	
    MONTHLY_TRAFIC
    # ..........
  ) %>%
  filter(PHASE == "ARR") %>%
  group_by(AIRPORT, APT_ICAO, PHASE, YEAR, MONTH_NUM,PUNCT_CAT) %>%
  summarise(
    TOT_FLT    = sum(NB_FLT,         na.rm = TRUE),
    TOT_TRAFIC = sum(MONTHLY_TRAFIC, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_PER_CATEG = (TOT_FLT / TOT_TRAFIC)*100
  )


# ****************************----
# APDF MONTLHY DATA ----
# ****************************----
APDF_MM_DF <- read_csv2(here("data","APT_DSHBD_APDF_DATA.csv"))

# ..ASMA YEARLY DATA ----
ASMA_YY_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_ASMA_FL,
    ASMA_REF_TIME_MIN,
    ADD_ASMA_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR) %>%
  summarise(
    TOT_UNIMP_TIME = sum(ASMA_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_ASMA_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_ASMA_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

# ..ASMA MONTHLY DATA ----
ASMA_MM_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_ASMA_FL,
    ASMA_REF_TIME_MIN,
    ADD_ASMA_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_UNIMP_TIME = sum(ASMA_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_ASMA_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_ASMA_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

# ..TAXI-OUT YEARLY DATA ----
TXOT_YY_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_TAXI_OUT_FL,
    TAXI_OUT_REF_TIME_MIN,
    ADD_TAXI_OUT_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR) %>%
  summarise(
    TOT_UNIMP_TIME = sum(TAXI_OUT_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_TAXI_OUT_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_TAXI_OUT_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

# ..TAXI-OUT MONTHLY DATA  ----
TXOT_MM_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_TAXI_OUT_FL,
    TAXI_OUT_REF_TIME_MIN,
    ADD_TAXI_OUT_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_UNIMP_TIME = sum(TAXI_OUT_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_TAXI_OUT_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_TAXI_OUT_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

# ..TAXI-IN YEARLY DATA ----
TXIN_YY_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_TAXI_IN_FL,
    TAXI_IN_REF_TIME_MIN,
    ADD_TAXI_IN_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR) %>%
  summarise(
    TOT_UNIMP_TIME = sum(TAXI_IN_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_TAXI_IN_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_TAXI_IN_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

# ..TAXI-IN MONTHLY DATA  ----
TXIN_MM_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_TAXI_IN_FL,
    TAXI_IN_REF_TIME_MIN,
    ADD_TAXI_IN_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_UNIMP_TIME = sum(TAXI_IN_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_TAXI_IN_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_TAXI_IN_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

# ..PDDLY YEARLY DATA ----
PDDLY_YY_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_DLY_DEP_FL,
    DLY_89_MIN,
    DLY_999_MIN,
    DLY_ZZZ_MIN,
    DLY_OTHER_MIN,
    UN_RPTED_DLY_MIN,
    OV_RPTED_DLY_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR) %>%
  summarise(
    TOT_FLT_DEP = sum(NB_DLY_DEP_FL, na.rm = TRUE),
    TOT_DLY_89 = sum(DLY_89_MIN, na.rm = TRUE),
    TOT_DLY_999 = sum(DLY_999_MIN, na.rm = TRUE),
    TOT_DLY_ZZZ = sum(DLY_ZZZ_MIN, na.rm = TRUE),
    TOT_DLY_OTHER = sum(DLY_OTHER_MIN, na.rm = TRUE),
    TOT_DLY_UNREPORTED = sum(UN_RPTED_DLY_MIN, na.rm = TRUE),
    TOT_DLY_OVREPORTED = sum(OV_RPTED_DLY_MIN, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    TOT_DLY_UNID = TOT_DLY_999 + TOT_DLY_ZZZ + TOT_DLY_UNREPORTED
  ) %>%
  select(AIRPORT, APT_ICAO, YEAR, TOT_DLY_89, TOT_DLY_OTHER, TOT_DLY_UNID)

# ..PDDLY MONTHLY DATA  ----
PDDLY_MM_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_DLY_DEP_FL,
    DLY_89_MIN,
    DLY_999_MIN,
    DLY_ZZZ_MIN,
    DLY_OTHER_MIN,
    UN_RPTED_DLY_MIN,
    OV_RPTED_DLY_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_FLT_DEP = sum(NB_DLY_DEP_FL, na.rm = TRUE),
    TOT_DLY_89 = sum(DLY_89_MIN, na.rm = TRUE),
    TOT_DLY_999 = sum(DLY_999_MIN, na.rm = TRUE),
    TOT_DLY_ZZZ = sum(DLY_ZZZ_MIN, na.rm = TRUE),
    TOT_DLY_OTHER = sum(DLY_OTHER_MIN, na.rm = TRUE),
    TOT_DLY_UNREPORTED = sum(UN_RPTED_DLY_MIN, na.rm = TRUE),
    TOT_DLY_OVREPORTED = sum(OV_RPTED_DLY_MIN, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    TOT_DLY_UNID = TOT_DLY_999 + TOT_DLY_ZZZ + TOT_DLY_UNREPORTED
  ) %>%
  select(AIRPORT, APT_ICAO, YEAR, MONTH_NUM, TOT_DLY_89, TOT_DLY_OTHER, TOT_DLY_UNID)

# ..PDDLY YEARLY AVERAGE  ----

#PDDLY_YY_AVG_DF <- APDF_MM_DF %>%
#  select(AIRPORT, APT_ICAO, YEAR, DLY_89_PER_FL_YY) %>%
#  group_by(AIRPORT, APT_ICAO, YEAR) %>%
#  summarise( DLY_89_PER_FL_YY = mean(DLY_89_PER_FL_YY)) %>%
#  ungroup()
PDDLY_YY_AVG_DF <- APDF_MM_DF %>%
  select(AIRPORT, APT_ICAO, YEAR, DLY_89_PER_FL_MM)%>%
  group_by(AIRPORT, APT_ICAO, YEAR)%>%
  summarise( DLY_89_PER_FL_YY = mean(DLY_89_PER_FL_MM, na.rm = TRUE)) %>%
  ungroup()

# ..PDDLY MONTLHY AVERAGE  ----
PDDLY_MM_AVG_DF <- APDF_MM_DF %>%
  select(AIRPORT, APT_ICAO, YEAR, MONTH_NUM, DLY_89_PER_FL_MM)  %>%
  mutate(MONTH_NUM = as.numeric(MONTH_NUM),
         YEAR =  as.numeric(YEAR))


# **************************----
# APDF MONTLHY DATA PER RWY ----
# **************************----

TXOT_RWY_DF <- read_csv2(here("data","APT_DSHBD_TXOT_RWY.csv"))

# ..TXOT_RWY YEARLY DATA ----
TXOT_RWY_YY_DF <- TXOT_RWY_DF %>%
  select( 
    AIRPORT,
    YEAR,
    MONTH_NUM,
    RUNWAY,
    NB_TAXI_OUT_FL,
    ADD_TAXI_OUT_TIME_MIN,
    TAXI_OUT_REF_TIME_MIN
  ) %>%
  group_by(AIRPORT, YEAR, RUNWAY) %>%
  summarise(
    TOT_REF_TIME = sum(TAXI_OUT_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME   = sum(ADD_TAXI_OUT_TIME_MIN,  na.rm = TRUE),
    TOT_FLT        = sum(NB_TAXI_OUT_FL,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_REF_TIME = TOT_REF_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )


# ..TXOT_RWY MONTHLY DATA ----
TXOT_RWY_MM_DF <- TXOT_RWY_DF %>%
  select( 
    AIRPORT,
    YEAR,
    MONTH_NUM,
    RUNWAY,
    NB_TAXI_OUT_FL,
    ADD_TAXI_OUT_TIME_MIN,
    TAXI_OUT_REF_TIME_MIN
  ) %>%
  group_by(AIRPORT, YEAR, MONTH_NUM, RUNWAY) %>%
  summarise(
    TOT_REF_TIME = sum(TAXI_OUT_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME   = sum(ADD_TAXI_OUT_TIME_MIN,  na.rm = TRUE),
    TOT_FLT        = sum(NB_TAXI_OUT_FL,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_REF_TIME = TOT_REF_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

TXIN_RWY_DF <- read_csv2(here("data","APT_DSHBD_TXIN_RWY.csv"))

# ..TXIN_RWY YEARLY DATA ----
TXIN_RWY_YY_DF <- TXIN_RWY_DF %>%
  select( 
    AIRPORT,
    YEAR,
    MONTH_NUM,
    RUNWAY,
    NB_TAXI_IN_FL,
    ADD_TAXI_IN_TIME_MIN,
    TAXI_IN_REF_TIME_MIN
  ) %>%
  group_by(AIRPORT, YEAR, RUNWAY) %>%
  summarise(
    TOT_REF_TIME = sum(TAXI_IN_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME   = sum(ADD_TAXI_IN_TIME_MIN,  na.rm = TRUE),
    TOT_FLT        = sum(NB_TAXI_IN_FL,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_REF_TIME = TOT_REF_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )


# ..TXOT_RWY MONTHLY DATA ----
TXIN_RWY_MM_DF <- TXIN_RWY_DF %>%
  select( 
    AIRPORT,
    YEAR,
    MONTH_NUM,
    RUNWAY,
    NB_TAXI_IN_FL,
    ADD_TAXI_IN_TIME_MIN,
    TAXI_IN_REF_TIME_MIN
  ) %>%
  group_by(AIRPORT, YEAR, MONTH_NUM, RUNWAY) %>%
  summarise(
    TOT_REF_TIME = sum(TAXI_IN_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME   = sum(ADD_TAXI_IN_TIME_MIN,  na.rm = TRUE),
    TOT_FLT        = sum(NB_TAXI_IN_FL,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_REF_TIME = TOT_REF_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

ASMA_RWY_DF <- read_csv2(here("data","APT_DSHBD_ASMA_RWY.csv"))

# ..ASMA_RWY YEARLY DATA ----
ASMA_RWY_YY_DF <- ASMA_RWY_DF %>%
  select( 
    AIRPORT,
    YEAR,
    MONTH_NUM,
    RUNWAY,
    NB_ASMA_FL,
    ASMA_REF_TIME_MIN,
    ADD_ASMA_TIME_MIN
  ) %>%
  group_by(AIRPORT, YEAR, RUNWAY) %>%
  summarise(
    TOT_UNIMP_TIME = sum(ASMA_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME   = sum(ADD_ASMA_TIME_MIN,  na.rm = TRUE),
    TOT_FLT        = sum(NB_ASMA_FL,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )


# ..ASMA_RWY MONTHLY DATA ----
ASMA_RWY_MM_DF <- ASMA_RWY_DF %>%
  select( 
    AIRPORT,
    YEAR,
    MONTH_NUM,
    RUNWAY,
    NB_ASMA_FL,
    ASMA_REF_TIME_MIN,
    ADD_ASMA_TIME_MIN
  ) %>%
  group_by(AIRPORT, YEAR, MONTH_NUM, RUNWAY) %>%
  summarise(
    TOT_UNIMP_TIME = sum(ASMA_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME   = sum(ADD_ASMA_TIME_MIN,  na.rm = TRUE),
    TOT_FLT        = sum(NB_ASMA_FL,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )


# ****************************----
# APDF DELAY GROUP DATA ----
# ****************************----

APDF_DELAY_DF <- read_csv2(here("data","APT_DSHBD_DELAY_DATA.csv"))

# ..DLY YEARLY DATA ----
DLY_YY_DF <- APDF_DELAY_DF %>%
  select( # ..........
    AIRPORT,
    YEAR,
    MONTH,
    # ..........
    AIRLINE,
    WEATHER,
    EN_ROUTE,
    SECURITY_AND_IMMIGRATION,
    DL_AIRPORT,
    REACTIONARY,
    MISCELLANEOUS,
    UNIDENTIFIED,
    OTHER    
    # ..........
  ) %>%
  group_by(AIRPORT, YEAR) %>%
  summarise(
    TOT_DLY_AIRLINE       = sum(AIRLINE, na.rm = TRUE),
    TOT_DLY_WEATHER       = sum(WEATHER, na.rm = TRUE),
    TOT_DLY_EN_ROUTE      = sum(EN_ROUTE, na.rm = TRUE),
    TOT_DLY_SECURITY_AND_IMMIGRATION = sum(SECURITY_AND_IMMIGRATION, na.rm = TRUE),
    TOT_DLY_AIRPORT       = sum(DL_AIRPORT, na.rm = TRUE),
    TOT_DLY_REACTIONARY   = sum(REACTIONARY, na.rm = TRUE),
    TOT_DLY_MISCELLANEOUS = sum(MISCELLANEOUS, na.rm = TRUE),
    TOT_DLY_UNIDENTIFIED  = sum(UNIDENTIFIED, na.rm = TRUE),
    TOT_DLY_OTHER         = sum(OTHER, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(AIRPORT, YEAR, 
         TOT_DLY_AIRLINE,
         TOT_DLY_WEATHER,
         TOT_DLY_EN_ROUTE,
         TOT_DLY_SECURITY_AND_IMMIGRATION,
         TOT_DLY_AIRPORT,
         TOT_DLY_REACTIONARY,
         TOT_DLY_MISCELLANEOUS,
         TOT_DLY_UNIDENTIFIED,
         TOT_DLY_OTHER)

# ..DLY MONTHLY DATA ----
DLY_MM_DF <- APDF_DELAY_DF %>%
  select( # ..........
    AIRPORT,
    YEAR,
    MONTH,
    # ..........
    AIRLINE,
    WEATHER,
    EN_ROUTE,
    SECURITY_AND_IMMIGRATION,
    DL_AIRPORT,
    REACTIONARY,
    MISCELLANEOUS,
    UNIDENTIFIED,
    OTHER    
  ) %>%
  mutate(MONTH = as.numeric(MONTH),
        YEAR =  as.numeric(YEAR))%>%
  group_by(AIRPORT, YEAR, MONTH) %>%
  summarise(
    TOT_DLY_AIRLINE       = sum(AIRLINE, na.rm = TRUE),
    TOT_DLY_WEATHER       = sum(WEATHER, na.rm = TRUE),
    TOT_DLY_EN_ROUTE      = sum(EN_ROUTE, na.rm = TRUE),
    TOT_DLY_SECURITY_AND_IMMIGRATION = sum(SECURITY_AND_IMMIGRATION, na.rm = TRUE),
    TOT_DLY_AIRPORT       = sum(DL_AIRPORT, na.rm = TRUE),
    TOT_DLY_REACTIONARY   = sum(REACTIONARY, na.rm = TRUE),
    TOT_DLY_MISCELLANEOUS = sum(MISCELLANEOUS, na.rm = TRUE),
    TOT_DLY_UNIDENTIFIED  = sum(UNIDENTIFIED, na.rm = TRUE),
    TOT_DLY_OTHER         = sum(OTHER, na.rm = TRUE)
  )  %>%
  ungroup() %>%
  select(AIRPORT, YEAR, MONTH,
         TOT_DLY_AIRLINE,
         TOT_DLY_WEATHER,
         TOT_DLY_EN_ROUTE,
         TOT_DLY_SECURITY_AND_IMMIGRATION,
         TOT_DLY_AIRPORT,
         TOT_DLY_REACTIONARY,
         TOT_DLY_MISCELLANEOUS,
         TOT_DLY_UNIDENTIFIED,
         TOT_DLY_OTHER)


# ****************************----
# APDF TURNAROUND DATA ----
# ****************************----

APDF_TURN_DF <- read_csv2(here("data","APT_DSHBD_TURNAROUND.csv"))

# ..TURN YEARLY DATA ----
TURN_YY_DF <- APDF_TURN_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    AC_CLASS,
    # ..........
    NB_TURN_ARROUND,
    NB_OVERSHOOT,
    TOT_SDTT_MIN,
    TOT_ACTT_MIN,
    TOT_ADTT_MIN
    # ..........
  ) %>%
  filter(AC_CLASS %in% c("H", "MJ", "MT")) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, AC_CLASS) %>%
  summarise(
    TOT_SDTT_MIN = sum(TOT_SDTT_MIN, na.rm = TRUE),
    TOT_ACTT_MIN = sum(TOT_ACTT_MIN, na.rm = TRUE),
    TOT_ADTT_MIN = sum(TOT_ADTT_MIN, na.rm = TRUE),
    TOT_TURN     = sum(NB_TURN_ARROUND, na.rm = TRUE),
    TOT_OVER     = sum(NB_OVERSHOOT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_SDTT = TOT_SDTT_MIN / TOT_TURN,
    AVG_ACTT = TOT_ACTT_MIN / TOT_TURN,
    AVG_ADTT = TOT_ADTT_MIN / TOT_TURN
  ) %>%
  select(AIRPORT, APT_ICAO, YEAR, AC_CLASS, TOT_TURN, TOT_OVER, AVG_SDTT, AVG_ACTT, AVG_ADTT)

# ..TURN MONTHLY DATA ----
TURN_MM_DF <- APDF_TURN_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    AC_CLASS,
    # ..........
    NB_TURN_ARROUND,
    NB_OVERSHOOT,    
    TOT_SDTT_MIN,
    TOT_ACTT_MIN,
    TOT_ADTT_MIN
    # ..........
  ) %>%
  filter(AC_CLASS %in% c("H", "MJ", "MT")) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM, AC_CLASS) %>%
  summarise(
    TOT_SDTT_MIN = sum(TOT_SDTT_MIN, na.rm = TRUE),
    TOT_ACTT_MIN = sum(TOT_ACTT_MIN, na.rm = TRUE),
    TOT_ADTT_MIN = sum(TOT_ADTT_MIN, na.rm = TRUE),
    TOT_TURN     = sum(NB_TURN_ARROUND, na.rm = TRUE),
    TOT_OVER     = sum(NB_OVERSHOOT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_SDTT = TOT_SDTT_MIN / TOT_TURN,
    AVG_ACTT = TOT_ACTT_MIN / TOT_TURN,
    AVG_ADTT = TOT_ADTT_MIN / TOT_TURN,
    AVG_OVER = TOT_OVER     / TOT_TURN
  ) %>%
  select(AIRPORT, APT_ICAO, YEAR, MONTH_NUM, AC_CLASS, TOT_TURN, TOT_OVER, AVG_SDTT, AVG_ACTT, AVG_ADTT, AVG_OVER)


# ..TURN NEW DATA ----
TURN_NEW_DF <- APDF_TURN_DF %>%
  filter(AC_CLASS %in% c("H", "MJ", "MT"))

TURN_NEW_DF <- TURN_NEW_DF %>%  
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_TURN_ARROUND,
    NB_OVERSHOOT,    
    TOT_SDTT_MIN,
    TOT_ACTT_MIN,
    TOT_ADTT_MIN,
    TOT_ERTT_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_SDTT_MIN = sum(TOT_SDTT_MIN, na.rm = TRUE),
    TOT_ACTT_MIN = sum(TOT_ACTT_MIN, na.rm = TRUE),
    TOT_ADTT_MIN = sum(TOT_ADTT_MIN, na.rm = TRUE),
    TOT_ERTT_MIN = sum(TOT_ERTT_MIN, na.rm = TRUE),
    TOT_TURN     = sum(NB_TURN_ARROUND, na.rm = TRUE),
    TOT_OVER     = sum(NB_OVERSHOOT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_SDTT = TOT_SDTT_MIN / TOT_TURN,
    AVG_ACTT = TOT_ACTT_MIN / TOT_TURN,
    AVG_ADTT = TOT_ADTT_MIN / TOT_TURN,
    AVG_ERTT = TOT_ERTT_MIN / TOT_TURN,
    AVG_OVER = TOT_OVER     / TOT_TURN
  ) %>%
  select(AIRPORT, APT_ICAO, YEAR, MONTH_NUM, TOT_TURN, TOT_OVER, AVG_SDTT, AVG_ACTT, AVG_ADTT, AVG_ERTT, AVG_OVER)


# ****************************----
# CDO/CCO DATA ----
# ****************************----

CDO_CCO_DF <- read_csv2(here("data","APT_DSHBD_CDO_CCO.csv")) %>%
  arrange(YEAR, MONTH_NUM, APT_ICAO) %>% 
  select(YEAR, MONTH_NUM, APT_ICAO,
         NBR_FLIGHTS_DESCENT, TOT_TIME_LEVEL_SECONDS_DESCENT, TOT_TIME_LEVEL_SEC_DESC_BLW_70,
         MEDIAN_CDO_ALT, NBR_CDO_FLIGHTS, NBR_CDO_FLIGHTS_BELOW_7000,
         TOT_DELTA_CO2_KG_DESCENT, TOT_DELTA_CO2_KG_DESC_BLW_70,
         NBR_FLIGHTS_CLIMB, TOT_TIME_LEVEL_SECONDS_CLIMB, TOT_TIME_LVL_SEC_CLIMB_BLW_100,
         MEDIAN_CCO_ALT, NBR_CCO_FLIGHTS, NBR_CCO_FLIGHTS_BELOW_10000,
         TOT_DELTA_CO2_KG_CLIMB, TOT_DELTA_CO2_KG_CLIMB_BLW_100) %>% 
  mutate(AVG_TIME_LVL_DESCENT=TOT_TIME_LEVEL_SECONDS_DESCENT/NBR_FLIGHTS_DESCENT,
         AVG_TIME_LVL_DESCENT_BLW_70=TOT_TIME_LEVEL_SEC_DESC_BLW_70/NBR_FLIGHTS_DESCENT,
         SHARE_CDO_FLIGHTS=NBR_CDO_FLIGHTS/NBR_FLIGHTS_DESCENT,
         SHARE_CDO_FLIGHTS_BLW_70=NBR_CDO_FLIGHTS_BELOW_7000/NBR_FLIGHTS_DESCENT,
         AVG_TIME_LVL_CLIMB=TOT_TIME_LEVEL_SECONDS_CLIMB/NBR_FLIGHTS_CLIMB,
         AVG_TIME_LVL_CLIMB_BLW_100=TOT_TIME_LVL_SEC_CLIMB_BLW_100/NBR_FLIGHTS_CLIMB,
         SHARE_CCO_FLIGHTS=NBR_CCO_FLIGHTS/NBR_FLIGHTS_CLIMB,
         SHARE_CCO_FLIGHTS_BLW_100=NBR_CCO_FLIGHTS_BELOW_10000/NBR_FLIGHTS_CLIMB) %>% 
  select(AIRPORT=APT_ICAO, YEAR, MONTH_NUM,
         NBR_FLIGHTS_DESCENT, AVG_TIME_LVL_DESCENT, AVG_TIME_LVL_DESCENT_BLW_70, 
         MEDIAN_CDO_ALT, NBR_CDO_FLIGHTS, SHARE_CDO_FLIGHTS, SHARE_CDO_FLIGHTS_BLW_70,
         TOT_DELTA_CO2_KG_DESCENT, TOT_DELTA_CO2_KG_DESC_BLW_70,
         NBR_FLIGHTS_CLIMB, AVG_TIME_LVL_CLIMB, AVG_TIME_LVL_CLIMB_BLW_100, 
         MEDIAN_CCO_ALT, NBR_CCO_FLIGHTS, SHARE_CCO_FLIGHTS, SHARE_CCO_FLIGHTS_BLW_100,
         TOT_DELTA_CO2_KG_CLIMB, TOT_DELTA_CO2_KG_CLIMB_BLW_100)


