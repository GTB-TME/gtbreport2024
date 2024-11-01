# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch5.rmd UHC and TB determinants
# Takuya Yamanaka, May 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(dplyr)
library(tidyr)
library(here)

library(tidyverse)
library(readxl)
library(magrittr)
library(scales)
library(stringr)
library(gtbreport)
library(metafor)
library("zoo")
library(english)

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))

# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

report_year <- 2024
snapshot_date <- latest_snapshot_date()

show_estimates <- T
latest_estimates <- T

# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

notification <- load_gtb("tb")
tpt <- load_gtb("tpt")
TBHIV_for_aggregates <- load_gtb("agg")
estimates_ltbi <- load_gtb("ltbi")
strategy <- load_gtb("sty")
estimates_population <- load_gtb("pop")
grpmbr <- load_gtb("grpmbr")
sdg <- load_gtb("sdg")
sp <- load_gtb("covid")

who_region_shortnames <- region_shortnames()

# load imf data for converting cost data from current USD to constant USD
load(here("data/imf/imfweo.Rda"))

# list iso3 - country
list_iso3_country <-
  grpmbr %>% filter(group_type == 'g_whoregion') %>%
  select(iso3,country)

list_watch_list <-
  list_iso3_country %>%
  filter(iso3 %in% c('KHM','RUS','ZWE'))

list_hbcs <-
  grpmbr %>%
  filter(group_type == "g_hb_tb" & group_name == 1) %>%
  select(iso3,country) %>%
  arrange(country)

list_hbcs_plus_wl <- list_hbcs %>% add_row(list_watch_list) %>%   arrange(country)

iso3_hbc <- list_hbcs$iso3
iso3_hbc_plus_wl <- list_hbcs_plus_wl$iso3

iso3_income <- grpmbr %>%
  filter(group_type == "g_income") %>% select(iso3,group_name) %>% rename(g_income=2)

list_hbcs_income <-
  list_hbcs %>%
  left_join(iso3_income)

list_hbcs_plus_wl_income <-
  list_hbcs_plus_wl %>%
  left_join(iso3_income)

dummydf <-
  list_hbcs_income %>%
  mutate(year=0,value=0) %>%
  select(iso3,year,value,country,g_income)


# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)

# Get global and regional estimates directly from the files that get imported into the database
if(show_estimates) {
  if(latest_estimates) {
    
    load(here::here("inc_mort/analysis/est.rda"))
    est_country <- est
    
    load(here::here("inc_mort/analysis/rf.global.rda"))
    rf_global <- rf.global %>%
      rename(risk_factor = risk.factor)
    load(here::here("inc_mort/analysis/rf.rda"))
    rf_country <- rf %>%
      rename(risk_factor = risk.factor)
    
    # Get Pete's aggregate incidence estimates by age group and sex
    load(here('disaggregation/output/db_estimates_country.Rdata'))
    load(here('disaggregation/output/db_estimates_group.Rdata'))
    
  } else {
    # until the first Rd estimates ready, use last year's estimates that were copied from last year's github repo to this year's local - DO NOT push!
    load(here::here("local/inc_mort/analysis/est.rda"))
    est_country <- est %>%
      select(-c.newinc, -g.whoregion)
    est_country_ym2 <- est_country %>%
      filter(year == report_year-2)
    est_country_ym2 <- est_country_ym2 %>%
      mutate(year = report_year-1)
    est_country <- rbind(est_country, est_country_ym2) %>%
      arrange(iso3, year)
    
    newinc_coutry <- notification %>%
      select(iso3, year, g.whoregion = g_whoregion, c.newinc = c_newinc) %>%
      filter(year>=2000)
    
    est_country <- newinc_coutry %>%
      left_join(est_country, by = c("iso3", "year"))
    
    load(here::here("local/inc_mort/analysis/rf.global.rda"))
    rf_global <- rf.global %>%
      rename(risk_factor = risk.factor) %>%
      mutate(year = report_year-1)
    
    load(here::here("local/inc_mort/analysis/rf.rda"))
    rf_country <- rf %>%
      rename(risk_factor = risk.factor) %>%
      mutate(year = report_year-1)
    
    # Get Pete's aggregate incidence estimates by age group and sex
    load(here('local/disaggregation/dboutput/db_estimates_country.Rdata'))
    db_estimates_country <- db_estimates_country %>%
      mutate(year = report_year-1)
    
    load(here('local/disaggregation/dboutput/db_estimates_group.Rdata'))
    db_estimates_group <- db_estimates_group %>%
      mutate(year = report_year-1)
  } 
}
