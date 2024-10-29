# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Common data preparation script for chapters (sections) 2
# Takuya Yamanaka, June 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load data packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(stringr)
library(dplyr)
library(tidyr)
library(here)
library(readr) # to save csv
library(magrittr) # to use tee pipe
library(data.table)
library(jsonlite) # for provisional monthly notification in India
library(gtbreport)

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))
source(here("report/functions/calculate_outcomes.R"))


# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2024
snapshot_date <- latest_snapshot_date()
csv_datestamp <- "2024-06-03"
lsaved_year <- 2010

# Set whether or not to include objects with estimates in the output ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

show_estimates <- T
latest_estimates <- T
latest_lsaved <- T
datacoll <- T

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))


# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

countries <- load_gtb("cty")
notification <- load_gtb("tb")
tpt <- load_gtb("tpt")
TBHIV_for_aggregates <- load_gtb("agg")
estimates_ltbi <- load_gtb("ltbi")
strategy <- load_gtb("sty")
estimates_population <- load_gtb("pop")
grpmbr <- load_gtb("grpmbr")
grp <- load_gtb("grp")
data_collection <- load_gtb("datacoll")
dr_surveillance <- load_gtb("drroutine")
outcomes <- load_gtb("tx")

who_region_shortnames <- region_shortnames()

# Fix lists of the three sets of 30 high burden countries (used to filter records for some figures)
hbc30 <- hb_list("tb")

hbmdr30 <- grpmbr %>%
  filter(group_type == "g_hb_mdr" & group_name == 1) %>%
  select(iso3,group_type)

hbtbhiv30 <- grpmbr %>%
  filter(group_type == "g_hb_tbhiv" & group_name == 1) %>%
  select(iso3,group_type)


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
iso3_hmdrc <- hbmdr30$iso3


iso3_income <- grpmbr %>%
  filter(group_type == "g_income") %>% select(iso3,group_name) %>% rename(g_income=2)

list_hbcs_income <-
  list_hbcs %>%
  left_join(iso3_income)

list_hbcs_plus_wl_income <-
  list_hbcs_plus_wl %>%
  left_join(iso3_income)

# WB income group list
wb_incomelist <- grpmbr %>%
  filter(group_type == "g_income") %>% select(iso3,group_name) %>% rename(income=2)

# Get global and regional estimates directly from the files that get imported into the database
if(show_estimates) {
  if(latest_estimates) {

  load(here::here("inc_mort/analysis/est.rda"))
  est_country <- est
  load(here::here("inc_mort/analysis/global.rda"))
  est_global <- global
  load(here::here("inc_mort/analysis/regional.rda"))
  est_regional <- regional
  
  load(here::here('drtb/output/db_dr_country.rda'))
  est_dr_country <- db_dr_country
  
  load(here::here('drtb/output/db_dr_group.rda'))
  est_dr_group <- db_dr_group

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

    load(here::here("local/inc_mort/analysis/regional.rda"))
    est_regional <- regional%>%
      select(-c.newinc)
    est_regional_ym2 <- est_regional %>%
      filter(year == report_year-2)
    est_regional_ym2 <- est_regional_ym2 %>%
      mutate(year = report_year-1)
    est_regional <- rbind(est_regional, est_regional_ym2) %>%
      arrange(g.whoregion,year)
    
    newinc_regional <- newinc_coutry %>%
      select(-iso3) %>%
      group_by(year, g.whoregion) %>%
      summarise_all(sum, na.rm = T) %>%
      ungroup()
    est_regional <- newinc_regional %>%
      left_join(est_regional, by = c("g.whoregion", "year"))
    
    load(here::here("local/inc_mort/analysis/global.rda"))
    est_global <- global %>%
      select(-c.newinc)
    est_global_ym2 <- est_global %>%
      filter(year == report_year-2)
    est_global_ym2 <- est_global_ym2 %>%
      mutate(year = report_year-1)
    est_global <- rbind(est_global, est_global_ym2) %>%
      arrange(year)
    
    newinc_global <- newinc_regional %>%
      select(-g.whoregion) %>%
      group_by(year) %>%
      summarise_all(sum, na.rm = T) %>%
      ungroup()
    est_global <- newinc_global %>%
      left_join(est_global, by = c("year"))
    
    load(here::here('local/drtb/dboutput/db_dr_country.rda'))
    est_dr_country <- db_dr_country
    est_dr_country_ym2 <- est_dr_country %>%
      filter(year == report_year-2)
    est_dr_country_ym2 <- est_dr_country_ym2 %>%
      mutate(year = report_year-1)
    est_dr_country <- rbind(est_dr_country, est_dr_country_ym2) %>%
      arrange(year)
    
    load(here::here('local/drtb/dboutput/db_dr_group.rda'))
    est_dr_group <- db_dr_group
    est_dr_group_ym2 <- est_dr_group %>%
      filter(year == report_year-2)
    est_dr_group_ym2 <- est_dr_group_ym2 %>%
      mutate(year = report_year-1)
    est_dr_group <- rbind(est_dr_group, est_dr_group_ym2) %>%
      arrange(year)
    
    # Get Pete's aggregate incidence estimates by age group and sex
    load(here('local/disaggregation/dboutput/db_estimates_country.Rdata'))
    db_estimates_country <- db_estimates_country %>%
      mutate(year = report_year-1)
    
    load(here('local/disaggregation/dboutput/db_estimates_group.Rdata'))
    db_estimates_group <- db_estimates_group %>%
      mutate(year = report_year-1)
  } 
}

if(latest_lsaved) {
  load(here::here(paste0('lives_saved/output/regional_saved_',lsaved_year,'.rda')))
  lives_saved <- saved.regional.print
  
  load(here::here(paste0('lives_saved/output/regional_saved_',2005,'.rda')))
  lives_saved_hiv <- saved.regional.print
} else {
  load(here::here(paste0('local/lives_saved/output/regional_saved_',lsaved_year,'.rda')))
  lives_saved <- saved.regional.print
  
  load(here::here(paste0('local/lives_saved/output/regional_saved_',2005,'.rda')))
  lives_saved_hiv <- saved.regional.print
  
}

# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)






