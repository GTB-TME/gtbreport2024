# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data loading script - testing version
# Takuya Yamanaka, July 2024
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
library(readxl)

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))
# source(here("report/functions/calculate_outcomes.R"))

# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2024
snapshot_date <- latest_snapshot_date()
# csv_datestamp <- "2024-06-03"

# Set variables
base_year <- report_year - 1

# Set whether or not to include objects with estimates in the output ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
show_estimates <- T

# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

notification <- load_gtb("tb")
tpt <- load_gtb("tpt")
TBHIV_for_aggregates <- load_gtb("agg")
estimates_population <- load_gtb("pop")
grpmbr <- load_gtb("grpmbr")
grp <- load_gtb("grp")
finance <- load_gtb("finance") # tentative addition for testing
outcomes <- load_gtb("tx")

who_region_shortnames <- region_shortnames()

# Fix lists of the three sets of 30 high burden countries (used to filter records for some figures)
hbc30 <- hb_list("tb") %>%
  mutate(g_hb_tb = 1) %>%
  select(-group_type)

hbmdr30 <- grpmbr %>%
  filter(group_type == "g_hb_mdr" & group_name == 1) %>%
  mutate(g_hb_mdr = 1) %>%
  select(iso3, g_hb_mdr)

hbtbhiv30 <- grpmbr %>%
  filter(group_type == "g_hb_tbhiv" & group_name == 1) %>%
  mutate(g_hb_tbhiv = 1) %>%
  select(iso3, g_hb_tbhiv)


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
# if(show_estimates) {
#   
#   load(here::here("inc_mort/analysis/est.rda"))
#   est_country <- est
#   load(here::here("inc_mort/analysis/global.rda"))
#   est_global <- global
#   load(here::here("inc_mort/analysis/regional.rda"))
#   est_regional <- regional
#   
#   names(est_country) <- gsub("\\.", "_", names(est_country))
#   
#   load(here::here('drtb/dboutput/db_dr_country.rda'))
#   est_dr_country <- db_dr_country
#   
#   load(here::here('drtb/dboutput/db_dr_group.rda'))
#   est_dr_group <- db_dr_group
#   
#   # Get Pete's aggregate incidence estimates by age group and sex
#   load(here('disaggregation/dboutput/db_estimates_country.Rdata'))
#   load(here('disaggregation/dboutput/db_estimates_group.Rdata'))
#   
# }

# Load WB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# source(here("import/save_wb.R")) # running this code may take 2-3 minutes
load(here("data/wb/wb.Rda"))

wb <- wb %>%
  select(iso3=iso3c,year=date,indicator_id,value) %>%
  pivot_wider(names_from = indicator_id, values_from = value) 

names(wb) <- gsub("\\.", "_", names(wb))
names(wb) <- tolower(names(wb)) 

wb <- wb %>%
  arrange(iso3, year)


# Load IMF data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# source(here("import/save_imfweo.R")) # running this code may take 2-3 minutes
load(here("data/imf/imfweo.Rda"))

weo <- weo %>%
  select(-dataset,-country) %>%
  select(iso3, year, imf_cabal_pcgdp=BCA_NGDPD, imf_debt_pcgdp=GGXWDG_NGDP, imf_govexp_pcgdp=GGX_NGDP,
         imf_gdp_pc_cur_usd=NGDPDPC, imf_gdp_pc_cur_lcu=NGDPPC, imf_gdp_pc_con_lcu=NGDPRPC, imf_deflator=NGDP_D,
         imf_ppp_conv=PPPEX, imf_gdp_pc_cur_int=PPPPC, deflator:deflator_us)

# recalculate 
def_base <- weo %>%
  group_by(iso3) %>%
  filter(year == report_year-1) %>%
  select(iso3, base_deflator = deflator, base_deflator_us = deflator_us, base_imf_deflator = imf_deflator)

weo <- weo %>%
  left_join(def_base, by = "iso3") %>%
  mutate(deflator = deflator/base_deflator,
         deflator_us = deflator_us/base_deflator_us,
         imf_deflator = imf_deflator/base_imf_deflator)

# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)

# Load pre-2014 finance data and merge with the snapshot data----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
finance_pre2014 <- haven::read_dta(here::here("finance/raw/finance1.dta")) %>%
  filter(year <= 2014)
finance_pre2014 <- finance_pre2014[, names(finance_pre2014) %in% names(finance)]

finance <- finance %>%
  filter(year > 2014) %>%
  plyr::rbind.fill(finance_pre2014) %>%
  arrange(country, year)

# UN operational rates of exchange ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
treasury <- haven::read_dta(here::here("finance/raw/treasury.dta"))

treasury_iso3 <- treasury %>%
  select(country, iso3) %>%
  distinct()

unore <-  read_excel(here::here('data/unore/unore.xlsx')) %>%
  select(abbrv_name:rate) %>%
  rename(country=1, currenty=2, un_pa_exch_rate=5) %>%
  mutate(eff_date = as.Date(eff_date, format = "%d %b %Y")) %>%
  mutate(year = as.numeric(format(eff_date, "%Y"))) %>%
  group_by(country, year) %>%
  summarize(un_pa_exch_rate = mean(un_pa_exch_rate, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 2023) %>%
  left_join(treasury_iso3, by = "country")

treasury <- plyr::rbind.fill(treasury,unore) %>%
  arrange(country, year)