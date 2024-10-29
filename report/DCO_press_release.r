# Requested by DCO for the press release for the publication of the Global TB report
# % of TB cases in low and middle-income countries

# Load data packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(dplyr)
library(tidyr)
library(here)
library(gtbreport)

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))



# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2024

# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

countries <- load_gtb("cty")
grpmbr <- load_gtb("grpmbr")



load(here::here("inc_mort/analysis/est.rda"))
est_country <- est
load(here::here("inc_mort/analysis/global.rda"))
est_global <- global
rm(est, global)

# Get list of countries in low and middle-income groups
iso3_LMIC <- grpmbr |> 
  filter(group_type == "g_income" & group_name %in% c("LIC", "LMC", "UMC")) |> 
  select(iso3)

# Get total number of incident cases in 2023 for the LMIC countries

inc_num_LMIC <- est_country |> 
  filter(year == report_year - 1) |> 
  inner_join(iso3_LMIC, by = "iso3") |> 
  summarise(e_inc_num = sum(inc.num, na.rm=TRUE))

# Calculate % of global incidence
inc_num_LMIC_pct <- inc_num_LMIC$e_inc_num * 100 / (est_global |> filter(year==report_year - 1) |> select(inc.num) )

