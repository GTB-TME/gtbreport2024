# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Common data preparation script for FT: MAF-TB and community engagement
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

# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2024
snapshot_date <- latest_snapshot_date()

datacoll <- T

# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
policy <- load_gtb("covid")
grpmbr <- load_gtb("grpmbr")
grp <- load_gtb("grp")
strategy <- load_gtb("sty")
data_collection <- load_gtb("datacoll")
finance <- load_gtb("finance") 

who_region_shortnames <- region_shortnames()

# Fix lists of the three sets of 30 high burden countries (used to filter records for some figures)
hbc30 <- hb_list("tb")

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

# Get the variable to identify countries requested to report on community indicators
maf_use_2020_for_2021 <- data_collection %>%
  filter(datcol_year==2021 & dc_unhlm_display==0) %>%
  select(iso3,
         dc_unhlm_display)

maf_use_2021_for_2022 <- data_collection %>%
  filter(datcol_year==2022 & dc_unhlm_display==0) %>%
  filter(!iso3 %in% maf_use_2020_for_2021$iso3) %>%
  select(iso3,
         dc_unhlm_display)

maf_use_2020_for_2022 <- data_collection %>%
  filter(datcol_year==2022 & dc_unhlm_display==0) %>%
  filter(iso3 %in% maf_use_2020_for_2021$iso3) %>%
  select(iso3,
         dc_unhlm_display)

# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 1 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# maf_data <- policy %>%
#   select(country:g_whoregion,annual_report_published:ms_review_civil_soc,min_agg_collab:min_tra_collab)

# 
# f1_2021_add <- maf_data %>%
#   filter(year == 2020) %>%
#   mutate(n_country = 1) %>%
#   select(iso3, year,n_country,annual_report_published:ms_review_civil_soc,-ms_review_doc) %>%
#   mutate(all_three = ifelse(annual_report_published==1&ms_review==1&ms_review_civil_soc, 1, 0)) %>%
#   filter(iso3 %in% maf_use_2020_for_2021$iso3) %>%
#   mutate(year = 2021)
# 
# f1_2022_add1 <- maf_data %>%
#   filter(year == 2021) %>%
#   mutate(n_country = 1) %>%
#   select(iso3, year,n_country,annual_report_published:ms_review_civil_soc,-ms_review_doc) %>%
#   mutate(all_three = ifelse(annual_report_published==1&ms_review==1&ms_review_civil_soc, 1, 0)) %>%
#   filter(iso3 %in% maf_use_2021_for_2022$iso3)
# 
# f1_2022_add2 <- maf_data %>%
#   filter(year == 2020) %>%
#   mutate(n_country = 1) %>%
#   select(iso3, year,n_country,annual_report_published:ms_review_civil_soc,-ms_review_doc) %>%
#   mutate(all_three = ifelse(annual_report_published==1&ms_review==1&ms_review_civil_soc, 1, 0)) %>%
#   filter(iso3 %in% maf_use_2020_for_2022$iso3)
# 
# f1_2022_add <- rbind(f1_2022_add1,f1_2022_add2) %>%
#   mutate(year = 2022)
# 
# f1_data <- f1_base_data %>%
#   rbind(f1_2021_add,f1_2022_add) %>%
#   arrange(iso3, year) 
# 
# f1_data <- f1_data %>%
#   group_by(year) %>%
#   summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
#   ungroup() 
# 
# f1_2020_data <- f1_data %>%
#   filter(year ==  2020)
# 

# %>%
#   pivot_longer(-c(year,n_country)) %>%
#   mutate(name = factor(name, levels = c( "ms_review", "ms_review_civil_soc","annual_report_published", "all_three"))) %>%
#   mutate(name = factor(name, labels = c(                  'National multisectoral and multistakeholder accountability and review mechanism, under high-level leadership available',
#                                                         'Engagement of civil society and affected communities in the multisectoral accountability and review mechanism',
#                                                         'Annual national TB report publicly available',
#                                                         'All three core elements')))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: Table 1 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
maf_data <- policy %>%
  select(country:g_whoregion,annual_report_published:ms_review_civil_soc,min_agg_collab:min_tra_collab)
  
t1_region_data <- maf_data %>%
  filter(year == report_year) %>%
  mutate(n_country = 1) %>%
  select(g_whoregion,n_country,annual_report_published:ms_review_civil_soc,-ms_review_doc) %>%
  mutate(all_three = ifelse(annual_report_published==1&ms_review==1&ms_review_civil_soc, 1, 0)) %>%
  # calculate regional aggregates
  group_by(g_whoregion) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  arrange(entity) %>%
  select(-g_whoregion)

# Add global summary to the regional summary
t1_global_data <- t1_region_data %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(entity="Global total")

# Add 30HBC summary to the regional summary
t1_hbc_data <- maf_data %>%
  filter(year == report_year, iso3 %in% iso3_hbc) %>%
  mutate(n_country = 1) %>%
  select(g_whoregion,n_country,annual_report_published:ms_review_civil_soc,-ms_review_doc) %>%
  mutate(all_three = ifelse(annual_report_published==1&ms_review==1&ms_review_civil_soc, 1, 0)) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(entity="High TB burden countries")

beg = " ("
end = "%)"

t1_data <- rbind(t1_hbc_data, t1_region_data, t1_global_data ) %>%
  mutate(pct_report = annual_report_published/n_country*100,
         pct_ms     = ms_review/n_country*100,
         pct_cs     = ms_review_civil_soc/n_country*100,
         pct_all    = all_three/n_country*100)

t1_data_table <- t1_data %>%
  mutate(report = paste0(annual_report_published, beg, ftb(pct_report), end),
         ms = paste0(ms_review, beg, ftb(pct_ms), end),
         cs = paste0(ms_review_civil_soc, beg, ftb(pct_cs), end),
         all = paste0(all_three, beg, ftb(pct_all), end)) %>%
  select(entity, n_country, ms:cs, report, all)

f1_txt_data <- maf_data %>%
  filter(year >= 2020) %>%
  mutate(n_country = 1) %>%
  select(iso3,year,n_country,annual_report_published:ms_review_civil_soc,-ms_review_doc) %>%
  mutate(all_three = ifelse(annual_report_published==1&ms_review==1&ms_review_civil_soc, 1, 0))

f1_txt_data <- f1_txt_data %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  ungroup()

t1_txt_latest <- f1_txt_data %>%
  filter(year ==  report_year) 

t1_txt_2020 <- f1_txt_data %>%
  filter(year ==  2020)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: Fig 1 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f1_data <- maf_data %>%
  filter(year == report_year) %>%
  select(min_agg_collab:min_tra_collab) 

f1_data <- f1_data %>% tidyr::gather(variable, category) %>%
  group_by(variable, category) %>%
  count() %>%
  ungroup %>% 
  mutate(pct = n/f1_data%>%nrow()*100) %>%
  filter(category>10) 

f1_data <- f1_data %>%
  mutate(variable = factor(variable, labels = c("Agriculture", "Defence", "Social development", "Education", "Finance", "Justice", "Labour", "Transport")),
         variable = factor(variable, levels = c("Agriculture", "Defence", "Education", "Finance", "Justice", "Labour", "Social development", "Transport")))
  
f1_txt <- f1_data %>%
  group_by(variable,category) %>%
  select(-n) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))


f1_txt2 <- maf_data %>%
  filter(year == report_year) %>%
  select(iso3, min_agg_collab:min_tra_collab) %>%
  filter(iso3 %in% iso3_hbc)

f1_txt2 <- f1_txt2 %>%
  select(-iso3) %>%
  tidyr::gather(variable, category) %>%
  group_by(variable, category) %>%
  count() %>%
  ungroup %>%
  mutate(pct = n/f1_txt2%>%nrow()*100) %>%
  filter(category>10)

f1_txt2 <- f1_txt2 %>%
  mutate(variable = factor(variable, labels = c("Aggriculture", "Defence", "Social development", "Education", "Finance", "Justice", "Labour", "Transport")),
         variable = factor(variable, levels = c("Aggriculture", "Defence", "Education", "Finance", "Justice", "Labour", "Social development", "Transport")))

f1_txt2 <- f1_txt2 %>%
  group_by(variable,category) %>%
  select(-n) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: Fig 2 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2_country_data <- maf_data %>%
  filter(year == report_year) %>%
  select(iso3,g_whoregion,min_agg_collab:min_tra_collab) 

f2_region_data <- f2_country_data %>% 
  select(-iso3) %>%
  tidyr::gather(variable, category, -g_whoregion) %>%
  group_by(g_whoregion, variable, category) %>%
  count() %>%
  ungroup %>% 
  filter(category>10) %>%
  select(-category) %>%
  group_by(g_whoregion, variable) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  # merge with regional names
  inner_join(select(t1_data, c("entity","n_country")), by = "entity") %>%
  ungroup %>% 
  select(-g_whoregion)

# Add global summary to the regional summary
f2_global_data <- f2_region_data %>%
  select(-n_country) %>%
  group_by(variable) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(n_country = 215) %>%
  mutate(entity="Global")


f2_hbc_data <- f2_country_data %>% 
  filter(iso3 %in% iso3_hbc) %>%
  select(-iso3, -g_whoregion) %>%
  tidyr::gather(variable, category) %>%
  group_by(variable, category) %>%
  count() %>%
  ungroup %>% 
  filter(category>10) %>%
  select(-category) %>%
  group_by(variable) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  ungroup %>% 
  mutate(entity="High TB burden countries",
         n_country = 30)

f2_data <- rbind(f2_global_data, f2_region_data, f2_hbc_data) %>%
  mutate(pct = n/n_country*100)

f2_data <- f2_data %>%
  mutate(variable = factor(variable, labels = c("Agriculture", "Defence", "Social development", "Education", "Finance", "Justice", "Labour", "Transport")),
         variable = factor(variable, levels = c("Agriculture", "Defence", "Education", "Finance", "Justice", "Labour", "Social development", "Transport")))

