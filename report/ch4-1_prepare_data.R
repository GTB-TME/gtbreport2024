# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch4
# Two datasets needed, a) finance_merged = imputed global finance data (balanced,
# const USD). In 2023 all LMICs were included in balanced dataset. Used 
# 2022 USD. b) Global Plan 2018 onward.
# Prepared by Takuya Yamanaka: July 2024
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
start_year <- 2015

grpmbr <- load_gtb("grpmbr")

list_iso3_country <-
  grpmbr %>% filter(group_type == 'g_whoregion') %>%
  select(iso3,country)

load(here::here('finance/output/finance_imputed_constantus_included.Rdata'))

# Sept 2023: Based on individual audit of country reported data, some small
# countries (identified via variable do_not_fill) don't have reliable rcvd_*
# data to inform the trends shown in the report

###### Resource needs estimated for LMICs, disaggregated and aggregated #####
# This section just confirms that the list of countries to be included in the analysis is complete
# eligible_gf_23 <- readxl::read_xlsx(path = here::here("finance/raw/GF eligible_2023.xlsx"),
#                                     sheet = "gf_elig_23") %>%  #How many countries included? 137 LMICs
#   select(iso3, GF_eligible_2023) %>%
#   filter(!is.na(iso3))
# sum(eligible_gf_23$GF_eligible_2021) # As at Jan 2021, 109 countries were eligible
# length(eligible_gf_23$GF_eligible_2021) # This would be 137 from the 2021 file, even thoguh Romania is now HIC
# eligible_gf_23 <- eligible_gf_23 %>% filter(iso3 != "ROU")

# Updated from https://www.theglobalfund.org/media/12505/core_eligiblecountries2023_list_en.pdf

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1.1: The 132 low- and middle-income countries for 2024 report ######
# (country included in the analysis for finance chapter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# included in analyses of TB financing, 2013–2023 
# a) List of countries, plus their inclusion status
f4.1.1_data <- finance_merged %>% 
  # filter(g_income != "HIC" | g_income == "RUS") %>% 
  filter(year == report_year - 1) %>% 
  mutate(var = ifelse(include == 0,"Not included","Included")) %>%  
  mutate(var = factor(var, levels = c("Included","Not included"),
                      labels = c("Included","Not included"))) %>% 
  select(iso3, var, g_income, country, c_notified) %>% 
  arrange(iso3) 

f4.1.1_data <- list_iso3_country %>%
  left_join(f4.1.1_data, by = "iso3") %>%
  mutate(var = ifelse(is.na(var),"Not included","Included")) 

include_list <- f4.1.1_data  %>% filter(var == "Included") %>%  select(iso3) 
n_country <- include_list %>% nrow() #How many countries included?

# Associated proportion notified among countries included above
notif_included <- f4.1.1_data  %>% filter(g_income != "HIC") %>%
  select(c_notified) %>%  summarise_all(sum , na.rm = T)

# Percentage of all notified, that are accounted for in the current gtb report
notif_total <- notification %>% filter(year == report_year - 1) %>% select(c_notified) %>% summarise_all(sum , na.rm = T)

notif_included_prop <- notif_included$c_notified/notif_total$c_notified*100

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1.2-4.1.6: base dataset for 4 figures
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Next set of graphs rely on this file
f4.1.2_4.1.6_data <- finance_merged %>% 
  filter((iso3 %in% include_list$iso3)) %>% 
  filter(year >= 2013 & year <= report_year - 1) 

# This block is retained for historical documentation purposes only, and ensures that
# rcvd_ variables have values in report_year. For the past when preliminary commitments 
# used to be reported as funding available in most recent year
f4.1.2_4.1.6_data <- f4.1.2_4.1.6_data  %>% 
  mutate(rcvd_mdr  = ifelse(year == report_year, cf_mdr , rcvd_mdr ),
         rcvd_nmdr_dot = ifelse(year == report_year, cf_nmdr_dot, rcvd_nmdr_dot),
         rcvd_nmdr_ndot_hiv = ifelse(year == report_year, cf_nmdr_ndot_hiv, rcvd_nmdr_ndot_hiv),
         rcvd_nmdr_ndot_tpt = ifelse(year == report_year, cf_nmdr_ndot_tpt, rcvd_nmdr_ndot_tpt),
         rcvd_nmdr_ndot_nhiv = ifelse(year == report_year, cf_nmdr_ndot_nhiv, rcvd_nmdr_ndot_nhiv),
         rcvd_nmdr_ndot_nhiv_noth = ifelse(year == report_year, cf_nmdr_ndot_nhiv_noth, rcvd_nmdr_ndot_nhiv_noth),
         rcvd_nmdr_ndot_nhiv_oth = ifelse(year == report_year, cf_nmdr_ndot_nhiv_oth, rcvd_nmdr_ndot_nhiv_oth),
         rcvd_int = ifelse(year == report_year, cf_int, rcvd_int),
         rcvd_ext_gf = ifelse(year == report_year, cf_ext_gf, rcvd_ext_gf),
         rcvd_ext_ngf = ifelse(year == report_year, cf_ext_ngf, rcvd_ext_ngf),
         rcvd_tot = ifelse(year == report_year, cf_tot, rcvd_tot)
  ) 

# Generate BRICs code. This manual coding needs review in the case that BRIC
# countries increase to 11 in 2024
f4.1.2_4.1.6_data <- f4.1.2_4.1.6_data  %>% 
  mutate(g_brics = ifelse(iso3 %in% c("CHN","BRA","IND","RUS","ZAF"),"bric",
                          ifelse(g_hb_tb == TRUE, "hbc","oth"))) %>%
  mutate(g_brics = ifelse(is.na(g_hb_tb),"oth", g_brics))

# How many countries in each g_brics category? Obtain for labelling below
g_bric_count <- f4.1.2_4.1.6_data %>% filter(year == report_year - 1) %>% group_by(g_brics) %>% summarise(count=n())

# label them
f4.1.2_4.1.6_data <- f4.1.2_4.1.6_data %>% 
  mutate(g_brics = factor(g_brics,
                          levels = c("bric","hbc","oth"),
                          labels = c(paste0("BRICS (n=",g_bric_count$count[g_bric_count$g_brics=="bric"],")"),
                                     paste0("High TB burden and global TB watchlist countries outside BRICS\u1d43 (n=",g_bric_count$count[g_bric_count$g_brics=="hbc"],")"),
                                     paste0("Rest of world (n=",g_bric_count$count[g_bric_count$g_brics=="oth"],")")
                          )))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1.2: Funding for TB prevention, diagnosis and treatment; total
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.1.2_data <- f4.1.2_4.1.6_data %>% 
  select(iso3, year)
f4.1.2_data$int <- f4.1.2_4.1.6_data %>%  select(rcvd_int, c_ghs_nmdr, c_ghs_mdr) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)
f4.1.2_data$ext <- f4.1.2_4.1.6_data %>%  select(rcvd_ext_gf, rcvd_ext_ngf) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)
f4.1.2_data$tot <- f4.1.2_4.1.6_data %>%  select(rcvd_tot, c_ghs_nmdr, c_ghs_mdr) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)

f4.1.2_data <- f4.1.2_data %>% 
  select(-iso3, -tot) %>% 
  group_by(year) %>% 
  summarise_all(sum, na.rm = T) %>% 
  pivot_longer(cols = c("int","ext")) %>% 
  filter(year >= 2013) %>%
  mutate(year = factor(year),
         name = factor(name, 
                       levels = c("int","ext"),
                       labels = c("Domestic funding","International donor funding"))) 


# Estimate how much domestic funding is captured here, and how much estimated GHS?
f4.1.2_data_domestic <- f4.1.2_4.1.6_data %>% 
  select(year,rcvd_int, c_ghs_inpatient, c_ghs_outpatient) %>% 
  group_by(year) %>% 
  summarise_all(sum, na.rm = T) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1.2b: Funding for TB prevention, diagnosis and treatment by funding
# source, 2010–2021, 121 countries with 97% of reported TB cases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.1.2b_data <- f4.1.2_4.1.6_data %>% 
  group_by(year) %>%
  select(iso3, g_brics, g_income, g_whoregion, g_hb_tb , rcvd_ext_gf, rcvd_ext_ngf) 

# Sum up individual groups
f4.1.2b_data$int <- f4.1.2_4.1.6_data %>%  select(rcvd_int, c_ghs_nmdr, c_ghs_mdr) %>%  mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)
f4.1.2b_data$ext <- f4.1.2_4.1.6_data %>%  select(rcvd_ext_gf, rcvd_ext_ngf) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)
f4.1.2b_data$tot <- f4.1.2_4.1.6_data %>%  select(rcvd_tot, c_ghs_nmdr, c_ghs_mdr) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)

# Proportion domestically funded in latest year
pcnt_domestic <- f4.1.2b_data %>% 
  filter(year >= 2013) %>% 
  summarise_at(vars(int, ext, tot), function(x) sum(x, na.rm = T)) %>% 
  mutate(pcnt = round(100 * int / tot,1))

# Amounts domestically funded in latest year by g_bric
domestic_by_grp <- f4.1.2b_data %>% filter(year == report_year - 1) %>% 
  group_by(g_brics) %>% 
  summarise(int = sum(int))

# Percentage of NTP reported external funding that is from GF
pcnt_gf <- f4.1.2b_data %>% 
  group_by(year) %>% 
  select(rcvd_ext_gf, rcvd_ext_ngf) %>% 
  summarise_if(.predicate = is.numeric, .funs = sum, na.rm = T) %>% 
  mutate(pcnt = round(100 * rcvd_ext_gf / (rcvd_ext_gf + rcvd_ext_ngf),1))

# Percentage of BRICS, HBC, and LIC group spending that is domestic sourced
pcnt_group <- f4.1.2b_data %>% filter(str_detect(g_brics, "BRICS ") & year == report_year - 1) %>% 
  select(int, tot) %>% summarise_all(.funs = sum) 

pcnt_group <- pcnt_group %>% 
  rbind(
    f4.1.2b_data %>% filter(g_income == "LIC" & year == report_year - 1) %>% 
      select(int, tot) %>% summarise_all(.funs = sum) 
  ) %>% 
  rbind(
    f4.1.2b_data %>% filter(g_hb_tb == 1 & !str_detect(g_brics, "BRICS ") & year == report_year - 1) %>% 
      select(int, tot) %>% summarise_all(.funs = sum) 
  )

pcnt_group <- pcnt_group %>% 
  mutate (group = c("brics","lic","hb"),
          pcnt = round(100 * int / tot,1))

# finalization of the dataset
f4.1.2_data <- f4.1.2b_data %>% 
  select(-rcvd_ext_gf, -rcvd_ext_ngf) %>% 
  group_by(year) %>% 
  summarise_at(vars(int, ext, tot), function(x) sum(x, na.rm = T)) %>% 
  filter(year <= report_year - 1) %>% 
  pivot_longer(cols = -year) 


# Percentage against 22 billion
pcnt_unhlm <- f4.1.2_data %>%
  filter(year == report_year-1 & name == "tot") %>%
  mutate(pcnt = value/22 * 100) 
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Fig 4.1.3 (9  panel) 
# use Fig 4.1.2b dataset (country level by source of available funding) and select specific groups
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.1.3a_data <- f4.1.2b_data %>% 
  select(-iso3,-tot) %>% 
  group_by(year,g_brics) %>% 
  summarise_at(vars(int,ext), sum, na.rm = T) %>% 
  rename(grp = g_brics) %>% 
  # needs to revert back to simple labels temporarily
  mutate(grp = factor(grp, labels = c("brics","hbc","rest"))) 

f4.1.3b_data <- f4.1.2b_data %>% 
  select(-iso3,-tot) %>% 
  group_by(year,g_income) %>% 
  summarise_at(vars(int,ext), sum, na.rm = T) %>% 
  rename(grp = g_income)

f4.1.3c_data <- f4.1.2b_data %>% 
  select(-iso3,-tot) %>% 
  mutate(g_whoregion = ifelse(g_whoregion %in% c("AFR","SEA"),g_whoregion,
                              # Include WPR in SEA group, and the rest in OTH group
                              ifelse(g_whoregion == "WPR","SEA","OTH"))) %>% 
  group_by(year,g_whoregion) %>% 
  summarise_at(vars(int,ext), sum, na.rm = T)%>% 
  rename(grp = g_whoregion)

f4.1.3_data <- rbind(f4.1.3a_data, f4.1.3b_data, f4.1.3c_data) %>%
  filter(year <= report_year) %>% 
  mutate(grp = stringr::str_to_lower(grp)) %>% 
  # Order the grouping variable by factoring it
  mutate(grp = factor(grp,
                      levels = c("brics","hbc","rest",
                                 "lic","lmc","umc",
                                 "afr","sea","oth"),
                      labels = c(paste0("BRICS (n=",g_bric_count$count[1],")"),
                                 paste0("High TB burden and global TB watchlist countries outside BRICS\u1d43 (n=",g_bric_count$count[2],")"),
                                 paste0("Rest of world (n=",g_bric_count$count[3],")"),
                                 "Low-income countries","Lower-middle-income countries","Upper-middle-income countries",
                                 "Africa","Asia\u1d47","Other regions\u1d9c")))  %>% 
  group_by(year,grp) %>% 
  summarise_all(sum, na.rm = T) 

# A similar graph is produced for the summary report, but combining the brics split (f4.1.3a_data) with the Fig 4.1.2b
f34_data <- f4.1.2b_data %>% select(-iso3,-tot) %>% 
  group_by(year) %>% 
  summarise_at(vars(int,ext), sum, na.rm = T) %>% 
  mutate(grp = "all") %>% # To allow grouped data to be appended next
  rbind(f4.1.3a_data) %>% 
  filter(year <= report_year) %>% 
  pivot_longer(cols = c("int","ext")) %>% 
  mutate(name = factor(name, 
                       levels = c("int","ext"),
                       labels = c("Domestic funding","International donor funding"))) %>% 
  mutate(grp = factor(grp,
                      levels = c("all","brics","hbc","rest"),
                      labels = c( paste0("All low and middle-income countries (n=",n_country,")"), # making the count dynamic
                                  "BRICS (n=5)",
                                  "High TB burden and global TB watchlist countries outside BRICS\u1d43 (n=28)",
                                  paste0("Other low and middle-income countries (n=",n_country - 33,")"))
  )) %>% 
  arrange(year, grp) 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Fig 4.1.4 country panel (here show only one of them, to save memory)
# these country graphs display only the country reported data (plus imputed values) but don't include
# the CHOICE estimated GHS costs. This is because the charts are designed to be consistent
# with the gap reported at country level (historically we used to show gap trend lines) 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.1.4_data <- f4.1.2_4.1.6_data %>% 
  filter(g_hb_tb == 1) %>% 
  select(country, year, rcvd_int, rcvd_ext, gap_tot) 

# If gap (or any value) is less tha zero, it is zeroed
f4.1.4_data <- f4.1.4_data %>% 
  ungroup() %>%
  mutate_at(vars(rcvd_int, rcvd_ext, gap_tot), 
            function(x) ifelse(x < 0, 0 , x/1E06)) %>% 
  pivot_longer(cols = c("rcvd_int","rcvd_ext", "gap_tot")) 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1.5: Spending on for TB prevention, diagnosis and treatment in total #####
# nd by category of expenditure, 2013-
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.1.5_data <- f4.1.2_4.1.6_data %>% 
  select(iso3, g_brics, year) 
f4.1.5_data$DSTB <- f4.1.2_4.1.6_data %>%  select(rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv, c_ghs_nmdr) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)  
f4.1.5_data$MDR <- f4.1.2_4.1.6_data %>%  select(rcvd_mdr, c_ghs_mdr) %>% mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)  
f4.1.5_data$TBHIV <- f4.1.2_4.1.6_data %>% select(rcvd_nmdr_ndot_hiv) %>% mutate_all(function(x) x/(1.0E09)) %>% unlist() 
f4.1.5_data$TPT <- f4.1.2_4.1.6_data %>%  select(rcvd_nmdr_ndot_tpt) %>% mutate_all(function(x) x/(1.0E09))  %>% unlist() 
# f4.1.5_data$Other <- f4.1.2_4.1.6_data %>%  select(rcvd_nmdr_ndot_nhiv_oth) %>% mutate_all(function(x) x/(1.0E09)) %>% unlist() # commented out: TA and AS
f4.1.5_data$Total <- f4.1.2_4.1.6_data %>%  select(rcvd_tot, c_ghs_mdr, c_ghs_nmdr) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)  

# Global summary by year
f4.1.5_data <- f4.1.5_data %>% 
  group_by(year) %>%
  summarise_at(vars(-iso3, -g_brics),sum, na.rm = T)

f4.1.5_data$TPT[f4.1.5_data$year < 2019] <- NA


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1.6: Funding for drug-susceptible TB and MDR-TB, 2010–2021, by country group
# Picking up from Fig 4.1.6 raw data, but keeping only DSTB and MDR costs keep only non 
# HIC + RUS using GF;s classification in the year of projection (which may be different from WB's current classification)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Sum up individual groups
f4.1.6_data <- f4.1.2_4.1.6_data %>% 
  select(iso3, g_brics, year)
f4.1.6_data$DSTB <- f4.1.2_4.1.6_data %>%  select(rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv, c_ghs_nmdr) %>%   mutate_all(function(x) x/(1.0E06)) %>%  rowSums(na.rm = T)  
f4.1.6_data$MDR <- f4.1.2_4.1.6_data %>%  select(rcvd_mdr, c_ghs_mdr) %>% mutate_all(function(x) x/(1.0E06)) %>%  rowSums(na.rm = T)  

# Aggregated by bric variable
f4.1.6_data <- f4.1.6_data %>%  
  group_by(year, g_brics) %>% 
  summarise_at(vars(DSTB,MDR),sum,na.rm = T ) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Fig 4.1.7_4.1.8 (Gaps by region)
# reload the finance dataset to revert from changes made that affct the trend lines, based on incomplete country reports  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Fig 4.1.8 (Gaps by region and income group)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f4.1.8_data <- finance_merged %>% 
  filter(year >= 2013 & year <= report_year) %>% # to include provisionally reported budget and cf values
  group_by(g_income, g_whoregion, year, iso3, country) %>%
  mutate_at(vars(gap_tot), function(x) ifelse(x < 0, 0 , x)) %>% 
  # in Millions
  transmute_at(vars(gap_tot), function(x) x/1.0E06 ) %>% 
  ungroup()


f4.1.8a_data <- f4.1.8_data %>%
  filter( year <= report_year-1) %>% 
  group_by(year, grp = g_income) %>% 
  mutate_at(vars(gap_tot), function(x) ifelse(x < 0, 0 , x)) %>% 
  summarise_at(vars(gap_tot), sum, na.rm = T) %>% 
  mutate(grp = stringr::str_to_lower(grp)) %>%
  mutate(grp = factor(grp,
                      levels = c("lic","lmc","umc"),
                      labels = c("Low-income countries","Lower-middle-income countries","Upper-middle-income countries"))) %>% 
  mutate(facet = 1)

f4.1.8b_data <- f4.1.8_data %>%
  filter( year <= report_year) %>% 
  group_by(year, grp = g_whoregion) %>% 
  mutate_at(vars(gap_tot), function(x) ifelse(x < 0, 0 , x)) %>% 
  summarise_at(vars(gap_tot), sum, na.rm = T) %>% 
  mutate(grp = stringr::str_to_lower(grp)) %>%
  mutate(grp = factor(grp,
                      levels = c(
                        "afr","amr", "sea",
                        "eur","emr","wpr"),
                      labels = c(
                        "African Region","Region of the Americas", 
                        "South-East Asia Region","European Region", 
                        "Eastern Mediterranean Region","Western Pacific Region" ))) %>% 
  mutate(facet = 2)


f4.1.8_data_lic <- finance_merged %>%
  filter(year == report_year) %>% 
  group_by(g_income) %>%
  summarise(gap_tot = sum(gap_tot/1e9, na.rm = TRUE)) %>%
  filter(g_income == "LIC")

# f4.1.8_data_GP_lic <- global_plan_23 %>% 
#   select(g_income = g_inc_2022, year, gp_tot = Total) %>%
#   filter(year == report_year) %>% 
#   group_by(g_income) %>% 
#   summarise(gp_tot = sum(gp_tot/1e9, na.rm = TRUE)) %>%
#   filter(g_income == "L") 
# 
# f4.1.8_data_lic_gap <- f4.1.8_data_lic$gap_tot / f4.1.8_data_GP_lic$gp_tot * 100

## These are objects from f4.1.8_data specifically referenced in the text portion of the markdown file. ##
# Total gap in financing in the year of the report (based on countries that are included in the analysis) 
latest_year_budgetgap_bn = f4.1.8_data %>% ungroup() %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(year == report_year & g_income != "HIC") %>% 
  summarise(sum(gap_tot)/1000) %>% 
  unlist() %>% 
  round(1)

# Total gap in financing in latest year referenced 
latest_year_budgetgap_bn = f4.1.8_data %>% ungroup() %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(year == report_year & g_income != "HIC") %>% 
  summarise(sum(gap_tot)/1000) %>% 
  unlist() %>% 
  round(1)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Fig 4.1.7 (map for countries with and without gaps)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# how many countries report having gaps/ shortfalls in financing?
# If gap (or any value) is less than zero, it is zeroed
countries_with_gaps <- f4.1.8_data %>% ungroup() %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(year == report_year-1) %>% 
  summarise_at(vars(gap_tot), length) %>% 
  unlist()

f4.1.7_data <- f4.1.8_data %>% ungroup() %>% 
  filter(year == report_year) 

f4.1.7_data <- list_iso3_country %>%
  right_join(f4.1.7_data, by = c("iso3", "country")) %>% 
  mutate(var = ifelse(!is.na(gap_tot), 1, 0)) %>%
  mutate(var = ifelse(iso3 %in% include_list$iso3, var, NA)) %>%
# Assign the categories for the map
  mutate(var = factor(var,
                      levels = c(1, 0),
                      labels = c("Reported funding gap", "Did not report funding gap")))


palatte_fig4.1.7 = c("#B10026","#FEB24C")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Fig 4.1.9 
# stacked bar chart sources with gap, 33 HB + outlook countries
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.1.9_data <- finance_merged %>% 
  filter(year == report_year & g_hb_tb == 1 & !is.na(cf_int)) %>%
  group_by(g_income, country) %>%
  mutate_at(.vars = vars(gap_tot), function(x) ifelse(x < 0, 0 , x)) %>% 
  # in Millions
  transmute_at(vars(cf_int, cf_ext_gf, cf_ext_ngf, gap_tot), function(x) x/1.0E06 ) %>% 
  ungroup()

f4.1.9_data <- f4.1.9_data %>% 
  # compute percentages of total committed funds + gaps 
  mutate(tot = f4.1.9_data %>% dplyr::select(cf_int, cf_ext_gf, cf_ext_ngf, gap_tot) %>% rowSums(na.rm = T)) %>% 
  mutate(int_pct = round(100 * cf_int / tot, 1),
         gf_pct = round(100 * cf_ext_gf / tot, 1),
         oth_pct = round(100 * cf_ext_ngf / tot, 1),
         gap_pct = round(100 * gap_tot / tot, 1))

# Extend country names for DPRK and DRC
f4.1.9_data <- f4.1.9_data %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = ifelse(country == "DPR Korea","Democratic People's Republic of Korea",country)) %>% 
  mutate(country = ifelse(country == "DR Congo","Democratic Republic of the Congo",country)) %>% 
  mutate(country = ifelse(country == "UR Tanzania","United Republic of Tanzania",country))  


f4.1.9_txt <- f4.1.9_data %>%
  filter(is.na(int_pct)) %>%
  arrange(country)

myvars <- c("int_pct", "gf_pct", "oth_pct", "gap_pct", "cf_int", "cf_ext_gf", "cf_ext_ngf", "gap_tot")

# LIC plot
# Specify country names in order of increasing domestic funding
ctry_a <- f4.1.9_data %>% filter(g_income == "LIC"& !is.na(int_pct)) %>% arrange(int_pct) %>% select(country) %>% unlist()

f4.1.9a_data <- f4.1.9_data  %>%
  filter(g_income=="LIC" & !is.na(int_pct))%>% 
  select(country,all_of(myvars)) %>%
  reshape2::melt(id=c("country"))

f4.1.9a_data$variable <- factor(f4.1.9a_data$variable, levels=myvars)

## LMC plot
# Specify country names in order of increasing domestic funding
ctry_b <- f4.1.9_data %>% filter(g_income == "LMC"  & !is.na(int_pct) ) %>% arrange(int_pct) %>% select(country) %>% unlist()

f4.1.9b_data <- f4.1.9_data %>% 
  filter(g_income=="LMC" & !is.na(int_pct))%>%  
  select(country,all_of(myvars)) %>%
  reshape2::melt(id=c("country"))

f4.1.9b_data$variable<-factor(f4.1.9b_data$variable, levels=myvars)


# ## UMC plot
# Specify country names in order of increasing domestic funding
ctry_c <- f4.1.9_data %>% filter(g_income == "UMC") %>% arrange(int_pct) %>% select(country) %>% unlist()
f4.1.9c_data <- f4.1.9_data %>% 
  filter(g_income=="UMC") %>%  
  select(country,all_of(myvars)) %>%
  reshape2::melt(id=c("country"))

f4.1.9c_data$variable<-factor(f4.1.9c_data$variable, levels=myvars)

# What are the top 5 country shortfalls in report_year? to be used texts for f4.1.9
table_country_gaps <- f4.1.8_data %>%
  filter(year == report_year) %>% 
  arrange(desc(gap_tot)) %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  select(country, gap_tot) %>% 
  slice(1:5) %>% 
  mutate(gap_tot  = round(gap_tot,0))

# Among LICs: How many countries? How much gap?
total_lic <- f4.1.1_data  %>% filter( g_income== "LIC") %>% dim() %>% '[['(1)
latest_year_budgetgap_lic <- f4.1.8_data %>% ungroup() %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(g_income == "LIC" & year == report_year) %>% 
  summarise(sum(gap_tot)) %>% 
  unlist() %>% 
  round(0)

lics_with_gaps <- f4.1.8_data %>% ungroup() %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(g_income == "LIC" & year == report_year) %>% 
  summarise_at(vars(gap_tot), length) %>% 
  unlist()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Fig 4.1.10 (gaps against Stop TB partnership's Global Plan)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# by income group
gp_income <-  readxl::read_excel(here::here("finance/stbp_globalplan/tbgp_income.xlsx")) %>%
  pivot_longer(cols = !g_income,
               names_to = "year", 
               values_to = "gp_tot")

f4.1.10_data_inc_grp <- gp_income %>% 
  filter(year == report_year) %>% #All that had a GP projection in 2018 - should be 127 (and not 128) in 2022 (since Romania is noe HIC)
  filter(g_income %in% c("LIC","LMC","UMC")) # %>% #Roumaina was UMC in 2018 but was reclassified as HIC in 2022. Eliminate from estimate
  # group_by(g_income) # %>% 
  # select(Total) %>%
  # # mutate_at(.vars = vars(GP_Total), .funs = function(x) x/deflator_us_2018) %>% 
  # summarise(GP_Total = sum(Total/1E09, na.rm = TRUE),
  #           GP_n = n())

f4.1.10_data_ghs <- finance_merged %>% 
  filter((iso3 %in% include_list$iso3)) %>%
  filter(year == report_year-1) %>%
  select(iso3, c_ghs) 
  
finance_merged %>% 
  filter((iso3 %in% include_list$iso3)) %>%
  filter(year == report_year) %>%
  select(iso3, g_income, budget_tot, cf_tot, gap_tot) %>%
  left_join(f4.1.10_data_ghs, by = "iso3") %>%
  mutate(budget_tot_ghs_plus = NZ(budget_tot) + NZ(c_ghs)) %>%
  mutate(cf_tot_ghs_plus = NZ(cf_tot) + NZ(c_ghs)) %>%
  group_by(g_income) %>% 
  # in Billions
  summarise(budget_tot = sum(budget_tot, na.rm = T)/1.0E09, 
            cf_tot = sum(cf_tot, na.rm = T)/1.0E09, 
            gap_tot= sum(gap_tot, na.rm=T)/1.0E09, 
            budget_tot_ghs_plus= sum(budget_tot_ghs_plus, na.rm=T)/1.0E09, 
            cf_tot_ghs_plus= sum(cf_tot_ghs_plus, na.rm=T)/1.0E09, 
            n = n()) %>% 
  ungroup() %>% 
  right_join(f4.1.10_data_inc_grp, by="g_income") %>%
  mutate(gap_tot_ghs_plus = gp_tot - cf_tot_ghs_plus) -> f4.1.10_data_inc_grp

# Compute proportion of reported gap (gap_tot) to GP gap (GP_Total - cf_tot) in each subgroup
f4.1.10_data_inc_grp <- f4.1.10_data_inc_grp %>% 
  add_row(f4.1.10_data_inc_grp %>% summarise_if(.predicate = is.numeric, .funs = sum)) %>% 
  select(!year) %>%
  mutate(g_income = ifelse(is.na(g_income),"All",g_income)) %>% 
  mutate(pcnt_diff = (100 * gap_tot / (gp_tot - cf_tot))) %>% 
  mutate(pcnt_diff_ghs_plus = (100 * gap_tot / (gp_tot - cf_tot_ghs_plus)))

# without including GHS estimates
f4.1.10_data <- f4.1.10_data_inc_grp %>%
  select(g_income, budget_tot, cf_tot, gp_tot) %>%
  pivot_longer(cols = contains("ot"),
               names_to = "grp",
               values_to = "value") %>%
  mutate(grp = factor(grp, levels = c("gp_tot", "budget_tot", "cf_tot"), labels = c("Global Plan","Required budget","Expected funding"))) %>%
  mutate(g_income = factor(g_income, levels = c("LIC", "LMC", "UMC", "All"), labels = c("Low-income coutnries", "Lower-middle-income countries","Upper-middle-income countries","All low- and middle-income countries")))

# with including GHS estimates
f4.1.10b_data <- f4.1.10_data_inc_grp %>%
  select(g_income, budget_tot_ghs_plus, cf_tot_ghs_plus, gp_tot) %>%
  pivot_longer(cols = contains("ot"),
               names_to = "grp",
               values_to = "value") %>%
  mutate(grp = factor(grp, levels = c("gp_tot", "budget_tot_ghs_plus", "cf_tot_ghs_plus"), labels = c("Global Plan","Required budget","Expected funding"))) %>%
  mutate(g_income = factor(g_income, levels = c("LIC", "LMC", "UMC", "All"), labels = c("Low-income coutnries", "Lower-middle-income countries","Upper-middle-income countries","All low- and middle-income countries")))


gp_income_txt <- gp_income %>%
  filter(year == report_year) %>%
  pivot_wider(names_from = "g_income",
              values_from = "gp_tot") %>%
  mutate(LMIC = LIC + LMC + UMC)

budget_txt <- f4.1.10b_data %>%
  filter(g_income == "All low- and middle-income countries" & grp == "Required budget")


# by WHO region

# by income group
gp_region <-  readxl::read_excel(here::here("finance/stbp_globalplan/tbgp_region.xlsx")) %>%
  pivot_longer(cols = !g_whoregion,
               names_to = "year", 
               values_to = "gp_tot")

f4.1.10_data_region <- gp_region %>% 
  filter(year == report_year)

f4.1.10_data_ghs <- finance_merged %>% 
  filter((iso3 %in% include_list$iso3)) %>%
  filter(year == report_year-1) %>%
  select(iso3, c_ghs) 

finance_merged %>% 
  filter((iso3 %in% include_list$iso3)) %>%
  filter(year == report_year) %>%
  select(iso3, g_whoregion, budget_tot, cf_tot, gap_tot) %>%
  left_join(f4.1.10_data_ghs, by = "iso3") %>%
  mutate(budget_tot_ghs_plus = NZ(budget_tot) + NZ(c_ghs)) %>%
  mutate(cf_tot_ghs_plus = NZ(cf_tot) + NZ(c_ghs)) %>%
  group_by(g_whoregion) %>% 
  # in Billions
  summarise(budget_tot = sum(budget_tot, na.rm = T)/1.0E09, 
            cf_tot = sum(cf_tot, na.rm = T)/1.0E09, 
            gap_tot= sum(gap_tot, na.rm=T)/1.0E09, 
            budget_tot_ghs_plus= sum(budget_tot_ghs_plus, na.rm=T)/1.0E09, 
            cf_tot_ghs_plus= sum(cf_tot_ghs_plus, na.rm=T)/1.0E09, 
            n = n()) %>% 
  ungroup() %>% 
  right_join(f4.1.10_data_region, by="g_whoregion") %>%
  mutate(gap_tot_ghs_plus = gp_tot - cf_tot_ghs_plus) -> f4.1.10_data_region

# f4.1.10_data_region proportion of reported gap (gap_tot) to GP gap (GP_Total - cf_tot) in each subgroup
f4.1.10_data_region <- f4.1.10_data_region %>% 
  select(!year) %>%
  mutate(pcnt_diff = (100 * gap_tot / (gp_tot - cf_tot))) %>% 
  mutate(pcnt_diff_ghs_plus = (100 * gap_tot / (gp_tot - cf_tot_ghs_plus))) %>%
  left_join(region_shortnames())

# with including GHS estimates
f4.1.10c_data <- f4.1.10_data_region %>%
  select(entity, budget_tot_ghs_plus, cf_tot_ghs_plus, gp_tot) %>%
  pivot_longer(cols = contains("ot"),
               names_to = "grp",
               values_to = "value") %>%
  mutate(grp = factor(grp, levels = c("gp_tot", "budget_tot_ghs_plus", "cf_tot_ghs_plus"), labels = c("Global Plan\u1D47","Required budget","Expected funding")))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Fig 4.1.11
# Estimated cost per patient treated for drug-susceptible TB by country, report year - 1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.1.11_data <- finance_merged
f4.1.11_data$imf_gdp_pc_con_usd <- ifelse(f4.1.11_data$year==(report_year - 1 ) & is.na(f4.1.11_data$imf_gdp_pc_con_usd),
                                     finance_merged$imf_gdp_pc_con_usd[finance_merged$year == (report_year - 2 )],
                                     f4.1.11_data$imf_gdp_pc_con_usd)

f4.1.11_data <- f4.1.11_data %>%
  # Received funds for DSTB and Other group (specifically the variables
  # rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv_noth, rcvd_nmdr_ndot_nhiv_oth and
  # c_ghs_nmdr) are divided by c_notif_less_mdr, and filtered to only countries with
  # above 100 DSTB cases notified
  mutate(rcvd_nmdr_dot = ifelse(rcvd_nmdr_dot == 0, cf_nmdr_dot, rcvd_nmdr_dot)) %>%
  mutate(DSTB = finance_merged %>% select(rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv_noth, rcvd_nmdr_ndot_nhiv_oth, c_ghs_nmdr) %>% rowSums(na.rm = T),
         c_pp_dots = DSTB / c_notif_less_mdr ,
         log10_c_pp_dots = ifelse(is.na(c_pp_dots),NA,log(c_pp_dots, base = 10)),
         log10_gdp_pc_con_usd = ifelse(is.na(imf_gdp_pc_con_usd), NA, log(imf_gdp_pc_con_usd, base = 10))) %>%
  # Restrict to latest year, Exclude countries where c_pp_dots is empty and
  # where it cannot be computed accurately i.e where rcvd_nmdr_dot is empty, or c_ghs_nmdr is empty
  filter(year == report_year - 1 & c_notif_less_mdr >= 100 & g_income != "HIC" &
           !is.na(c_pp_dots) & !is.na(imf_gdp_pc_con_usd) &
           !is.na(rcvd_nmdr_dot) & !is.na(c_ghs_nmdr)) %>%
  # In 2024 report, we drop Bhutan as all data are reported in oth
  filter(iso3 != "BTN" & iso3 != "MDV") %>%
  select(iso3, country, imf_gdp_pc_con_usd,log10_gdp_pc_con_usd, c_pp_dots,log10_c_pp_dots,
         c_ghs_nmdr, DSTB, g_whoregion, g_income, g_hb_tb,
         c_notified=c_notif_less_mdr) %>%
  # Sorted in descending order to let larger bubbles be plotted before smaller ones
  arrange(desc(c_notified)) %>%
  mutate(g_hb_tb = ifelse(is.na(g_hb_tb),0,g_hb_tb)) %>%
  # Label WHO regions
  mutate(g_whoregion =  factor(g_whoregion,
                               levels = c("AFR","AMR","SEA","EUR", "EMR", "WPR"),
                               labels = c("African Region","Region of the Americas", "South-East Asia Region","European Region", "Eastern Mediterranean Region","Western Pacific Region" )))

##CREATE SCATTERPLOT WHERE WHO REGIONS ARE DIFFERENT COLORS AND BUBBLE SIZE IS TB CASELOAD

dstb_cpp_no <- f4.1.11_data %>% nrow()

# Median provider cost per case notified (DSTB)
c_pp_dots <- f4.1.11_data %>% ungroup() %>% summarise(median(c_pp_dots)) %>% round(0) %>%  unlist()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Fig 4.1.12  TB 2nd LINE DRUG PATIENT COST 
# Estimated cost per patient treated for DR TB by country, report year - 1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f4.1.12_data <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(imf_gdp_pc_con_usd = ifelse(year == report_year-1 & is.na(imf_gdp_pc_con_usd),
                                     lag(imf_gdp_pc_con_usd), imf_gdp_pc_con_usd)) %>%
  ungroup() %>%
  # consistent with Fig 4.4 above, received funds for DSTB (including c_ghs_nmdr) are divided by c_notified, only for countries with c_notified above 100
  mutate(g_hb_mdr = ifelse(is.na(g_hb_mdr),0,g_hb_mdr)) %>%

  mutate(rcvd_mdr = ifelse(rcvd_mdr == 0, cf_mdr, rcvd_mdr)) %>%
  mutate(MDR = finance_merged %>% select(rcvd_mdr, c_ghs_mdr) %>% rowSums(na.rm = T),
         c_pp_mdr = MDR / mdr_tx 
         ) %>%

  # Restrict to latest year, Exclude countries where c_pp_mdr is empty and where it cannot be computed accurately i.e where rcvd_mdr_sld is zero,
  # or c_ghs_mdr is empty (but keep those with c_ghs_mdr == 0 eg CHN, KAZ, RUS etc)
  filter(year == report_year - 1 & mdr_tx >= 20 & g_income != "HIC" &
           ((!is.na(c_pp_mdr) & !is.na(imf_gdp_pc_con_usd) &
           !is.na(rcvd_mdr_sld) & rcvd_mdr_sld > 0) | iso3 == "GTM")) %>% # GTM exceptional: SLD is zero for recent years, but data looks fine
  # In 2024 report, we drop Bhutan as all data are reported in oth
  filter(iso3 != "BTN" & iso3 != "MDV") %>%
  select(iso3, country, g_whoregion, g_income, g_hb_mdr, imf_gdp_pc_con_usd, c_pp_mdr, MDR,  mdr_tx) %>%
  # Sorted in descending order to let larger bubbles be plotted before smaller ones
  arrange(desc(mdr_tx)) %>%
  # Label WHO regions
  mutate(g_whoregion =  factor(g_whoregion,
                               levels = c("AFR","AMR","SEA","EUR", "EMR", "WPR"),
                               labels = c("African Region","Region of the Americas", "South-East Asia Region","European Region", "Eastern Mediterranean Region","Western Pacific Region" )))

mdr_cpp_no <- f4.1.12_data %>% nrow()

# What's the Median provider cost per case notified (MDR)?
c_pp_mdr <- f4.1.12_data %>% ungroup() %>% summarise(median(c_pp_mdr)) %>% round(0) %>% unlist()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ##### Numbers for the methods box ######
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# # Summary of methods used to fill in missing rcvd_tot values
methods_box <- finance_merged %>%
  filter(year == report_year - 1 & include == 1) %>%
  select(iso3, country, g_hb_tb, rcvd_imputation, do_not_fill, rcvd_tot, c_notified)

methods_box_hbc <- methods_box %>% filter(g_hb_tb==1 & rcvd_imputation != "Reported data" & !is.na(rcvd_tot) & g_hb_tb==1 & !(do_not_fill == 1) &iso3!="KHM")

###### Creating an extract of variables for the regional profiles website page #####
# Lines that produces data in csv format for theregional profiles. Adapted from
# Hazim's stata stript (Regional profiles.do) so as to work directly from github.

# Purpose is to produce a CSV output with the following variables in it:
# rcvd_tot, rcvd_int, c_ghs, rcvd_ext_gf, rcvd_ext_ngf, cf_int, cf_ext, gap_tot,
# g_whoregion, and year

# The numbers should be the same that are used in the
# Global TB report, and should be disaggregated by WHO region and year

regional_profile <- finance_merged %>%
  group_by(year, g_whoregion) %>%
  select(rcvd_tot, rcvd_int, c_ghs, rcvd_ext_gf, rcvd_ext_ngf, cf_int, cf_ext, gap_tot) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  group_by(year, g_whoregion) %>%
  mutate_at(.vars = vars(-group_cols()), .funs = round, digits = 0)

# Clear c_ghs values for report_year since they are not used
regional_profile <- regional_profile %>%
  mutate(c_ghs = ifelse(year == report_year,0,c_ghs))

# regional_profile %>%
#   write_csv(here::here(paste0("finance/output/regionalglobalprofile_finance_v",Sys.Date(),".csv")))
# 
# finance_merged %>% filter(year == 2022 & do_not_fill == 0) %>% select(iso3) %>%
#   write_csv(here::here(paste0("finance/output/countries_for_profile_finance_v",Sys.Date(),".csv")))

