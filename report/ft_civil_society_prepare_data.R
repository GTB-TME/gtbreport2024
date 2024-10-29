# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Common data preparation script for FT: community engagement
# Takuya Yamanaka, June 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load chapter 2 packages, settings and data
source(here::here('report/ch2_load_data.r'))

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

# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# produce datasets for review
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ce_2023_data <- strategy %>%
  filter(year==report_year - 1) %>%
  select(iso3,
         country,
         g_whoregion,
         year,
         bmu,
         bmu_community_impl,
         community_data_available,
         community_nsp:cf_community)%>%
  mutate(comm_pct = ifelse(bmu > 0,
                           bmu_community_impl * 100 / bmu,
                           NA)) 


# Get the variable to identify countries requested to report on community indicators
comm_datarequest <- data_collection %>%
  filter(datcol_year==report_year) %>%
  select(iso3,
         dc_engage_community_display)

ce_2023_data <- ce_2023_data %>%
  left_join(hbc30, by = "iso3") %>%
  rename(hbc30 = group_type) %>%
  mutate(hbc30 = ifelse(!is.na(hbc30),1,0)) %>%
  left_join(hbmdr30, by = "iso3") %>%
  rename(hbmdr30 = group_type) %>%
  mutate(hbmdr30 = ifelse(!is.na(hbmdr30),1,0)) %>%
  left_join(hbtbhiv30, by = "iso3") %>%
  rename(hbtbhiv30 = group_type) %>%
  mutate(hbtbhiv30 = ifelse(!is.na(hbtbhiv30),1,0)) %>%
  mutate(anyhbc49 = ifelse(hbc30==1|hbmdr30==1|hbtbhiv30==1, 1, 0))

finance <- load_gtb("finance") 

ce_2023_data <- ce_2023_data %>%
  left_join(select(finance, iso3, year, cf_tot), by = c("iso3","year"))

ce_2023_data <- ce_2023_data %>%
  inner_join(comm_datarequest, by = "iso3") %>%
  select(iso3:year,hbc30:anyhbc49,dc_engage_community_display,bmu:cf_community,cf_tot,comm_pct)

writexl::write_xlsx(ce_2023_data, here::here(paste0("./report/local/ce_2023_data_",Sys.Date(),".xlsx")))

#------
ce_trend_data <- strategy %>%
  filter(year<=report_year - 1) %>%
  select(iso3,
         country,
         g_whoregion,
         year,
         bmu,
         bmu_community_impl,
         community_data_available,
         bmu_ref_data:notified_ref_community,
         bmu_rxsupport_data:rxsupport_community_succ) %>%
  left_join(select(notification, iso3, year, c_newinc), by = c("iso3","year")) %>%
  mutate(comm_pct = ifelse(bmu > 0,
                           bmu_community_impl * 100 / bmu,
                           NA)) 



ce_trend_data <- ce_trend_data %>%
  inner_join(comm_datarequest, by = "iso3") %>%
  filter(dc_engage_community_display==1 & year>=2014) %>%
  select(iso3:year,dc_engage_community_display,bmu:comm_pct)

writexl::write_xlsx(ce_trend_data, here::here(paste0("./report/local/ce_trend_data_",Sys.Date(),".xlsx")))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: texts related to TB notifications
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ce_n_country_available <- ce_trend_data %>%
  filter(year==report_year-1,(!is.na(rxsupport_community_coh)|!is.na(notified_ref_community))) %>% nrow()

comm_notif <- ce_trend_data %>%
  filter(year==report_year-1,community_data_available == 1) %>%
  select(iso3, c_newinc, notified_ref, notified_ref_community) %>%
  filter(!is.na(notified_ref_community)) 

ce_n_country_notif <- comm_notif %>% nrow()

comm_notif <- comm_notif %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(ce_notified_pct = notified_ref_community/c_newinc*100)
  
ce_n_country_outcomes <- ce_trend_data %>%
  filter(year==report_year-1,community_data_available == 1,!is.na(rxsupport_community_coh)) %>% nrow()
  
outcomes_text <- ce_trend_data %>%
  filter(year==report_year-1,community_data_available == 1) %>%
  select(iso3, bmu_rxsupport_data:rxsupport_community_succ) %>%
  filter(!is.na(rxsupport_community_coh )) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(ce_notified_pct = rxsupport_community_succ/rxsupport_community_coh *100)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 1 ----
# (Map showing percentage of management units with community contributions to case finding and/or treatment support)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f1_data <- strategy %>%
  filter(year==report_year - 1) %>%
  select(iso3,
         country,
         bmu,
         bmu_community_impl,
         community_data_available)%>%
  mutate(comm_pct = ifelse(bmu > 0,
                           bmu_community_impl * 100 / bmu,
                           NA))  %>%

  # Assign the categories for the map
  mutate(var = cut(comm_pct,
                   c(0, 25, 50, 75, Inf),
                   c('0-24', '25-49', '50-74', '\u226575'),
                   right=FALSE)) #%>%

  # Link to data request status (used in the footnote for the figure)
if(datacoll){

  # Get the variable to identify countries requested to report on community indicators
  comm_datarequest <- data_collection %>%
    filter(datcol_year==report_year) %>%
    select(iso3,
           dc_engage_community_display)

f1_data <- f1_data %>%
  inner_join(comm_datarequest, by = "iso3")

 ce_n_country <- comm_datarequest %>%
   filter(dc_engage_community_display==1) %>% nrow()
 
 # ce_n_country_available <- f1_data %>%
 #   filter(community_data_available==1) %>% nrow()

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: table 1 and fig 2 ----
# (status of community engagement by region and for HBCs)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# option 1: table
ce_data <- strategy %>%
  filter(year == report_year-1) %>%
  select(country:g_whoregion,community_nsp:community_manuals) %>%
  mutate(community_nsp = ifelse(community_nsp==2|community_nsp==3, 0, community_nsp),
         community_annual_report = ifelse(community_annual_report==2|community_annual_report==3, 0, community_annual_report),
         community_prog_review = ifelse(community_prog_review==2|community_prog_review==3, 0, community_prog_review),
         community_manuals = ifelse(community_manuals==2|community_manuals==3, 0, community_manuals))

# Get the variable to identify countries requested to report on community indicators
comm_datarequest <- data_collection %>%
  filter(datcol_year==report_year) %>%
  select(iso3,
         dc_engage_community_display)

ce_data <- ce_data %>%
  left_join(comm_datarequest, by = "iso3")

t1_region_all <- ce_data %>%
  mutate(n_country = 1) %>%
  select(g_whoregion,n_country) %>%
  group_by(g_whoregion) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  ungroup()
  
t1_region_data <- ce_data %>%
  mutate(n_country = 1) %>%
  select(g_whoregion,dc_engage_community_display,community_nsp:community_manuals) %>%
  # calculate regional aggregates
  group_by(g_whoregion) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(t1_region_all, by = "g_whoregion") %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  arrange(entity) %>%
  select(-g_whoregion)

# Add global summary to the regional summary
t1_global_data <- t1_region_data %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(entity="Global total")

# Add 30HBC summary to the regional summary
t1_hbc_data <- ce_data %>%
  filter(year == report_year-1, iso3 %in% iso3_hbc) %>%
  mutate(n_country = 1) %>%
  select(g_whoregion,n_country,dc_engage_community_display,community_nsp:community_manuals) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(entity="High TB burden countries")

beg = " ("
end = "%)"

t1_data <- rbind(t1_region_data, t1_global_data, t1_hbc_data) %>%
  mutate(pct_nsp = community_nsp /dc_engage_community_display*100,
         pct_prog_review = community_prog_review/dc_engage_community_display*100,
         pct_annual_report  = community_annual_report /dc_engage_community_display*100,
         pct_manuals = community_manuals /dc_engage_community_display*100
         )

t1_data_table <- t1_data %>%
  mutate(nsp = paste0(community_nsp, beg, ftb(pct_nsp), end),
         prog_review = paste0(community_prog_review, beg, ftb(pct_prog_review), end),
         annual_report = paste0(community_annual_report, beg, ftb(pct_annual_report), end),
         manual = paste0(community_manuals, beg, ftb(pct_manuals), end),
  ) %>%
  select(entity, -n_country, dc_engage_community_display, nsp:manual)

# option 2: fig
f2_country_data <- ce_data %>%
  select(iso3,g_whoregion,community_nsp:community_manuals)


f2_region_data <- f2_country_data %>% 
  select(-iso3) %>%
  tidyr::gather(variable, category, -g_whoregion) %>%
  group_by(g_whoregion, variable, category) %>%
  count() %>%
  ungroup %>% 
  filter(category==1) %>%
  select(-category) %>%
  group_by(g_whoregion, variable) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  # merge with regional names
  inner_join(select(t1_data, c("entity","dc_engage_community_display")), by = "entity") %>%
  ungroup() %>% 
  select(-g_whoregion) %>%
  rename(n_country = dc_engage_community_display)

# Add global summary to the regional summary
f2_global_data <- f2_region_data %>%
  select(-n_country) %>%
  group_by(variable) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(n_country = 101) %>%
  mutate(entity="Global")

f2_hbc_data <- f2_country_data %>% 
  filter(iso3 %in% iso3_hbc) %>%
  select(-iso3, -g_whoregion) %>%
  tidyr::gather(variable, category) %>%
  group_by(variable, category) %>%
  count() %>%
  ungroup %>% 
  filter(category == 1) %>%
  select(-category) %>%
  group_by(variable) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  ungroup %>% 
  mutate(entity="High TB burden countries",
         n_country = 30)

f2_data <- rbind(f2_global_data, f2_region_data, f2_hbc_data) %>%
  mutate(pct = n/n_country*100)

f2_data <- f2_data %>%
  mutate(variable = factor(variable, labels = c('Development of the annual national TB report',
                                                'Development or update of national TB guidelines or manuals',
                                                'Development of the national strategic plan',
                                                'Preparation and conduct of the TB programme review')),
         variable = factor(variable, levels = c('Development of the national strategic plan',
                                                'Preparation and conduct of the TB programme review',
                                                'Development of the annual national TB report',
                                                'Development or update of national TB guidelines or manuals')))

f2_hbc_txt <- f2_hbc_data %>%
  pivot_wider(names_from = variable, values_from = n)

f2_region_txt <- f2_region_data %>%
  pivot_wider(names_from = variable, values_from = n)

f2_region_txt_AFR <- f2_region_txt %>%
  filter(entity == "African Region")

f2_region_txt_AMR <- f2_region_txt %>%
  filter(entity == "Region of the Americas")

f2_region_txt_EMR <- f2_region_txt %>%
  filter(entity == "Eastern Mediterranean Region")

f2_region_txt_EUR <- f2_region_txt %>%
  filter(entity == "European Region")

f2_region_txt_SEA <- f2_region_txt %>%
  filter(entity == "South-East Asia Region")

f2_region_txt_WPR <- f2_region_txt %>%
  filter(entity == "Western Pacific Region")

# map (a)
f2a_data <- ce_data %>%
  # Assign the categories for the map
  mutate(var = factor(community_nsp,
                      levels = c(1, 0),
                      labels = c("Yes", "No"))) 
palatte_f2a = c("#2171B5","#C6DBEF")

# map (b)
f2b_data <- ce_data %>%
  # Assign the categories for the map
  mutate(var = factor(community_prog_review,
                      levels = c(1, 0),
                      labels = c("Yes", "No"))) 
palatte_f2b = c("#99000D","#FCBBA1")

# map (c)
f2c_data <- ce_data %>%
  # Assign the categories for the map
  mutate(var = factor(community_annual_report,
                      levels = c(1, 0),
                      labels = c("Yes", "No"))) 
palatte_f2c = c("#238B45","#C7E9C0")

# map (d)
f2d_data <- ce_data %>%
  # Assign the categories for the map
  mutate(var = factor(community_manuals,
                      levels = c(1, 0),
                      labels = c("Yes", "No"))) 
palatte_f2d = c("#6A51A3","#DADAEB")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 3 ----
# (Map showing countries with funding available for community engagement)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3_data <- filter(strategy, year == report_year-1) %>%
  select(country,
         iso3,
         year,
         g_whoregion,
         community_budget_available,
         cf_community
         ) %>%
  
  # Assign the categories for the map
  mutate(var = factor(community_budget_available,
                      levels = c(1, 0),
                      labels = c("Funding available", "Funding not available")))

f3_data_n <- f3_data %>% filter(community_budget_available==1) %>% nrow()

f3_data_cf_global <- f3_data %>%
  left_join(select(finance, iso3, year, cf_tot), by = c("iso3","year")) %>%
  select(community_budget_available,cf_community,cf_tot) %>%
  filter(community_budget_available==1) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(pct = cf_community/cf_tot*100)
  
f3_data_cf_region <- f3_data %>%
  left_join(select(finance, iso3, year, cf_tot), by = c("iso3","year")) %>%
  select(g_whoregion,community_budget_available,cf_community,cf_tot) %>%
  filter(community_budget_available==1) %>%
  group_by(g_whoregion) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(pct = cf_community/cf_tot*100) %>%
  ungroup() %>%
  arrange(desc(pct))
  
f3_data_cf_hbc <- f3_data %>%
  filter(iso3 %in% hbc30$iso3) %>%
  left_join(select(finance, iso3, year, cf_tot), by = c("iso3","year")) %>%
  select(community_budget_available,cf_community,cf_tot) %>%
  filter(community_budget_available==1, !is.na(cf_community)) 

f3_data_cf_hbc <- f3_data_cf_hbc %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(pct = cf_community/cf_tot*100)


