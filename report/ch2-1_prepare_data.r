# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch2-1.rmd
# Takuya Yamanaka, June 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load chapter 2, settings and data
source(here::here('report/ch2_load_data.r'))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: table 2.1.1 ----
# (Notifications of TB, HIV-positive TB, and DR-TB cases, globally and for WHO regions)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

t2.1.1_data <- filter(notification, year == (report_year - 1)) %>%
  select(g_whoregion,
         c_notified,
         c_newinc,
         new_labconf, new_clindx, new_ep,
         ret_rel_labconf, ret_rel_clindx, ret_rel_ep,
         newrel_hivpos,
         conf_rr_nfqr,
         conf_rr_fqr) %>%

  # calculate regional aggregates
  group_by(g_whoregion) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  arrange(entity) %>%
  select(-g_whoregion)


# Add global summary to the regional summary
t2.1.1_global <- t2.1.1_data %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(entity="Global")


t2.1.1_data <- rbind(t2.1.1_data, t2.1.1_global)


# Calculate total pulmonary and %ages that are bac confirmed and that are extrapulmonary
t2.1.1_data <- t2.1.1_data %>%
  mutate( newrel_pulm = new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx,
          newrel_pulm_conf_pct = (new_labconf + ret_rel_labconf) * 100 / (new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx),
          newrel_ep_pct = (new_ep + ret_rel_ep) * 100 / (c_newinc)
  ) %>%
  # Restrict to variables needed in the final output
  select(entity,
         c_notified,
         c_newinc,
         newrel_pulm,
         newrel_pulm_conf_pct,
         newrel_ep_pct,
         newrel_hivpos,
         conf_rr_nfqr,
         conf_rr_fqr)

# summary dataset for the text
t2.1.1_region <- filter(t2.1.1_data, entity!="Global") %>%
  arrange(desc(c_notified)) %>%
  mutate(c_total_p = c_notified/t2.1.1_global$c_notified*100) %>%
  mutate(c_newinc_cum_p = cumsum(c_total_p))

t2.1.1_txt <- filter(t2.1.1_data, entity=="Global") %>%
  mutate(c_pulm_p = newrel_pulm/c_newinc *100)

t2.1.1_txt <- filter(t2.1.1_region, entity=="Western Pacific Region") %>%
  select(c_newinc_cum_p = c_newinc_cum_p) %>%
  cbind(t2.1.1_txt)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.1 and 2.1.2 ----
# (Global trend in case notifications of people newly diagnosed with TB, 2010–2023)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f2.1.2_data <- filter(notification, year >= 2010) %>%
  select(year,
         g_whoregion,
         c_newinc) %>%
  
  # calculate regional aggregates
  group_by(g_whoregion,year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  arrange(entity) %>%
  select(-g_whoregion)

# to adjust yaxis range in facet_wrap
f2.1.2_data <- data.table(f2.1.2_data)
f2.1.2_data[g_whoregion == "AFR",y_min := 1.25*1e6]
f2.1.2_data[g_whoregion == "AFR",y_max := 1.5*1e6]
f2.1.2_data[g_whoregion == "AMR",y_min := 0.19*1e6]
f2.1.2_data[g_whoregion == "AMR",y_max := 0.24*1e6]
f2.1.2_data[g_whoregion == "SEA",y_min := 2.5*1e6]
f2.1.2_data[g_whoregion == "SEA",y_max := 3.5*1e6]
f2.1.2_data[g_whoregion == "EUR",y_min := 0.15*1e6]
f2.1.2_data[g_whoregion == "EUR",y_max := 0.25*1e6]
f2.1.2_data[g_whoregion == "EMR",y_min := 0.39*1e6]
f2.1.2_data[g_whoregion == "EMR",y_max := 0.55*1e6]
f2.1.2_data[g_whoregion == "WPR",y_min := 1*1e6]
f2.1.2_data[g_whoregion == "WPR",y_max := 1.5*1e6]

# texts!
f2.1.2_text <- f2.1.2_data %>%
  filter(entity=="African Region", year==2019|year==2020) %>%
  pivot_wider(names_from = year,
              values_from = c_newinc) %>%
  mutate(pct_decline=(1-`2020`/`2019`)*100)

f2.1.2_txt <- filter(f2.1.2_data, year>=2019) %>%
  mutate(c_newinc_p = lag(c_newinc)) %>%
  mutate(pct_dif = (c_newinc - c_newinc_p)*100/c_newinc_p) %>%
  filter(year==2020, entity=="African Region")


# Add global summary to the regional summary
f2.1.1_data <- f2.1.2_data %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(entity="Global")

f2.1.1_txt <- filter(f2.1.1_data, year>=2019) %>%
  mutate(c_newinc_p = lag(c_newinc)) %>%
  mutate(pct_dif = (c_newinc - c_newinc_p)*100/c_newinc_p) %>%
  filter(year==2020)

f2.1.1_txt2 <- filter(f2.1.1_data, year>=2019) %>%
  select(year,c_newinc) %>%
  mutate(c_newinc = c_newinc/1e6) %>%
  pivot_wider(names_from = year, values_from = c_newinc) %>%
  rename(c_newinc_2019 = `2019`, c_newinc_2020 = `2020`, c_newinc_2021 = `2021`, c_newinc_2022 = `2022`, c_newinc_2023 = `2023`) %>%
  mutate(pct_2023 = c_newinc_2023/c_newinc_2019*100 - 100)

f2.1.2_txt_AFR <- filter(f2.1.2_data, year>=2019 & g_whoregion == "AFR") %>%
  select(year,c_newinc) %>%
  pivot_wider(names_from = year, values_from = c_newinc) %>%
  rename(c_newinc_2019 = `2019`, c_newinc_2020 = `2020`, c_newinc_2021 = `2021`, c_newinc_2022 = `2022`, c_newinc_2023 = `2023`) %>%
  mutate(pct = c_newinc_2023/c_newinc_2019*100 - 100)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.3 ----
# (Trends in case notifications of people newly diagnosed with TB, 30 high TB burden countries, 2016-2021)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f2.1.3_data <- notification %>%
  select(iso3,year,country,g_whoregion,c_newinc) %>%
  filter(iso3 %in% list_hbcs_plus_wl$iso3) %>%
  subset(year>=2010) 

f2.1.2_txt_WPR <- f2.1.3_data %>%
  filter((iso3 == "CHN" |iso3 == "PHL") & year>=2019) %>%
  summarise(across(starts_with("c_new"), sum, na.rm = TRUE))

f2.1.2_txt_WPR <- f2.1.2_data %>%
  filter((g_whoregion == "WPR") & year>=2019) %>%
  summarise(across(starts_with("c_new"), sum, na.rm = TRUE)) %>%
  cbind(f2.1.2_txt_WPR) %>%
  rename(c_newinc_CHN_PHL = 2) %>%
  mutate(pct = c_newinc_CHN_PHL/c_newinc * 100)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.4 ----
# (Bar chart showing numbers of adults and children notified with TB each year since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.1.4_data <- filter(notification, year >= 2010) %>%

  select(iso2, year, c_newinc, c_new_014) %>%

  group_by(year) %>%
  summarise(across(starts_with("c_new"), sum, na.rm = TRUE)) %>%

  # calculate the "adult" fraction
  mutate(c_new_15plus = c_newinc - c_new_014) %>%

  # switch to long format for plotting
  pivot_longer(cols = starts_with("c_new_"),
               names_to = "age_group",
               values_to = "how_many")

f2.1.4_txt <- filter(notification, year >= 2013) %>%

  select(iso2, year, c_newinc, c_new_014, newrel_f15plus, newrel_m15plus) %>%

  group_by(year) %>%
  summarise(across(contains("new"), sum, na.rm = TRUE)) %>%

  # calculate the "adult" fraction
  mutate(c_new_15plus = c_newinc - c_new_014) %>%

  #calculate pct
  mutate(pct_m = newrel_m15plus/c_newinc * 100,
         pct_f = newrel_f15plus/c_newinc * 100,
         pct_c = c_new_014/c_newinc * 100)



# global notification by age and sex group
agesex_notification <- filter(notification, year >= 2013) %>%
  
  select(iso2, year, c_new_014, newrel_f15plus, newrel_m15plus) %>%
  
  group_by(year) %>%
  summarise(across(contains("new"), sum, na.rm = TRUE)) 

f2.1.4b_data <- agesex_notification %>%
  # switch to long format for plotting
  pivot_longer(cols = contains(c("new")),
               names_to = "age_sex",
               values_to = "how_many") %>%
  mutate(age_sex = factor(age_sex, levels = c("newrel_m15plus", "newrel_f15plus", "c_new_014" ),
                          labels = c("Men (aged \u226515 years)", "Women (aged \u226515 years)", "Children (aged 0\u201314 years)")))

f2.1.4b_pct_data <- f2.1.4_txt %>%
  select(year, contains(c("pct"))) %>%
  # switch to long format for plotting
  pivot_longer(cols = contains(c("pct")),
               names_to = "age_sex",
               values_to = "pct") %>%
  mutate(age_sex = factor(age_sex, levels = c("pct_m", "pct_f", "pct_c" ),
                          labels = c("Men (aged \u226515 years)", "Women (aged \u226515 years)", "Children (aged 0\u201314 years)")))

f2.1.4b_data <- f2.1.4b_data %>%
  left_join(f2.1.4b_pct_data, by = c("year","age_sex"))

f2.1.4b_txt <- agesex_notification %>%
  mutate(c_new_014_lag = lag(c_new_014),
         newrel_f15plus_lag = lag(newrel_f15plus),
         newrel_m15plus_lag = lag(newrel_m15plus)) %>%
  mutate(c_new_014_pct_dif = (c_new_014 - c_new_014_lag)*100/c_new_014_lag,
         newrel_f15plus_pct_dif = (newrel_f15plus - newrel_f15plus_lag)*100/newrel_f15plus_lag,
         newrel_m15plus_pct_dif = (newrel_m15plus - newrel_m15plus_lag)*100/newrel_m15plus_lag) %>%
  select(year, contains("pct")) %>%
  filter(year==2020) 

# 30 HBCs
f2.1.4b_hbc_low_data <- filter(notification, year == report_year-1 & iso3 %in% list_hbcs$iso3) %>%
  
  select(iso3, country, year, c_newinc, c_new_014, newrel_f15plus, newrel_m15plus) %>%
  
  group_by(iso3, country, year) %>%
  summarise(across(contains("new"), sum, na.rm = TRUE)) %>%
  
  # calculate the "adult" fraction
  mutate(c_new_15plus = c_newinc - c_new_014) %>%
  
  #calculate pct
  mutate(pct_m = newrel_m15plus/c_newinc * 100,
         pct_f = newrel_f15plus/c_newinc * 100,
         pct_c = c_new_014/c_newinc * 100) %>%

  select(country, year, contains(c("pct"))) %>%
  filter(pct_c < 5) %>%
  ungroup()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.5 ----
# (Map showing percentage of new and relapse TB cases that were children)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

kids_data <- filter(notification, year>=report_year-2) %>%

  select(iso3,
         country,
         year,
         c_new_014,
         newrel_m15plus,
         newrel_mu,
         newrel_f15plus,
         newrel_sexunk15plus,
         newrel_fu) %>%

  # calculate % of children in the age/sex data
  rowwise() %>%
  mutate(agesex_tot = sum(c_across(c_new_014:newrel_fu), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(kids_pct = ifelse(agesex_tot > 0,
                           c_new_014 * 100 / agesex_tot,
                           NA)) %>%

  # Assign the categories for the map
  mutate(var = cut(kids_pct,
                   c(0, 5.0, 10.0, 15.0, Inf),
                   c('0\u20134.9', '5\u20139.9', '10\u201314.9', '\u226515'),
                   right=FALSE))

# Find the countries with empty data for latest year and see if there are data for the previous year
kids_prev_year_data <- kids_data %>%
  filter(year == report_year - 1 & is.na(kids_pct)) %>%
  select(iso3) %>%
  inner_join(filter(kids_data, year == report_year - 2), by = "iso3") %>%
  filter(!is.na(kids_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f2.1.5_data <- kids_data %>%
  filter(year == report_year - 1) %>%
  anti_join(kids_prev_year_data, by= "iso3") %>%
  rbind(kids_prev_year_data)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.6 ----
# (Map showing percentage of extrapulmonary cases among new and relapse TB cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ep_data <- notification %>%
  filter(year  >= report_year - 2) %>%
  select(iso3,
         country,
         year,
         new_labconf, new_clindx, new_ep,
         ret_rel_labconf, ret_rel_clindx, ret_rel_ep) %>%

  # calculate % of extrapulmonary cases
  rowwise() %>%
  mutate(newrel_tot = sum(c_across(new_labconf:ret_rel_ep), na.rm = TRUE)) %>%
  mutate(ep_tot = sum(c_across(contains("_ep")), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ep_pct = ifelse(newrel_tot > 0,
                         ep_tot * 100 / newrel_tot,
                         NA)) %>%

  # Assign the categories for the map
  mutate(var = cut(ep_pct,
                   c(0, 10, 20, 30, Inf),
                   c('0\u20139.9', '10\u201319', '20\u201329', '\u226530'),
                   right=FALSE))

# Find the countries with empty data for latest year and see if there are data for the previous year
ep_prev_year_data <- ep_data %>%
  filter(year == report_year - 1 & is.na(ep_pct)) %>%
  select(iso3) %>%
  inner_join(filter(ep_data, year == report_year - 2), by = "iso3") %>%
  filter(!is.na(ep_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f2.1.6_data <- ep_data %>%
  filter(year == report_year - 1) %>%
  anti_join(ep_prev_year_data, by= "iso3") %>%
  rbind(ep_prev_year_data)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.7 ----
# (Map showing % of foreign borne cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.1.7_data <- notification %>%
  select(iso3,country,year,c_notified,notif_foreign) %>%
  filter(year==report_year-1|year==report_year-2) %>%
  mutate(notif_foreign = ifelse(iso3 == "TCD" & year==2023 & notif_foreign == c_notified, 0, notif_foreign)) %>% # Chad reported all cases as foreign cases: replaced with zero.
  mutate(pct_foreign = notif_foreign/c_notified * 100) %>%
  pivot_wider(names_from = year, values_from = c_notified:pct_foreign) %>%
  mutate(pct_foreign = ifelse(is.na(pct_foreign_2022),pct_foreign_2022,pct_foreign_2023)) %>%
  # Assign the categories for the map
  mutate(var = cut(pct_foreign,
                   c(0, 5, 25, 50, 75, Inf),
                   c('0\u20134','5\u201324','25\u201349','50\u201374','\u226575'),
                   right=FALSE)) 

f2.1.7_data %>%
  filter(!is.na(pct_foreign)) %>% nrow()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.8 ----
# Case notifications among prisoners
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f2.1.8_data <- strategy %>%
  filter(year == report_year - 1) %>%
  select(iso3,
         year,
         # ident_pris,
         newrel_prisoners) %>%
  left_join(filter(notification, year == report_year-1), by = c("iso3")) %>%
  select(iso3, country, newrel_prisoners, c_newinc) %>%
  mutate(prison_pct = #ident_pris > 0,
           newrel_prisoners * 100 / c_newinc)  %>%
  
  # Assign the categories for the map
  mutate(var = cut(prison_pct,
                   c(0, 1, 5, 10, Inf),
                   c('0\u20130.9', '1\u20134', '5\u20139', '\u226510'),
                   right=FALSE)) #%>%

f2.1.8_txt <- f2.1.8_data %>%
  filter(!is.na(newrel_prisoners))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.9 ----
# (Panel plot of line charts showing percentage contribution of the private sector to TB notifications by year since 2010)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# for Monica: country selection
ppm_disp <- data_collection %>%
  filter(dc_ppm_display==1 & datcol_year==2024) %>%
  select(iso2)

# Get PPM data for 7 priority countries
f2.1.9_data <- 
  filter(strategy, year >= 2010 & iso2 %in% ppm_disp$iso2) %>%
  select(iso2, iso3, year, country, priv_new_dx,pub_new_dx) %>%
  
  # Merge with notifications
  inner_join(select(notification, iso2, year, c_notified), by = c("iso2", "year")) %>%
  
  # Calculate percent contributions
  mutate(private_pcnt = ifelse(c_notified > 0,
                               priv_new_dx * 100 / c_notified,
                               NA),
         public_pcnt = ifelse(c_notified > 0,
                              pub_new_dx * 100 / c_notified,
                              NA)) %>%
  select(iso2:country,private_pcnt:public_pcnt) %>%
  pivot_longer(names_to = "pp", cols = private_pcnt:public_pcnt, values_to = "pcnt") 

writexl::write_xlsx(f2.1.9_data, here::here("./report/local/f2.1.9_data.xlsx"))

f2.1.9a_data <- 
  filter(f2.1.9_data, iso2 %in% c('BD', 'IN', 'ID', 'KE', 'NG', 'PK', 'PH')) %>%
  # mutate(pcnt = ifelse(pcnt==0,NA,pcnt)) %>%
  mutate(pcnt = ifelse(pp=="public_pcnt" & pcnt>50 & iso3=="KEN", NA ,pcnt)) %>%
  filter(!is.na(pcnt))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.10 ----
# (map showing percentage contribution of the private sector to TB notifications, the latest year)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get PPM data for the latest year.
## private in the latest year
f2.1.10_data <- f2.1.9_data %>%
  filter(pp == "private_pcnt", year == report_year-1) %>%
  # Assign the categories for the map
  mutate(var = cut(pcnt,
                   c(0, 5, 10, 25, 50, Inf),
                   c('0\u20134','5\u20139','10\u201324','25\u201349','\u226550'),
                   right=FALSE)) 

## public in the latest year
# f2.1.10b_data <- f2.1.9_data %>%
#   filter(pp == "public_pcnt", year == report_year-1) %>%
#   # Assign the categories for the map
#   mutate(var = cut(pcnt,
#                    c(0, 5, 10, 25, 50, Inf),
#                    c('0\u20134','5\u20139','10\u201324','25\u201349','\u226550'),
#                    right=FALSE)) 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.11 ----
# (Map showing which countries have case-based TB surveillance systems)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if(datacoll){
  
cb_data <- strategy %>%
  filter(year >= report_year - 3) %>%
  select(iso3,
         country,
         year,
         caseb_err_nat)

# Make sure all ECDC countries are marked as having case-based surveillance for all TB cases
cb_ecdc <- data_collection %>%
  filter(datcol_year == report_year & dc_ecdc == 1) %>%
  select(iso3)

cb_data <- cb_data %>%
  mutate(caseb_err_nat = ifelse(iso3 %in% cb_ecdc$iso3, 42, caseb_err_nat)) %>%

  # UK hasn't responded, but we know it has a case-based system, so fudge it
  mutate(caseb_err_nat = ifelse(iso3=="GBR", 42, caseb_err_nat)) %>%

  # Assign the categories for the map
  mutate(var = factor(caseb_err_nat,
                      levels = c(0, 44, 43, 42),
                      labels = c("No case-based digital system", "Partially (in transition)", "People diagnosed with MDR-TB only", "All people diagnosed with TB")))

# Find the countries with empty data for latest year and see if there are data for the previous years
cb_prev_year_data <- cb_data %>%
  filter(year == report_year - 1 & is.na(caseb_err_nat)) %>%
  select(iso3) %>%
  inner_join(filter(cb_data, year >= report_year - 5), by = "iso3") %>%
  arrange(desc(year)) %>%
  filter(!is.na(caseb_err_nat)) %>% distinct(iso3, .keep_all = TRUE)

# Now combine into one dataframe, with previous data used if latest year's data are not available
f2.1.11_data <- cb_data %>%
  filter(year == report_year - 1) %>%
  anti_join(cb_prev_year_data, by= "iso3") %>%
  rbind(cb_prev_year_data)


# Simple summary for the section text
## the latest year
f2.1.11_txt <- filter(f2.1.11_data, caseb_err_nat == 42) %>%
  select(iso3) %>%
  inner_join(filter(notification, year==report_year-1), by = "iso3") %>%
  summarise(across(c_newinc, sum, na.rm=TRUE))

f2.1.11_txt <- filter(notification, year==report_year-1) %>%
  summarise(across(c_newinc, sum, na.rm=TRUE)) %>%
  select(c_newinc_glob = c_newinc) %>%
  cbind(f2.1.11_txt) %>%
  mutate(pct_notif_caseb = c_newinc*100/c_newinc_glob) %>%
  select(pct_notif_caseb)

f2.1.11_txt <- 
  filter(f2.1.11_data, caseb_err_nat == 42) %>% nrow() %>%
  cbind(f2.1.11_txt) %>%
  rename(cb_n = 1)
   
## the latest year - 1
cb_2022_data <- cb_data %>%
  filter(year == report_year - 2 & is.na(caseb_err_nat))  %>%
  select(iso3) %>%
  inner_join(filter(cb_data, year <= report_year - 3, year >= report_year - 7), by = "iso3") %>%
  filter(!is.na(caseb_err_nat))

cb_2022_data <- cb_data %>%
  filter(year == report_year - 2) %>%
  anti_join(cb_2022_data, by= "iso3") %>%
  rbind(cb_2022_data)

cb_2022_data_txt <- filter(cb_2022_data, caseb_err_nat == 42) %>%
  select(iso3) %>%
  inner_join(filter(notification, year==report_year-2), by = "iso3") %>%
  summarise(across(c_newinc, sum, na.rm=TRUE))

cb_2022_data_txt <- filter(notification, year==report_year-2) %>%
  summarise(across(c_newinc, sum, na.rm=TRUE)) %>%
  select(c_newinc_glob = c_newinc) %>%
  cbind(cb_2022_data_txt) %>%
  mutate(pct_notif_caseb = c_newinc*100/c_newinc_glob) %>%
  select(pct_notif_caseb)

cb_2022_data_txt <- 
  filter(cb_2022_data, caseb_err_nat == 42) %>% nrow() %>%
  cbind(cb_2022_data_txt) %>%
  rename(cb_n = 1)

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: text only ----
# (Number of zoonotic TB cases and countries reported zoonotic TB cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

zoonotic <- strategy %>%
  filter(year == report_year - 1) %>%
  select(iso3,
         country,
         year,
         ident_zoonotic,
         zoonotic) %>%
  filter(ident_zoonotic == 1)

zoonotic_country <- zoonotic %>% nrow()
zoonotic_cases <- zoonotic %>% select(zoonotic) %>% summarise(across(where(is.numeric), sum, na.rm = TRUE))

f2.1.12_data <- strategy %>%
  filter(year == report_year - 1) %>%
  select(iso3,
         country,
         year,
         ident_zoonotic) %>%
  mutate(ident_zoonotic = ifelse(ident_zoonotic == 3, 0, ident_zoonotic)) %>%
  
  # Assign the categories for the map
  mutate(var = factor(ident_zoonotic,
                      levels = c(1, 0),
                      labels = c("Reported data on zoonotic TB", "Not reported data on zoonotic TB"))) 

palatte_fig2.1.12 = c("blueviolet","#EFF3FF")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# for main texts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# grouping countries by changes in 2021 and 2023
ranking <- notification %>%
  select(iso3,year,country,g_whoregion,c_newinc) %>%
  pivot_wider(names_from = year, values_from = c_newinc) %>%
  mutate(pct2123 = `2023`/`2021`-1,
         num2123 = `2023`-`2021`) %>%
  select(iso3,country,g_whoregion,`2020`:`2023`,pct2123:num2123) %>%
  left_join(list_hbcs_plus_wl,by="iso3") %>%
  rename(country=country.x, country_hbc=country.y) %>%
  mutate(hbc=ifelse(is.na(country_hbc),0,1))

## Percentage of shortfall
ranking <- ranking %>%
  mutate(tot2123=sum(num2123,na.rm=T))

## for 2020
ranking <- ranking %>%
  arrange(desc(num2123)) %>%
  slice(1:10) %>%
  mutate(pct_contribute=num2123/tot2123*100) %>%
  mutate(cumsum=cumsum(pct_contribute)) %>%  # mutate(hit90=ifelse(cumsum>=0.9&cumsum<=0.902,"yes","no"))
  mutate(hit90=ifelse(cumsum<=90,"yes","no")) %>%
  select(iso3, country, pct_contribute, cumsum, hit90)

