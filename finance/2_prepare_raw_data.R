# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation/imputation script, Part I up to running cleaning script
# tThis is testing version! 
# Translated from Stata version written by A SiroKa and P Nguhiu
# Before running this code, you must make sure that ch4_clean_data.R is revised and 
# up-to-date according to the latest finance data review 
# Takuya Yamanaka, July 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load chapter 4, settings and data
source(here::here('finance/1_load_data.r'))

# Merge datasets
wb <- wb %>%
  mutate(year = as.integer(year))

finance_merged <- finance %>%
  left_join(iso3_income, by = c("iso3"), suffix = c("", ".y")) %>%
  left_join(select(outcomes,-country,-iso2,-iso_numeric,-g_whoregion), by = c("iso3","year"), suffix = c("", ".y")) %>%
  # left_join(est_country, by = c("iso3","year"), suffix = c("", ".y")) %>%
  # left_join(select(est_dr_country, -source_new, -source_ret), by = c("iso3","year"), suffix = c("", ".y")) %>%
  # left_join(select(estimates_population, -country,-iso2,-g_whoregion), by = c("iso3","year")) %>%
  left_join(select(notification, -country,-iso2,-iso_numeric,-g_whoregion), by = c("iso3","year"), suffix = c("", ".y")) %>%
  select(-contains(".y"))

finance_merged <- left_join(finance_merged, wb, by = c("iso3", "year"))
finance_merged <- left_join(finance_merged, weo, by = c("iso3", "year"))

finance_merged <- finance_merged %>%
  left_join(hbc30, by = c("iso3")) %>%
  left_join(hbmdr30, by = c("iso3")) %>%
  left_join(hbtbhiv30, by = c("iso3"))
  
# Drop and keep observations
finance_merged <- finance_merged %>%
  filter((country!="" | is.na(country)) & year >= 2013)

finance_merged <- finance_merged %>%
  left_join(select(treasury, iso3, year, un_pa_exch_rate), by = c("iso3", "year"))

# FILLING IN MISSING NOTIFICATIONS
# After speaking with PG we decided to not attempt to scale up notifications.
# Instead we will use the previous year's notification
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  mutate(c_notified = if_else(is.na(c_notified), lag(c_notified), c_notified)) %>%
  # rowwise() %>%
  # Derive total number detected and total enrolled on treatment
  mutate(
    # rr_detected = ifelse(year < 2014,
    #                           sum(across(rapid_dx_dr_r:conf_mdr), na.rm = TRUE),
    #                           # the next three are mutually exclusive so can be added
    #                           sum(across(conf_rrmdr:conf_rr_fqr), na.rm = TRUE)),
         # treatment variables are in mutually exclusive sets so again can be added
         mdr_tx = rowSums(select(., conf_mdr_tx,
                                      unconf_mdr_tx,
                                      conf_rrmdr_tx,
                                      unconf_rrmdr_tx,
                                      conf_rr_nfqr_tx,
                                      unconf_rr_nfqr_tx,
                                      conf_rr_fqr_tx), na.rm = TRUE)) %>%
  mutate(mdr_tx = if_else(year == report_year-1 & mdr_tx == 0 & lag(mdr_tx)>0, lag(mdr_tx), mdr_tx)) 
  

# Percentage of TB burden for base year
finance_merged <- finance_merged %>%
  group_by(country) %>%
  arrange(country, year) %>%
  mutate(percent_of_cases = ifelse(year == base_year, c_notified / sum(c_notified, na.rm = TRUE) * 100, NA)) %>%
  mutate(percent_of_cases = if_else(is.na(percent_of_cases), lag(percent_of_cases), percent_of_cases)) %>%
  ungroup()

# Zero treated same as missing
finance_merged <- finance_merged %>%
  mutate(
    rcvd_tot = if_else(rcvd_tot == 0, NA_real_, rcvd_tot),
    exp_tot = if_else(exp_tot == 0, NA_real_, exp_tot),
    cf_tot = if_else(cf_tot == 0, NA_real_, cf_tot)
  )

# Load cleaning script
source(here::here('finance/2a_clean_raw_data.R'))
# source(here::here('finance/2a_clean_raw_data.R'))

# Manually setting countries where not enough data was available to accurately fill in rcvd_tot
# These THREE low/middle income countries are Dominica, Grenada, and Uzbekistan who have last reported data from 2014, 2016, and 2018, respectively.
# FOUR Other countries: Costa Rica, Turkey, Mauritius, and St. Vincent
# There are a further TWO countries which should not be backwards filled as the reported data is more than 4 years into the future: Albania, and Gambia.
finance_merged <- finance_merged %>%
  mutate(
    do_not_fill = if_else(
      iso3 %in% c(#"ALB", "CRI", 
        "DMA", #"GMB", 
        "GRD", "MUS", "TUR", "UZB", "VCT"),1,0))

finance_merged <- finance_merged %>%
  mutate(flag = 0)

# Reported data (1)
finance_merged <- finance_merged %>%
  mutate(flag = if_else(!is.na(rcvd_tot) & year != report_year, 1, flag))

# Use expenditure data (2)
finance_merged <- finance_merged %>%
  mutate(flag = if_else(is.na(rcvd_tot) & !is.na(exp_tot) & year != report_year & do_not_fill != 1 & flag == 0, 2, flag))

# Use committed funding (3)
finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  mutate(flag = if_else(is.na(rcvd_tot) & !is.na(cf_tot) & year != report_year & do_not_fill != 1 & flag == 0, 3, flag))

# Use last year's rcvd (4)
finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  group_by(iso3) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(rcvd_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0, 5, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(rcvd_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0, 5, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(rcvd_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0, 5, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(rcvd_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0, 5, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(rcvd_tot, 5)) & year != report_year & do_not_fill != 1 & flag == 0, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 5)) & year != report_year & do_not_fill != 1 & flag == 0, 5, flag)) %>%
  ungroup()
  

# If year 2020 rcvd is based on expenditure and year 2021 is blank, we want a flag of 4 for 2021 saying it is based 
# off last year's rcvd, which itself would have a flag of 2 (based off of expenditure or CF)
finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  group_by(iso3) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(exp_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 1) == 2, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(exp_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 2) == 2, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(exp_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 3) == 2, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(exp_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 4) == 2, 4, flag)) %>%
  ungroup()

finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  group_by(iso3) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(cf_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 1) == 3, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(cf_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 2) == 3, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(cf_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 3) == 3, 4, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lag(cf_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0 & lag(flag, 4) == 3, 4, flag))  %>%
  ungroup()

# same logic for the year using next year's data
finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  group_by(iso3) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(exp_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 1) == 2, 5, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(exp_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 2) == 2, 5, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(exp_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 3) == 2, 5, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(exp_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 4) == 2, 5, flag)) %>%
  ungroup()

finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  group_by(iso3) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(cf_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 1) == 3, 5, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(cf_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 2) == 3, 5, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(cf_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 3) == 3, 5, flag)) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & !is.na(lead(cf_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0 & lead(flag, 4) == 3, 5, flag))  %>%
  ungroup()


# Fill holes in earlier years making all blanks equal to next year with rcvd_tot (5)
# finance_merged <- finance_merged %>%
#   arrange(country, year) %>%
#   mutate(flag = case_when(
#     is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 1)) & year != report_year & do_not_fill != 1 & flag == 0 ~ 5,
#     is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 2)) & year != report_year & do_not_fill != 1 & flag == 0 ~ 5,
#     is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 3)) & year != report_year & do_not_fill != 1 & flag == 0 ~ 5,
#     is.na(rcvd_tot) & !is.na(lead(rcvd_tot, 4)) & year != report_year & do_not_fill != 1 & flag == 0 ~ 5,
#     TRUE ~ flag
#   ))

finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  mutate(flag = ifelse(is.na(rcvd_tot) & year == 2013 & do_not_fill != 1 & flag == 4, 5, flag)) #%>% # temporal arrangement for Angola 2013, not to use 2012 data

finance_merged <- finance_merged %>%
  dplyr::select(country:g_whoregion, g_income, g_hb_tb:g_hb_tbhiv, everything())


# save intermediate output .rda
rm(notification, outcomes, #regional, 
   tpt, wb, wb_incomelist, weo, finance, finance_pre2014#, estimates_population
   )
save.image(here::here("finance/local/finance_cleaned_raw.Rdata"))

writexl::write_xlsx(finance_merged, here::here("finance/local/finance_cleaned_raw.xlsx"))