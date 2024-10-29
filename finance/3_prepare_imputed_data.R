# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation/imputation script, Part II imputation
# Translated from Stata version written by A SiroKa and P Nguhiu
# Takuya Yamanaka, June 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load chapter 4, cleaned raw data
source(here::here('finance/1_load_data.r'))
load(here::here('finance/local/finance_cleaned_raw.Rdata'))

#--- imputation to be added ---#
# /*************************************************************************************************************************
#   Fill in missing received funding. First with expenditures, then with that year's committed, 
# then with previous year's spending. Also backwards fill for early years. Assume reported 0 totals are missing.
# 
# A problem here is that the clean code often does similar steps to the quick fixes thus labeling the imputation method is difficult.
# For example, South Africa expenditure is set to CF in the clean file so this should be labelled as "Uses committed funding", 
# however, we must label the imputation before the clean.do is run and the quick fixes will not change the data because it has already
# been altered by the cleaning.
# 
# There is probably a more elegant way to code this step. Any suggestions welcome!
#   *************************************************************************************************************************/
# Use expenditure data
finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(exp_tot) & year != report_year &  do_not_fill != 1 & flag == 2, exp_tot, rcvd_tot))

# Use committed funding
finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(cf_tot) & year != report_year & do_not_fill != 1 & flag == 3, cf_tot, rcvd_tot)) %>%
  ungroup()

# Use last year's rcvd
finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lag(rcvd_tot)) & year != report_year & do_not_fill != 1 & flag == 4, lag(rcvd_tot), rcvd_tot)) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lag(rcvd_tot,2)) & year != report_year & do_not_fill != 1 & flag == 4, lag(rcvd_tot,2), rcvd_tot)) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lag(rcvd_tot,3)) & year != report_year & do_not_fill != 1 & flag == 4, lag(rcvd_tot,3), rcvd_tot)) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lag(rcvd_tot,4)) & year != report_year & do_not_fill != 1 & flag == 4, lag(rcvd_tot,4), rcvd_tot)) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lag(rcvd_tot,5)) & year != report_year & do_not_fill != 1 & flag == 4, lag(rcvd_tot,5), rcvd_tot)) %>%
  # mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lag(rcvd_tot)) & year != report_year & do_not_fill != 1 & flag == 4, lag(rcvd_tot), rcvd_tot)) %>%
  ungroup()

# Fill holes in earlier years making all blanks equal to next year with rcvd_tot
finance_merged <- finance_merged %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lead(rcvd_tot)) & year != report_year & do_not_fill != 1 & flag == 5, lead(rcvd_tot), rcvd_tot)) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lead(rcvd_tot,2)) & year != report_year & do_not_fill != 1 & flag == 5, lead(rcvd_tot,2), rcvd_tot)) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lead(rcvd_tot,3)) & year != report_year & do_not_fill != 1 & flag == 5, lead(rcvd_tot,3), rcvd_tot)) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lead(rcvd_tot,4)) & year != report_year & do_not_fill != 1 & flag == 5, lead(rcvd_tot,4), rcvd_tot)) %>%
  mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lead(rcvd_tot,5)) & year != report_year & do_not_fill != 1 & flag == 5, lead(rcvd_tot,5), rcvd_tot)) %>%
  # mutate(rcvd_tot = if_else(is.na(rcvd_tot) & !is.na(lead(rcvd_tot)) & year != report_year & do_not_fill != 1 & flag == 5, lead(rcvd_tot), rcvd_tot)) %>%
  ungroup()

# Generate rcvd_imputation
finance_merged <- finance_merged %>%
  mutate(rcvd_imputation = case_when(
    flag == 1 ~ "Reported data",
    flag == 2 ~ "Expenditure data",
    flag == 3 ~ "Committed funding",
    flag == 4 ~ "Last year's received",
    flag == 5 ~ "Next year's received",
    flag == 0 ~ "Data could not be imputed"
  )) %>%
  mutate(flag_rcvd = flag) %>%
  arrange(country, year)


flag <- finance_merged %>%
  select(iso3, country, year, g_income, flag, rcvd_imputation) %>%
  filter(g_income!="HIC" & flag==0 & year!=report_year)

writexl::write_xlsx(flag, here::here("finance/local/imp_excluded.xlsx"))

writexl::write_xlsx(finance_merged, here::here("finance/local/finance_cleaned_rcvd_replaced.xlsx"))

# FILL IN MISSING COUNTRY NAMES, ISO2 CODES, ISO_NUMERIC VALUES, GROUPINGS
vars_to_fill <- c("iso3", "iso_numeric", "country", "g_whoregion", "g_hb_tb", "g_hb_tbhiv", "g_hb_mdr", "g_income")

for (var in vars_to_fill) {
  finance_merged <- finance_merged %>%
    arrange(iso2, !!sym(var)) %>%
    group_by(iso2) %>%
    mutate(!!sym(var) := zoo::na.locf(!!sym(var), na.rm = FALSE)) %>%
    ungroup()
}

# Sort country year
finance_merged <- finance_merged %>%
  arrange(country, year)

# Convert e_pop_num to numeric
finance_merged$e_pop_num <- as.numeric(as.character(finance_merged$e_pop_num))

# ** 2021: deflator_us variable isn't updated for 22 countries (those who're not in WEO database). Manually update these (use code from WEO section above)
missing_deflator <- finance_merged %>%
  filter(is.na(deflator_us) & year >= base_year)

# Print the table of iso3 and year for missing deflator_us values
print(missing_deflator %>% select(iso3, year))

calculate_mode <- function(x) {
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])
}

# Calculate the mode of deflator_us for each year
finance_merged <- finance_merged %>%
  group_by(year) %>%
  mutate(deflator_us_replacer = calculate_mode(deflator_us)) %>%
  ungroup()

# // PN: This ensures uniform deflation for USD values across all countries. Keep it this way.
finance_merged <- finance_merged %>%
  mutate(deflator_us = if_else(is.na(deflator_us), deflator_us_replacer, deflator_us)) %>%
  select(-deflator_us_replacer)

# // ADJUSTMENTS TO WB AND IMF DATA
# // PN Jul 2023: 8 countries had their WB income group changed in Jul 2023. Confirmed that these are updated on country_info.dta
# // https://blogs.worldbank.org/opendata/new-world-bank-group-country-classifications-income-level-fy24
# // PN Jul 2022: 6 countries had their WB income group changed in Jul 2022. Confirmed that these are updated on country_info.dta
# // https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2022-2023
finance_merged <- finance_merged %>%
  mutate(imf_gdp_pc_cur_usd = if_else(iso3 %in% c("PSE", "ASM", "SYR", "CUB"), ny_gdp_pcap_cd, imf_gdp_pc_cur_usd))


# // 1B. GDP Per capita, in International PPP adjusted dollars.
# // This is primarily obtained from the WB dataset and is used as the principal input for choice model,
# // but a few countries don't have values.
finance_merged <- finance_merged %>%
  # filter(year > 2010 & iso3 == "YEM") %>%
  # summarise(ny_gdp_pcap_pp_cd_mode = mode(ny_gdp_pcap_pp_cd), imf_gdp_pc_cur_int_mode = mode(imf_gdp_pc_cur_int)) %>%
  # ungroup() %>%
  mutate(ny_gdp_pcap_pp_cd = if_else(iso3 %in% c("SYR", "YEM", "CUB", "ERI", "PRK"), imf_gdp_pc_cur_int, ny_gdp_pcap_pp_cd))


# // 2. OFFICIAL EXCHANGE RATES 
# * PN 2023: Compared exchange rates from GHED but realise that these are drawn from WB dataset, and are 1 year behind 
# * Decision is to use WB's exchange rates.
# 	
# 	/* IN 2022, there were missing exchange rate values for the following non HICs
# 	
# 	misstable summ pa_nus_fcrf if year == base_year & g_income != "HIC"
# 	
# 	                                                              Obs<.
#                                                 +------------------------------
#                |                                | Unique
#       Variable |     Obs=.     Obs>.     Obs<.  | values        Min         Max
#   -------------+--------------------------------+------------------------------
#    pa_nus_fcrf |        29                 107  |     84        .71    23271.21
#   -----------------------------------------------------------------------------
# 
# 	*/    

finance_merged <- finance_merged %>%
  rename(off_exch_rate = pa_nus_fcrf) %>%
  mutate(off_exch_rate = if_else(is.na(off_exch_rate) & iso3 %in% c("CUB", "PRK", "SYR", "SOM", "TKM", "SDN"), un_pa_exch_rate, off_exch_rate))

# Further updates for missing exchange rates
update_countries <- c("AFG", "COD", "ETH", "GIN", "IRQ", "MMR", "GUY", "KGZ", "LBR", "LKA", "MNG", "MRT", "MWI", "NGA", "PNG", "SLB", "SSD", "STP", "TUR", "TZA")
finance_merged <- finance_merged %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(off_exch_rate = if_else(is.na(off_exch_rate) & iso3 %in% update_countries & year > 2012 & year < report_year & g_income != "HIC", un_pa_exch_rate, off_exch_rate),
         off_exch_rate = if_else(is.na(off_exch_rate) & iso3 %in% c("IRN", "VEN", "ZWE") & year > 2012 & year < report_year & g_income != "HIC", lag(off_exch_rate), off_exch_rate),
         off_exch_rate = if_else(iso3 == "ZWE" & year >= 2007 & year <= 2022, 1, off_exch_rate),  # ZWE adopted USD as currency in 2008, and reverted to ZWE bonds in 2018
         off_exch_rate = if_else(iso3 == "VEN" & year >= 2018 & year < report_year, 9.975, off_exch_rate),  # Venezuela's hyperinflation in 2016 makes period average exchange rates invalid
         off_exch_rate = if_else(iso3 == "IRN" & year > 2018 & year < report_year, lag(off_exch_rate), off_exch_rate))  # Iran's exchange rate is carried forward from last year

# PN 2022: Cuba's exchange rate is government-controlled at 1 to the dollar for state agencies. Fill backwards
finance_merged <- finance_merged %>%
  mutate(off_exch_rate = if_else(iso3 == "CUB" & is.na(off_exch_rate), 1, off_exch_rate))

# PN 2023: PSE uses the Israeli Shekel as currency (but there are parallel currencies eg JOR dinar, and USD)
finance_merged <- finance_merged %>%
  mutate(temp_var1 = if_else(iso3 == "ISR", off_exch_rate, NA_real_))

finance_merged <- finance_merged %>%
  group_by(year) %>%
  mutate(temp_var2 = min(temp_var1, na.rm = TRUE)) %>%
  ungroup()

finance_merged <- finance_merged %>%
  mutate(off_exch_rate = if_else(iso3 == "PSE", temp_var2, off_exch_rate)) 

finance_merged <- finance_merged %>%
  select(-temp_var1, -temp_var2)


# // 3. PPP in WB's dataset (pa_nus_ppp) compared to IMF.
# 	** PN 2022: From 2020 we use WB's variable for the unit cost computation (instead of the IMF predicted values). 
# ** However WB's data has more missing values - see list below	 
# 	* We may use imf_ppp_conv for some specific country changes (having checked that these specific changes are reasonable)
finance_merged <- finance_merged %>%
  mutate(pa_nus_ppp = if_else(!is.na(imf_ppp_conv) & iso3 %in% c("AND", "CUB", "DJI", "ERI", "PRK", "SSD", "SYR", "YEM", "VEN"), imf_ppp_conv, pa_nus_ppp),
         pa_nus_ppp = if_else(is.na(pa_nus_ppp) & iso3 == "SOM" & year <= 2010, imf_ppp_conv, pa_nus_ppp),
         # * We leave VEN and ZWE  with the 2017 value since the exchange rate is also carried forward from 2017 for now 
         pa_nus_ppp = if_else(iso3 == "VEN" & year >= 2018 & year <= report_year, 2.6809826, pa_nus_ppp),
         pa_nus_ppp = if_else(iso3 == "ZWE" & year >= 2018 & year <= report_year, 1.032, pa_nus_ppp))


# * Sierra leone. WB provides exchange rate in rebased currency (1 SLL = 1000 previous SLL
# * convert pa_nus_ppp which hasn't yet been rebased to new currency.
finance_merged <- finance_merged %>%
  mutate(pa_nus_ppp = if_else(iso3 == "SLE" & pa_nus_ppp > 10, pa_nus_ppp / 1000, pa_nus_ppp)) #// previous range was 600 - 3600. Desired range is below 10 for now.

# // 4. DEFLATOR (IMF) - imf_deflator (from WEO NGDP_D) versus (WB) - ny_gdp_defl_zs
# 	** PN 2023: Decision to use IMF deflator based on guidance from the Discussion paper on macroeconomic data sources for GHED
# 	** "IMF WEO is suggested for its completeness, stability, abundant metadata and the consistency that it brings to the indicators involving GDP"
# 	** Source "Indikadahena CK, Brindley C, Xu K, Roubal T. Sources of macro-economic data for global health expenditure indicators. Geneva: World Health Organization; 2018."

# // In the 2023 run, there are 5 countries whose IMF deflator (the principal variable to use) is missing for 2022 and therefore couldn't compute rebasing ratio 
# // AFG, ASM, CUB, LBN, PRK, PSE and SYR
# replace imf_deflator = ny_gdp_defl_zs if inlist(iso3, "PSE", "ASM", "SYR", "CUB")

# ** In the unit cost script, this is dealt with by projecting the log transformed variable
# 
# * PN 2023: replace ZWE  and VEN latest deflator values (for 2019 and 2020) with missing, 
# * to aid UC script to interpolate (challenge with hyperinflation 2018 -20)
# * (2019 value was 992%, 2020 was 6537%)
finance_merged <- finance_merged %>%
  mutate(
    ny_gdp_defl_zs = if_else(iso3 == "ZWE" & year >= 2019 & year <= report_year, NA_real_, ny_gdp_defl_zs),
    imf_deflator = if_else(iso3 == "ZWE" & year >= 2019 & year <= report_year, NA_real_, imf_deflator),
    ny_gdp_defl_zs = if_else(iso3 == "VEN" & year >= 2017 & year <= report_year, NA_real_, ny_gdp_defl_zs),
    imf_deflator = if_else(iso3 == "VEN" & year >= 2017 & year <= report_year, NA_real_, imf_deflator)
  )

# ** PN 2022: As opposed to last year when we used IMF deflator, this time we revert to WB even though it is not yet rebased to 2018 - it's using 2014 base)
# 	//replace deflator_wb = deflator if iso3 == "BLR"
# 	* PN 2022: SSD data in WB reports only till 2015. IMF projects from 2019 onwards but results in outlier values (14295% in 2022)
# 	* As done in 2021, replace South Sudan values with IMF reported values (WB values missing) but only until 2018
# 	//replace ny_gdp_defl_zs = imf_deflator if iso3 == "SSD" & year <= 2018 & ny_gdp_defl_zs == . // from 2019, IMF values are projected and not real data points
# 
# 	* PN 2022: Sudan's deflator reported is high at 20225% in 2020 (due to the political situation). Change to missing this year
# //replace ny_gdp_defl_zs = . if iso3 == "SDN" & (year == 2020 | year == 2021) 
# 
# * PN Jul 16 2022 Other countries (AFG, LKA, LBY, ARG, VEN) look consistent based on a comparison with last year.
# 
# // RENAME COUNTRIES FOR WHICH COMMAS CREATE PROBLEMS WHEN WE EXPORT TO CSV FILES
finance_merged <- finance_merged %>%
  mutate(
    country = if_else(country == "China, Hong Kong SAR", "China (Hong Kong SAR)", country),
    country = if_else(country == "China, Macao SAR", "China (Macao SAR)", country),
    country = if_else(country == "Bonaire, Saint Eustatius and Saba", "Bonaire Saint Eustatius and Saba", country)
  )

# // CORRECTIONS TO AUTO-CALCULATION OF FIELDS IN THE ONLINE FORM
# // FOR THE TOTALS AND ROW SUBTOTALS, WE DO NOT ALLOW ZEROES
sources <- c("gap", "exp", "budget", "cf", "rcvd")

# Loop through each source variable
for (source in sources) {
  finance_merged <- finance_merged %>%
    mutate(!!paste0(source, "_tot") := if_else(!!sym(paste0(source, "_tot")) == 0, NA_real_, !!sym(paste0(source, "_tot"))))
}

# // REPLACE NEGATIVE GAPS WITH MISSING WHEN TOTAL REQUIRED IS NOT REPORTED
vars <- c("prog", "fld", "lab", "staff", "sld", "mdrmgt", "tpt", "tbhiv", "patsup", "orsrvy", "oth", "tot")

# Loop through each variable
for (var in vars) {
  finance_merged <- finance_merged %>%
    mutate(!!paste0("gap_", var) := if_else(!!sym(paste0("gap_", var)) < 0, NA_real_, !!sym(paste0("gap_", var))))
}

save(finance_merged, file=here::here("finance/local/finance_cleaned.Rda"))

load(here::here("finance/local/finance_cleaned.Rda"))


# /******************************************************************************************************************
#   Calculate GHS costs with unit costs.
# ********************************************************************************************************************/
#   
# // UTILIZATION OF GENERAL HEALTH-CARE SERVICES (GHS) IGB revised 26.06.13 I hid ZAF, BRA sent to AP check if 2012 needs adj!!
# //Importing IER unit costs from Christopher's 2012. Source: finance_pred_cint_dta 7/25/2012. Since I have not run PART III, I will use financingadj1

source(here::here('finance/3a_unit_cost.R'))


unit_cost <- finance_merged %>%
  select(iso3, year, ny_gdp_pcap_pp_cd, off_exch_rate, pa_nus_ppp, starts_with("uc_"), deflator)

unit_cost <- unit_cost %>%
filter(!is.na(iso3) & iso3 != "")

save.image(here::here("finance/local/unit_cost.Rdata"))
writexl::write_xlsx(unit_cost, here::here("finance/local/unit_cost.xlsx"))

# /****************************** 
#   Clean utilization data
# ******************************/		
#   // Missing outpatient utilization data set to previous year.  Also MDR patients on treatment
vars_to_replace <- c("hcfvisit_mdr", "hospd_mdr_prct", "hospd_mdr_dur", 
                     "hcfvisit_dstb", "hospd_dstb_prct", "hospd_dstb_dur", "mdr_tx")

# Loop through each variable
for (var in vars_to_replace) {
  finance_merged <- finance_merged %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate_at(vars(var), ~ifelse(is.na(.x), lag(.x,1), .x)) %>%
    mutate_at(vars(var), ~ifelse(is.na(.x), lag(.x,2), .x)) %>%
    mutate_at(vars(var), ~ifelse(is.na(.x), lag(.x,3), .x)) %>%
    mutate_at(vars(var), ~ifelse(is.na(.x), lag(.x,4), .x)) %>%
    mutate_at(vars(var), ~ifelse(is.na(.x), lead(.x,1), .x)) %>%
    mutate_at(vars(var), ~ifelse(is.na(.x), lead(.x,2), .x)) %>%
    mutate_at(vars(var), ~ifelse(is.na(.x), lead(.x,3), .x)) %>%
    mutate_at(vars(var), ~ifelse(is.na(.x), lead(.x,4), .x)) %>%
    ungroup() 
}

finance_merged <- finance_merged %>%
  mutate(neg_mdr_tx = -mdr_tx)

# // rowtotal ensures that missing values in one variable are treated as a zero
finance_merged <- finance_merged %>%
  mutate(c_notif_less_mdr = rowSums(select(., c_notified, neg_mdr_tx), na.rm = TRUE)) %>%
  select(-neg_mdr_tx)

# //Calculate # of bed days (Aug 2023: using c_notif_less_mdr for ds calculation)

finance_merged <- finance_merged %>%
  mutate(
    beddays_nmdr = NA,
    beddays_mdr = NA,
    beddays_nmdr = #ifelse(#year >= 2015, 
      c_notif_less_mdr * hospd_dstb_prct / 100 * hospd_dstb_dur, #beddays_nmdr,
    beddays_mdr = #ifelse(#year >= 2015, 
      mdr_tx * hospd_mdr_prct / 100 * hospd_mdr_dur#, beddays_mdr#)
  )

# //Calculate # of bed days - South Sudan exception . SP/SN utilization blank
finance_merged <- finance_merged %>%
  mutate(
    hcfvisit_dstb = ifelse(iso3 == "SSD" & year < 2015, 23, hcfvisit_dstb),
    beddays_nmdr = ifelse(iso3 == "SSD", c_notif_less_mdr * hospd_dstb_prct / 100 * hospd_dstb_dur, beddays_nmdr),
    beddays_mdr = ifelse(iso3 == "SSD", mdr_tx * hospd_mdr_prct / 100 * hospd_mdr_dur, beddays_mdr)
  )


# IGB/PN 15 Jul 2020: INDia 2014 beddays to be backfilled from 2015
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year)) %>%
  mutate(
    hospd_dstb_prct = ifelse(iso3 == "IND" & year == 2014 & is.na(hospd_dstb_prct), lag(hospd_dstb_prct), hospd_dstb_prct),
    hospd_dstb_dur = ifelse(iso3 == "IND" & year == 2014 & is.na(hospd_dstb_dur), lag(hospd_dstb_dur), hospd_dstb_dur),
    beddays_nmdr = ifelse(iso3 == "IND" & year == 2014, c_notified * hospd_dstb_prct / 100 * hospd_dstb_dur, beddays_nmdr)
  )

# // Fill in bed days with previous years. 
vars_to_fill <- c("beddays_nmdr", "beddays_mdr")
for (var in vars_to_fill) {
  finance_merged <- finance_merged %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate_at(vars(var), ~ifelse(is.na(.x), lag(.x), .x)) %>%
    ungroup()
}

# Fill in MDR visits with previous ones
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(mdr_tx = ifelse(is.na(mdr_tx), lag(mdr_tx), mdr_tx)) %>%
  ungroup()

# Make remaining missing utilization equal to 0
vars_to_replace_zero <- c("beddays_nmdr", "beddays_mdr", "hcfvisit_dstb", "hcfvisit_mdr")
finance_merged <- finance_merged %>%
  mutate_at(vars(vars_to_replace_zero), ~ifelse(is.na(.x), 0, .x))

# /*****************************
#   GHS
# ****************************/
# // Calculate GHS
# //NMDR for 2014 onwards (plus SSD's exceptional circumstance)
finance_merged <- finance_merged %>%
  mutate(
    c_clinic_nmdr = ifelse(year >= 2013 | iso3 == "SSD", c_notif_less_mdr * hcfvisit_dstb * uc_visit_cur_usd, NA),
    c_hospital_nmdr = ifelse(year >= 2013 | iso3 == "SSD", beddays_nmdr * uc_bedday_nmdr_cur_usd, NA),
    c_clinic_mdr = ifelse(year >= 2013 | iso3 == "SSD", mdr_tx * hcfvisit_mdr * uc_visit_cur_usd, NA),
    c_hospital_mdr = ifelse(year >= 2013 | iso3 == "SSD", beddays_mdr * uc_bedday_mdr_cur_usd, NA)
  )

# /*****************************
#   GHS SUBTOTALS 
# ****************************/
#   ///GHS Non-MDR and MDR split
finance_merged <- finance_merged %>%
  mutate(
    c_ghs_nmdr = rowSums(select(., c_clinic_nmdr, c_hospital_nmdr), na.rm = TRUE),
    c_ghs_mdr = rowSums(select(., c_clinic_mdr, c_hospital_mdr), na.rm = TRUE)
  ) 

# //GHS Inpatient and Outpatient Split
finance_merged <- finance_merged %>%
  mutate(
    c_ghs_inpatient = rowSums(select(., c_hospital_nmdr, c_hospital_mdr), na.rm = TRUE),
    c_ghs_outpatient = rowSums(select(., c_clinic_nmdr, c_clinic_mdr), na.rm = TRUE)
  )

# //TOTAL GHS
finance_merged <- finance_merged %>%
  mutate(
    c_ghs = rowSums(select(., c_ghs_nmdr, c_ghs_mdr), na.rm = TRUE)
  )


# /* FIX  MDR BREAKDOWN FOR COUNTRIES WHERE GHS IS THOUGH TO BE INCLUDED IN NTP REPORTING 
# 33% of beddays were for MDR in 2013, 2014 around 10% in 2008-2012. The proportion has increased to 71% 2018 and 83.2% in 2020 . Now 93% 
# Andrew and Taghreed discussed and felt it better to split by estimated GHS costs ratio for MDR and Non-MDR as this includes visit data
# as well as hopsital level/staffing mix. It was also felt this hsould be done for those ocuntries with GHS thought to be included in NTP reported
# spending. This was only done for countries where we had some information on which category the GHS costs were being reported as (from Lela).
# It is unclear which categories for China and Turkmenistan contain GHS so not adjustment for them. */
#   
# /* Note also that latest data on service utilisation is linked to the expenditure year, not the budget year, i.e. report_year-1, and then
# carry that forward to report_year to do the budget adjustments */
  
countries <- c("RUS", "CHN", "KAZ", "TKM", "ARM", "AZE", "BLR", "KGZ", "TJK", "UKR", "GEO", "HND", "GUY", "SWZ")
for (i in countries) {
  col_name <- paste0("mdr_pct_", i)
  finance_merged <- finance_merged %>%
    mutate(
      !!sym(col_name) := c_ghs_mdr / (c_ghs_nmdr + c_ghs_mdr),
      !!sym(col_name) := ifelse(year == report_year, lag(!!sym(col_name)), !!sym(col_name))
    )
}



# // Countries where GHS is believed to be in Staff and Other: Russia, KAZ
countries <- c("ARM","KGZ","TJK", "RUS", "KAZ", "GUY", "GEO")
types <- c("rcvd", "exp")
type <- "rcvd"
i <- "RUS"

for (i in countries) {
  for (type in types) {
    finance_merged <- finance_merged %>%
      mutate(
        !!paste0(type, "_staff_temp") := ifelse(is.na(!!sym(paste0(type, "_staff"))), lag(!!sym(paste0(type, "_staff")),1)* !!sym(paste0("mdr_pct_", i)), !!sym(paste0(type, "_staff"))* !!sym(paste0("mdr_pct_", i))),
        !!paste0(type, "_oth_temp") := ifelse(is.na(!!sym(paste0(type, "_oth"))), lag(!!sym(paste0(type, "_oth")),1)* !!sym(paste0("mdr_pct_", i)), !!sym(paste0(type, "_oth"))* !!sym(paste0("mdr_pct_", i))) 
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt_temp") := ifelse(is.na(!!sym(paste0(type, "_mdrmgt"))), lag(!!sym(paste0(type, "_mdrmgt")),1), !!sym(paste0(type, "_mdrmgt")))) %>%
      mutate(
        !!paste0(type, "_mdrmgt_temp") := rowSums(select(., !!paste0(type, "_mdrmgt_temp"), !!paste0(type, "_staff_temp"), !!paste0(type, "_oth_temp")), na.rm = TRUE)
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt") := ifelse(iso3 == i, !!sym(paste0(type, "_mdrmgt_temp")), !!sym(paste0(type, "_mdrmgt"))),
        !!paste0(type, "_staff") := ifelse(iso3 == i & !is.na(!!sym(paste0(type, "_staff"))), round(!!sym(paste0(type, "_staff")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), 
                                           ifelse(iso3 == i & is.na(!!sym(paste0(type, "_staff"))), round(lag(!!sym(paste0(type, "_staff")),1) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_staff")))),
        !!paste0(type, "_oth") := ifelse(iso3 == i & !is.na(!!sym(paste0(type, "_oth"))), round(!!sym(paste0(type, "_oth")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), 
                                         ifelse(iso3 == i & is.na(!!sym(paste0(type, "_oth"))), round(lag(!!sym(paste0(type, "_oth")),1) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_oth"))))
      ) %>%
      select(-matches(".*_temp"))
  }
  
  types_budget <- c("budget", "cf", "gap")
  for (type_budget in types_budget) {
    finance_merged <- finance_merged %>%
      mutate(
        !!paste0(type_budget, "_staff_temp") := ifelse(iso3 == i, !!sym(paste0(type_budget, "_staff")) * !!sym(paste0("mdr_pct_", i)), NA_real_),
        !!paste0(type_budget, "_oth_temp") := ifelse(iso3 == i, !!sym(paste0(type_budget, "_oth")) * !!sym(paste0("mdr_pct_", i)), NA_real_)
      ) %>%
      mutate(
        !!paste0(type_budget, "_mdrmgt_temp") := rowSums(select(., !!paste0(type_budget, "_mdrmgt"), !!paste0(type_budget, "_staff_temp"), !!paste0(type_budget, "_oth_temp")), na.rm = TRUE)
      ) %>%
      mutate(
        !!paste0(type_budget, "_mdrmgt") := ifelse(iso3 == i, !!sym(paste0(type_budget, "_mdrmgt_temp")), !!sym(paste0(type_budget, "_mdrmgt"))),
        !!paste0(type_budget, "_staff") := ifelse(iso3 == i, round(!!sym(paste0(type_budget, "_staff")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type_budget, "_staff"))),
        !!paste0(type_budget, "_oth") := ifelse(iso3 == i, round(!!sym(paste0(type_budget, "_oth")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type_budget, "_oth")))
      ) %>%
      select(-matches(".*_temp"))
  }
}

# Countries where GHS is believed to be in Other only starting from 2019 spending: ARM, KGZ, TJK, UKR
# add in GEO for all years
countries <- c(  "UKR")
types <- c("rcvd", "exp")

for (i in countries) {
  for (type in types) {
    finance_merged <- finance_merged %>%
      mutate(
        !!paste0(type, "_oth_temp") := ifelse(iso3 == i & !is.na(!!sym(paste0(type, "_oth"))), !!sym(paste0(type, "_oth")) * !!sym(paste0("mdr_pct_", i)), NA_real_)
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt_temp") := ifelse(!is.na(!!sym(paste0(type, "_oth_temp"))), rowSums(select(., !!paste0(type, "_mdrmgt"), !!paste0(type, "_oth_temp")), na.rm = TRUE), !!sym(paste0(type, "_mdrmgt")))
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt") := ifelse((iso3 == i & year >= 2019) | iso3 == "GEO", !!sym(paste0(type, "_mdrmgt_temp")), !!sym(paste0(type, "_mdrmgt"))),
        !!paste0(type, "_oth") := ifelse((iso3 == i & year >= 2019) | iso3 == "GEO", round(!!sym(paste0(type, "_oth")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_oth")))
      ) %>%
      select(-ends_with("_temp"))
  }
  
  for (type in c("budget", "cf", "gap")) {
    finance_merged <- finance_merged %>%
      mutate(
        !!paste0(type, "_oth_temp") := ifelse(iso3 == i, !!sym(paste0(type, "_oth")) * !!sym(paste0("mdr_pct_", i)), NA_real_)
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt_temp") := rowSums(select(., !!paste0(type, "_mdrmgt"), !!paste0(type, "_oth_temp")), na.rm = TRUE)
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt") := ifelse((iso3 == i & year >= 2020) | iso3 == "GEO", !!sym(paste0(type, "_mdrmgt_temp")), !!sym(paste0(type, "_mdrmgt"))),
        !!paste0(type, "_oth") := ifelse((iso3 == i & year >= 2020) | iso3 == "GEO", round(!!sym(paste0(type, "_oth")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_oth")))
      ) %>%
      select(-ends_with("_temp"))
  }
}

# // AZERBIJAN & BELARUS STAFF AND OTHER CATEGORIES ALL BELIEVED TO INCLUDE GHS
countries <- c("AZE", "BLR")
types <- c("rcvd", "exp")

for (i in countries) {
  for (type in types) {
    finance_merged <- finance_merged %>%
      mutate(
        !!paste0(type, "_staff_temp") := ifelse(is.na(!!sym(paste0(type, "_staff"))), lag(!!sym(paste0(type, "_staff")),1)* !!sym(paste0("mdr_pct_", i)), !!sym(paste0(type, "_staff"))* !!sym(paste0("mdr_pct_", i))),
        !!paste0(type, "_oth_temp") := ifelse(is.na(!!sym(paste0(type, "_oth"))), lag(!!sym(paste0(type, "_oth")),1)* !!sym(paste0("mdr_pct_", i)), !!sym(paste0(type, "_oth"))* !!sym(paste0("mdr_pct_", i))) 
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt_temp") := ifelse(is.na(!!sym(paste0(type, "_mdrmgt"))), lag(!!sym(paste0(type, "_mdrmgt")),1), !!sym(paste0(type, "_mdrmgt")))) %>%
      mutate(
        !!paste0(type, "_mdrmgt_temp") := rowSums(select(., !!paste0(type, "_mdrmgt_temp"), !!paste0(type, "_staff_temp"), !!paste0(type, "_oth_temp")), na.rm = TRUE)
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt") := ifelse(iso3 == i, !!sym(paste0(type, "_mdrmgt_temp")), !!sym(paste0(type, "_mdrmgt"))),
        !!paste0(type, "_staff") := ifelse(iso3 == i & !is.na(!!sym(paste0(type, "_staff"))), round(!!sym(paste0(type, "_staff")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), 
                                           ifelse(iso3 == i & is.na(!!sym(paste0(type, "_staff"))), round(lag(!!sym(paste0(type, "_staff")),1) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_staff")))),
        !!paste0(type, "_oth") := ifelse(iso3 == i & !is.na(!!sym(paste0(type, "_oth"))), round(!!sym(paste0(type, "_oth")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), 
                                         ifelse(iso3 == i & is.na(!!sym(paste0(type, "_oth"))), round(lag(!!sym(paste0(type, "_oth")),1) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_oth"))))
      ) %>%
      select(-ends_with("_temp"))
  }
  
  for (type in c("budget", "cf", "gap")) {
    finance_merged <- finance_merged %>%
      mutate(
        !!paste0(type, "_staff_temp") := ifelse(is.na(!!sym(paste0(type, "_staff"))), lag(!!sym(paste0(type, "_staff")),1)* !!sym(paste0("mdr_pct_", i)), !!sym(paste0(type, "_staff"))* !!sym(paste0("mdr_pct_", i))),
        !!paste0(type, "_oth_temp") := ifelse(is.na(!!sym(paste0(type, "_oth"))), lag(!!sym(paste0(type, "_oth")),1)* !!sym(paste0("mdr_pct_", i)), !!sym(paste0(type, "_oth"))* !!sym(paste0("mdr_pct_", i))) 
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt_temp") := ifelse(is.na(!!sym(paste0(type, "_mdrmgt"))), lag(!!sym(paste0(type, "_mdrmgt")),1), !!sym(paste0(type, "_mdrmgt")))) %>%
      mutate(
        !!paste0(type, "_mdrmgt_temp") := rowSums(select(., !!paste0(type, "_mdrmgt_temp"), !!paste0(type, "_staff_temp"), !!paste0(type, "_oth_temp")), na.rm = TRUE)
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt") := ifelse(iso3 == i, !!sym(paste0(type, "_mdrmgt_temp")), !!sym(paste0(type, "_mdrmgt"))),
        !!paste0(type, "_staff") := ifelse(iso3 == i & !is.na(!!sym(paste0(type, "_staff"))), round(!!sym(paste0(type, "_staff")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), 
                                           ifelse(iso3 == i & is.na(!!sym(paste0(type, "_staff"))), round(lag(!!sym(paste0(type, "_staff")),1) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_staff")))),
        !!paste0(type, "_oth") := ifelse(iso3 == i & !is.na(!!sym(paste0(type, "_oth"))), round(!!sym(paste0(type, "_oth")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), 
                                          ifelse(iso3 == i & is.na(!!sym(paste0(type, "_oth"))), round(lag(!!sym(paste0(type, "_oth")),1) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_oth"))))
      ) %>%
      select(-ends_with("_temp"))
  }
}


# Countries where GHS is believed to be in staff only starting in some years: HND
countries <- c("HND")
types <- c("rcvd", "exp")

for (i in countries) {
  for (type in types) {
    finance_merged <- finance_merged %>%
      mutate(
        !!paste0(type, "_staff_temp") := ifelse(iso3 == i & !is.na(!!sym(paste0(type, "_staff"))), !!sym(paste0(type, "_staff")) * !!sym(paste0("mdr_pct_", i)), NA_real_)
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt_temp") := ifelse(!is.na(!!sym(paste0(type, "_staff_temp"))), rowSums(select(., !!paste0(type, "_mdrmgt"), !!paste0(type, "_staff_temp")), na.rm = TRUE), !!sym(paste0(type, "_mdrmgt")))
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt") := ifelse((iso3 == i & year %in% c(2017:2019,2023)), !!sym(paste0(type, "_mdrmgt_temp")), !!sym(paste0(type, "_mdrmgt"))),
        !!paste0(type, "_staff") := ifelse((iso3 == i & year %in% c(2017:2019,2023)), round(!!sym(paste0(type, "_staff")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_staff")))
      ) %>%
      select(-ends_with("_temp"))
  }
  
  for (type in c("budget", "cf", "gap")) {
    for (type in types) {
      finance_merged <- finance_merged %>%
        mutate(
          !!paste0(type, "_staff_temp") := ifelse(iso3 == i & !is.na(!!sym(paste0(type, "_staff"))), !!sym(paste0(type, "_staff")) * !!sym(paste0("mdr_pct_", i)), NA_real_)
        ) %>%
        mutate(
          !!paste0(type, "_mdrmgt_temp") := ifelse(!is.na(!!sym(paste0(type, "_staff_temp"))), rowSums(select(., !!paste0(type, "_mdrmgt"), !!paste0(type, "_staff_temp")), na.rm = TRUE), !!sym(paste0(type, "_mdrmgt")))
        ) %>%
        mutate(
          !!paste0(type, "_mdrmgt") := ifelse((iso3 == i & year %in% c(2015,2017:2019,2023)), !!sym(paste0(type, "_mdrmgt_temp")), !!sym(paste0(type, "_mdrmgt"))),
          !!paste0(type, "_staff") := ifelse((iso3 == i & year %in% c(2015, 2017:2019,2023)), round(!!sym(paste0(type, "_staff")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_staff")))
        ) %>%
        select(-ends_with("_temp"))
    }
  }
}

# Countries where GHS is believed to be in staff only starting in some years: HND
countries <- c("SWZ")
types <- c("budget", "cf", "gap")

for (i in countries) {
  for (type in types) {
    finance_merged <- finance_merged %>%
      mutate(
        !!paste0(type, "_staff_temp") := ifelse(iso3 == i & !is.na(!!sym(paste0(type, "_staff"))), !!sym(paste0(type, "_staff")) * !!sym(paste0("mdr_pct_", i)), NA_real_)
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt_temp") := ifelse(!is.na(!!sym(paste0(type, "_staff_temp"))), rowSums(select(., !!paste0(type, "_mdrmgt"), !!paste0(type, "_staff_temp")), na.rm = TRUE), !!sym(paste0(type, "_mdrmgt")))
      ) %>%
      mutate(
        !!paste0(type, "_mdrmgt") := ifelse((iso3 == i & year >=2024), !!sym(paste0(type, "_mdrmgt_temp")), !!sym(paste0(type, "_mdrmgt"))),
        !!paste0(type, "_staff") := ifelse((iso3 == i & year >=2024), round(!!sym(paste0(type, "_staff")) * (1 - !!sym(paste0("mdr_pct_", i))), 1), !!sym(paste0(type, "_staff")))
      ) %>%
      select(-ends_with("_temp"))
  }
  
}


# //SET GHS to 0 for countries where it is believed to be already reported
countries <- c("ARM", "RUS", "CHN", "KAZ", "TKM", "AZE",  "GUY")
vars <- grep("^c_clinic|^c_hospital|^c_ghs", names(finance_merged), value = TRUE)

finance_merged <- finance_merged %>%
  mutate(
    across(all_of(vars), ~ ifelse(iso3 %in% countries, 0, .x)),
    across(all_of(vars), ~ as.numeric(.))
  )

countries <- c("KGZ", "TJK", "UKR", "GEO")
finance_merged <- finance_merged %>%
  mutate(
    across(all_of(vars), ~ ifelse(iso3 %in% countries & year >=2019, 0, .x)),
    across(all_of(vars), ~ as.numeric(.))
  )

countries <- c("BLR")
finance_merged <- finance_merged %>%
  mutate(
    across(all_of(vars), ~ ifelse(iso3 %in% countries & year >=2016, 0, .x)),
    across(all_of(vars), ~ as.numeric(.))
  )


countries <- c("HND")
finance_merged <- finance_merged %>%
  mutate(
    across(all_of(vars), ~ ifelse(iso3 %in% countries & year %in% c(2015, 2017:2019,2023), 0, .x)),
    across(all_of(vars), ~ as.numeric(.))
  )

countries <- c("SWZ")
finance_merged <- finance_merged %>%
  mutate(
    across(all_of(vars), ~ ifelse(iso3 %in% countries & year >=2024, 0, .x)),
    across(all_of(vars), ~ as.numeric(.))
  )

# /********************************
#   CALCULATE SHARES
# *********************************/
#   // INT, EXT
# CF
finance_merged <- finance_merged %>%
  mutate(
    # cf_int = if_else(!is.na(cf_tot) & cf_tot != 0 & year <= 2014, rowSums(select(., cf_tot_gov, cf_tot_loan)), NA_real_),
    cf_int = NZ(cf_tot_domestic),
    cf_int_sh = cf_int / cf_tot,
    cf_int_sh = if_else(cf_int_sh > 1 & !is.na(cf_int_sh), 1, cf_int_sh),
    cf_int_sh = if_else(cf_int_sh < 0, 0, cf_int_sh)
  )

# //if cf_int_sh = missing set it to previous year
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(cf_int_sh = if_else(is.na(cf_int_sh), lag(cf_int_sh), cf_int_sh)) %>%
  ungroup()

# ASSUME ALL HIGH-INCOME ARE 100% INTERNALLY FINANCED (IF SHARE IS MISSING)
finance_merged <- finance_merged %>%
  mutate(
    cf_int_sh = if_else(is.na(cf_int_sh) & g_income == "HIC", 1, cf_int_sh),
    cf_int_sh = if_else(is.na(cf_int_sh) & iso3 == "RUS", 1, cf_int_sh),
    cf_int_sh = if_else(is.na(cf_int_sh) & iso3 == "ALB", 1, cf_int_sh),
    cf_int = if_else(is.na(cf_int) & is.na(cf_int_sh), cf_int_sh * cf_tot, cf_int),
    cf_ext_sh = 1 - cf_int_sh
  )

# Replace missing values in cf_ext_sh with the previous year's value
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(cf_ext_sh = if_else(is.na(cf_ext_sh), lag(cf_ext_sh), cf_ext_sh),
         cf_ext = cf_ext_sh * cf_tot)

# RCVD
finance_merged <- finance_merged %>%
  mutate(
    rcvd_int = ifelse(rcvd_imputation == "Last year's received", lag(rcvd_tot_domestic),
                      ifelse(rcvd_imputation == "Committed funding", cf_tot_domestic,
                             ifelse(rcvd_imputation == "Next year's received", lead(rcvd_tot_domestic), rcvd_tot_domestic))),
    rcvd_int_sh = rcvd_int / rcvd_tot,
    rcvd_int_sh = if_else(rcvd_int_sh > 1, 1, rcvd_int_sh),
    rcvd_int_sh = if_else(rcvd_int_sh < 0, 0, rcvd_int_sh)
  )

# Following the same logic as received totals, we first use cf data if available, then previous year (max of 4), then following year( max of 4). 
# However, we do ASSUME ALL HIGH-INCOME as well as several formerly UMCs/HICs ARE 100% INTERNALLY FINANCED (IF SHARE IS MISSING).
finance_merged <- finance_merged %>%
  mutate(
    rcvd_int_sh = if_else(is.na(rcvd_int_sh) & g_income == "HIC", 1, rcvd_int_sh),
    rcvd_int_sh = if_else(is.na(rcvd_int_sh) & iso3 %in% c("RUS", "ALB", "COM", "CUB", "DMA", "GRD"), 1, rcvd_int_sh),
    rcvd_int_sh = if_else(is.na(rcvd_int_sh) & iso3 %in% c("LCA", "TON", "TUV", "WSM"), 1, rcvd_int_sh),
    rcvd_int_sh = if_else(is.na(rcvd_int_sh) & is.na(rcvd_int) & (!is.na(rcvd_tot_gf) | !is.na(rcvd_tot_usaid) | !is.na(rcvd_tot_grnt)) & rcvd_tot_sources != 0 & year >= 2014, 0, rcvd_int_sh)
  )


# If still missing (e.g. ZAF use committed funds)
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(rcvd_int_sh = if_else((is.na(rcvd_int_sh)) & !is.na(cf_int_sh) , cf_int_sh, rcvd_int_sh)) %>%
  ungroup()


# If rcvd_int_sh is still missing, set it to previous year (max 4 years)
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    rcvd_int_sh = if_else((is.na(rcvd_int_sh)), lag(rcvd_int_sh,1), rcvd_int_sh),
    rcvd_int_sh = if_else((is.na(rcvd_int_sh)), lag(rcvd_int_sh,2), rcvd_int_sh),
    rcvd_int_sh = if_else((is.na(rcvd_int_sh)), lag(rcvd_int_sh,3), rcvd_int_sh),
    rcvd_int_sh = if_else((is.na(rcvd_int_sh)), lag(rcvd_int_sh,4), rcvd_int_sh)
  ) %>%
  ungroup()

# If still missing, use following year up to max 4 years
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    rcvd_int_sh = if_else((is.na(rcvd_int_sh)), lead(rcvd_int_sh,1), rcvd_int_sh),
    rcvd_int_sh = if_else((is.na(rcvd_int_sh)), lead(rcvd_int_sh,2), rcvd_int_sh),
    rcvd_int_sh = if_else((is.na(rcvd_int_sh)), lead(rcvd_int_sh,3), rcvd_int_sh),
    rcvd_int_sh = if_else((is.na(rcvd_int_sh)), lead(rcvd_int_sh,4), rcvd_int_sh)
  ) %>%
  ungroup()

# additional assumption for expenditure data
finance_merged <- finance_merged %>%
  mutate(
    rcvd_int_sh = if_else((is.na(rcvd_int_sh)&rcvd_imputation == "Expenditure data"), 1, rcvd_int_sh)
  )

finance_merged <- finance_merged %>%
  mutate(
    rcvd_int_sh = if_else((rcvd_tot!=0&!is.na(rcvd_tot)&is.na(rcvd_int_sh)), 0, rcvd_int_sh)
  )

# manual adjustment for int share
source(here::here('finance/3c_adjust_share.R'))


# Replace RCVD_INT using share if missing
finance_merged <- finance_merged %>%
  mutate(rcvd_int = if_else((rcvd_int==0|is.na(rcvd_int)), rcvd_int_sh * rcvd_tot, rcvd_int))

# Generate RCVD_EXT and set to 1 minus rcvd_int_sh
finance_merged <- finance_merged %>%
  mutate(
    rcvd_ext_sh = 1 - rcvd_int_sh,
    rcvd_ext_sh = if_else(is.na(rcvd_ext_sh), lag(rcvd_ext_sh), rcvd_ext_sh),
    rcvd_ext = rcvd_ext_sh * rcvd_tot
  )

# Calculate cf_ext_gf and cf_ext_gf_sh
finance_merged <- finance_merged %>%
  mutate(
    cf_ext_gf = rowSums(select(., starts_with("cf_tot_gf")), na.rm = F),
    cf_ext_gf_sh = cf_ext_gf / cf_ext
  )

# Replace invalid values in cf_ext_gf_sh with NA
finance_merged$cf_ext_gf_sh[finance_merged$cf_ext_gf_sh > 1e10 | finance_merged$cf_ext_gf_sh < 0] <- NA

# Sort the data and replace missing values of cf_ext_gf_sh
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    cf_ext_gf_sh = if_else(is.na(cf_ext_gf_sh), lag(cf_ext_gf_sh), cf_ext_gf_sh),
    cf_ext_gf_sh = if_else(is.na(cf_ext_gf_sh), lead(cf_ext_gf_sh), cf_ext_gf_sh)
  )

# Calculate cf_ext_ngf_sh and cf_ext_ngf
finance_merged <- finance_merged %>%
  mutate(
    cf_ext_ngf_sh = 1 - cf_ext_gf_sh,
    cf_ext_ngf = cf_ext_ngf_sh * cf_ext
  )

# Calculate rcvd_ext_gf and rcvd_ext_gf_sh
finance_merged <- finance_merged %>%
  mutate(
    rcvd_ext_gf = ifelse(!is.na(rcvd_tot) & rcvd_tot != 0, rcvd_tot_gf, NA),
    rcvd_ext_gf_sh = rcvd_ext_gf / rcvd_ext
  )

# Replace invalid values in rcvd_ext_gf_sh with NA
finance_merged$rcvd_ext_gf_sh[finance_merged$rcvd_ext_gf_sh > 1.e10 | finance_merged$rcvd_ext_gf_sh < 0] <- NA

# If still missing, use cf_ext_gf_sh value
finance_merged <- finance_merged %>%
  group_by(iso3, year) %>%
  mutate(rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh) & !is.na(cf_ext_gf_sh) & flag_rcvd == 3, cf_ext_gf_sh, rcvd_ext_gf_sh),
         rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh) & flag_rcvd == 4, lag(rcvd_ext_gf_sh,1), rcvd_ext_gf_sh),
         rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh) & flag_rcvd == 5, lead(rcvd_ext_gf_sh,1), rcvd_ext_gf_sh))

# Sort the data and replace missing values of rcvd_ext_gf_sh
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh), lag(rcvd_ext_gf_sh,1), rcvd_ext_gf_sh),
    rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh), lead(rcvd_ext_gf_sh,1), rcvd_ext_gf_sh),
    rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh), lag(rcvd_ext_gf_sh,2), rcvd_ext_gf_sh),
    rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh), lead(rcvd_ext_gf_sh,2), rcvd_ext_gf_sh),
    rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh), lag(rcvd_ext_gf_sh,3), rcvd_ext_gf_sh),
    rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh), lead(rcvd_ext_gf_sh,3), rcvd_ext_gf_sh),
    rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh), lag(rcvd_ext_gf_sh,4), rcvd_ext_gf_sh),
    rcvd_ext_gf_sh = if_else(is.na(rcvd_ext_gf_sh), lead(rcvd_ext_gf_sh,4), rcvd_ext_gf_sh),
  ) %>%
  ungroup()


# Calculate rcvd_ext_gf if missing
finance_merged <- finance_merged %>%
  mutate(rcvd_ext_gf = if_else(is.na(rcvd_ext_gf), rcvd_ext_gf_sh * rcvd_ext, rcvd_ext_gf))

# Calculate rcvd_ext_ngf_sh and rcvd_ext_ngf
finance_merged <- finance_merged %>%
  mutate(
    rcvd_ext_ngf_sh = 1 - rcvd_ext_gf_sh,
    rcvd_ext_ngf = rcvd_ext_ngf_sh * rcvd_ext
  )


# List of columns for each type
exp_columns <- c("exp_fld", "exp_staff", "exp_prog", "exp_lab", "exp_tbhiv", "exp_sld", "exp_mdrmgt", "exp_patsup", "exp_orsrvy", "exp_tpt", "exp_oth")
budget_columns <- c("budget_fld", "budget_staff", "budget_prog", "budget_lab", "budget_tbhiv", "budget_sld", "budget_mdrmgt", "budget_patsup", "budget_orsrvy", "budget_tpt", "budget_oth")

# Applying row totals for each type
finance_merged <- transform(finance_merged,
                            exp_all_lines = rowSums(finance_merged[, exp_columns], na.rm = TRUE),
                            budget_all_lines = rowSums(finance_merged[, budget_columns], na.rm = TRUE))

# /*****************************************************************
#   * To more fully understand the mapping between line items and our
# * reported intervention areas created below see the PPTX file on:
#   * J:\t-TME\UnitData\Financing 2015.
# *****************************************************************/
# // making correction for those countries in which the sum of the inner matrix does not come to the sum of the 1st column and/or last row
# // we are effectively assuming that if the inner matrix total differs from the outer matrix total, we use the outer matrix total but determine shares using the inner matrix
for (type in c("cf", "rcvd")) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0(type, "_mdr") := if_else(!is.na(!!sym(paste0(type, "_sld")))|!is.na(!!sym(paste0(type, "_mdrmgt"))), 
                                        rowSums(select(., all_of(c(paste0(type, "_sld"), paste0(type, "_mdrmgt")))), na.rm = TRUE), NA),
      # //do not allow piece to be greater than parent.
      !!paste0(type, "_mdr") := if_else(!is.na(!!sym(paste0(type, "_mdr"))) & !is.na(!!sym(paste0(type, "_tot"))) & !!sym(paste0(type, "_mdr")) > !!sym(paste0(type, "_tot")),
                                        !!sym(paste0(type, "_tot")),!!sym(paste0(type, "_mdr"))),
      !!paste0(type, "_mdr_sh") := !!sym(paste0(type, "_mdr")) / !!sym(paste0(type, "_tot")),
      !!paste0(type, "_mdr_sh") := if_else(!!sym(paste0(type, "_mdr_sh")) > 1 & !is.na(!!sym(paste0(type, "_mdr_sh"))), 1, !!sym(paste0(type, "_mdr_sh"))),
      !!paste0(type, "_mdr_sh") := if_else(!!sym(paste0(type, "_mdr_sh")) < 0 & !is.na(!!sym(paste0(type, "_mdr_sh"))), 0, !!sym(paste0(type, "_mdr_sh")))
    ) %>%
    arrange(iso3, year) %>%
    mutate(
      !!paste0(type, "_mdr_sh") := if_else(is.na(!!sym(paste0(type, "_mdr_sh"))), lag(!!sym(paste0(type, "_mdr_sh"))), !!sym(paste0(type, "_mdr_sh"))),
      !!paste0(type, "_mdr_sh") := if_else(is.na(!!sym(paste0(type, "_mdr_sh"))), lag(!!sym(paste0(type, "_mdr_sh"))), !!sym(paste0(type, "_mdr_sh"))),
      !!paste0(type, "_mdr_sh") := if_else(is.na(!!sym(paste0(type, "_mdr_sh"))), lag(!!sym(paste0(type, "_mdr_sh"))), !!sym(paste0(type, "_mdr_sh"))),
      !!paste0(type, "_mdr_sh") := if_else(is.na(!!sym(paste0(type, "_mdr_sh"))), lag(!!sym(paste0(type, "_mdr_sh"))), !!sym(paste0(type, "_mdr_sh"))),
      !!paste0(type, "_mdr_sh") := if_else(is.na(!!sym(paste0(type, "_mdr_sh"))), lag(!!sym(paste0(type, "_mdr_sh"))), !!sym(paste0(type, "_mdr_sh"))),
      # // If still missing (e.g. ZAF use committed funds)
      !!paste0(type, "_mdr_sh") := if_else(is.na(!!sym(paste0(type, "_mdr_sh"))) & !is.na(!!sym(paste0("cf", "_mdr_sh"))), 
                                           !!sym(paste0("cf", "_mdr_sh")), !!sym(paste0(type, "_mdr_sh"))),
      !!paste0(type, "_mdr") := if_else(is.na(!!sym(paste0(type, "_mdr")))|!!sym(paste0(type, "_mdr"))==0, 
                                        !!sym(paste0(type, "_mdr_sh")) * !!sym(paste0(type, "_tot")), 
                                        !!sym(paste0(type, "_mdr"))),
      !!paste0(type, "_nmdr_sh") := 1 - !!sym(paste0(type, "_mdr_sh")),
      # // If a country has no MDR management and no SLD then make Non-MDR 100% share of funds.
      !!paste0(type, "_nmdr_sh") := if_else((is.na(!!sym(paste0(type, "_mdrmgt"))) & is.na(!!sym(paste0(type, "_sld")))) & is.na(!!sym(paste0(type, "_nmdr_sh"))), 
                                            1, !!sym(paste0(type, "_nmdr_sh"))),
      !!paste0(type, "_nmdr") := !!sym(paste0(type, "_nmdr_sh")) * !!sym(paste0(type, "_tot"))
    )
}

# Calculate the total MDR for each type
for (type in c("exp", "budget")) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0(type, "_mdr") := ifelse(
        !is.na(!!sym(paste0(type, "_tot"))) & !!sym(paste0(type, "_tot")) != 0 & 
          !is.na(!!sym(paste0(type, "_all_lines"))) & !!sym(paste0(type, "_all_lines")) != 0,
        rowSums(select(., all_of(c(paste0(type, "_sld"), paste0(type, "_mdrmgt")))), na.rm = TRUE), NA
      )
    ) %>%
    mutate(
      !!sym(paste0(type, "_mdr")) := ifelse(
        !is.na(!!sym(paste0(type, "_mdr"))) & 
          !is.na(!!sym(paste0(type, "_tot"))) & 
          !!sym(paste0(type, "_mdr")) > !!sym(paste0(type, "_tot")),
        !!sym(paste0(type, "_tot")),
        !!sym(paste0(type, "_mdr"))
      ),
      !!sym(paste0(type, "_mdr_sh")) := !!sym(paste0(type, "_mdr")) / !!sym(paste0(type, "_tot")),
      !!sym(paste0(type, "_mdr_sh")) := ifelse(!!sym(paste0(type, "_mdr_sh")) > 1 & !is.na(!!sym(paste0(type, "_mdr_sh"))), 1, !!sym(paste0(type, "_mdr_sh"))),
      !!sym(paste0(type, "_mdr_sh")) := ifelse(!!sym(paste0(type, "_mdr_sh")) < 0 & !is.na(!!sym(paste0(type, "_mdr_sh"))), 0, !!sym(paste0(type, "_mdr_sh")))
    ) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(
      !!sym(paste0(type, "_mdr_sh")) := ifelse(is.na(!!sym(paste0(type, "_mdr_sh"))), lag(!!sym(paste0(type, "_mdr_sh")), default = NA), !!sym(paste0(type, "_mdr_sh"))),
      !!sym(paste0(type, "_mdr_sh")) := ifelse(is.na(!!sym(paste0(type, "_mdr_sh"))), lead(!!sym(paste0(type, "_mdr_sh")), default = NA), !!sym(paste0(type, "_mdr_sh")))
    ) %>%
  ungroup() %>%
    mutate(
      !!sym(paste0(type, "_mdr")) := ifelse(is.na(!!sym(paste0(type, "_mdr"))), !!sym(paste0(type, "_mdr_sh")) * !!sym(paste0(type, "_tot")), !!sym(paste0(type, "_mdr"))),
      !!sym(paste0(type, "_nmdr_sh")) := 1 - !!sym(paste0(type, "_mdr_sh")),
      !!sym(paste0(type, "_nmdr_sh")) := ifelse(is.na(!!sym(paste0(type, "_mdrmgt"))) & is.na(!!sym(paste0(type, "_sld"))) & is.na(!!sym(paste0(type, "_nmdr_sh"))), 1, !!sym(paste0(type, "_nmdr_sh"))),
      !!sym(paste0(type, "_nmdr")) := !!sym(paste0(type, "_nmdr_sh")) * !!sym(paste0(type, "_tot"))
    )
}

# // MDR_SLD, MDR_NSLD
for (type in c("exp", "budget")) {
  # Calculate the total of type_sld variables
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0(type, "_mdr_sld") := ifelse(!!sym(paste0(type, "_tot")) != 0 & !is.na(!!sym(paste0(type, "_tot"))) & 
                                             !!sym(paste0(type, "_all_lines")) != 0 & 
                                             !is.na(!!sym(paste0(type, "_all_lines"))),
                                           rowSums(select(., matches(paste0("^", type, "_sld"))), na.rm = TRUE),
                                           NA)
      ) %>%
    # Ensure that piece is not greater than parent
    mutate(
      !!paste0(type, "_mdr_sld") := ifelse((!is.na(!!sym(paste0(type, "_mdr_sld"))) & !is.na(!!sym(paste0(type, "_mdr_sld")))) & !!sym(paste0(type, "_mdr_sld")) > !!sym(paste0(type, "_mdr")),
                                           !!sym(paste0(type, "_mdr_sld")), NA),
      !!paste0(type, "_mdr_sld_sh") := !!sym(paste0(type, "_mdr_sld")) / !!sym(paste0(type, "_mdr")),
      !!paste0(type, "_mdr_sld_sh") := ifelse(!!sym(paste0(type, "_mdr_sld_sh")) > 1 &!is.na(!!sym(paste0(type, "_mdr_sld_sh"))), 1,
                                              ifelse(!!sym(paste0(type, "_mdr_sld_sh")) < 0 &
                                                       !is.na(!!sym(paste0(type, "_mdr_sld_sh"))), 0,
                                                     !!sym(paste0(type, "_mdr_sld_sh")))
      )
    ) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(
      !!paste0(type, "_mdr_sld_sh") := ifelse(is.na(!!sym(paste0(type, "_mdr_sld_sh"))),
                                              lag(!!sym(paste0(type, "_mdr_sld_sh"))),
                                              !!sym(paste0(type, "_mdr_sld_sh"))),
      !!paste0(type, "_mdr_sld_sh") := ifelse(is.na(!!sym(paste0(type, "_mdr_sld_sh"))),
                                              lead(!!sym(paste0(type, "_mdr_sld_sh"))),
                                              !!sym(paste0(type, "_mdr_sld_sh"))),
      !!paste0(type, "_mdr_sld") := ifelse(is.na(!!sym(paste0(type, "_mdr_sld"))),
                                           !!sym(paste0(type, "_mdr_sld_sh")) * !!sym(paste0(type, "_mdr")),
                                           !!sym(paste0(type, "_mdr_sld"))),
      !!paste0(type, "_mdr_nsld_sh") := 1 - !!sym(paste0(type, "_mdr_sld_sh")),
      !!paste0(type, "_mdr_nsld") := !!sym(paste0(type, "_mdr_nsld_sh")) * !!sym(paste0(type, "_mdr"))
    ) %>%
    ungroup()
}
type <- "cf"
for (type in c("cf", "rcvd")) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0(type, "_mdr_sld") := ifelse(!is.na(!!sym(paste0(type, "_sld"))), rowSums(select(., matches(paste0("^", type, "_sld"))), na.rm = TRUE), NA),
      !!paste0(type, "_mdr_sld") := ifelse(
        !is.na(!!sym(paste0(type, "_mdr_sld"))) & 
          !is.na(!!sym(paste0(type, "_mdr"))) & 
          !!sym(paste0(type, "_mdr_sld")) > !!sym(paste0(type, "_mdr")),
        !!sym(paste0(type, "_mdr")),
        !!sym(paste0(type, "_mdr_sld"))),
      !!paste0(type, "_mdr_sld_sh") := !!sym(paste0(type, "_mdr_sld")) / !!sym(paste0(type, "_mdr")),
      !!paste0(type, "_mdr_sld_sh") := ifelse(
        !!sym(paste0(type, "_mdr_sld_sh")) > 1 & !is.na(!!sym(paste0(type, "_mdr_sld_sh"))),
        1,
        ifelse(
          !!sym(paste0(type, "_mdr_sld_sh")) < 0 & !is.na(!!sym(paste0(type, "_mdr_sld_sh"))),
          0,
          !!sym(paste0(type, "_mdr_sld_sh"))
        )
      )) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_mdr_sld_sh") := ifelse(is.na(!!sym(paste0(type, "_mdr_sld_sh"))),lag(!!sym(paste0(type, "_mdr_sld_sh")),1),!!sym(paste0(type, "_mdr_sld_sh")))) %>%
    mutate(!!paste0(type, "_mdr_sld_sh") := ifelse(is.na(!!sym(paste0(type, "_mdr_sld_sh"))),lag(!!sym(paste0(type, "_mdr_sld_sh")),2),!!sym(paste0(type, "_mdr_sld_sh")))) %>%
    mutate(!!paste0(type, "_mdr_sld_sh") := ifelse(is.na(!!sym(paste0(type, "_mdr_sld_sh"))),lag(!!sym(paste0(type, "_mdr_sld_sh")),3),!!sym(paste0(type, "_mdr_sld_sh")))) %>%
    mutate(!!paste0(type, "_mdr_sld_sh") := ifelse(is.na(!!sym(paste0(type, "_mdr_sld_sh"))),lag(!!sym(paste0(type, "_mdr_sld_sh")),4),!!sym(paste0(type, "_mdr_sld_sh")))) %>%
    mutate(!!paste0(type, "_mdr_sld_sh") := ifelse(is.na(!!sym(paste0(type, "_mdr_sld_sh"))),lag(!!sym(paste0(type, "_mdr_sld_sh")),5),!!sym(paste0(type, "_mdr_sld_sh")))) %>%
    ungroup() %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_mdr_sld_sh") := ifelse(is.na(!!sym(paste0(type, "_mdr_sld_sh"))), lead(!!sym(paste0(type, "_mdr_sld_sh"))),!!sym(paste0(type, "_mdr_sld_sh")))) %>% 
    ungroup() %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(
      !!paste0(type, "_mdr_sld_sh") := if_else(
        !is.na(!!sym(paste0(type, "_mdr_sld_sh"))) & 
          !is.na(!!sym(paste0("cf_mdr_sld_sh"))) & 
          is.na(!!sym(paste0(type, "_mdr_sld_sh"))),
        !!sym(paste0("cf_mdr_sld_sh")),
        !!sym(paste0(type, "_mdr_sld_sh"))
      ),
      !!paste0(type, "_mdr_sld") := !!sym(paste0(type, "_mdr_sld_sh")) * !!sym(paste0(type, "_mdr")),
      !!paste0(type, "_mdr_nsld_sh") := 1 - !!sym(paste0(type, "_mdr_sld_sh")),
      !!paste0(type, "_mdr_nsld") := !!sym(paste0(type, "_mdr_nsld_sh")) * !!sym(paste0(type, "_mdr"))
    ) %>%
    ungroup()
}


# // NMDR_DOT, NMDR_NDOT
# // By DOT we mean FLD, staff, programme management, labs and (pre-2005) variables for buildings and "other". 
type  <- "budget"

for (type in c("exp", "budget")) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0(type, "_nmdr_dot") := ifelse((!is.na(!!sym(paste0(type, "_tot"))) & !!sym(paste0(type, "_tot")) != 0 & 
                                                      !is.na(!!sym(paste0(type, "_all_lines"))) & !!sym(paste0(type, "_all_lines")) != 0), 
                                                   rowSums(select(., !!paste0(type, "_fld"), !!paste0(type, "_staff"), !!paste0(type, "_prog"), !!paste0(type, "_lab")), na.rm = TRUE),
                                                   NA),
      !!paste0(type, "_nmdr_dot") := ifelse(!is.na(!!sym(paste0(type, "_nmdr_dot"))) & 
                                              !is.na(!!sym(paste0(type, "_nmdr"))) & 
                                              !!sym(paste0(type, "_nmdr_dot")) > !!sym(paste0(type, "_nmdr")), 
                                            !!sym(paste0(type, "_nmdr")), 
                                            !!sym(paste0(type, "_nmdr_dot"))),
      !!paste0(type, "_nmdr_dot_sh") := ifelse(!is.na(!!sym(paste0(type, "_nmdr_dot"))) & 
                                                 !is.na(!!sym(paste0(type, "_nmdr"))), 
                                               !!sym(paste0(type, "_nmdr_dot")) / !!sym(paste0(type, "_nmdr")), 
                                               NA_real_),
      !!paste0(type, "_nmdr_dot_sh") := ifelse(!!sym(paste0(type, "_nmdr_dot_sh")) > 1 & 
                                                 !is.na(!!sym(paste0(type, "_nmdr_dot_sh"))),  1, 
                                               !!sym(paste0(type, "_nmdr_dot_sh"))),
      !!paste0(type, "_nmdr_dot_sh") := ifelse(!!sym(paste0(type, "_nmdr_dot_sh")) < 0 &!is.na(!!sym(paste0(type, "_nmdr_dot_sh"))), 
                                               0, !!sym(paste0(type, "_nmdr_dot_sh")))) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_nmdr_dot_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_sh"))),
                                                    lag(!!sym(paste0(type, "_nmdr_dot_sh"))),
                                                    !!sym(paste0(type, "_nmdr_dot_sh"))),
           !!paste0(type, "_nmdr_dot_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_sh"))),
                                                    lead(!!sym(paste0(type, "_nmdr_dot_sh"))),
                                                    !!sym(paste0(type, "_nmdr_dot_sh")))) %>%
    ungroup() %>%
    mutate(!!paste0(type, "_nmdr_dot") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot"))),
                                                 !!sym(paste0(type, "_nmdr_dot_sh")) * !!sym(paste0(type, "_nmdr")),
                                                 !!sym(paste0(type, "_nmdr_dot"))),
           !!paste0(type, "_nmdr_ndot_sh") := 1 - !!sym(paste0(type, "_nmdr_dot_sh")),
           !!paste0(type, "_nmdr_ndot") := !!sym(paste0(type, "_nmdr_ndot_sh")) * !!sym(paste0(type, "_nmdr"))
    )
}

for (type in c("cf", "rcvd")) {
  finance_merged <- finance_merged %>%
    mutate(!!paste0(type, "_nmdr_dot") := rowSums(select(., !!paste0(type, "_fld"),
                                                         !!paste0(type, "_staff"),
                                                         !!paste0(type, "_prog"),
                                                         !!paste0(type, "_lab")), 
                                                  na.rm = TRUE),
           !!paste0(type, "_nmdr_dot") := ifelse(!is.na(!!sym(paste0(type, "_nmdr_dot"))) &
                                                   !is.na(!!sym(paste0(type, "_nmdr"))) &
                                                   !!sym(paste0(type, "_nmdr_dot")) > !!sym(paste0(type, "_nmdr")),
                                                 !!sym(paste0(type, "_nmdr")),
                                                 !!sym(paste0(type, "_nmdr_dot"))),
           !!paste0(type, "_nmdr_dot_sh") := !!sym(paste0(type, "_nmdr_dot")) / !!sym(paste0(type, "_nmdr")),
           !!paste0(type, "_nmdr_dot_sh") := ifelse(!!sym(paste0(type, "_nmdr_dot_sh")) > 1 & 
                                                      !is.na(!!sym(paste0(type, "_nmdr_dot_sh"))),
                                                    1,
                                                    ifelse(!!sym(paste0(type, "_nmdr_dot_sh")) < 0 &
                                                             !is.na(!!sym(paste0(type, "_nmdr_dot_sh"))),
                                                           0,
                                                           !!sym(paste0(type, "_nmdr_dot_sh"))))) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_nmdr_dot_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_sh"))) &
                                                      !is.na(cf_nmdr_dot_sh) &
                                                      cf_nmdr_dot_sh != ".",
                                                    cf_nmdr_dot_sh,
                                                    ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_sh"))),
                                                           lag(!!sym(paste0(type, "_nmdr_dot_sh"))),
                                                           !!sym(paste0(type, "_nmdr_dot_sh")))),
           !!paste0(type, "_nmdr_dot") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot"))),
                                                 !!sym(paste0(type, "_nmdr_dot_sh")) * !!sym(paste0(type, "_nmdr")),
                                                 !!sym(paste0(type, "_nmdr_dot"))),
           !!paste0(type, "_nmdr_ndot_sh") := 1 - !!sym(paste0(type, "_nmdr_dot_sh")),
           !!paste0(type, "_nmdr_ndot") := !!sym(paste0(type, "_nmdr_ndot_sh")) * !!sym(paste0(type, "_nmdr"))) %>%
    ungroup()
}

for (type in c("exp", "budget")) {
  finance_merged <- finance_merged %>%
    mutate(!!paste0(type, "_nmdr_dot_fld") := rowSums(select(., starts_with(paste0(type, "_fld"))),
                                                      na.rm = TRUE),
           !!paste0(type, "_nmdr_dot_fld") := ifelse(!is.na(!!sym(paste0(type, "_nmdr_dot_fld"))) &
                                                       !is.na(!!sym(paste0(type, "_nmdr_dot"))) &
                                                       !!sym(paste0(type, "_nmdr_dot_fld")) > !!sym(paste0(type, "_nmdr_dot")),
                                                     !!sym(paste0(type, "_nmdr_dot")),
                                                     !!sym(paste0(type, "_nmdr_dot_fld"))),
           !!paste0(type, "_nmdr_dot_fld_sh") := !!sym(paste0(type, "_nmdr_dot_fld")) / !!sym(paste0(type, "_nmdr_dot")),
           !!paste0(type, "_nmdr_dot_fld_sh") := ifelse(!!sym(paste0(type, "_nmdr_dot_fld_sh")) > 1 &
                                                          !is.na(!!sym(paste0(type, "_nmdr_dot_fld_sh"))),
                                                        1,
                                                        ifelse(!!sym(paste0(type, "_nmdr_dot_fld_sh")) < 0 &
                                                                 !is.na(!!sym(paste0(type, "_nmdr_dot_fld_sh"))),
                                                               0,
                                                               !!sym(paste0(type, "_nmdr_dot_fld_sh"))))) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_nmdr_dot_fld_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_fld_sh"))) &
                                                          !is.na(cf_nmdr_dot_sh) &
                                                          cf_nmdr_dot_sh != ".",
                                                        cf_nmdr_dot_sh,
                                                        ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_fld_sh"))),
                                                               lag(!!sym(paste0(type, "_nmdr_dot_fld_sh"))),
                                                               !!sym(paste0(type, "_nmdr_dot_fld_sh")))),
           !!paste0(type, "_nmdr_dot_fld") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_fld"))),
                                                     !!sym(paste0(type, "_nmdr_dot_fld_sh")) * !!sym(paste0(type, "_nmdr_dot")),
                                                     !!sym(paste0(type, "_nmdr_dot_fld"))),
           !!paste0(type, "_nmdr_dot_nfld_sh") := 1 - !!sym(paste0(type, "_nmdr_dot_fld_sh")),
           !!paste0(type, "_nmdr_dot_nfld") := !!sym(paste0(type, "_nmdr_dot_nfld_sh")) * !!sym(paste0(type, "_nmdr_dot"))) %>%
    ungroup()
}

for (type in c("cf", "rcvd")) {
  finance_merged <- finance_merged %>%
    mutate(!!paste0(type, "_nmdr_dot_fld") := rowSums(select(., !!paste0(type, "_fld")), na.rm = TRUE),
           !!paste0(type, "_nmdr_dot_fld") := ifelse(!is.na(!!sym(paste0(type, "_nmdr_dot_fld"))) &
                                                       !is.na(!!sym(paste0(type, "_nmdr_dot"))) &
                                                       !!sym(paste0(type, "_nmdr_dot_fld")) > !!sym(paste0(type, "_nmdr_dot")),
                                                     !!sym(paste0(type, "_nmdr_dot")),
                                                     !!sym(paste0(type, "_nmdr_dot_fld"))),
           !!paste0(type, "_nmdr_dot_fld_sh") := !!sym(paste0(type, "_nmdr_dot_fld")) / !!sym(paste0(type, "_nmdr_dot")),
           !!paste0(type, "_nmdr_dot_fld_sh") := ifelse(!!sym(paste0(type, "_nmdr_dot_fld_sh")) > 1 &
                                                          !is.na(!!sym(paste0(type, "_nmdr_dot_fld_sh"))),
                                                        1,
                                                        ifelse(!!sym(paste0(type, "_nmdr_dot_fld_sh")) < 0 &
                                                                 !is.na(!!sym(paste0(type, "_nmdr_dot_fld_sh"))),
                                                               0,
                                                               !!sym(paste0(type, "_nmdr_dot_fld_sh"))))) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_nmdr_dot_fld_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_fld_sh"))) &
                                                          !is.na(cf_nmdr_dot_fld_sh) &
                                                          cf_nmdr_dot_fld_sh != ".",
                                                        cf_nmdr_dot_fld_sh,
                                                        ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_fld_sh"))),
                                                               lag(!!sym(paste0(type, "_nmdr_dot_fld_sh"))),
                                                               !!sym(paste0(type, "_nmdr_dot_fld_sh")))),
           !!paste0(type, "_nmdr_dot") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot"))),
                                                 !!sym(paste0(type, "_nmdr_dot_sh")) * !!sym(paste0(type, "_nmdr")),
                                                 !!sym(paste0(type, "_nmdr_dot"))),
           !!paste0(type, "_nmdr_dot_fld") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_fld"))),
                                                     !!sym(paste0(type, "_nmdr_dot_fld_sh")) * !!sym(paste0(type, "_nmdr_dot")),
                                                     !!sym(paste0(type, "_nmdr_dot_fld"))),
           !!paste0(type, "_nmdr_dot_nfld_sh") := 1 - !!sym(paste0(type, "_nmdr_dot_fld_sh")),
           !!paste0(type, "_nmdr_dot_nfld") := !!sym(paste0(type, "_nmdr_dot_nfld_sh")) * !!sym(paste0(type, "_nmdr_dot"))) %>%
    ungroup()
}

# // NMDR_DOT_NFLD_LAB, NMDR_DOT_NFLD_NLAB
for (type in c("exp", "budget")) {
  finance_merged <- finance_merged %>%
    mutate(!!paste0(type, "_nmdr_dot_nfld_lab") := rowSums(select(., !!paste0(type, "_lab")), na.rm = TRUE),
           !!paste0(type, "_nmdr_dot_nfld_lab") := ifelse(!is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab"))) &
                                                            !is.na(!!sym(paste0(type, "_nmdr_dot_nfld"))) &
                                                            !!sym(paste0(type, "_nmdr_dot_nfld_lab")) > !!sym(paste0(type, "_nmdr_dot_nfld")),
                                                          !!sym(paste0(type, "_nmdr_dot_nfld")),
                                                          !!sym(paste0(type, "_nmdr_dot_nfld_lab"))),
           !!paste0(type, "_nmdr_dot_nfld_lab_sh") := !!sym(paste0(type, "_nmdr_dot_nfld_lab")) / !!sym(paste0(type, "_nmdr_dot_nfld")),
           !!paste0(type, "_nmdr_dot_nfld_lab_sh") := ifelse(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")) > 1 & 
                                                               !is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                             1,
                                                             ifelse(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")) < 0 &
                                                                      !is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                                    0,
                                                                    !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))))) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(
      !!paste0(type, "_nmdr_dot_nfld_lab_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))) &
                                                          !is.na(lag(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")))),
                                                        lag(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                        !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
      !!paste0(type, "_nmdr_dot_nfld_lab_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                        lead(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                        !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")))
    ) %>%
    mutate(
      !!paste0(type, "_nmdr_dot_nfld_lab") := ifelse(!!sym(paste0(type, "_nmdr_dot_nfld_lab")) == ".",
                                                     !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")) * !!sym(paste0(type, "_nmdr_dot_nfld")),
                                                     !!sym(paste0(type, "_nmdr_dot_nfld_lab"))),
      !!paste0(type, "_nmdr_dot_nfld_nlab_sh") := 1 - !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")),
      !!paste0(type, "_nmdr_dot_nfld_nlab") := !!sym(paste0(type, "_nmdr_dot_nfld_nlab_sh")) * !!sym(paste0(type, "_nmdr_dot_nfld"))
    ) %>%
    ungroup()
}


for (type in c("cf", "rcvd")) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0(type, "_nmdr_dot_nfld_lab") := rowSums(select(., !!paste0(type, "_lab")), na.rm = TRUE),
      !!paste0(type, "_nmdr_dot_nfld_lab") := ifelse(!is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab"))) &
                                                       !is.na(!!sym(paste0(type, "_nmdr_dot_nfld"))) &
                                                       !!sym(paste0(type, "_nmdr_dot_nfld_lab")) > !!sym(paste0(type, "_nmdr_dot_nfld")),
                                                     !!sym(paste0(type, "_nmdr_dot_nfld")),
                                                     !!sym(paste0(type, "_nmdr_dot_nfld_lab"))),
      !!paste0(type, "_nmdr_dot_nfld_lab_sh") := !!sym(paste0(type, "_nmdr_dot_nfld_lab")) / !!sym(paste0(type, "_nmdr_dot_nfld")),
      !!paste0(type, "_nmdr_dot_nfld_lab_sh") := ifelse(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")) > 1 & 
                                                          !is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                        1,
                                                        ifelse(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")) < 0 &
                                                                 !is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                               0,
                                                               !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))))) %>%
    arrange(iso3) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_nmdr_dot_nfld_lab_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))) & lead(iso3) == iso3,
                                                             lead(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                             !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
           !!paste0(type, "_nmdr_dot_nfld_lab_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))) & lag(iso3) == iso3,
                                                             lag(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                             !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")))) %>%
    ungroup() %>%
    mutate(
      !!paste0(type, "_nmdr_dot_nfld_lab_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))) &
                                                          !is.na(cf_nmdr_dot_nfld_lab_sh) &
                                                          cf_nmdr_dot_nfld_lab_sh != ".",
                                                        cf_nmdr_dot_nfld_lab_sh,
                                                        ifelse(is.na(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                               lag(!!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))),
                                                               !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh"))))) %>%
    mutate(!!paste0(type, "_nmdr_dot_nfld_lab") := ifelse(!!sym(paste0(type, "_nmdr_dot_nfld_lab")) == ".",
                                                          !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")) * !!sym(paste0(type, "_nmdr_dot_nfld")),
                                                          !!sym(paste0(type, "_nmdr_dot_nfld_lab"))),
           !!paste0(type, "_nmdr_dot_nfld_nlab_sh") := 1 - !!sym(paste0(type, "_nmdr_dot_nfld_lab_sh")),
           !!paste0(type, "_nmdr_dot_nfld_nlab") := !!sym(paste0(type, "_nmdr_dot_nfld_nlab_sh")) * !!sym(paste0(type, "_nmdr_dot_nfld"))) %>%
    ungroup()
}


# **PN Jul 2020: Include TPT category. Previous years' values are zeroed. This creates a 'residual' category of other costs where
# 	** TBHIV and TPT costs have been subtracted. The name of this category is retained as NMDR_NDOT_NHIV
# 	// NMDR_NDOT_HIV, NMDR_NDOT_TPT & NMDR_NDOT_NHIV

for (type in c("exp", "budget")) {
  # HIV
  finance_merged <- finance_merged %>%
    mutate(!!paste0(type, "_nmdr_ndot_hiv") := rowSums(select(., !!paste0(type, "_tbhiv")),na.rm = TRUE),
           !!paste0(type, "_nmdr_ndot_hiv") := ifelse(!is.na(!!sym(paste0(type, "_nmdr_ndot_hiv"))) &
                                                        !is.na(!!sym(paste0(type, "_nmdr_ndot"))) &
                                                        !!sym(paste0(type, "_nmdr_ndot_hiv")) > !!sym(paste0(type, "_nmdr_ndot")),
                                                      !!sym(paste0(type, "_nmdr_ndot")),
                                                      !!sym(paste0(type, "_nmdr_ndot_hiv"))),
           !!paste0(type, "_nmdr_ndot_hiv_sh") := !!sym(paste0(type, "_nmdr_ndot_hiv")) / !!sym(paste0(type, "_nmdr_ndot")),
           !!paste0(type, "_nmdr_ndot_hiv_sh") := ifelse(!!sym(paste0(type, "_nmdr_ndot_hiv_sh")) > 1 & 
                                                           !is.na(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         1,
                                                         ifelse(!!sym(paste0(type, "_nmdr_ndot_hiv_sh")) < 0 &
                                                                  !is.na(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                                0,
                                                                !!sym(paste0(type, "_nmdr_ndot_hiv_sh"))))) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_nmdr_ndot_hiv_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         lag(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         !!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
           !!paste0(type, "_nmdr_ndot_hiv_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         lead(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         !!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
           !!paste0(type, "_nmdr_ndot_hiv") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_hiv"))),
                                                      !!sym(paste0(type, "_nmdr_ndot_hiv_sh")) * !!sym(paste0(type, "_nmdr_ndot")),
                                                      !!sym(paste0(type, "_nmdr_ndot_hiv")))) %>%
  ungroup()

# // TPT
  finance_merged <- finance_merged %>%
    mutate(!!paste0(type, "_nmdr_ndot_tpt") := rowSums(select(., !!paste0(type, "_tpt")),
                                                       na.rm = TRUE),
           !!paste0(type, "_nmdr_ndot_tpt") := ifelse(!is.na(!!sym(paste0(type, "_nmdr_ndot_tpt"))) &
                                                        !is.na(!!sym(paste0(type, "_nmdr_ndot"))) &
                                                        !!sym(paste0(type, "_nmdr_ndot_tpt")) > !!sym(paste0(type, "_nmdr_ndot")),
                                                      !!sym(paste0(type, "_nmdr_ndot")),
                                                      !!sym(paste0(type, "_nmdr_ndot_tpt"))),
           !!paste0(type, "_nmdr_ndot_tpt_sh") := !!sym(paste0(type, "_nmdr_ndot_tpt")) / !!sym(paste0(type, "_nmdr_ndot")),
           !!paste0(type, "_nmdr_ndot_tpt_sh") := ifelse(!!sym(paste0(type, "_nmdr_ndot_tpt_sh")) > 1 & 
                                                           !is.na(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))),
                                                         1,
                                                         ifelse(!!sym(paste0(type, "_nmdr_ndot_tpt_sh")) < 0 &
                                                                  !is.na(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))),
                                                                0,
                                                                !!sym(paste0(type, "_nmdr_ndot_tpt_sh"))))) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_nmdr_ndot_tpt_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), lag(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), !!sym(paste0(type, "_nmdr_ndot_tpt_sh"))),
           !!paste0(type, "_nmdr_ndot_tpt_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), lead(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), !!sym(paste0(type, "_nmdr_ndot_tpt_sh"))),
           !!paste0(type, "_nmdr_ndot_tpt") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_tpt"))),
                                                      !!sym(paste0(type, "_nmdr_ndot_tpt_sh")) * !!sym(paste0(type, "_nmdr_ndot")),
                                                      !!sym(paste0(type, "_nmdr_ndot_tpt")))) %>%
    ungroup()

# //NHIV (and also subtracting TPT)
  finance_merged <- finance_merged %>%
    mutate(!!paste0(type, "_hiv_tpt_sh") := rowSums(select(., !!paste0(type, "_nmdr_ndot_hiv_sh"), !!paste0(type, "_nmdr_ndot_tpt_sh")), na.rm = TRUE),
           !!paste0(type, "_nmdr_ndot_nhiv_sh") := 1 - !!sym(paste0(type, "_hiv_tpt_sh")),
           !!paste0(type, "_nmdr_ndot_nhiv_sh") := ifelse(is.na(!!sym(paste0(type, "_hiv_tpt_sh"))) & is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv_sh"))), 1, !!sym(paste0(type, "_nmdr_ndot_nhiv_sh"))),
           !!paste0(type, "_nmdr_ndot_nhiv") := !!sym(paste0(type, "_nmdr_ndot_nhiv_sh")) * !!sym(paste0(type, "_nmdr_ndot")))

}


types <- c("cf", "rcvd")
for (type in types) {
  # // HIV
  finance_merged <- finance_merged %>%
    mutate(!!paste0(type, "_nmdr_ndot_hiv") := rowSums(select(., !!paste0(type, "_tbhiv")), na.rm = TRUE),
           !!paste0(type, "_nmdr_ndot_hiv") := ifelse(!is.na(!!sym(paste0(type, "_nmdr_ndot_hiv"))) &
                                                        !is.na(!!sym(paste0(type, "_nmdr_ndot"))) &
                                                        !!sym(paste0(type, "_nmdr_ndot_hiv")) > !!sym(paste0(type, "_nmdr_ndot")),
                                                      !!sym(paste0(type, "_nmdr_ndot")),
                                                      !!sym(paste0(type, "_nmdr_ndot_hiv"))),
           !!paste0(type, "_nmdr_ndot_hiv_sh") := !!sym(paste0(type, "_nmdr_ndot_hiv")) / !!sym(paste0(type, "_nmdr_ndot")),
           !!paste0(type, "_nmdr_ndot_hiv_sh") := ifelse(!!sym(paste0(type, "_nmdr_ndot_hiv_sh")) > 1 &
                                                           !is.na(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         1,
                                                         ifelse(!!sym(paste0(type, "_nmdr_ndot_hiv_sh")) < 0 &
                                                                  !is.na(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                                0,
                                                                !!sym(paste0(type, "_nmdr_ndot_hiv_sh"))))) %>%
    
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_nmdr_ndot_hiv_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         lag(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         !!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
           !!paste0(type, "_nmdr_ndot_hiv_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         lead(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         !!sym(paste0(type, "_nmdr_ndot_hiv_sh")))) %>%
    ungroup() %>%
    mutate(
      !!sym(paste0(type, "_nmdr_ndot_hiv_sh")) := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
                                                         cf_nmdr_ndot_hiv_sh,
                                                         !!sym(paste0(type, "_nmdr_ndot_hiv_sh"))),
      !!sym(paste0(type, "_nmdr_ndot_hiv")) := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_hiv"))),
                                                      !!sym(paste0(type, "_nmdr_ndot_hiv_sh")) * !!sym(paste0(type, "_nmdr_ndot")),
                                                      !!sym(paste0(type, "_nmdr_ndot_hiv")))
    ) 

  # // TPT
  finance_merged <- finance_merged %>%
    mutate(!!paste0(type, "_nmdr_ndot_tpt") := rowSums(select(., !!paste0(type, "_tpt")), na.rm = TRUE),
           !!paste0(type, "_nmdr_ndot_tpt") := ifelse((!is.na(!!sym(paste0(type, "_nmdr_ndot_tpt"))) & !is.na(!!sym(paste0(type, "_nmdr_ndot")))) &
                                                        (!!sym(paste0(type, "_nmdr_ndot_tpt")) > !!sym(paste0(type, "_nmdr_ndot"))),
                                                      !!sym(paste0(type, "_nmdr_ndot")), !!sym(paste0(type, "_nmdr_ndot_tpt"))),
           !!paste0(type, "_nmdr_ndot_tpt_sh") := !!sym(paste0(type, "_nmdr_ndot_tpt")) / !!sym(paste0(type, "_nmdr_ndot")),
           !!paste0(type, "_nmdr_ndot_tpt_sh") := ifelse(!!sym(paste0(type, "_nmdr_ndot_tpt_sh")) > 1 & 
                                                           !is.na(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), 
                                                         1, 
                                                         ifelse(!!sym(paste0(type, "_nmdr_ndot_tpt_sh")) < 0 & 
                                                                  !is.na(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), 
                                                                0, 
                                                                !!sym(paste0(type, "_nmdr_ndot_tpt_sh"))))) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(   !!paste0(type, "_nmdr_ndot_tpt_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), lag(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), !!sym(paste0(type, "_nmdr_ndot_tpt_sh"))),
              !!paste0(type, "_nmdr_ndot_tpt_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), lead(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), !!sym(paste0(type, "_nmdr_ndot_tpt_sh")))
    ) %>%
    ungroup() %>%
    mutate(      !!sym(paste0(type, "_nmdr_ndot_tpt_sh")) := ifelse(!is.na(cf_nmdr_ndot_tpt_sh) & 
                                                                      is.na(!!sym(paste0(type, "_nmdr_ndot_tpt_sh"))), 
                                                                    cf_nmdr_ndot_tpt_sh, 
                                                                    !!sym(paste0(type, "_nmdr_ndot_tpt_sh"))),
                 # Replace `type'_nmdr_ndot_tpt with `type'_nmdr_ndot_tpt_sh * `type'_nmdr_ndot if conditions are met
                 !!sym(paste0(type, "_nmdr_ndot_tpt")) := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_tpt"))), 
                                                                 !!sym(paste0(type, "_nmdr_ndot_tpt_sh")) * !!sym(paste0(type, "_nmdr_ndot")), 
                                                                 !!sym(paste0(type, "_nmdr_ndot_tpt"))))
  
  
  # // NHIV (and also subtracting TPT)
  finance_merged <- finance_merged %>%
    mutate(
      !!sym(paste0(type, "_hiv_tpt_sh")) := rowSums(select(., !!paste0(type, "_nmdr_ndot_hiv_sh"), !!paste0(type, "_nmdr_ndot_tpt_sh")), na.rm = TRUE),
      !!sym(paste0(type, "_nmdr_ndot_nhiv_sh")) := 1 - !!sym(paste0(type, "_hiv_tpt_sh"))
    ) %>%
    # Replace missing values with 1 for `type'_nmdr_ndot_nhiv_sh
    mutate(!!sym(paste0(type, "_nmdr_ndot_nhiv_sh")) := ifelse(is.na(!!sym(paste0(type, "_hiv_tpt_sh"))) & is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv_sh"))), 1, !!sym(paste0(type, "_nmdr_ndot_nhiv_sh")))) %>%
    # Calculate `type'_nmdr_ndot_nhiv
    mutate(!!sym(paste0(type, "_nmdr_ndot_nhiv")) := !!sym(paste0(type, "_nmdr_ndot_nhiv_sh")) * !!sym(paste0(type, "_nmdr_ndot")))
}


# // NMDR_NDOT_NHIV_OTH, NMDR_NDOT_NHIV_NOTH 
# // Correcting for the fact that "Other" is treated differently pre and post 2006
# // PN Jul 2020: Also correcting to remove tpt from nhiv_noth
type <- "exp"
for (type in c("exp", "budget")) {
  finance_merged <- finance_merged %>%
    # Calculate row totals for `type'_nmdr_ndot_nhiv_oth
    mutate(
      !!sym(paste0(type, "_nmdr_ndot_nhiv_oth")) := rowSums(select(., starts_with(paste0(type, "_oth"))), na.rm = TRUE)
    ) %>%
    mutate(
      !!sym(paste0(type, "_nmdr_ndot_nhiv_oth")) :=
        ifelse(
          !is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth"))) & !is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv"))) & !!sym(paste0(type, "_nmdr_ndot_nhiv_oth")) > !!sym(paste0(type, "_nmdr_ndot_nhiv")),
          !!sym(paste0(type, "_nmdr_ndot_nhiv")),
          !!sym(paste0(type, "_nmdr_ndot_nhiv_oth"))
        )
    ) %>%
    mutate(
      !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh")) :=
        !!sym(paste0(type, "_nmdr_ndot_nhiv_oth")) / !!sym(paste0(type, "_nmdr_ndot_nhiv"))
    ) %>%
    mutate(
      !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh")) :=
        ifelse(
          !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh")) > 1 & !is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))),
          1,
          ifelse(
            !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh")) < 0 & !is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))),
            0,
            !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))
          )
        )
    ) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, '_nmdr_ndot_nhiv_oth_sh') :=
             ifelse(is.na(!!sym(paste0(type, '_nmdr_ndot_nhiv_oth_sh'))),
                    lag(!!sym(paste0(type, '_nmdr_ndot_nhiv_oth_sh'))),
                    !!sym(paste0(type, '_nmdr_ndot_nhiv_oth_sh'))),
           !!paste0(type, '_nmdr_ndot_nhiv_oth_sh') :=
             ifelse(is.na(!!sym(paste0(type, '_nmdr_ndot_nhiv_oth_sh'))),
                    lead(!!sym(paste0(type, '_nmdr_ndot_nhiv_oth_sh'))),
                    !!sym(paste0(type, '_nmdr_ndot_nhiv_oth_sh')))) %>%
    ungroup() %>%
    mutate(
      !!sym(paste0(type, '_nmdr_ndot_nhiv_oth')) := ifelse(is.na(!!sym(paste0(type, '_nmdr_ndot_nhiv_oth'))),
                                                           !!sym(paste0(type, '_nmdr_ndot_nhiv_oth_sh')) * !!sym(paste0(type, '_nmdr_ndot_nhiv')),
                                                           !!sym(paste0(type, '_nmdr_ndot_nhiv_oth'))),
      !!sym(paste0(type, '_nmdr_ndot_nhiv_noth_sh')) := 1 - !!sym(paste0(type, '_nmdr_ndot_nhiv_oth_sh')),
      !!sym(paste0(type, '_nmdr_ndot_nhiv_noth')) := !!sym(paste0(type, '_nmdr_ndot_nhiv_noth_sh')) * !!sym(paste0(type, '_nmdr_ndot_nhiv'))
    )
}

type <- "cf"
types <- c("cf", "rcvd")
for (type in types) {
  finance_merged <- finance_merged %>%
    mutate(!!paste0(type, "_nmdr_ndot_nhiv_oth") := rowSums(select(., !!paste0(type, "_oth")), na.rm = TRUE),
           !!sym(paste0(type, "_nmdr_ndot_nhiv_oth")) := ifelse((!is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth"))) & !is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv"))) & !!sym(paste0(type, "_nmdr_ndot_nhiv_oth")) > !!sym(paste0(type, "_nmdr_ndot_nhiv"))), !!sym(paste0(type, "_nmdr_ndot_nhiv")), !!sym(paste0(type, "_nmdr_ndot_nhiv_oth"))),
           !!paste0(type, "_nmdr_ndot_nhiv_oth_sh") := !!sym(paste0(type, "_nmdr_ndot_nhiv_oth")) / !!sym(paste0(type, "_nmdr_ndot_nhiv")),
           !!paste0(type, "_nmdr_ndot_nhiv_oth_sh") := ifelse(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh")) > 1, 1, ifelse(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh")) < 0, 0, !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))))
    ) %>%
    arrange(iso3, year) %>%
    group_by(iso3) %>%
    mutate(!!paste0(type, "_nmdr_ndot_nhiv_oth_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))), lag(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))), !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))),
           !!paste0(type, "_nmdr_ndot_nhiv_oth_sh") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))), lead(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))), !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh")))
    ) %>%
    mutate(
      !!paste0(type, "_nmdr_ndot_nhiv_oth_sh") := ifelse(!is.na(cf_nmdr_ndot_nhiv_oth_sh) & is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))), cf_nmdr_ndot_nhiv_oth_sh, !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh"))),
      !!paste0(type, "_nmdr_ndot_nhiv_oth") := ifelse(is.na(!!sym(paste0(type, "_nmdr_ndot_nhiv_oth"))), !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh")) * !!sym(paste0(type, "_nmdr_ndot_nhiv")), !!sym(paste0(type, "_nmdr_ndot_nhiv_oth"))),
      !!paste0(type, "_nmdr_ndot_nhiv_noth_sh") := 1 - !!sym(paste0(type, "_nmdr_ndot_nhiv_oth_sh")),
      !!paste0(type, "_nmdr_ndot_nhiv_noth") := !!sym(paste0(type, "_nmdr_ndot_nhiv_noth_sh")) * !!sym(paste0(type, "_nmdr_ndot_nhiv"))
    ) %>%
    ungroup()
}

#--- manual adjustment codes for imputed variables ---#
# Load a script for adjustment
source(here::here('finance/3b_adjust_imputed_data.r'))

save.image(here::here("finance/local/finance_imputed.Rdata"))

# /*******************************************************************
#   SAVE DATASETS IN NOMINAL USD, CONSTANT USD AND CONSTANT I$ VERSIONS
# ********************************************************************/
# // NOMINAL
load(here::here('finance/local/finance_imputed.Rdata'))

finance_merged <- finance_merged %>%
  mutate(include = ifelse(g_income %in% c("LIC", "LMC", "UMC"), 1, 0)) # 1 for includsion in the GTBR 2023

# //foreach country in DZA CRI {
#   // PN 2019: In 2018 the list for exclusion was ALB AZE CRI DMA DZA EGY GNQ GRD IRN LBY LCA MUS PSE SYR TKM VCT WSM
#   // PN 2020: In 2019 the list for exclusion was ALB BDI CRI DMA DZA ECU EGY FSM GMB IRN JAM LBY MUS WSM MKD TUR TKM UZB
#   // PN 2021: In 2020 the list was ALB CRI DZA GMB GRD  LBY   PSE  VCT   DMA  EGY JAM MUS TKM UZB
#   // AS 2023: The inlusion rule was discussed and set for any country with either 2022 GHS cost or 2022 rcvd_total after data filling. 
#   // This include all countries but for 3 they will only have GHS costs: Costa Rica, Mauritius, and St. Vincent.
#   //replace include=0 if iso3=="`country'"
#   //}

var_list <- c("iso2", "iso_numeric", "country", "g_whoregion", "g_hb_tb", "g_hb_tbhiv", "g_hb_mdr", "g_income")


# foreach var of varlist iso2 iso_numeric country g_whoregion g_hb_tb g_hb_tbhiv g_hb_mdr g_income{
#   gsort iso3 - `var'
# 	by iso3: carryforward (`var'), replace
# }
# 
# foreach var of varlist include include gf_eligible  usaid_amount  gf_funding_need gf_domestic  gf_gf_funding{
#   replace `var'=0 if `var'==.
# 	}
	
# ** PN / IGB 2021: update g_hb_tb to include the three countries in the global watchlist in 2021 - 30
finance_merged <- finance_merged %>%
  mutate(g_hb_tb = if_else(iso3 %in% c("RUS", "KHM", "ZWE"), 1, g_hb_tb))

# /* Check what countries are being dropped here!!! It should just be non-countries like regions from WB */
#   //br if country == ""

finance_merged <- finance_merged %>%
  filter(country != "") %>%
  arrange(country, year)

# Save the modified dataframe to a new file
save.image(here::here("finance/local/finance_imputed_nomusd.Rdata"))

# Preserve the current dataframe
finance_map <- finance_merged %>%
  filter(year == report_year)

# Create a new column 'var' and assign values based on 'include' column
finance_map <- finance_map %>%
  mutate(var = ifelse(include == 1, "Included", "Excluded")) %>%
  select(iso3, var)

# Sort by iso3
finance_map <- finance_map %>%
  arrange(iso3)

# included countries only
finance_merged <- finance_merged %>%
  filter(include==1)

save.image(here::here("finance/local/finance_imputed_nomusd_included.Rdata"))

# /**********************************/
#   // CONSTANT DOLLARS NOT BALANCED
# /**********************************/
load(here::here('finance/local/finance_imputed_nomusd.Rdata'))

dollarvars <- c(
  "budget_cpp_dstb", "budget_cpp_mdr", "budget_cpp_xdr",
  "budget_prog", "gap_prog", "budget_fld", "gap_fld",
  "budget_staff", "gap_staff", "budget_lab", "gap_lab",
  "budget_tbhiv", "gap_tbhiv", "budget_sld", "gap_sld",
  "budget_mdrmgt", "gap_mdrmgt", "budget_or", "gap_or",
  "budget_orsrvy", "gap_orsrvy", "budget_patsup", "gap_patsup",
  "budget_oth", "gap_oth", "budget_tot", "gap_tot",
  "cf_tot_gov", "cf_tot_loan", "cf_tot_gf", "cf_tot_grnt",
  "cf_tot_usaid", "cf_tot", "cf_tot_sources", "cf_tot_domestic",
  "exp_cpp_dstb", "exp_cpp_mdr", "exp_cpp_xdr", "exp_prog",
  "exp_fld", "exp_staff", "exp_lab", "exp_tbhiv", "exp_sld",
  "exp_mdrmgt", "exp_or", "exp_orsrvy", "exp_patsup", "exp_oth",
  "exp_tot", "rcvd_tot_gov", "rcvd_tot_loan", "rcvd_tot_gf",
  "rcvd_tot_grnt", "rcvd_tot_usaid", "rcvd_tot", "rcvd_tot_domestic",
  "rcvd_tot_sources", "cf_prog", "cf_fld", "cf_staff", "cf_lab",
  "cf_tbhiv", "cf_sld", "cf_mdrmgt", "cf_or", "cf_orsrvy",
  "cf_patsup", "cf_oth", "rcvd_prog", "rcvd_fld", "rcvd_staff",
  "rcvd_lab", "rcvd_tbhiv", "rcvd_sld", "rcvd_mdrmgt", "rcvd_or",
  "rcvd_orsrvy", "rcvd_patsup", "rcvd_oth", "c_clinic_sp",
  "c_hospital_sp", "c_clinic_sn", "c_hospital_sn", "c_hospital_nmdr",
  "c_clinic_nmdr", "c_clinic_mdr", "c_hospital_mdr", "c_ghs_sp",
  "c_ghs_sn", "c_ghs_nmdr", "c_ghs_mdr", "c_ghs_inpatient",
  "c_ghs_outpatient", "c_ghs", "exp_mdr", "exp_nmdr", "budget_mdr",
  "budget_nmdr", "rcvd_mdr", "rcvd_nmdr", "cf_mdr", "cf_nmdr",
  "exp_mdr_sld", "exp_mdr_nsld", "budget_mdr_sld", "budget_mdr_nsld",
  "rcvd_mdr_sld", "rcvd_mdr_nsld", "cf_mdr_sld", "cf_mdr_nsld",
  "exp_nmdr_dot", "exp_nmdr_ndot", "budget_nmdr_dot", "budget_nmdr_ndot",
  "rcvd_nmdr_dot", "rcvd_nmdr_ndot", "cf_nmdr_dot", "cf_nmdr_ndot",
  "exp_nmdr_dot_fld", "exp_nmdr_dot_nfld", "budget_nmdr_dot_fld",
  "budget_nmdr_dot_nfld", "rcvd_nmdr_dot_fld", "rcvd_nmdr_dot_nfld",
  "cf_nmdr_dot_fld", "cf_nmdr_dot_nfld", "exp_nmdr_dot_nfld_lab",
  "exp_nmdr_dot_nfld_nlab", "budget_nmdr_dot_nfld_lab",
  "budget_nmdr_dot_nfld_nlab", "rcvd_nmdr_dot_nfld_lab",
  "rcvd_nmdr_dot_nfld_nlab", "cf_nmdr_dot_nfld_lab",
  "cf_nmdr_dot_nfld_nlab", "exp_nmdr_ndot_hiv", "exp_nmdr_ndot_nhiv",
  "budget_nmdr_ndot_hiv", "budget_nmdr_ndot_nhiv",
  "rcvd_nmdr_ndot_hiv", "rcvd_nmdr_ndot_nhiv", "cf_nmdr_ndot_hiv",
  "cf_nmdr_ndot_nhiv", "exp_nmdr_ndot_nhiv_oth",
  "exp_nmdr_ndot_nhiv_noth", "budget_nmdr_ndot_nhiv_oth",
  "budget_nmdr_ndot_nhiv_noth", "rcvd_nmdr_ndot_nhiv_oth",
  "rcvd_nmdr_ndot_nhiv_noth", "cf_nmdr_ndot_nhiv_oth",
  "cf_nmdr_ndot_nhiv_noth", "gf_rcvd_ext_gf", "rcvd_int", "rcvd_ext",
  "rcvd_ext_ngf", "rcvd_ext_gf", "cf_int", "cf_ext", "cf_ext_ngf",
  "cf_ext_gf",
  "budget_tpt","cf_tpt","gap_tpt","exp_tpt","rcvd_tpt",
  "exp_nmdr_ndot_tpt","budget_nmdr_ndot_tpt","cf_nmdr_ndot_tpt","rcvd_nmdr_ndot_tpt"
)

# Print the results
included_variables <- dollarvars %in% colnames(finance_merged)
for (i in seq_along(dollarvars)) {
  cat(dollarvars[i], ": ", ifelse(included_variables[i], "Included", "Not included"), "\n")
}

for (var in dollarvars) {
  tryCatch({
    finance_merged <- finance_merged %>%
      mutate(!!sym(var) := !!sym(var) / deflator_us) %>%
      # mutate(!!sym(var) := round(!!sym(var), 0.000001)) %>%
      mutate(!!sym(paste0(var, "_constant_USD")) := glue::glue("{var} constant US$"))
  }, error = function(e) {
    cat("Skipping variable:", var, "\n")
    # You can choose to do nothing or handle the error in a different way
  })
}

# Generate imf_gdp_pc_con_usd
finance_merged <- finance_merged %>%
  mutate(imf_gdp_pc_con_usd = imf_gdp_pc_cur_usd / deflator_us) %>%
  arrange(iso3, year)

save.image(here::here("finance/local/finance_imputed_constantus.Rdata"))

# /********************************/
#   // CONSTANT DOLLARS BALANCED
# /******************************/
load(here::here('finance/local/finance_imputed_nomusd.Rdata'))

finance_merged <- finance_merged %>% 
  arrange(iso3, year)

# Replace missing values in deflator_us with the previous non-missing value by iso3
finance_merged <- finance_merged %>% 
  group_by(iso3) %>% 
  mutate(deflator_us = ifelse(is.na(deflator_us), lag(deflator_us), deflator_us)) %>%
  ungroup()

for (var in dollarvars) {
  tryCatch({
    finance_merged <- finance_merged %>%
      mutate(!!sym(var) := !!sym(var) / deflator_us) %>%
      # mutate(!!sym(var) := round(!!sym(var), 0.000001)) %>%
      mutate(!!sym(paste0(var, "_constant_USD")) := glue::glue("{var} constant US$"))
  }, error = function(e) {
    cat("Skipping variable:", var, "\n")
    # You can choose to do nothing or handle the error in a different way
  })
}

finance_merged <- finance_merged %>%
  mutate(imf_gdp_pc_con_usd = imf_gdp_pc_cur_usd / deflator_us) %>%
  arrange(iso3, year)

finance_merged <- finance_merged %>% 
  mutate(include = 0) %>% 
  mutate(include = ifelse(g_income %in% c("LIC", "LMC", "UMC"), 1, include))


# /* LAST MINUTE ADJUSTMENT TO ENSURE TB SPENDING AREAS EQUAL TOTAL TB SPENDING */
# source(here::here('finance/3c_last minute adjustment.R')) # this adjustment is unnecessary

finance_merged <- subset(finance_merged, include == 1)
finance_merged <- arrange(finance_merged, iso3, year)

finance_merged <- finance_merged %>% 
  mutate(rcvd_tot = ifelse(do_not_fill==1,0,rcvd_tot),
         rcvd_mdr = ifelse(do_not_fill==1,0,rcvd_mdr),
         rcvd_nmdr = ifelse(do_not_fill==1,0,rcvd_nmdr),
         rcvd_mdr_sld = ifelse(do_not_fill==1,0,rcvd_mdr_sld),
         rcvd_nmdr_dot = ifelse(do_not_fill==1,0,rcvd_nmdr_dot),
         rcvd_nmdr_ndot_nhiv_oth = ifelse(do_not_fill==1,0,rcvd_nmdr_ndot_nhiv_oth),
         rcvd_nmdr_ndot_nhiv_noth = ifelse(do_not_fill==1,0,rcvd_nmdr_ndot_nhiv_noth),
         rcvd_int = ifelse(do_not_fill==1,0,rcvd_int),
         rcvd_ext_ngf = ifelse(do_not_fill==1,0,rcvd_ext_ngf),
         rcvd_ext_gf = ifelse(do_not_fill==1,0,rcvd_ext_gf),
         rcvd_nmdr_ndot_nhiv = ifelse(do_not_fill==1,0,rcvd_nmdr_ndot_nhiv),
         rcvd_nmdr_ndot_hiv = ifelse(do_not_fill==1,0,rcvd_nmdr_ndot_hiv)
  ) %>%
  filter(year >= start_year)

save.image(here::here("finance/output/finance_imputed_constantus_included.Rdata"))
