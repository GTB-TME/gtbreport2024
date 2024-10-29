# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data cleaning script, translated from Stata version - testing version!
# Takuya Yamanaka, January 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# this script will be run by "ch4_prepare_data_part1.R"

#-- Data cleaning starts from here --#
# /****************
#   2023
# ****************/
# Adding missing WB income group to some countries
finance_merged <- finance_merged %>%
  mutate(g_income = if_else(iso3 %in% c("AIA", "COK", "MSR", "NIU", "TKL", "WLF") & g_income == "", "HIC", g_income),
         g_income = if_else(iso3 == "VEN" & g_income == "", "UMC", g_income))


# Define the suffixes: create a local variable to store the specific suffixes of the financial data's disaggregates
suffix <- c("lab", "staff", "fld", "prog", "sld", "mdrmgt", "tpt", "tbhiv", "patsup", "orsrvy", "oth", "tot")


# Belarus
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = case_when(
    iso3 == "BLR" & year == 2016 ~ 69266597,
    iso3 == "BLR" & year == 2017 ~ 71159292,
    iso3 == "BLR" & year == 2018 ~ 70765651,
    TRUE ~ rcvd_tot
  ))

# Bangladesh
# * Reported GF component (and Lab component)  received in 2022 - 130 million was a Covid RF grant for lab commodities. Remove from the lab and the GF source components
finance_merged <- finance_merged %>%
  ungroup() %>%
  mutate(
    rcvd_lab = if_else(rcvd_lab == 181076092 & iso3 == "BGD" & year == 2022, rcvd_lab - 130000000, rcvd_lab),
    exp_lab = if_else(exp_lab == 180483721 & iso3 == "BGD" & year == 2022, exp_lab - 130000000, exp_lab),
    rcvd_tot_gf = if_else(rcvd_tot_gf == 199434217 & iso3 == "BGD" & year == 2022, rcvd_tot_gf - 130000000, rcvd_tot_gf),
    new_rcvd_tot = rowSums(select(., starts_with("rcvd_")&!contains("tot")), na.rm = TRUE),
    new_exp_tot = rowSums(select(., starts_with("exp_")&!contains("tot")), na.rm = TRUE),
    rcvd_tot = if_else(iso3 == "BGD" & year == 2022, new_rcvd_tot, rcvd_tot),
    exp_tot = if_else(iso3 == "BGD" & year == 2022, new_exp_tot, exp_tot)
  ) %>%
  select(-new_rcvd_tot, -new_exp_tot)


# *Belarus. Starting data set had incorrect rcvd total amounts. Reverting to country reported amounts
finance_merged <- finance_merged %>%
  mutate(
    rcvd_tot = case_when(
      iso3 == "BLR" & year == 2016 ~ 69266597,
      iso3 == "BLR" & year == 2017 ~ 71159292,
      iso3 == "BLR" & year == 2018 ~ 70765651,
      TRUE ~ rcvd_tot
    )
  )

# Cambodia
# Iterate over each suffix and perform the replacement
# expenditure data were not reported for 2022 >> Let previous report's cf_ values be used as this year's expenditure value
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      # Replace the specified columns if conditions are met
      !!sym(paste0("rcvd_", item)) := ifelse(is.na(rcvd_tot) & iso3 == "KHM" & year == 2022,
                                              !!sym(paste0("cf_", item)),
                                              !!sym(paste0("rcvd_", item))),
      !!sym(paste0("exp_", item)) := if_else(is.na(exp_tot) & iso3 == "KHM" & year == 2022,
                                             !!sym(paste0("cf_", item)),
                                             !!sym(paste0("exp_", item)))
    )
}

finance_merged <- finance_merged %>%
  mutate(
    rcvd_tot_domestic = if_else(is.na(rcvd_tot_domestic) & iso3 == "KHM" & year == 2022, cf_tot_domestic, rcvd_tot_domestic),
    rcvd_tot_gf = if_else(is.na(rcvd_tot_gf) & iso3 == "KHM" & year == 2022, cf_tot_gf, rcvd_tot_gf),
    rcvd_tot_usaid = if_else(is.na(rcvd_tot_usaid) & iso3 == "KHM" & year == 2022, cf_tot_usaid, rcvd_tot_usaid),
    rcvd_tot_grnt = if_else(is.na(rcvd_tot_grnt) & iso3 == "KHM" & year == 2022, cf_tot_grnt, rcvd_tot_grnt)
  )

# COG
# * temporarily change reported values (rcvd 2022 and budget / cf 2023) to USD using echange rate (XOF?)
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      # Round the specified columns if conditions are met
      !!sym(paste0("rcvd_", item)) := if_else(rcvd_tot == 767510775 & iso3 == "COG" & year == 2022,
                                              round(!!sym(paste0("rcvd_", item))/243.509, 0),
                                              !!sym(paste0("rcvd_", item))),
      !!sym(paste0("exp_", item)) := if_else(exp_tot == 767510775 & iso3 == "COG" & year == 2022,
                                             round(!!sym(paste0("exp_", item))/243.509, 0),
                                             !!sym(paste0("exp_", item)))
    )
}

for (item in c("tot_gf", "tot_usaid", "tot_domestic")) {
  finance_merged <- finance_merged %>%
    mutate(
      # Replace the specified columns if conditions are met
      !!sym(paste0("rcvd_", item)) := if_else(rcvd_tot == 767510775 & iso3 == "COG" & year == 2022,
                                              round(!!sym(paste0("rcvd_", item))/243.509, 0),
                                              !!sym(paste0("rcvd_", item)))
    )
}

# * Cuba
# li year rcvd_tot rcvd_int rcvd_ext_gf rcvd_ext_ngf if iso3 == "CUB" & year > 2012
# * Notice that the rcvd_int variable is empty, and total sources don't add up to rcvd_tot in 2014-19
finance_merged <- finance_merged %>%
  mutate(
    rcvd_tot = if_else(iso3 == "CUB" & is.na(rcvd_tot_domestic) & is.na(rcvd_tot_gf), exp_tot, rcvd_tot),
    rcvd_tot_domestic = if_else(iso3 == "CUB" & is.na(rcvd_tot_domestic) & is.na(rcvd_tot_gf), rcvd_tot, rcvd_tot_domestic)
  )

# DPRK
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  mutate(
    rcvd_tot = if_else(iso3 == "PRK" & year == 2022 & is.na(rcvd_tot), lag(cf_tot), rcvd_tot),
    exp_tot = if_else(iso3 == "PRK" & year == 2022 & is.na(exp_tot), lag(cf_tot), exp_tot),
  )

for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!sym(paste0("rcvd_", item)) := if_else(iso3 == "PRK" & year == 2022 & is.na(!!sym(paste0("rcvd_", item))),
                                              lag(!!sym(paste0("cf_", item))), !!sym(paste0("rcvd_", item)))
    )
}

# Gabon
# Move research expenditure reported in 2022 from domestic to other grant for Gabon (GAB)
finance_merged <- finance_merged %>%
  mutate(
    # List relevant variables for Gabon from 2021 onwards
    rcvd_oth = ifelse(iso3 == "GAB" & year == 2022, rcvd_oth + rcvd_orsrvy, rcvd_oth), # Move research expenditure to other grant
    rcvd_tot_domestic = ifelse(rcvd_orsrvy == 2329986 & rcvd_tot_domestic == 3701009 & iso3 == "GAB" & year == 2022,
                               rcvd_tot_domestic - rcvd_orsrvy, rcvd_tot_domestic), # Adjust rcvd_tot_domestic
    rcvd_tot_grnt = ifelse(rcvd_orsrvy == 2329986 & rcvd_tot_grnt == 73280 & iso3 == "GAB" & year == 2022,
                           rcvd_tot_grnt + rcvd_orsrvy, rcvd_tot_grnt) # Adjust rcvd_tot_grnt
  )

# Kryzygstan. Starting data set had incorrect rcvd total amounts. Reverting to country reported amounts
# finance_merged <- finance_merged %>%
#   mutate(
#     rcvd_tot = case_when(
#       iso3 == "KGZ" & year == 2019 ~ 15328783,
#       iso3 == "KGZ" & year == 2020 ~ 17273868,
#       iso3 == "KGZ" & year == 2021 & rcvd_tot == 2089734 ~ 12676719,
#       TRUE ~ rcvd_tot
#     )
#   )

# * Liberia
# * Include last year's late submitted report (2021 rcvd and exp, exp_cpp_ and utilisation variables) from the DCF.
# finance_merged <- finance_merged %>%
#   mutate(
#     rcvd_lab = if_else(is.na(rcvd_lab) & year == 2021 & iso3 == "LBR", 185131, rcvd_lab),
#     rcvd_staff = if_else(is.na(rcvd_staff) & year == 2021 & iso3 == "LBR", 160229, rcvd_staff),
#     rcvd_fld = if_else(is.na(rcvd_fld) & year == 2021 & iso3 == "LBR", 147564, rcvd_fld),
#     rcvd_prog = if_else(is.na(rcvd_prog) & year == 2021 & iso3 == "LBR", 573185, rcvd_prog),
#     rcvd_sld = if_else(is.na(rcvd_sld) & year == 2021 & iso3 == "LBR", 222085, rcvd_sld),
#     rcvd_mdrmgt = if_else(year == 2021 & iso3 == "LBR", NA, rcvd_mdrmgt),
#     rcvd_tpt = if_else(year == 2021 & iso3 == "LBR", NA, rcvd_tpt),
#     rcvd_tbhiv = if_else(is.na(rcvd_tbhiv) & year == 2021 & iso3 == "LBR", 123396, rcvd_tbhiv),
#     rcvd_patsup = if_else(is.na(rcvd_patsup) & year == 2021 & iso3 == "LBR", 83631, rcvd_patsup),
#     rcvd_orsrvy = if_else(year == 2021 & iso3 == "LBR", NA, rcvd_orsrvy),
#     rcvd_oth = if_else(is.na(rcvd_oth) & year == 2021 & iso3 == "LBR", 21037, rcvd_oth),
#     rcvd_tot = if_else(is.na(rcvd_tot) & year == 2021 & iso3 == "LBR", 1516258, rcvd_tot),
#     rcvd_tot_domestic = if_else(year == 2021 & iso3 == "LBR", NA, rcvd_tot_domestic),
#     rcvd_tot_gf = if_else(year == 2021 & iso3 == "LBR", 1516258, rcvd_tot_gf),
#     rcvd_tot_usaid = if_else(year == 2021 & iso3 == "LBR", NA, rcvd_tot_usaid),
#     rcvd_tot_grnt = if_else(year == 2021 & iso3 == "LBR", NA, rcvd_tot_grnt)
#   )

# Iterate over each suffix and perform replacements
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!sym(paste0("exp_", item)) := if_else(is.na(!!sym(paste0("exp_", item))) & year == 2021 & iso3 == "LBR",
                                             !!sym(paste0("rcvd_", item)), !!sym(paste0("exp_", item)))
    )
}

# Adjust specific variables for Liberia in 2021
finance_merged <- finance_merged %>%
  mutate(
    exp_cpp_dstb = if_else(is.na(exp_cpp_dstb) & year == 2021 & iso3 == "LBR", 104, exp_cpp_dstb),
    exp_cpp_mdr = if_else(is.na(exp_cpp_mdr) & year == 2021 & iso3 == "LBR", 2037, exp_cpp_mdr),
    exp_cpp_tpt = if_else(is.na(exp_cpp_tpt) & year == 2021 & iso3 == "LBR", 70, exp_cpp_tpt),
    hospd_dstb_dur = if_else(year == 2021 & iso3 == "LBR", 60, hospd_dstb_dur)
  )

# * Now to correct this reporting year's values (they filled in zeros, but with no confirmation of change in protocols for dstb)
finance_merged <- finance_merged %>%
  mutate(
    hospd_dstb_dur = if_else(hospd_dstb_dur == 0 & year == 2022 & iso3 == "LBR", NA, hospd_dstb_dur),
    hospd_dstb_prct = if_else(hospd_dstb_prct == 0 & year == 2022 & iso3 == "LBR", NA, hospd_dstb_prct)
  )

# North Macedonia
# Temporary fill in backward from 2018 for North Macedonia (MKD)
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year)) %>%
  group_by(iso3) %>%
  mutate(
    exp_tot = if_else(!is.na(exp_tot) & year > 2017 & year < 2022 & iso3 == "MKD", lag(exp_tot), exp_tot),
    budget_tot = if_else(!is.na(budget_tot) & year > 2018 & year < 2023 & iso3 == "MKD", lag(budget_tot), budget_tot)
  ) %>%
  ungroup()

# * Niger (NER)
# //update expenditure and received funds 2022, with what's on the dcf. They may have made updates after the snapshot was taken
finance_merged <- finance_merged %>%
  mutate(
    exp_lab = if_else(is.na(exp_lab) & iso3 == "NER" & year == 2022, 7632, exp_lab),
    exp_prog = if_else(is.na(exp_prog) & iso3 == "NER" & year == 2022, 101861, exp_prog),
    exp_fld = if_else(is.na(exp_fld) & iso3 == "NER" & year == 2022, 640000, exp_fld),
    exp_sld = if_else(is.na(exp_sld) & iso3 == "NER" & year == 2022, 18542, exp_sld),
    exp_patsup = if_else(is.na(exp_patsup) & iso3 == "NER" & year == 2022, 31775, exp_patsup),
    exp_oth = if_else(is.na(exp_oth) & iso3 == "NER" & year == 2022, 221057, exp_oth),
    exp_tot = if_else(is.na(exp_tot) & iso3 == "NER" & year == 2022, 1020867, exp_tot),
    rcvd_tot_gf = if_else(is.na(rcvd_tot_gf) & iso3 == "NER" & year == 2021, 2072687, rcvd_tot_gf),
    rcvd_tot = if_else(is.na(rcvd_tot) & iso3 == "NER" & year == 2021, 2072687, rcvd_tot),
    rcvd_tot_domestic = if_else(is.na(rcvd_tot_domestic) & iso3 == "NER" & year == 2022, 800700, rcvd_tot_domestic),
    rcvd_tot_gf = if_else(is.na(rcvd_tot_gf) & iso3 == "NER" & year == 2022, 356006, rcvd_tot_gf),
    rcvd_tot_usaid = if_else(is.na(rcvd_tot_usaid) & iso3 == "NER" & year == 2022, 123448, rcvd_tot_usaid),
    rcvd_tot = if_else(is.na(rcvd_tot) & iso3 == "NER" & year == 2022, 1280154, rcvd_tot)
  )


# * Nigeria (NGA)- updated budget data from 11 Aug 2023. Should be unnecessary as matches reported data
finance_merged <- finance_merged %>%
  mutate(
    budget_tot = if_else(is.na(budget_tot) & iso3 == "NGA" & year == 2023, 388005000, budget_tot),
    cf_tot = if_else(is.na(cf_tot) & iso3 == "NGA" & year == 2023, 115895037, cf_tot),
    gap_tot = if_else(is.na(gap_tot) & iso3 == "NGA" & year == 2023, 272109963, gap_tot),
    cf_tot_domestic = if_else(is.na(cf_tot_domestic) & iso3 == "NGA" & year == 2023, 23766517, cf_tot_domestic),
    cf_tot_gf = if_else(is.na(cf_tot_gf) & iso3 == "NGA" & year == 2023, 67128520, cf_tot_gf),
    cf_tot_usaid = if_else(is.na(cf_tot_usaid) & iso3 == "NGA" & year == 2023, 25000000, cf_tot_usaid)
  )


# Russia (RUS)
# Temporarily in 2023, use what was committed in 2022 as expenditure in 2022
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    rcvd_tot = if_else(is.na(rcvd_tot) & iso3 == "RUS" & year == 2022, lag(cf_tot), rcvd_tot),
    exp_tot = if_else(is.na(exp_tot) & iso3 == "RUS" & year == 2022, lag(cf_tot), exp_tot)
  ) %>%
  ungroup()

# South Africa (ZAF)
# Replace rcvd_tot and exp_tot with cf_tot from the previous year (report_year - 1)
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    rcvd_tot = if_else(is.na(rcvd_tot) & iso3 == "ZAF" & year == 2022, lag(cf_tot), rcvd_tot),
    exp_tot = if_else(is.na(exp_tot) & iso3 == "ZAF" & year == 2022, lag(cf_tot), exp_tot)
  ) %>%
  ungroup()

# Senegal
# // Clear the 1500 value reported as received funds for drugs in 2022
finance_merged <- finance_merged %>%
  mutate(
    rcvd_fld = if_else(rcvd_fld == 1500 & iso3 == "SEN" & year == 2022, NA, rcvd_fld),
    rcvd_tot = if_else(rcvd_tot == 1500 & iso3 == "SEN" & year == 2022, NA, rcvd_tot),
    rcvd_tot_domestic = if_else(rcvd_tot_domestic == 1500 & iso3 == "SEN" & year == 2022, NA, rcvd_tot_domestic),
    exp_cpp_dstb = if_else(exp_cpp_dstb == 1000 & iso3 == "SEN" & year == 2022, NA, exp_cpp_dstb),
    exp_cpp_mdr = if_else(exp_cpp_mdr == 100 & iso3 == "SEN" & year == 2022, NA, exp_cpp_mdr),
    exp_cpp_xdr = if_else(exp_cpp_xdr == 10 & iso3 == "SEN" & year == 2022, NA, exp_cpp_xdr),
    exp_cpp_tpt = if_else(exp_cpp_tpt == 5 & iso3 == "SEN" & year == 2022, NA, exp_cpp_tpt),
    exp_cpp_dstb = if_else(is.na(exp_cpp_dstb) & iso3 == "SEN" & (year == 2022 | year == 2021), lag(exp_cpp_dstb), exp_cpp_dstb),
    exp_cpp_mdr = if_else(is.na(exp_cpp_mdr) & iso3 == "SEN" & (year == 2022 | year == 2021), lag(exp_cpp_mdr), exp_cpp_mdr),
    exp_cpp_xdr = if_else(is.na(exp_cpp_xdr) & iso3 == "SEN" & (year == 2022 | year == 2021), lag(exp_cpp_xdr), exp_cpp_xdr),
    exp_cpp_tpt = if_else(is.na(exp_cpp_tpt) & iso3 == "SEN" & (year == 2022 | year == 2021), lag(exp_cpp_tpt), exp_cpp_tpt)
  )

# // Vanuatu - undervalued. Awaiting clarifications from country
finance_merged <- finance_merged %>%
  mutate(
    budget_tot = if_else(budget_tot == 614 & iso3 == "VUT" & year == 2022, NA, budget_tot),
    cf_tot = if_else(cf_tot == 614 & iso3 == "VUT" & year == 2022, NA, cf_tot),
    exp_tot = if_else(exp_tot == 460 & iso3 == "VUT" & year == 2021, NA, exp_tot),
    rcvd_tot = if_else(rcvd_tot == 460 & iso3 == "VUT" & year == 2021, NA, rcvd_tot),
    exp_tot = if_else(exp_tot == 0 & iso3 == "VUT" & year == 2022, NA, exp_tot),
    rcvd_tot = if_else(rcvd_tot == 0 & iso3 == "VUT" & year == 2022, NA, rcvd_tot)
  )

# Turkmenistan (TKM)
finance_merged <- finance_merged %>%
  mutate(
    exp_tot = if_else(iso3 == "TKM" & year == 2021 & exp_tot == 0, NA, exp_tot),
    rcvd_tot = if_else(iso3 == "TKM" & year == 2021 & rcvd_tot == 0, NA, rcvd_tot)
  )

# Timor-Leste (TLS)
finance_merged <- finance_merged %>%
  mutate(
    budget_cpp_tpt = if_else(budget_cpp_tpt == 120 & year == 2023 & iso3 == "TLS", 12, budget_cpp_tpt),
    exp_cpp_tpt = if_else(exp_cpp_tpt == 5672646 & year == 2023 & iso3 == "TLS", 12, exp_cpp_tpt),
    exp_cpp_dstb = if_else(exp_cpp_dstb == 1582129 & year == 2023 & iso3 == "TLS", 24, exp_cpp_dstb),
    exp_cpp_mdr = if_else(exp_cpp_mdr == 51594 & year == 2023 & iso3 == "TLS", 31, exp_cpp_mdr)
  )


# * TUR
# //	foreach item in budget cf rcvd exp {
#   //	gsort iso3 -year
#   //	replace `item'_tot = 0 if `item'_tot == . & year > 2012 & iso3 == "TUR"
# //	}
# 
# // YEMEN
# * PN / TA Aug 2023. Yemen never reports in patient utilisation - next followup with country to check why they choose this model of care
# 

# * UKRAINE 2019/2020 fix (shown in 2020 clean file) do not seem to be saved in financing.dta. Added with hard numbers. Other included GHS 
# //so other was subtracted from total. This amount should also be removed from domestic sources.
finance_merged <- finance_merged %>%
  mutate(
    budget_tot = if_else(year == 2020 & iso3 == "UKR", 147710296 - 109986985, budget_tot),
    cf_tot = if_else(year == 2020 & iso3 == "UKR", 74982083 - 40857325, cf_tot),
    budget_oth = if_else(year == 2020 & iso3 == "UKR", 0, budget_oth),
    cf_oth = if_else(year == 2020 & iso3 == "UKR", 0, cf_oth)
    # If you need to replace values for 2019, uncomment the lines below
    # exp_tot = if_else(year == 2019 & iso3 == "UKR", 128959449 - 96628827, exp_tot),
    # rcvd_tot = if_else(year == 2019 & iso3 == "UKR", 132703841 - 97684450, rcvd_tot),
    # exp_oth = if_else(year == 2019 & iso3 == "UKR", 0, exp_oth),
    # rcvd_oth = if_else(year == 2019 & iso3 == "UKR", 0, rcvd_oth),
    # rcvd_tot_domestic = if_else(year == 2019 & iso3 == "UKR", 116680262 - 97684450, rcvd_tot_domestic),
    # rcvd_ext_gf = if_else(year == 2019 & iso3 == "UKR" & is.na(rcvd_ext_gf), 15963579, rcvd_ext_gf),
    # rcvd_tot_usaid = if_else(year == 2019 & iso3 == "UKR" & is.na(rcvd_tot_usaid), 60000, rcvd_tot_usaid)
  )
# -----------------
# 2017
# -----------------

# Update for Angola
finance_merged <- finance_merged %>%
  mutate(
    hcfvisit_dstb = ifelse(year <= 2017 & iso3 == "AGO", 4, hcfvisit_dstb),
    hcfvisit_mdr = ifelse(year <= 2017 & iso3 == "AGO", 24, hcfvisit_mdr)
  )

# Update for Burundi
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("exp_", item) := ifelse(iso3 == "BDI" & year == 2016, !!sym(paste0("budget_", item)), !!sym(paste0("exp_", item)))
    )
}

variables <- c("lab", "staff", "fld", "prog", "sld", "mdrmgt", "tbhiv", "patsup", "orsrvy", "oth", "tot", "tot_domestic", "tot_gf", "tot_usaid", "tot_grnt", "tot_sources")
for (line in variables) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("rcvd_", line) := ifelse(iso3 == "BDI" & year == 2016, !!sym(paste0("cf_", line)), !!sym(paste0("rcvd_", line)))
    )
}


# Central African Republic (CAF)
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("exp_", item) := ifelse(iso3 == "CAF" , !!sym(paste0("budget_", item)), !!sym(paste0("exp_", item)))
    )
}

for (line in variables) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("rcvd_", line) := ifelse(iso3 == "CAF", !!sym(paste0("cf_", line)), !!sym(paste0("rcvd_", line)))
    )
}

# Cabo Verde
finance_merged <- finance_merged %>%
  mutate(
    budget_staff = case_when(
      year == 2016 & iso3 == "CPV" ~ 16560,
      between(year, 2009, 2014) & iso3 == "CPV" ~ 20000,
      TRUE ~ budget_staff
    ),
    cf_staff = case_when(
      between(year, 2009, 2014) & iso3 == "CPV" ~ 20000,
      TRUE ~ cf_staff
    )
  )

# DPR Korea
finance_merged <- finance_merged %>%
  mutate(
    exp_cpp_xdr = ifelse(iso3 == "PRK" & year == 2016, NA, exp_cpp_xdr),
    hcfvisit_mdr = ifelse(iso3 == "PRK" & year == 2016, NA, hcfvisit_mdr)
  )

# India
finance_merged <- finance_merged %>%
  mutate(
    cf_tot_domestic = ifelse(iso3 == "IND" & year == 2017, 387345254, cf_tot_domestic),
    cf_tot_gf = ifelse(iso3 == "IND" & year == 2017, 135000000, cf_tot_gf)
  )

# Marshall Islands
finance_merged <- finance_merged %>%
  mutate(
    hcfvisit_dstb = ifelse(year == 2016 & iso3 == "MHL", 3, hcfvisit_dstb),
    hcfvisit_mdr = ifelse(year == 2016 & iso3 == "MHL", 20, hcfvisit_mdr)
  )

# Panama
finance_merged <- finance_merged %>%
  mutate(
    hcfvisit_dstb = ifelse(iso3 == "PAN" & year == 2014, 108, hcfvisit_dstb)
  )

# Papua New Guinea (PNG)
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("exp_", item) := ifelse(iso3 == "PNG", !!sym(paste0("budget_", item)), !!sym(paste0("exp_", item))),
      !!paste0("rcvd_", item) := ifelse(iso3 == "PNG", !!sym(paste0("cf_", item)), !!sym(paste0("rcvd_", item))),
    )
}


# South Africa (ZAF)
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("exp_", item) := ifelse(iso3 == "ZAF" , !!sym(paste0("budget_", item)), !!sym(paste0("exp_", item)))
    )
}

for (line in variables) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("rcvd_", line) := ifelse(iso3 == "ZAF", !!sym(paste0("cf_", line)), !!sym(paste0("rcvd_", line)))
    )
}

finance_merged <- finance_merged %>%
  mutate(
    budget_lab = if_else(year == 2016 & iso3 == "ZAF", 113793471, budget_lab),
    cf_lab = if_else(year == 2016 & iso3 == "ZAF", 113793471, cf_lab),
    budget_tbhiv = if_else(year == 2016 & iso3 == "ZAF", 8391981, budget_tbhiv),
    cf_tbhiv = if_else(year == 2016 & iso3 == "ZAF", 8391981, cf_tbhiv),
    budget_tot = if_else(year == 2016 & iso3 == "ZAF", 299991836, budget_tot),
    cf_tot = if_else(year == 2016 & iso3 == "ZAF", 278413032, cf_tot),
    cf_tot_domestic = if_else(year == 2016 & iso3 == "ZAF", 243559597, cf_tot_domestic),
    cf_tot_sources = if_else(year == 2016 & iso3 == "ZAF", 278413032, cf_tot_sources)
  )

# Swaziland / Eswatini
finance_merged <- finance_merged %>%
  mutate(
    budget_orsrvy = ifelse(iso3 == "SWZ" & year == 2016, 2331961, budget_orsrvy),
    budget_tot = ifelse(iso3 == "SWZ" & year == 2016, 19254521, budget_tot)
  )

# Tuvalu
finance_merged <- finance_merged %>%
  mutate(
    hcfvisit_dstb = ifelse(iso3 == "TUV" & year == 2016, 168, hcfvisit_dstb)
  )

# Vanuatu
finance_merged <- finance_merged %>%
  mutate(
    exp_cpp_dstb = if_else(iso3 == "VUT" & year == 2016, 61, exp_cpp_dstb),
    exp_fld = if_else(iso3 == "VUT" & year == 2016, 6100, exp_fld),
    rcvd_fld = if_else(iso3 == "VUT" & year == 2016, 6100, rcvd_fld),
    exp_tot = if_else(iso3 == "VUT" & year == 2016, 292909, exp_tot),
    rcvd_tot = if_else(iso3 == "VUT" & year == 2016, 292909, rcvd_tot),
    rcvd_tot_domestic = if_else(iso3 == "VUT" & year == 2016, 235793, rcvd_tot_domestic),
    rcvd_tot_sources = if_else(iso3 == "VUT" & year == 2016, 292909, rcvd_tot_sources)
  )

# revising multiple countries
# //Changes after the deadline during country profile review
# //budget
finance_merged <- finance_merged %>%
  mutate(
    budget_lab = if_else(iso2 == "DO" & year == 2017, 1410044, budget_lab),
    budget_mdrmgt = if_else(iso2 == "DO" & year == 2017, 900000, budget_mdrmgt),
    budget_orsrvy = if_else(iso2 == "DO" & year == 2017, 350000, budget_orsrvy),
    budget_oth = if_else(iso2 == "DO" & year == 2017, 700000, budget_oth),
    budget_patsup = if_else(iso2 == "DO" & year == 2017, 950000, budget_patsup),
    budget_prog = if_else(iso2 == "DO" & year == 2017, 1100000, budget_prog),
    budget_tbhiv = if_else(iso2 == "DO" & year == 2017, 950000, budget_tbhiv),
    budget_tot = if_else(iso2 == "DO" & year == 2017, 14524294, budget_tot),
    gap_lab = if_else(iso2 == "DO" & year == 2017, 700000, gap_lab),
    gap_mdrmgt = if_else(iso2 == "DO" & year == 2017, 200000, gap_mdrmgt),
    gap_orsrvy = if_else(iso2 == "DO" & year == 2017, 200000, gap_orsrvy),
    gap_oth = if_else(iso2 == "DO" & year == 2017, 300000, gap_oth),
    gap_patsup = if_else(iso2 == "DO" & year == 2017, 300000, gap_patsup),
    gap_prog = if_else(iso2 == "DO" & year == 2017, 200000, gap_prog),
    gap_tbhiv = if_else(iso2 == "DO" & year == 2017, 500000, gap_tbhiv),
    gap_tot = if_else(iso2 == "DO" & year == 2017, 2400000, gap_tot),
    cf_tot_domestic = if_else(iso2 == "IN" & year == 2017, 387345254, cf_tot_domestic),
    cf_tot_gf = if_else(iso2 == "IN" & year == 2017, 135000000, cf_tot_gf),
    budget_lab = if_else(iso2 == "KE" & year == 2017, 3620428, budget_lab),
    budget_mdrmgt = if_else(iso2 == "KE" & year == 2017, 2444835, budget_mdrmgt),
    budget_orsrvy = if_else(iso2 == "KE" & year == 2017, 3087050, budget_orsrvy),
    budget_oth = if_else(iso2 == "KE" & year == 2017, 8309758, budget_oth),
    budget_patsup = if_else(iso2 == "KE" & year == 2017, 6401372, budget_patsup),
    budget_prog = if_else(iso2 == "KE" & year == 2017, 19431319, budget_prog),
    budget_tbhiv = if_else(iso2 == "KE" & year == 2017, 4675980, budget_tbhiv),
    budget_tot = if_else(iso2 == "KE" & year == 2017, 62202386, budget_tot),
    gap_lab = if_else(iso2 == "KE" & year == 2017, 2173948, gap_lab),
    gap_mdrmgt = if_else(iso2 == "KE" & year == 2017, 874844, gap_mdrmgt),
    gap_orsrvy = if_else(iso2 == "KE" & year == 2017, 2636210, gap_orsrvy),
    gap_oth = if_else(iso2 == "KE" & year == 2017, 3694116, gap_oth),
    gap_patsup = if_else(iso2 == "KE" & year == 2017, 6362571, gap_patsup),
    gap_prog = if_else(iso2 == "KE" & year == 2017, 5403752, gap_prog),
    gap_tbhiv = if_else(iso2 == "KE" & year == 2017, 4491796, gap_tbhiv),
    gap_tot = if_else(iso2 == "KE" & year == 2017, 25637237, gap_tot),
    budget_fld = if_else(iso2 == "NP" & year == 2017, 857142, budget_fld),
    budget_mdrmgt = if_else(iso2 == "NP" & year == 2017, 1058933, budget_mdrmgt),
    budget_oth = if_else(iso2 == "NP" & year == 2017, 655556, budget_oth),
    budget_sld = if_else(iso2 == "NP" & year == 2017, 1478424, budget_sld),
    budget_staff = if_else(iso2 == "NP" & year == 2017, 1703925, budget_staff),
    budget_tot = if_else(iso2 == "NP" & year == 2017, 17344529, budget_tot),
    cf_fld = if_else(iso2 == "NP" & year == 2017, 857142, cf_fld),
    cf_mdrmgt = if_else(iso2 == "NP" & year == 2017, 1058933, cf_mdrmgt),
    cf_oth = if_else(iso2 == "NP" & year == 2017, 655556, cf_oth),
    cf_sld = if_else(iso2 == "NP" & year == 2017, 1478424, cf_sld),
    cf_staff = if_else(iso2 == "NP" & year == 2017, 1703925, cf_staff),
    cf_tot = if_else(iso2 == "NP" & year == 2017, 17344529, cf_tot),
    cf_tot_domestic = if_else(iso2 == "NP" & year == 2017, 10215981, cf_tot_domestic),
    cf_tot_gf = if_else(iso2 == "NP" & year == 2017, 6961972, cf_tot_gf),
    cf_tot_sources = if_else(iso2 == "NP" & year == 2017, 17344529, cf_tot_sources),
    cf_lab = if_else(iso2 == "PH" & year == 2017, 21959530, cf_lab),
    cf_oth = if_else(iso2 == "PH" & year == 2017, 3798047, cf_oth),
    cf_patsup = if_else(iso2 == "PH" & year == 2017, 11607680, cf_patsup),
    cf_prog = if_else(iso2 == "PH" & year == 2017, 12404208, cf_prog),
    cf_staff = if_else(iso2 == "PH" & year == 2017, 5665811, cf_staff),
    cf_tot = if_else(iso2 == "PH" & year == 2017, 75378393, cf_tot),
    cf_tot_sources = if_else(iso2 == "PH" & year == 2017, 75378393, cf_tot_sources),
    gap_lab = if_else(iso2 == "PH" & year == 2017, 6071323, gap_lab),
    gap_oth = if_else(iso2 == "PH" & year == 2017, 670960, gap_oth),
    gap_patsup = if_else(iso2 == "PH" & year == 2017, 20124270, gap_patsup),
    gap_prog = if_else(iso2 == "PH" & year == 2017, 103428, gap_prog),
    gap_staff = if_else(iso2 == "PH" & year == 2017, 339267, gap_staff),
    gap_tot = if_else(iso2 == "PH" & year == 2017, 28853018, gap_tot),
    budget_patsup = if_else(iso2 == "RU" & year == 2017, 5962011, budget_patsup),
    budget_tot = if_else(iso2 == "RU" & year == 2017, 1175429504, budget_tot),
    cf_patsup = if_else(iso2 == "RU" & year == 2017, 5962011, cf_patsup),
    cf_tot = if_else(iso2 == "RU" & year == 2017, 1175429504, cf_tot),
    cf_tot_domestic = if_else(iso2 == "RU" & year == 2017, 1175429504, cf_tot_domestic),
    cf_tot_sources = if_else(iso2 == "RU" & year == 2017, 1175429504, cf_tot_sources)
  )


# expenditure
finance_merged <- finance_merged %>%
  mutate(
    hosp_type_mdr = if_else(iso2 == "JM" & year == 2016, 142, hosp_type_mdr),
    exp_oth = if_else(iso2 == "PH" & year == 2016, 2824580, exp_oth),
    exp_tot = if_else(iso2 == "PH" & year == 2016, 59463013, exp_tot),
    rcvd_oth = if_else(iso2 == "PH" & year == 2016, 2898980, rcvd_oth),
    rcvd_tot = if_else(iso2 == "PH" & year == 2016, 60628704, rcvd_tot),
    rcvd_tot_sources = if_else(iso2 == "PH" & year == 2016, 60628704, rcvd_tot_sources),
    exp_patsup = if_else(iso2 == "RU" & year == 2016, 5962011, exp_patsup),
    exp_tot = if_else(iso2 == "RU" & year == 2016, 1175137183, exp_tot),
    rcvd_patsup = if_else(iso2 == "RU" & year == 2016, 5962011, rcvd_patsup),
    rcvd_tot = if_else(iso2 == "RU" & year == 2016, 1175429504, rcvd_tot),
    rcvd_tot_domestic = if_else(iso2 == "RU" & year == 2016, 1173778731, rcvd_tot_domestic),
    rcvd_tot_sources = if_else(iso2 == "RU" & year == 2016, 1175429504, rcvd_tot_sources),
    exp_tot = if_else(iso2 == "LC" & year == 2016, 10000, exp_tot),
    hospd_mdr_prct = if_else(iso2 == "VN" & year == 2016, 90, hospd_mdr_prct),
    cf_lab = if_else(year == 2017 & iso3 == "IDN", 18570842, cf_lab),
    cf_fld = if_else(year == 2017 & iso3 == "IDN", 16923077, cf_fld),
    cf_prog = if_else(year == 2017 & iso3 == "IDN", 4209785, cf_prog),
    cf_tot = if_else(year == 2017 & iso3 == "IDN", 86972963, cf_tot),
    gap_lab = if_else(year == 2017 & iso3 == "IDN", 7270429, gap_lab),
    gap_fld = if_else(year == 2017 & iso3 == "IDN", 1337723, gap_fld),
    gap_prog = if_else(year == 2017 & iso3 == "IDN", 25450096, gap_prog),
    gap_tot = if_else(year == 2017 & iso3 == "IDN", 98474163, gap_tot),
    cf_tot_domestic = if_else(year == 2017 & iso3 == "IDN", 52961190, cf_tot_domestic),
    cf_tot_gf = if_else(year == 2017 & iso3 == "IDN", 26673139, cf_tot_gf),
    cf_tot_usaid = if_else(year == 2017 & iso3 == "IDN", 7338634, cf_tot_usaid),
    cf_tot_sources = if_else(year == 2017 & iso3 == "IDN", 86972963, cf_tot_sources),
    rcvd_tot_domestic = if_else(year == 2016 & iso3 == "IDN", 48787964, rcvd_tot_domestic),
    rcvd_tot_gf = if_else(year == 2016 & iso3 == "IDN", 43719570, rcvd_tot_gf)
  )



# //federal stats of micronesia, Jamaica, Madagascar, Marshall Islands, Mauritania, Somalia, Somomon Islands
finance_merged <- finance_merged %>%
  mutate(
    hcfvisit_dstb = if_else(iso3 == "FSM" & year == 2017, 5, hcfvisit_dstb),
    hospd_dstb_dur = if_else(iso3 == "FSM" & year == 2017, 21, hospd_dstb_dur),
    hospd_mdr_dur = if_else(iso3 == "FSM" & year == 2017, 90, hospd_mdr_dur),
    exp_fld = if_else(iso3 == "JAM" & year == 2017, 25767.44, exp_fld),
    rcvd_fld = if_else(iso3 == "JAM" & year == 2017, 25767.44, rcvd_fld),
    budget_cpp_mdr = if_else(iso3 == "MDG" & year == 2018, 0, budget_cpp_mdr),
    budget_cpp_dstb = if_else(iso3 == "MHL" & year == 2018, NA, budget_cpp_dstb),
    budget_cpp_mdr = if_else(iso3 == "MHL" & year == 2018, NA, budget_cpp_mdr),
    exp_lab = if_else(iso3 == "MRT" & year == 2017, rcvd_lab, exp_lab),
    exp_staff = if_else(iso3 == "MRT" & year == 2017, rcvd_staff, exp_staff),
    exp_fld = if_else(iso3 == "MRT" & year == 2017, rcvd_fld, exp_fld),
    exp_prog = if_else(iso3 == "MRT" & year == 2017, rcvd_prog, exp_prog),
    exp_sld = if_else(iso3 == "MRT" & year == 2017, rcvd_sld, exp_sld),
    exp_mdrmgt = if_else(iso3 == "MRT" & year == 2017, rcvd_mdrmgt, exp_mdrmgt),
    exp_tbhiv = if_else(iso3 == "MRT" & year == 2017, rcvd_tbhiv, exp_tbhiv),
    exp_patsup = if_else(iso3 == "MRT" & year == 2017, rcvd_patsup, exp_patsup),
    exp_orsrvy = if_else(iso3 == "MRT" & year == 2017, rcvd_orsrvy, exp_orsrvy),
    exp_oth = if_else(iso3 == "MRT" & year == 2017, rcvd_oth, exp_oth),
    exp_tot = if_else(iso3 == "MRT" & year == 2017, rcvd_tot, exp_tot),
    budget_lab = if_else(iso3 == "SOM" & year == 2018, cf_lab, budget_lab),
    budget_staff = if_else(iso3 == "SOM" & year == 2018, cf_staff, budget_staff),
    budget_fld = if_else(iso3 == "SOM" & year == 2018, cf_fld, budget_fld),
    budget_prog = if_else(iso3 == "SOM" & year == 2018, cf_prog, budget_prog),
    budget_sld = if_else(iso3 == "SOM" & year == 2018, cf_sld, budget_sld),
    budget_mdrmgt = if_else(iso3 == "SOM" & year == 2018, cf_mdrmgt, budget_mdrmgt),
    budget_tbhiv = if_else(iso3 == "SOM" & year == 2018, cf_tbhiv, budget_tbhiv),
    budget_patsup = if_else(iso3 == "SOM" & year == 2018, cf_patsup, budget_patsup),
    budget_orsrvy = if_else(iso3 == "SOM" & year == 2018, cf_orsrvy, budget_orsrvy),
    budget_oth = if_else(iso3 == "SOM" & year == 2018, cf_oth, budget_oth),
    budget_tot = if_else(iso3 == "SOM" & year == 2018, cf_tot, budget_tot),
    hcfvisit_mdr = if_else(iso3 == "SLB" & year >= 2016, 270, hcfvisit_mdr),
    hospd_mdr_dur = if_else(iso3 == "SLB" & year >= 2016, 270, hospd_mdr_dur),
    hospd_mdr_prct = if_else(iso3 == "SLB" & year >= 2016, 100, hospd_mdr_prct)
  )


# //Tuvalu reported in Australian dollars 1 AUD = .75 USD
finance_merged <- finance_merged %>%
  mutate(
    across(starts_with("budget_"), ~ if_else(iso3 == "TUV" & year == 2018, . * 0.75, .)),
    across(starts_with("cf_"), ~ if_else(iso3 == "TUV" & year == 2017, . * 0.75, .))
  )


#---------------------
#  2020
#---------------------
# Armenia (ARM): replace budget 2019 with expenditure 2018 (budget 2019 appears understated).
for (item in suffix) {
  finance_merged <- finance_merged %>%
    arrange(iso3, year) %>%
    mutate(
      !!sym(paste0("budget_", item)) := if_else(iso3 == "ARM" & year == 2019 ,
                                              lag(!!sym(paste0("exp_", item))), !!sym(paste0("budget_", item)))
    )
}

  # // Azerbaijan (AZE): For budget and expenditure breakdown, the country to use updated total amounts minus drugs and apply breakdown from 2019 expenditures 
  # // then add drug amounts reported separately (in 2020 to update historic data)
  # ** PN Jul 2020: Budget data has been provided by country, in long form, no need to modify anything **
  #   // Missing budget_tot will be imputed since there are 5 records so far (PN confirmed)
  # gsort iso3 year //(sorted ascending with 2020 last (i.e. 19th place with 2002 as first?)
  #                    li budget_lab budget_staff budget_fld budget_prog budget_sld budget_mdrmgt budget_tpt budget_tbhiv budget_patsup budget_orsrvy budget_oth budget_tot if iso3 == "AZE"
  #                    li exp_lab exp_staff exp_fld exp_prog exp_sld exp_mdrmgt exp_tpt exp_tbhiv exp_patsup exp_orsrvy exp_oth exp_tot if iso3 == "AZE" 
  #                    
  #                    // Algeria (DZA): Have only reported GHS data. 
  #                    li iso3 year budget_tot cf_tot exp_tot rcvd_tot c_notified hcfvisit_mdr hcfvisit_dstb hospd_dstb_dur hospd_mdr_dur  if iso3 == "DZA"
  #                    // With 7 values for budget_tot in the past, this may still get imputed (PN confirmed)
  #                    
  #                    // Belarus (BLR):
  #                      // PN Make hard coded updates based on audit done in 2020 (ref Leia's excel worksheets) 
  # ** Those figures include expenditures data for all in- and out-patient treatment , which are allocated under lines "NTP Staff", "MDR-TB: Program Costs", "DS-TB: Program Cost"
  # ** historical updates affecting exp and rcvd data 2016 - 2018
  # 
  # li year exp_fld exp_staff exp_lab exp_tbhiv exp_sld exp_mdrmgt exp_oth  exp_prog ///
  #  exp_orsrvy exp_patsup exp_tot if iso3 == "BLR" & year >= 2015
  # /** 2016
  # replace exp_lab = 2021186 if iso3 == "BLR" & year == 2016 
  # replace exp_staff = 18690711 if iso3 == "BLR" & year == 2016 
  # replace exp_fld = 154400 if iso3 == "BLR" & year == 2016 
  # replace exp_prog = 18795904 if iso3 == "BLR" & year == 2016 
  # replace exp_sld = 12936659 if iso3 == "BLR" & year == 2016 
  # replace exp_mdrmgt = 15245689 if iso3 == "BLR" & year == 2016 
  # replace exp_tbhiv = 375297 if iso3 == "BLR" & year == 2016 
  # replace exp_patsup = 336394 if iso3 == "BLR" & year == 2016 
  # replace exp_orsrvy = 189473 if iso3 == "BLR" & year == 2016 
  # replace exp_oth = 520884 if iso3 == "BLR" & year == 2016 
  # replace exp_tot = 69266597 if iso3 == "BLR" & year == 2016 
  # 
  # ** 2017
  # replace exp_lab = 1273722 if iso3 == "BLR" & year == 2017 
  # replace exp_staff = 25224550 if iso3 == "BLR" & year == 2017 
  # replace exp_fld = 127000 if iso3 == "BLR" & year == 2017 
  # replace exp_prog = 18639507 if iso3 == "BLR" & year == 2017 
  # replace exp_sld = 8426229 if iso3 == "BLR" & year == 2017 
  # replace exp_mdrmgt = 15949239 if iso3 == "BLR" & year == 2017 
  # replace exp_tbhiv = 461486 if iso3 == "BLR" & year == 2017 
  # replace exp_patsup = 334641 if iso3 == "BLR" & year == 2017 
  # replace exp_orsrvy = 202802 if iso3 == "BLR" & year == 2017 
  # replace exp_oth = 520116 if iso3 == "BLR" & year == 2017 
  # replace exp_tot = 71159292 if iso3 == "BLR" & year == 2017 
  # 
  # ** 2018
  # replace exp_lab = 1841652 if iso3 == "BLR" & year == 2018 
  # replace exp_staff = 27358864 if iso3 == "BLR" & year == 2018 
  # replace exp_fld = 106150 if iso3 == "BLR" & year == 2018 
  # replace exp_prog = 16362125 if iso3 == "BLR" & year == 2018 
  # replace exp_sld = 7818988 if iso3 == "BLR" & year == 2018 
  # replace exp_mdrmgt = 14902823 if iso3 == "BLR" & year == 2018 
  # replace exp_tbhiv = 427908 if iso3 == "BLR" & year == 2018 
  # replace exp_patsup = 321153 if iso3 == "BLR" & year == 2018 
  # replace exp_orsrvy = 149230 if iso3 == "BLR" & year == 2018 
  # replace exp_oth = 1476759 if iso3 == "BLR" & year == 2018 
  # replace exp_tot = 70765652 if iso3 == "BLR" & year == 2018 
  # 
  # ** Note that 2019 expenditure data was correctly reported and is not adjusted in this hard code.
  # 
  # * Update budget and cf fields in prev years (make equal to exp).
  # sort iso3 year 
  # foreach var in $suffix {
  # 	replace rcvd_`var' = exp_`var' if iso3 == "BLR" & year >= 2016 & year <=2018 
  # 	replace cf_`var' = exp_`var'[_n - 1] if iso3 == "BLR" & year >= 2017 & year <=2019 
  # 	replace budget_`var' = exp_`var'[_n - 1] if iso3 == "BLR" & year >= 2017 & year <=2019
  # 	replace gap_`var' = 0 if iso3 == "BLR" & year >= 2017 & year <=2019
  #                                                                                 }
  # 
  # ** Sources of funds
  # replace rcvd_tot_domestic = 64032000 if iso3 == "BLR" & year == 2016 
  # replace rcvd_tot_gf = 4713714 if iso3 == "BLR" & year == 2016 
  # replace rcvd_tot_grnt = 520884 if iso3 == "BLR" & year == 2016 
  # replace rcvd_tot_usaid = 0 if iso3 == "BLR" & year == 2016 
  # replace rcvd_tot_sources = 69266598 if iso3 == "BLR" & year == 2016 
  # 
  # replace rcvd_tot_domestic = 67056000 if iso3 == "BLR" & year == 2017 
  # replace rcvd_tot_gf = 3583176 if iso3 == "BLR" & year == 2017 
  # replace rcvd_tot_grnt = 520116 if iso3 == "BLR" & year == 2017 
  # replace rcvd_tot_usaid = 0 if iso3 == "BLR" & year == 2017 
  # replace rcvd_tot_sources = 71159292 if iso3 == "BLR" & year == 2017 
  # 
  # replace rcvd_tot_domestic = 65878200 if iso3 == "BLR" & year == 2018 
  # replace rcvd_tot_gf = 3410692 if iso3 == "BLR" & year == 2018 
  # replace rcvd_tot_grnt = 1476759 if iso3 == "BLR" & year == 2018 
  # replace rcvd_tot_usaid = 0 if iso3 == "BLR" & year == 2018 
  # replace rcvd_tot_sources = 70765651 if iso3 == "BLR" & year == 2018 
  # 
  # replace cf_tot_domestic = rcvd_tot_domestic[_n-1] if iso3 == "BLR" & year >= 2017 & year <=2019
  # replace cf_tot_gf = rcvd_tot_gf[_n-1] if iso3 == "BLR" & year >= 2017 & year <=2019
  # replace cf_tot_grnt = rcvd_tot_grnt[_n-1] if iso3 == "BLR" & year >= 2017 & year <=2019
  # replace cf_tot_usaid = rcvd_tot_usaid[_n-1] if iso3 == "BLR" & year >= 2017 & year <=2019
  # replace cf_tot_sources = rcvd_tot_sources[_n-1] if iso3 == "BLR" & year >= 2017 & year <=2019
  # 
  # */

# Bosjnia BIH: fill in with previous years data: CF_tot2020=Cf_tot2019, Exp_2019=CF_2019
# finance_merged <- finance_merged %>% 
#   arrange(iso3, year) %>%
#   mutate(
#     cf_tot = if_else(year == 2020 & iso3 == "BIH", lag(cf_tot), cf_tot),
#     budget_tot = if_else(year == 2020 & iso3 == "BIH", lag(cf_tot), budget_tot),
#     exp_tot = if_else(year == 2019 & iso3 == "BIH", lag(cf_tot), exp_tot),
#     rcvd_tot = if_else(year == 2019 & iso3 == "BIH", lag(cf_tot), rcvd_tot)
#   )


# Botswana (BwA): Fill in 2020 cf_tot 2020 based on c_notified2019 (PN used tx_dstb instead since there's no c_notified )
# and cf_tot2019. rcvd_*2019 based on rcvd_* 2018 
finance_merged <- finance_merged %>%
  mutate(
    cf_tot = if_else(is.na(cf_tot) & iso3 == "BWA" & year == 2020,
                     lag(cf_tot) * round(tx_dstb / lag(tx_dstb)), cf_tot),
    budget_tot = if_else(is.na(budget_tot) & iso3 == "BWA" & year == 2020,
                         lag(budget_tot) * round(tx_dstb / lag(tx_dstb)), budget_tot)
  )

for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!sym(paste0("rcvd_", item)) := if_else(is.na(!!sym(paste0("rcvd_", item))) & iso3 == "BWA" & year == 2019,
                                              lag(!!sym(paste0("rcvd_", item))), !!sym(paste0("rcvd_", item)))
    )
}

# CHad (TCD): Fill in exp with received funding: exp_* = rcvd_* if exp_* is missing for 2019
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!sym(paste0("exp_", item)) := if_else(is.na(!!sym(paste0("exp_", item))) & iso3 == "TCD" & year == 2019,
                                             !!sym(paste0("rcvd_", item)), !!sym(paste0("exp_", item)))
    )
}

# // Chile (CHL): use budget_tot instead of cf_tot, for 2015-2020. Short form report
# // PN: CHL being HIC, shouldn't have gap_tot values?
# // Cases notified in 2019 = blank. Shall be carried forward from 2018
finance_merged <- finance_merged %>%
  mutate(
    cf_tot = if_else(year >= 2014 & iso3 == "CHL", budget_tot, cf_tot),
    gap_tot = if_else(year >= 2014 & iso3 == "CHL", NA_real_, gap_tot)
  )

# Colombia (COL): correct hc_fvisit_mdr from 2013-2019 to 350.
# MDR years 2013-2018 should be max 350
finance_merged <- finance_merged %>%
  mutate(
    hcfvisit_mdr = if_else(year >= 2013 & iso3 == "COL", 350, hcfvisit_mdr)
  )


# Comoros (COM): Budget_tot 2020 = budget_tot2019, exp_tot2020=exp_tot2019
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  mutate(
    budget_tot = if_else(iso3 == "COM" & year == 2020 & is.na(budget_tot), lag(budget_tot), budget_tot),
    cf_tot = if_else(iso3 == "COM" & year == 2020 & is.na(cf_tot), budget_tot, cf_tot),
    exp_tot = if_else(iso3 == "COM" & year == 2019 & is.na(exp_tot), lag(exp_tot), exp_tot),
    rcvd_tot = if_else(iso3 == "COM" & year == 2019 & is.na(rcvd_tot), exp_tot, rcvd_tot)
  )

# Cuba: fill rcvd_tot with exp_tot
finance_merged <- finance_merged %>%
  mutate(
    rcvd_tot = if_else(is.na(rcvd_tot) & iso3 == "CUB" & year == 2019, exp_tot, rcvd_tot),
    cf_tot = if_else(is.na(cf_tot) & iso3 == "CUB" & year == 2020, budget_tot, cf_tot)
  )

# Djibouti (DJI):fill in budget_oth=cf_oth in 2020. 
finance_merged <- finance_merged %>%
  mutate(
    budget_oth = if_else(year == 2020 & iso3 == "DJI", cf_oth, budget_oth)
  )

# //DPRK Jun 2020: PRK's gdp I$ (const 2017) data and US$ (from WB and IMF) is all blank. Replace with data from 
# // World CIA Factbook https://www.cia.gov/library/publications/the-world-factbook/geos/kn.html
# // as curated by Index Mundi in I$ (const 2015). No deflation index too.
# //		Country	1999	2000	2002	2003	2004	2005	2006	2007	2008	2009	2011	2014	2015
# //	North Korea	1,000	1,000	1,000	1,300	1,700	1,700	1,800	1,700	1,800	1,800	1,800	1,800	1,700
# ** Estimating gross national product in North Korea is a difficult task because of a dearth of economic data and 
# ** the problem of choosing an appropriate rate of exchange for the North Korean won, the nonconvertible North Korean currency.
finance_merged <- finance_merged %>%
  mutate(
    imf_gdp_pc_cur_int = case_when(
      iso3 == "PRK" & year == 2002 ~ 1000,
      iso3 == "PRK" & year == 2003 ~ 1300,
      iso3 == "PRK" & (year == 2004 | year == 2005) ~ 1700,
      iso3 == "PRK" & year == 2006 ~ 1800,
      iso3 == "PRK" & (year == 2007 | year == 2015) ~ 1700,
      iso3 == "PRK" & year >= 2008 & year < 2015 ~ 1800,
      TRUE ~ imf_gdp_pc_cur_int  # Keep other cases unchanged
    )
  )

# Gambia (GMB): Budget and cf missing for 2020
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year)) %>%
  mutate(
    budget_tot = if_else(is.na(budget_tot) & year == 2020 & iso3 == "GMB", lag(rcvd_tot), budget_tot),
    cf_tot = if_else(is.na(cf_tot) & year == 2020 & iso3 == "GMB", budget_tot, cf_tot),
    budget_cpp_mdr = if_else(year == 2020 & iso3 == "GMB", NA, budget_cpp_mdr)
  )

# Guatemala 
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!sym(paste0("budget_", item)) := if_else(iso3 == "GTM" & year >= 2019 & get(paste0("gap_", item)) < 0, 
                                                get(paste0("cf_", item)), 
                                                !!sym(paste0("budget_", item))),
      !!sym(paste0("gap_", item)) := if_else(iso3 == "GTM" & year >= 2019, 
                                             get(paste0("budget_", item)) - get(paste0("cf_", item)), 
                                             !!sym(paste0("gap_", item)))
    )
}

# Iran (IRN): Budget not available, use sum of 4.3= dx_*  x  budget_cpp_*.
# // PN: This is done only for 2019 & 2020.
finance_merged <- finance_merged %>%
  mutate(
    budget_tot = if_else(year == 2020 & iso3 == "IRN" & is.na(budget_tot),
                         (budget_cpp_dstb * tx_dstb) + (budget_cpp_mdr * tx_mdr) +
                           (budget_cpp_xdr * tx_xdr) + (budget_cpp_tpt * tx_tpt),
                         budget_tot),
    budget_tot = if_else(year == 2019 & iso3 == "IRN" & is.na(budget_tot),
                         (budget_cpp_dstb * tx_dstb) + (budget_cpp_mdr * tx_mdr) +
                           (budget_cpp_xdr * tx_xdr),
                         budget_tot)
  )

# Lesotho (LSO): Change hospd_mdr_prct= 91% ( in 2018) with hospd_mdr_prct= 30%; also change hospd_mdr_dur =30 in 2018 with hospd_mdr_dur=21 
finance_merged <- finance_merged %>%
  mutate(
    hospd_mdr_prct = if_else(year == 2018 & iso3 == "LSO", 30, hospd_mdr_prct),
    hospd_mdr_dur = if_else(year == 2018 & iso3 == "LSO", 21, hospd_mdr_dur)
  )

# Libya (LBY):  cf_*=budget_*   for 2020.  
finance_merged <- finance_merged %>%
  arrange(iso3, year) 

for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!sym(paste0("cf_", item)) := if_else(.data[[paste0("cf_", item)]] == 0 & iso3 == "LBY" & year == 2020, 
                                            .data[[paste0("budget_", item)]], 
                                            .data[[paste0("cf_", item)]]),
      !!sym(paste0("gap_", item)) := if_else(iso3 == "LBY" & year == 2020, 
                                             0, 
                                             .data[[paste0("gap_", item)]]),
      !!sym(paste0("exp_", item)) := if_else(iso3 == "LBY" & year == 2020, 
                                             lag(.data[[paste0("exp_", item)]]), 
                                             .data[[paste0("exp_", item)]])
    )
}

# Mauritania: Fill hospd_* fill in with last year data. 
# Fill in budget_* with cf_* if budget_*=. & year==2020
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!sym(paste0("budget_", item)) := if_else(iso3 == "MRT" & year == 2020 & is.na(.data[[paste0("budget_", item)]]), 
                                                .data[[paste0("cf_", item)]], 
                                                .data[[paste0("budget_", item)]]),
      !!sym(paste0("gap_", item)) := if_else(iso3 == "MRT" & year == 2020, 
                                             .data[[paste0("budget_", item)]] - .data[[paste0("cf_", item)]], 
                                             .data[[paste0("gap_", item)]])
    )
}

utilization <- c("hcfvisit_dstb", "hcfvisit_mdr", "hospd_dstb_prct", "hospd_mdr_prct", "hospd_dstb_dur", "hospd_mdr_dur")

finance_merged <- finance_merged %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(across(all_of(utilization), ~ ifelse(iso3 == "MRT" & year == 2020, lag(.), .))) %>%
  ungroup()


# // Federal state of micronesia: use previous year's values for utilization
# // Note from country: Patients are visited at home for DOT, can have 5 visits to the clinic for sputum collection.
# // Non MDR=TB patients can be hospitalized up to 3 weeks
# // MDR-TB patients are hospitalized up to 2-3  months
finance_merged <- finance_merged %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(across(all_of(utilization), ~ ifelse(iso3 == "FSM" & year == 2020, lag(.), .))) %>%
  ungroup()

# Moldova : Use 2019 expenditure data to replace 2020 expected funding lines for everyhting 
# //except FLD, SLD, patient support and operational research
vars_to_replace <- c("lab", "staff", "prog", "mdrmgt", "tbhiv", "oth")

for (var in vars_to_replace) {
  finance_merged <- finance_merged %>%
    mutate(!!paste0("cf_", var) := ifelse(iso3 == "MDA" & year == 2020, lag(!!sym(paste0("exp_", var))), !!sym(paste0("cf_", var))))
}

# Nauru: budget_tot 2020 = budget_tot 2019. Utilization 2019 as utilization 2018
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%  # Sorting dataframe by iso3 and year (2020 last)
  mutate(
    budget_tot = if_else(iso3 == "NRU" & year == 2020 & is.na(budget_tot), lag(budget_tot), budget_tot),
    cf_tot = if_else(iso3 == "NRU" & year == 2020 & is.na(cf_tot), budget_tot, cf_tot),
    exp_tot = if_else(iso3 == "NRU" & year == 2019 & is.na(exp_tot), lag(exp_tot), exp_tot),
    rcvd_tot = if_else(iso3 == "NRU" & year == 2019 & is.na(rcvd_tot), exp_tot, rcvd_tot)
  )

finance_merged <- finance_merged %>%
  arrange(iso3, year)

for (var in c("hcfvisit_dstb", "hcfvisit_mdr", "hospd_dstb_prct", "hospd_mdr_prct", "hospd_dstb_dur", "hospd_mdr_dur")) {
  finance_merged <- finance_merged %>%
    mutate(
      !!var := if_else(iso3 == "NRU" & year == 2020 & is.na(!!sym(var)), lag(!!sym(var)), !!sym(var))
    )
}

# Panama: Use tx_dstb to scale rcvd_tot 2019 (to estimate av funding 2020)
finance_merged <- finance_merged %>%
  arrange(iso3, year)

# Iterate through the suffix vector
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      # Replace missing values with the previous year's value for 2020 and cf_tot == NA
      !!paste0("budget_", item) := if_else(iso3 == "PAN" & year == 2020 & is.na(cf_tot), lag(!!sym(paste0("budget_", item))), !!sym(paste0("budget_", item))),
      !!paste0("cf_", item) := if_else(iso3 == "PAN" & year == 2020 & is.na(cf_tot), lag(!!sym(paste0("cf_", item))), !!sym(paste0("cf_", item))),
      !!paste0("gap_", item) := if_else(iso3 == "PAN" & year == 2020 & is.na(cf_tot), lag(!!sym(paste0("gap_", item))), !!sym(paste0("gap_", item))),
      # Replace missing values with the previous year's value for 2019 and exp_tot == NA
      !!paste0("exp_", item) := if_else(iso3 == "PAN" & year == 2019 & is.na(exp_tot), lag(!!sym(paste0("exp_", item))), !!sym(paste0("exp_", item))),
      !!paste0("rcvd_", item) := if_else(iso3 == "PAN" & year == 2019 & is.na(rcvd_tot), lag(!!sym(paste0("rcvd_", item))), !!sym(paste0("rcvd_", item)))
    )
}

# PNG: use budget data to fill exp.
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!sym(paste0("exp_", item)) := ifelse(iso3 == "PNG" & year == 2020 & is.na(!!sym(paste0("exp_", item))),
                                            !!sym(paste0("budget_", item)), !!sym(paste0("exp_", item)))
    )
}

# //RUS: 
#   // PN Jul 2020: Fix RUS share of external and internal funding in 2005 and 2006, to match 2007. This is because there was 
# // a mistake in share proportions values 
# finance_merged <- finance_merged %>%
#   arrange(iso3, desc(year)) %>%
#   mutate(
#     rcvd_tot_gov = if_else(year == 2005 & iso3 == "RUS", rcvd_ext_ngf, rcvd_tot_gov),
#     rcvd_ext_ngf = if_else(year == 2005 & iso3 == "RUS", NA_real_, rcvd_ext_ngf),
#     rcvd_tot = if_else(year == 2006 & iso3 == "RUS", 647555850, rcvd_tot),
#     rcvd_tot_gov = if_else(year == 2006 & iso3 == "RUS", rcvd_tot, rcvd_tot_gov)
#   )
# 
# li year rcvd_tot rcvd_mdr rcvd_nmdr rcvd_sld rcvd_fld rcvd_lab if iso3 == "RUS" & year >= 2002
# gsort iso3 -year
# // To modify the proportion of dr and ds in 2005 (and subsequently in 2006) to be consistent with previously reported values (see GTB 2018)..
# // in 2018, the proportional share of total rcvd that was allocated to 2006 DR was 0.100, DS_DOT was .364 and tbhiv was .535
# replace rcvd_sld = round(0.10034428 * rcvd_tot) if year == 2005 & iso3 == "RUS" 
# replace rcvd_fld = round(0.36424288 * rcvd_tot)  if year == 2005 & iso3 == "RUS" 
# replace rcvd_tbhiv = round(0.53541284 * rcvd_tot)  if year == 2005 & iso3 == "RUS" 
# 
# replace rcvd_sld = round(0.10034428 * rcvd_tot) if year == 2006 & iso3 == "RUS" 
# replace rcvd_fld = round(0.36424288 * rcvd_tot)  if year == 2006 & iso3 == "RUS" 
# replace rcvd_tbhiv = round(0.53541284 * rcvd_tot)  if year == 2006 & iso3 == "RUS" 


# Senegal (SEN): hcfvisit_dstb 2019 fill in with 2018. 
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  mutate(
    hcfvisit_dstb = if_else(year == 2019 & iso3 == "SEN", lag(hcfvisit_dstb), hcfvisit_dstb)
  )

# Tunisia (TUN):fill budget_*2020 = budget_*2019 adjusted with tx_dstb, same for cf and exp
finance_merged <- finance_merged %>% 
  arrange(iso3, year)

for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("budget_", item) := if_else(iso3 == "TUN" & year == 2020 & is.na(!!sym(paste0("budget_", item))),
                                           lag(!!sym(paste0("budget_", item))), !!sym(paste0("budget_", item))),
      !!paste0("cf_", item) := if_else(iso3 == "TUN" & year == 2020 & is.na(!!sym(paste0("cf_", item))),
                                       !!sym(paste0("budget_", item)), !!sym(paste0("cf_", item))),
      !!paste0("exp_", item) := if_else(iso3 == "TUN" & year == 2019 & is.na(!!sym(paste0("exp_", item))),
                                        lag(!!sym(paste0("exp_", item))), !!sym(paste0("exp_", item))),
      !!paste0("rcvd_", item) := if_else(iso3 == "TUN" & year == 2019 & is.na(!!sym(paste0("rcvd_", item))),
                                         !!sym(paste0("exp_", item)), !!sym(paste0("rcvd_", item))))
}

# Tuvalu not reported, use last year's values
#sort by country and ascending order of year (2020 last)
finance_merged <- finance_merged %>%
  arrange(iso3, year)

for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("budget_", item) := if_else(iso3 == "TUV" & year == 2020 & is.na(!!sym(paste0("budget_", item))),
                                           lag(!!sym(paste0("budget_", item))), !!sym(paste0("budget_", item))),
      !!paste0("cf_", item) := if_else(iso3 == "TUV" & year == 2020 & is.na(!!sym(paste0("cf_", item))),
                                       !!sym(paste0("budget_", item)), !!sym(paste0("cf_", item))),
      !!paste0("exp_", item) := if_else(iso3 == "TUV" & year == 2019 & is.na(!!sym(paste0("exp_", item))),
                                        lag(!!sym(paste0("exp_", item))), !!sym(paste0("exp_", item))),
      !!paste0("rcvd_", item) := if_else(iso3 == "TUV" & year == 2019 & is.na(!!sym(paste0("rcvd_", item))),
                                         !!sym(paste0("exp_", item)), !!sym(paste0("rcvd_", item))))
}

# Ukraine (UKR): Budget_oth cf_oth in 2020 and exp_oth rcvd_oth in 2019 include GHS costs. 
# finance_merged <- finance_merged %>%
#   mutate(
#     budget_tot = if_else(year == 2020 & iso3 == "UKR", budget_tot - budget_oth, budget_tot),
#     cf_tot = if_else(year == 2020 & iso3 == "UKR", cf_tot - cf_oth, cf_tot),
#     exp_tot = if_else(year == 2019 & iso3 == "UKR", exp_tot - exp_oth, exp_tot),
#     rcvd_tot = if_else(year == 2019 & iso3 == "UKR", rcvd_tot - rcvd_oth, rcvd_tot),
#     rcvd_tot_domestic = if_else(year == 2019 & iso3 == "UKR", rcvd_tot_domestic - rcvd_oth, rcvd_tot_domestic),
#   )

# Vanuatu (VUT): fill rcvd_*2019= rcvd_*2018 with c_notified 2019
finance_merged <- finance_merged %>%
  arrange(iso3, year)

# for (item in suffix) {
#   # Replace missing exp_`item' values in 2019 with the previous year's values, scaled by c_notified
#   finance_merged <- finance_merged %>%
#     mutate(
#       !!paste0("exp_", item) := if_else(iso3 == "VUT" & year == 2019 , 
#                                         lag(!!sym(paste0("exp_", item))) * round(c_notified / lag(c_notified)), 
#                                         !!sym(paste0("exp_", item))),
#       !!paste0("rcvd_", item) := if_else(iso3 == "VUT" & year == 2019 , 
#                                          !!sym(paste0("exp_", item)), 
#                                          !!sym(paste0("rcvd_", item)))
#     )
# }

# ZAF: change hcfvisit_mdr in 2019 from 9 to prevoius years' value
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  mutate(
    hcfvisit_mdr = ifelse(iso3 == "ZAF" & year == 2019 & hcfvisit_mdr == 9, lag(hcfvisit_mdr), hcfvisit_mdr)
  )

# // IGB/PN 23Jul2020: Values in 2006 are manually fixed (and allocated) because imputation results in
# // halving of figures, ncausing the total global value to reduce by 0.1 B USD
# 
# replace rcvd_tot =  299932088 if iso3 == "ZAF" & year == 2006 //  299932088.4 in 2006 nominal US$, equivalent to 366944991 in 2018 US$
#   replace rcvd_sld = round(0.45188398 * rcvd_tot) if year == 2006 & iso3 == "ZAF" 
# replace rcvd_fld = round(0.47110833 * rcvd_tot)  if year == 2006 & iso3 == "ZAF"  
# replace rcvd_tbhiv = round(0.06382217 * rcvd_tot)  if year == 2006 & iso3 == "ZAF" 
# replace rcvd_orsrvy = round(0.01318551 * rcvd_tot)  if year == 2006 & iso3 == "ZAF" 
# 
# replace rcvd_tot_gov = round(0.99204674 * rcvd_tot) if year == 2006 & iso3 == "ZAF"
# replace rcvd_tot_grnt = round(0.00179391 * rcvd_tot) if year == 2006 & iso3 == "ZAF"
# replace rcvd_tot_gf = round(0.00615935 * rcvd_tot) if year == 2006 & iso3 == "ZAF"


# ZWE
# // Also change hcfvisit_dstb and hcf_visit_mdr values for 2015 and before with those of 2016
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year)) %>%  # sort by country and year in descending order
  mutate(
    hcfvisit_dstb = ifelse(year == 2016 & iso3 == "ZWE" & hcfvisit_dstb == 64, lag(hcfvisit_dstb), hcfvisit_dstb),
    hcfvisit_dstb = ifelse(year <= 2015 & iso3 == "ZWE", lag(hcfvisit_dstb), hcfvisit_dstb),
    hcfvisit_mdr = ifelse(year <= 2015 & year >= 2014 & iso3 == "ZWE", lag(hcfvisit_mdr), hcfvisit_mdr)
  )


# // BRA: inpatient utilsation for 2014 ought to be replaced since it's great conpared to 2015. Seems more like 2013'svalue but this ignores a UC change in 2014 
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year)) %>%  # sort by country and year in descending order
  mutate(
    hospd_dstb_dur = ifelse(year == 2014 & iso3 == "BRA", lag(hospd_dstb_dur), hospd_dstb_dur),
    hospd_dstb_prct = ifelse(year == 2014 & iso3 == "BRA", lag(hospd_dstb_prct), hospd_dstb_prct)
  )


# Comoros (COM): backfill utilisation values from 2017 till 2014 with 2018's report
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year)) %>%
  mutate(
    hospd_dstb_dur = ifelse(iso3 == "COM" & year < 2018 & year >= 2014, lag(hospd_dstb_dur), hospd_dstb_dur),
    hospd_dstb_prct = ifelse(iso3 == "COM" & year < 2018 & year >= 2014, lag(hospd_dstb_prct), hospd_dstb_prct)
  )

# DJI 2011 and 2012 OP
finance_merged <- finance_merged %>% 
  arrange(iso3, desc(year))

finance_merged <- finance_merged %>% 
  mutate(hcfvisit_dstb = ifelse(iso3 == "DJI" & year < 2019 & year >= 2014,
                                lag(hcfvisit_dstb),
                                hcfvisit_dstb))

# NER change hospd_mdr_dur = 120 if hospd_mdr_dur  == 30
finance_merged <- finance_merged %>% 
  mutate(
    hospd_mdr_dur = ifelse(hospd_mdr_dur == 30 & year == 2014 & iso3 == "NER",
                           120,
                           hospd_mdr_dur)
  )

# PER 2015 mdr % 
finance_merged <- finance_merged %>% 
  arrange(iso3, desc(year)) %>% # Sort dataframe by iso3 and year in descending order
  mutate(
    hospd_mdr_prct = ifelse(year == 2015 & iso3 == "PER", lag(hospd_mdr_prct), hospd_mdr_prct)
  )


# SOM 2017: utilisation (hcfvisit_dstb) low
finance_merged  <- finance_merged  %>%
  arrange(iso3, desc(year))

finance_merged  <- finance_merged  %>%
  mutate(
    hcfvisit_dstb = ifelse(year == 2017 & iso3 == "SOM", lag(hcfvisit_dstb), hcfvisit_dstb)
  )


# /****************
#   2021
# ****************/
# //Also missing is Venezuela (2021) but for consistency keep it as prev year.
finance_merged$g_income <- ifelse(finance_merged$iso3 == "VEN", "UMC", finance_merged$g_income)

# Armenia (ARM): replace per patient costs 2020 with budget 2021 if still empty.
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year))  # Sort dataframe by iso3 and year in descending order


for (item in c("dstb", "mdr", "xdr", "tpt")) {
  finance_merged <- finance_merged %>%
    group_by(iso3) %>%
    mutate(!!paste0("exp_cpp_", item) := ifelse(iso3 == "ARM" & year == 2020 & is.na(!!sym(paste0("exp_cpp_", item))), lag(!!sym(paste0("budget_cpp_", item))), !!sym(paste0("exp_cpp_", item)))) %>%
    ungroup() 
}

finance_merged <- finance_merged %>% 
  arrange(iso3, year)  # Sort dataframe by iso3 and year


# Bangladesh (BGD): if hcvisit_mdr is 130 in 2019, replace with 80
finance_merged <- finance_merged %>%
  mutate(hcfvisit_mdr = ifelse(iso3 == "BGD" & year == 2019 & hcfvisit_mdr == 130, 80, hcfvisit_mdr))

# Algeria (DZA): Have only reported GHS data. 
# li iso3 year budget_tot cf_tot exp_tot rcvd_tot c_notified hcfvisit_mdr hcfvisit_dstb hospd_dstb_dur hospd_mdr_dur  if iso3 == "DZA"
# li year budget_cpp* budget_sld tx_* if iso3 == "DZA" & year >= 2010
# Correct the historical anomalous value of budget_cpp_* in 2015
finance_merged <- finance_merged %>%
  mutate(
    budget_cpp_dstb = ifelse(iso3 == "DZA" & year == 2015 & budget_cpp_dstb == 440000, budget_cpp_dstb / tx_dstb, budget_cpp_dstb),
    budget_cpp_mdr = ifelse(iso3 == "DZA" & year == 2015 & budget_cpp_mdr == 42000, budget_cpp_mdr / tx_mdr, budget_cpp_mdr),
    budget_cpp_xdr = ifelse(iso3 == "DZA" & year == 2015 & is.na(budget_cpp_xdr), NA, budget_cpp_xdr)
  )
#  In 2021 the _cpp values were used to estimate budget and cf subcomponent values, code under the temporary assumptions section of prepare do file


# 	// Belarus (BLR): CONFIRM that exp_* values 2020 are larger than before, since they should include ghs costs
# li year exp_fld exp_staff exp_lab exp_tbhiv exp_sld exp_mdrmgt exp_oth  exp_prog ///
#   exp_orsrvy exp_patsup exp_tot if iso3 == "BLR" & year >= 2015
# 
# 
# // Bosnia BIH: hcfvisit and hospd data for 2020 are reported and are similar to 2019. 
# // Note: budget and exp data are much lower than previous years 
# li iso3 year c_notified hcfvisit_dstb hospd_dstb_prct hospd_dstb_dur if iso3 == "BIH" // utilisation for DSTB
# li iso3 year tx_mdr hcfvisit_mdr hospd_mdr_prct hospd_mdr_dur if iso3 == "BIH" //Utilization for MDR
# li iso3 year budget_tot rcvd_tot cf_tot exp_tot c_notified tx_dstb if iso3 == "BIH"
# 
# 
# // Bulgaria BGR: utilization data are reported for 2020
# li iso3 year c_notified tx_dstb hcfvisit_dstb hospd_dstb_prct hospd_dstb_dur if iso3 == "BGR" // utilisation for DSTB
# li iso3 year tx_mdr hcfvisit_mdr hospd_mdr_prct hospd_mdr_dur if iso3 == "BGR" //Utilization for MDR
# //C_Notified will be carried over from 2019


# Cuba (CUB): change 0 to missing to allow imputation (the country could not isolate TB budgets or expenses from overall health data in 2020, so reported 0 instead of missing)
finance_merged <- finance_merged %>%
  mutate(budget_tot = ifelse(budget_tot == 0 & iso3 == "CUB" & year == report_year, NA, budget_tot),
         exp_tot = ifelse(exp_tot == 0 & iso3 == "CUB" & year == (report_year - 1), NA, exp_tot))

# CHad (TCD): Fill in exp with received funding: fill with last year if missing for 2020
finance_merged <- finance_merged %>%
  arrange(iso3, year)

for (item in suffix) {
  # Replace exp_`item' if iso3 is "TCD", year is 2020, and exp_tot is NA
  finance_merged <- finance_merged %>%
    mutate(!!paste0("exp_", item) := ifelse(iso3 == "TCD" & year == 2020 & is.na(exp_tot), lag(!!sym(paste0("exp_", item))), !!sym(paste0("exp_", item))),
           !!paste0("rcvd_", item) := ifelse(iso3 == "TCD" & year == 2020 & is.na(rcvd_tot), lag(!!sym(paste0("rcvd_", item))), !!sym(paste0("rcvd_", item))))
}

# // Colombia (COL): correct hc_fvisit_DSTB in 2020 (from 12532 to what was reported previous year).
# // MDR years 2013-2018 should be max 350
finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = if_else(year == 2020 & iso3 == "COL" & hcfvisit_dstb == 12532, lag(hcfvisit_dstb), hcfvisit_dstb))

# PRK: Replace hcfvisit_mdr in 2019 with 301 (confirmed by country in 2021). Note that exp_sld == 0 in 2020 (no DR drugs were purchased in 2020
finance_merged <- finance_merged %>%
  mutate(hcfvisit_mdr = if_else(iso3 == "PRK" & year == 2019, 301, hcfvisit_mdr))

# Djibouti (DJI):HCFVISIT 2020 are not blank. 
finance_merged <- within(finance_merged, {
  budget_oth[year == 2020 & iso3 == "DJI"] <- cf_oth[year == 2020 & iso3 == "DJI"]
})

# Fiji: HCFvisit_dstb erroneously reported as c_notified, correct to reflect last year's value.
finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = if_else(iso3 == "FJI" & year == 2020 & hcfvisit_dstb == c_notified,
                                 lag(hcfvisit_dstb),
                                 hcfvisit_dstb))

# // Gambia (GMB): Budget and cf missing for 2020
# // IGB/PN Jul 2021: Gambia has exp reported in 2019. Fill in 2020 exp with 2019 and use same to fill n budget 2021 and 2020
finance_merged <- finance_merged %>%
  mutate(
    exp_tot = if_else(exp_tot == 0 & year == 2020 & iso3 == "GMB", lag(exp_tot), exp_tot),
    rcvd_tot = if_else(rcvd_tot == 0 & year == 2020 & iso3 == "GMB", lag(rcvd_tot), rcvd_tot),
    budget_tot = if_else(is.na(budget_tot) & (year == 2021 | year == 2020) & iso3 == "GMB", lag(exp_tot), budget_tot),
    cf_tot = if_else(is.na(cf_tot) & (year == 2021 | year == 2020) & iso3 == "GMB", lag(exp_tot), cf_tot)
  )

# Guatemala (GTM): Use expected finding instead of budget to remove negative gaps.
for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("budget_", item) := ifelse(iso3 == "GTM" & year >= 2019 & year <= 2021 & (get(paste0("gap_", item)) < 0 | is.na(get(paste0("gap_", item)))), get(paste0("cf_", item)), !!sym(paste0("budget_", item))),
      !!paste0("gap_", item) := ifelse(iso3 == "GTM" & year >= 2019 & year <= 2021, get(paste0("budget_", item)) - get(paste0("cf_", item)), !!sym(paste0("gap_", item)))
    )
}

# Libya (LBY): Fill in expenditure values 2019 and 2020, with budget values from 2020 and 2021 respectively

finance_merged <- finance_merged %>%
  arrange(iso3, desc(year)) %>%
  mutate(
    budget_tot = ifelse(is.na(budget_tot) & year == 2021 & iso3 == "LBY", lag(budget_tot), budget_tot)
  )

for (item in suffix) {
  finance_merged <- finance_merged %>%
    mutate(
      !!paste0("exp_", item) := ifelse((is.na(!!sym(paste0("exp_", item))) | !!sym(paste0("exp_", item)) == 0) & iso3 == "LBY" & year >= 2019 & year <= 2021, lag(!!sym(paste0("budget_", item))), !!sym(paste0("exp_", item))),
      !!paste0("rcvd_", item) := ifelse((is.na(!!sym(paste0("rcvd_", item))) | !!sym(paste0("rcvd_", item)) == 0) & iso3 == "LBY" & year >= 2019 & year <= 2021, lag(!!sym(paste0("budget_", item))), !!sym(paste0("rcvd_", item)))
    )
}

# marshall (MHL):  exp_* replaced with exp_* [_n-1] for 2020.   
for (item in suffix) {
  finance_merged <- finance_merged %>%
    arrange(iso3, year) %>%
    mutate(
      !!sym(paste0("exp_", item)) := ifelse(!!sym(paste0("exp_", item)) == "." & iso3 == "MHL" & year == 2020, lag(!!sym(paste0("exp_", item))), !!sym(paste0("exp_", item)))
    )
}
# exp_cpp_dstb is wrongly estimated (200000 usd in 2020 compared to 100 in 2019. Replace with bidget_cpp_dstb for 2021
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year)) %>%
  mutate(
    exp_cpp_dstb = ifelse(exp_cpp_dstb == 200000 & iso3 == "MHL" & year == 2020, lag(exp_cpp_dstb), exp_cpp_dstb)
  )


# Moldova (MDA): if utilization data in 2020 is blank, carry forward from 2019
# li year hcfvisit_dstb hospd_dstb_* hcfvisit_mdr hospd_mdr_* if iso3 == "MDA" & year >= 2019

# West Bank (PSE):  exp_cpp_dstb=budget_cpp_dstb{_n-1] if year==2020.. (the have GHS data also for wo previous years)
# li year budget_cpp* exp_cpp_* c_notified if iso3 == "PSE"
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year)) %>%
  mutate(
    budget_cpp_dstb = ifelse(iso3 == "PSE" & year == 2019, lag(budget_cpp_dstb), budget_cpp_dstb),
    budget_cpp_mdr = ifelse(iso3 == "PSE" & year == 2019, lag(budget_cpp_mdr), budget_cpp_mdr),
    budget_cpp_xdr = ifelse(iso3 == "PSE" & year == 2019, lag(budget_cpp_xdr), budget_cpp_xdr),
    budget_cpp_tpt = ifelse(iso3 == "PSE" & year == 2019, lag(budget_cpp_tpt), budget_cpp_tpt),
    exp_cpp_dstb = ifelse(iso3 == "PSE" & year == 2020, lag(exp_cpp_dstb), exp_cpp_dstb),
    exp_cpp_mdr = ifelse(iso3 == "PSE" & year == 2020, lag(exp_cpp_mdr), exp_cpp_mdr),
    exp_cpp_xdr = ifelse(iso3 == "PSE" & year == 2020, lag(exp_cpp_xdr), exp_cpp_xdr),
    exp_cpp_tpt = ifelse(iso3 == "PSE" & year == 2020, lag(exp_cpp_tpt), exp_cpp_tpt)
  )



# RUS: Note that patient costs for mdr and xdr are inseparable and are reported in exp_cpp_mdr
# // also, $612 485 in exp_oth in 2020 is for TB vaccines
# li year exp_tot exp_mdr exp_oth if iso3 == "RUS" & year == 2020

# Senegal (SEN): move 2020 budget_oth to budget_prog,  
# // cf_oth to cf_prog and  gap_oth to gap_prog 
# li year budget_oth cf_oth gap_oth budget_prog cf_prog gap_prog if iso3 == "SEN" & year >= 2019
finance_merged <- finance_merged %>%
  mutate(
    cf_prog = ifelse(iso3 == "SEN" & year == 2020, cf_prog + cf_oth, cf_prog),
    cf_oth = ifelse(iso3 == "SEN" & year == 2020, NA, cf_oth),
    budget_prog = ifelse(iso3 == "SEN" & year == 2020, budget_prog + budget_oth, budget_prog),
    budget_oth = ifelse(iso3 == "SEN" & year == 2020, NA, budget_oth),
    gap_prog = ifelse(iso3 == "SEN" & year == 2020, gap_prog + gap_oth, gap_prog),
    gap_oth = ifelse(iso3 == "SEN" & year == 2020, NA, gap_oth)
  )

# // correct mdr utilization for 2019
# li year hcfvisit* if iso3 == "SEN"
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  mutate(
    hcfvisit_mdr = ifelse(year == 2019 & iso3 == "SEN", lag(hcfvisit_mdr), hcfvisit_mdr)
  )

# Tuvalu IF 2021 not reported, use last year's values
# 		li iso3 year budget_tot cf_tot exp_tot gap_tot rcvd_tot c_notified tx_dstb if iso3 == "TUV"
# //sort by country and ascending order of year (2020 last)
finance_merged <- finance_merged %>%
  arrange(iso3, year) 

for (item in suffix) {
  finance_merged <- finance_merged %>%
      mutate(
        !!sym(paste0("budget_", item)) := ifelse(iso3 == "TUV" & year == 2021 & is.na(!!sym(paste0("budget_", item))), lag(!!sym(paste0("budget_", item))), !!sym(paste0("budget_", item))),
      !!sym(paste0("cf_", item)) := ifelse(iso3 == "TUV" & year == 2021 & is.na(!!sym(paste0("cf_", item))), lag(!!sym(paste0("budget_", item))), !!sym(paste0("cf_", item))),
      !!sym(paste0("exp_", item)) := ifelse(iso3 == "TUV" & year == 2020 & is.na(!!sym(paste0("exp_", item))), lag(!!sym(paste0("exp_", item))), !!sym(paste0("exp_", item))),
      !!sym(paste0("rcvd_", item)) := ifelse(iso3 == "TUV" & year == 2020 & is.na(!!sym(paste0("rcvd_", item))), lag(!!sym(paste0("exp_", item))), !!sym(paste0("rcvd_", item)))
      ) 
  }

		
# UZB : Correction to exp_cpp_dstb, mdr and xdr in 2015 values were filled ni wrong place
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  mutate(
    exp_cpp_xdr = ifelse(year == 2015 & iso3 == "UZB", exp_cpp_mdr, exp_cpp_xdr),
    exp_cpp_mdr = ifelse(year == 2015 & iso3 == "UZB", exp_cpp_dstb, exp_cpp_mdr),
    exp_cpp_dstb = ifelse(year == 2015 & iso3 == "UZB", NA, exp_cpp_dstb),
    exp_cpp_xdr = ifelse(year >= 2014 & year < 2016 & iso3 == "UZB", lag(exp_cpp_xdr), exp_cpp_xdr),
    exp_cpp_mdr = ifelse(year >= 2014 & year < 2016 & iso3 == "UZB", lag(exp_cpp_mdr), exp_cpp_mdr),
    exp_cpp_dstb = ifelse(year >= 2014 & year < 2016 & iso3 == "UZB", lag(exp_cpp_dstb), exp_cpp_dstb)
  )
		

# /****************
#   2022
# ****************/
# replace g_income="HIC" if inlist(iso3, "AIA", "COK","MSR","NIU", "TKL", "WLF") & g_income == ""
# //Also missing is Venezuela (since July 2021) but for consistency keep it as prev year.
# replace g_income = "UMC" if iso3 == "VEN" & g_income == ""
# 
# gsort iso3 year
# 
# //create a local variable to store the specific suffixes of the financial data's disaggregates
# global suffix lab staff fld prog sld mdrmgt tpt tbhiv patsup orsrvy oth tot

# Armenia (ARM)
# li year exp_cpp_* if iso3 == "ARM" & year > 2018
# li year budget_cpp_* if iso3 == "ARM" & year > 2018

 # Note that 2021 budget_fld was about 9 fold of prior years, but expenditure in 2022 reflects expected amounts. In future country should correct this. For now, hard code the change where we move the _fld values to _oth variable
# li year budget_fld budget_oth budget_tot if iso3 == "ARM" & year > 2018
finance_merged <- finance_merged %>%
  mutate(
    budget_oth = ifelse(iso3 == "ARM" & year == 2021 & budget_fld == 428693, budget_oth + budget_fld, budget_oth),
    budget_fld = ifelse(iso3 == "ARM" & year == 2021 & budget_fld == 428693, NA, budget_fld),
    cf_oth = ifelse(iso3 == "ARM" & year == 2021 & cf_fld == 359663, cf_oth + cf_fld, cf_oth),
    cf_fld = ifelse(iso3 == "ARM" & year == 2021 & cf_fld == 359663, NA, cf_fld),
    gap_oth = ifelse(iso3 == "ARM" & year == 2021 & gap_fld == 69030, gap_oth + gap_fld, gap_oth),
    gap_fld = ifelse(iso3 == "ARM" & year == 2021 & gap_fld == 69030, NA, gap_fld)
  )

# Azerbaijan: No report for finance nor utilisation. 
# li iso3 year budget_tot cf_tot exp_tot rcvd_tot c_notified tx_dstb if iso3 == "AZE" & year >= 2016 
# // PN 2022: Allowing imputation to fill in finance (since there are spending records from 2016 to 2020 and budget 2021)
# // for _cpp, carry forward from last report. Utilization will be automatically carried forward in prepare do file
# li iso3 year budget_cpp_* if iso3 == "AZE" & year >= 2016
# li iso3 year exp_cpp_* if iso3 == "AZE" & year >= 2016
finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  mutate(
    exp_cpp_dstb = ifelse(iso3 == "AZE" & year >= (report_year - 2) & year < report_year & is.na(exp_cpp_dstb), lag(exp_cpp_dstb), exp_cpp_dstb),
    exp_cpp_mdr = ifelse(iso3 == "AZE" & year >= (report_year - 2) & year < report_year & is.na(exp_cpp_mdr), lag(exp_cpp_mdr), exp_cpp_mdr),
    exp_cpp_xdr = ifelse(iso3 == "AZE" & year >= (report_year - 2) & year < report_year & is.na(exp_cpp_xdr), lag(exp_cpp_xdr), exp_cpp_xdr),
    exp_cpp_tpt = ifelse(iso3 == "AZE" & year >= (report_year - 2) & year < report_year & is.na(exp_cpp_tpt), lag(exp_cpp_tpt), exp_cpp_tpt),
    budget_cpp_dstb = ifelse(iso3 == "AZE" & (year >= report_year - 1) & is.na(budget_cpp_dstb), lag(budget_cpp_dstb), budget_cpp_dstb),
    budget_cpp_mdr = ifelse(iso3 == "AZE" & (year >= report_year - 1) & is.na(budget_cpp_mdr), lag(budget_cpp_mdr), budget_cpp_mdr),
    budget_cpp_xdr = ifelse(iso3 == "AZE" & (year >= report_year - 1) & is.na(budget_cpp_xdr), lag(budget_cpp_xdr), budget_cpp_xdr),
    budget_cpp_tpt = ifelse(iso3 == "AZE" & (year >= report_year - 1) & is.na(budget_cpp_tpt), lag(budget_cpp_tpt), budget_cpp_tpt)
  )
# Botswana: No report in 2022. Allow imputation to fill in - No further action.
# // Utilization available. Budget_cpp values are same as last year, and have reduced from previous years (_dstb reduced from 
# // 120 to 25, _mdr reduced from 8000 to 900.) Since exp_cpp are not yet reported in 2021, replace with budget_cpp  
# li iso3 year budget_cpp_* if iso3 == "BWA" & year > 2016
# li iso3 year exp_cpp_* if iso3 == "BWA" & year >= 2016
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year)) %>%
  mutate(
    exp_cpp_dstb = ifelse(iso3 == "BWA" & year >= (report_year - 2) & year < report_year & is.na(exp_cpp_dstb), lag(budget_cpp_dstb), exp_cpp_dstb),
    exp_cpp_mdr = ifelse(iso3 == "BWA" & year >= (report_year - 2) & year < report_year & is.na(exp_cpp_mdr), lag(budget_cpp_mdr), exp_cpp_mdr),
    exp_cpp_xdr = ifelse(iso3 == "BWA" & year >= (report_year - 2) & year < report_year & is.na(exp_cpp_xdr), lag(budget_cpp_xdr), exp_cpp_xdr),
    exp_cpp_tpt = ifelse(iso3 == "BWA" & year >= (report_year - 2) & year < report_year & is.na(exp_cpp_tpt), lag(budget_cpp_tpt), exp_cpp_tpt)
  ) %>%
  arrange(iso3, year)




# Algeria (DZA): Have only reported utilization data. 
# li year budget_tot cf_tot exp_tot rcvd_tot if iso3 == "DZA" & year > 2016
# li year c_notified hcfvisit_mdr hcfvisit_dstb hospd_dstb_dur hospd_mdr_dur  if iso3 == "DZA" & year > 2016
# li year budget_cpp* budget_sld tx_* if iso3 == "DZA" & year >= 2010

# // hospd_mdr_dur in 2021 reported as 60 from prevoius years' 30. change back to 30 for 2021.
finance_merged <- finance_merged %>%
  mutate(hospd_mdr_dur = ifelse(iso3 == "DZA" & year == 2021 & hospd_mdr_dur == 60, 30, hospd_mdr_dur))
# // In 2021 the _cpp values were used to estimate budget and cf subcomponent values, code under the temporary assumptions section of prepare do file

# //Djibouti (DJI): No reporting. We are aware that GF disbusrsements are closed as at 2021. 
# li year budget_tot cf_tot cf_int cf_ext_gf exp_tot rcvd_tot if iso3 == "DJI" & year > 2016
# li year c_notified hcfvisit_mdr hcfvisit_dstb hospd_dstb_dur hospd_mdr_dur  if iso3 == "DJI" & year > 2016
# li year budget_cpp* budget_sld tx_* if iso3 == "DJI" & year >= 2010

# // Egypt: Temporarily fill in with GF data as done last year. They reported 2022 budgets
# li year budget_tot cf_tot cf_int cf_ext_gf exp_tot rcvd_tot if iso3 == "EGY" & year > 2016
# 
# // Guinea Bissou: Correct 2022 error
# li year budget_prog budget_tot gap_prog gap_tot if iso3 == "GNB" & year == 2022
finance_merged <- finance_merged %>%
  mutate(
    budget_prog = ifelse(budget_prog == 16407340 & iso3 == "GNB" & year == 2022, 1640734, budget_prog),
    gap_prog = ifelse(gap_prog == 15009281 & iso3 == "GNB" & year == 2022, budget_prog - cf_prog, gap_prog),
    budget_tot = ifelse(iso3 == "GNB" & year == 2022, 
                        budget_lab + budget_staff + budget_fld + budget_prog + budget_sld + 
                          budget_mdrmgt + budget_tpt + budget_tbhiv + budget_patsup + 
                          budget_orsrvy + budget_oth, budget_tot),
    gap_tot = ifelse(iso3 == "GNB" & year == 2022, 
                     gap_lab + gap_staff + gap_fld + gap_prog + gap_sld + gap_mdrmgt + 
                       gap_tpt + gap_tbhiv + gap_patsup + gap_orsrvy + gap_oth, gap_tot)
  )

# * PN 6 Sept 2022: India program staff spending in 2021
# * India noted that there was a release of salary of large number of sub national staff directly 
# * from the State Health Mission and not from NTEP budgets (40 million). Therefore this is added into their received funding
# *
# li year rcvd_staff exp_staff gap_staff rcvd_tot exp_tot if iso3 == "IND" & year >= 2020
finance_merged <- finance_merged %>%
  mutate(
    rcvd_staff = ifelse(iso3 == "IND" & year == 2021 & rcvd_staff == 39795347, rcvd_staff + 40000000, rcvd_staff),
    exp_staff = ifelse(iso3 == "IND" & year == 2021, rcvd_staff, exp_staff),
    rcvd_tot = ifelse(iso3 == "IND" & year == 2021 & rcvd_tot == 296775986, rcvd_tot + 40000000, rcvd_tot),
    exp_tot = ifelse(iso3 == "IND" & year == 2021, rcvd_tot, exp_tot),
    rcvd_tot_domestic = ifelse(iso3 == "IND" & year == 2021 & rcvd_tot_domestic == 143040058, rcvd_tot_domestic + 40000000, rcvd_tot_domestic)
  )

# li year budget_tot cf_tot rcvd_tot exp_tot rcvd_int if iso3 == "IND" & year >= 2020
 
# Jordan
# li year budget_oth budget_lab budget_tot if iso3 == "JOR" & year >= 2021
finance_merged <- finance_merged %>%
  mutate(
    budget_lab = ifelse(budget_oth == 4000000 & iso3 == "JOR" & year == 2022, budget_lab + 4000000 - lag(budget_oth), budget_lab),
    budget_oth = ifelse(budget_oth == 4000000 & iso3 == "JOR" & year == 2022, lag(budget_oth), budget_oth)
  )

#  Libya (LBY): Fill in expenditure values 2019 and 2020, with budget values from 2020 and 2021 respectively
# li iso3 year budget_tot cf_tot rcvd_tot exp_tot gap_tot if iso3 == "LBY" 
finance_merged <- finance_merged %>%
  arrange(iso3, desc(year))
# Estimate expenditures with cf_* data.
finance_merged <- finance_merged %>%
  mutate(
    budget_tot = ifelse(is.na(budget_tot) & year == 2022 & iso3 == "LBY", lag(budget_tot), budget_tot)
  )

for (item in suffix) {
  finance_merged <- finance_merged %>%
    arrange(iso3, year) %>%
    mutate(
      !!sym(paste0("exp_", item)) := ifelse((is.na(!!sym(paste0("exp_", item))) | !!sym(paste0("exp_", item)) == 0) & iso3 == "LBY" & year >= 2020 & year <= 2022, lag(!!sym(paste0("budget_", item))), !!sym(paste0("exp_", item))),
      !!sym(paste0("rcvd_", item)) := ifelse((is.na(!!sym(paste0("rcvd_", item))) | !!sym(paste0("rcvd_", item)) == 0) & iso3 == "LBY" & year >= 2020 & year <= 2022, lag(!!sym(paste0("budget_", item))), !!sym(paste0("rcvd_", item)))
    )
}

# Marshall island MHL: Utilisation (hcfvisit ) reported as 119 visits up from 3 in 2020. Headed back to previous value 180 in 2019. No change to be dones 

# Vanuatu - undervalued. Awaiting clarifications from country
# li country iso3 year budget_tot cf_tot rcvd_tot exp_tot gap_tot if iso3 == "VUT"
finance_merged <- finance_merged %>%
  mutate(
    budget_tot = ifelse(budget_tot == 614 & iso3 == "VUT" & year == 2022, NA, budget_tot),
    cf_tot = ifelse(cf_tot == 614 & iso3 == "VUT" & year == 2022, NA, cf_tot),
    exp_tot = ifelse(exp_tot == 460 & iso3 == "VUT" & year == 2021, NA, exp_tot),
    rcvd_tot = ifelse(rcvd_tot == 460 & iso3 == "VUT" & year == 2021, NA, rcvd_tot)
  )

# * TKM
finance_merged <- finance_merged %>%
  mutate(
    exp_tot = ifelse(iso3 == "TKM" & year == 2021 & exp_tot == 0, NA, exp_tot),
    rcvd_tot = ifelse(iso3 == "TKM" & year == 2021 & rcvd_tot == 0, NA, rcvd_tot)
  )

#--------------------
# 2023 revision April-May 2024
#--------------------
# adjustment for the data reported in 2024 will be added here
finance_merged <- finance_merged %>%
  mutate(
    hcfvisit_dstb = ifelse(iso3 == "SLE" & year == 2016 & hcfvisit_dstb == 14114, 60, hcfvisit_dstb),
    rcvd_tot = ifelse(iso3 == "RUS" & year == 2014 & rcvd_tot == 1888984052, 1893234063, rcvd_tot)
  )

finance_merged <- finance_merged %>%
  mutate(rcvd_lab = ifelse(iso3 == "SEN" & year == 2022, 1379669, rcvd_lab),
         rcvd_staff = ifelse(iso3 == "SEN" & year == 2022, 1345000, rcvd_staff),
         rcvd_fld = ifelse(iso3 == "SEN" & year == 2022, 336842, rcvd_fld),
         rcvd_prog = ifelse(iso3 == "SEN" & year == 2022, 433684, rcvd_prog),
         rcvd_sld = ifelse(iso3 == "SEN" & year == 2022, 206697, rcvd_sld),
         rcvd_mdrmgt = ifelse(iso3 == "SEN" & year == 2022, 400000, rcvd_mdrmgt),
         rcvd_tpt = ifelse(iso3 == "SEN" & year == 2022, 0, rcvd_tpt),
         rcvd_tbhiv = ifelse(iso3 == "SEN" & year == 2022, 142426, rcvd_tbhiv),
         rcvd_patsup = ifelse(iso3 == "SEN" & year == 2022, 173699, rcvd_patsup),
         rcvd_orsrvy = ifelse(iso3 == "SEN" & year == 2022, 39237, rcvd_orsrvy),
         rcvd_oth = ifelse(iso3 == "SEN" & year == 2022, 251984, rcvd_oth),
         rcvd_tot = ifelse(iso3 == "SEN" & year == 2022, 4709238, rcvd_tot))


# Bangladesh
# * Reported GF component (and Lab component)  received in 2022 - 130 million was a Covid RF grant for lab commodities. Remove from the lab and the GF source components
finance_merged <- finance_merged %>%
  ungroup() %>%
  mutate(
    rcvd_lab = if_else(rcvd_lab == 181076092 & iso3 == "BGD" & year == 2022, rcvd_lab - 130000000, rcvd_lab),
    exp_lab = if_else(exp_lab == 180483721 & iso3 == "BGD" & year == 2022, exp_lab - 130000000, exp_lab),
    rcvd_tot_gf = if_else(rcvd_tot_gf == 199434217 & iso3 == "BGD" & year == 2022, rcvd_tot_gf - 130000000, rcvd_tot_gf),
    new_rcvd_tot = rowSums(select(., starts_with("rcvd_")&!contains("tot")), na.rm = TRUE),
    new_exp_tot = rowSums(select(., starts_with("exp_")&!contains("tot")), na.rm = TRUE),
    rcvd_tot = if_else(iso3 == "BGD" & year == 2022, new_rcvd_tot, rcvd_tot),
    exp_tot = if_else(iso3 == "BGD" & year == 2022, new_exp_tot, exp_tot)
  ) %>%
  select(-new_rcvd_tot, -new_exp_tot)


finance_merged <- finance_merged %>%
  mutate(
    rcvd_tot_temp = rowSums(select(., rcvd_tot_domestic, rcvd_tot_gf, rcvd_tot_usaid, rcvd_tot_grnt), na.rm = TRUE),
    cf_tot_temp = rowSums(select(., cf_tot_domestic, cf_tot_gf, cf_tot_usaid, cf_tot_grnt), na.rm = TRUE)
  ) %>%
  mutate(
    rcvd_tot_domestic = ifelse(is.na(rcvd_tot_domestic)&(rcvd_tot_temp==rcvd_tot),NZ(rcvd_tot_domestic),rcvd_tot_domestic),
    cf_tot_domestic = ifelse(is.na(cf_tot_domestic)&(cf_tot_temp==cf_tot),NZ(cf_tot_domestic),cf_tot_domestic)
  )

# filling missing domestic funding based on previous year's data
# hcfvisit_dstb, hcfvisit_mdr, 
## AFG
finance_merged <- finance_merged %>%
  group_by("iso3") %>%
  mutate(hcfvisit_dstb = ifelse(iso3 == "AFG" & year == 2013 & (hcfvisit_dstb==0|is.na(hcfvisit_dstb)), lead(hcfvisit_dstb,1), hcfvisit_dstb)) 

## AZE
aze_value <- (finance_merged$cf_tot_domestic[finance_merged$iso3=="AZE" & finance_merged$year==2015])


finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "AZE" & (year %in% c(2013:2014)), rcvd_tot + aze_value, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "AZE" & (year %in% c(2013:2014)), rcvd_tot_domestic + aze_value, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "AZE" & (year %in% c(2013:2014)), rcvd_staff + aze_value, rcvd_staff))

## BGR
finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = ifelse(iso3 == "BGR" & (is.na(hcfvisit_dstb)|hcfvisit_dstb==0) & year != report_year, 
                                finance_merged$hcfvisit_dstb[finance_merged$iso3 == "BGR" & finance_merged$year==2014], hcfvisit_dstb),
         hcfvisit_mdr = ifelse(iso3 == "BGR" & (is.na(hcfvisit_mdr)|hcfvisit_mdr==0) & year != report_year, 
                                finance_merged$hcfvisit_mdr[finance_merged$iso3 == "BGR" & finance_merged$year==2014], hcfvisit_mdr),
         hospd_dstb_dur = ifelse(iso3 == "BGR" & (is.na(hospd_dstb_dur)|hospd_dstb_dur==0) & year != report_year, 
                                finance_merged$hospd_dstb_dur[finance_merged$iso3 == "BGR" & finance_merged$year==2014], hospd_dstb_dur),
         hospd_mdr_dur = ifelse(iso3 == "BGR" & (is.na(hospd_mdr_dur)|hospd_mdr_dur==0) & year != report_year, 
                               finance_merged$hospd_mdr_dur[finance_merged$iso3 == "BGR" & finance_merged$year==2014], hospd_mdr_dur),
         hospd_dstb_prct = ifelse(iso3 == "BGR" & is.na(hospd_dstb_prct), lead(hospd_dstb_prct,1), hospd_dstb_prct),
         hospd_mdr_prct = ifelse(iso3 == "BGR" & is.na(hospd_mdr_prct), lead(hospd_mdr_prct,1), hospd_mdr_prct)) 

## BLZ
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "BLZ" & year == 2016, rcvd_tot + (lead(rcvd_tot_domestic,1)), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "BLZ" & year == 2016, rcvd_tot_domestic + (lead(rcvd_tot_domestic,1)), rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "BLZ" & year == 2016, rcvd_fld + (lead(rcvd_tot_domestic,1)), rcvd_fld))

## BTN
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "BTN" & year == 2017, rcvd_tot + (lead(rcvd_tot_domestic,1)), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "BTN" & year == 2017, rcvd_tot_domestic + (lead(rcvd_tot_domestic,1)), rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "BTN" & year == 2017, rcvd_fld + (lead(rcvd_tot_domestic,1)), rcvd_fld))

# BWA
# finance_merged <- finance_merged %>%
#   mutate(rcvd_tot = ifelse(iso3 == "BWA" & year == 2022, NA, rcvd_tot),
#          rcvd_tot_domestic = ifelse(iso3 == "BWA" & year == 2022, NA, rcvd_tot_domestic),
#          rcvd_tot_gf = ifelse(iso3 == "BWA" & year == 2022, NA, rcvd_tot_gf))

## CMR
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "CMR" & year == 2013, rcvd_tot + (lead(rcvd_tot_domestic,1)), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "CMR" & year == 2013, rcvd_tot_domestic + (lead(rcvd_tot_domestic,1)), rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "CMR" & year == 2013, rcvd_fld + (lead(rcvd_tot_domestic,1)), rcvd_fld))

## CIV
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "CIV" & year == 2014, rcvd_tot + (lead(rcvd_tot_domestic,2) + lead(rcvd_tot_domestic,3))/2, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "CIV" & year == 2014, rcvd_tot_domestic + (lead(rcvd_tot_domestic,2) + lead(rcvd_tot_domestic,3))/2, rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "CIV" & year == 2014, rcvd_fld + (lead(rcvd_tot_domestic,2) + lead(rcvd_tot_domestic,3))/2, rcvd_fld),
         cf_tot = ifelse(iso3 == "CIV" & year == 2015, rcvd_tot + (lead(rcvd_tot_domestic,1) + lead(rcvd_tot_domestic,2))/2, cf_tot),
         cf_tot_domestic = ifelse(iso3 == "CIV" & year == 2015, cf_tot_domestic + (lead(rcvd_tot_domestic,1) + lead(rcvd_tot_domestic,2))/2, cf_tot_domestic),
         cf_fld = ifelse(iso3 == "CIV" & year == 2015, rcvd_fld + (lead(rcvd_tot_domestic,1) + lead(rcvd_tot_domestic,2))/2, cf_fld)) 

## CMR
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "CMR" & year == 2022, rcvd_tot + (lag(rcvd_tot_domestic,1) + lag(rcvd_tot_domestic,2))/2, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "CMR" & year == 2022, rcvd_tot_domestic + (lag(rcvd_tot_domestic,1) + lag(rcvd_tot_domestic,2))/2, rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "CMR" & year == 2022, rcvd_fld + (lag(rcvd_tot_domestic,1) + lag(rcvd_tot_domestic,2))/2, rcvd_fld),
         rcvd_tot = ifelse(iso3 == "CMR" & year == 2013, rcvd_tot + (lead(rcvd_tot_domestic,1) + lead(rcvd_tot_domestic,2))/2, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "CMR" & year == 2013, rcvd_tot_domestic + (lead(rcvd_tot_domestic,1) + lead(rcvd_tot_domestic,2))/2, rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "CMR" & year == 2013, rcvd_fld + (lead(rcvd_tot_domestic,1) + lead(rcvd_tot_domestic,2))/2, rcvd_fld),) 
## COG
finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = ifelse(iso3 == "COG" & year == 2015 & (is.na(hcfvisit_dstb)|hcfvisit_dstb==0), lead(hcfvisit_dstb,1), hcfvisit_dstb),
         hospd_dstb_dur = ifelse(iso3 == "COG" & year == 2015 & (is.na(hospd_dstb_dur)|hospd_dstb_dur==0), lead(hospd_dstb_dur,1), hospd_dstb_dur),
         hospd_dstb_prct = ifelse(iso3 == "COG" & year == 2015 & (is.na(hospd_dstb_prct)|hospd_dstb_prct==0), lead(hospd_dstb_prct,1), hospd_dstb_prct),
         hcfvisit_mdr = ifelse(iso3 == "COG" & year == 2015 & (is.na(hcfvisit_mdr)|hcfvisit_mdr==0), lead(hcfvisit_mdr,1), hcfvisit_mdr),
         hospd_mdr_dur = ifelse(iso3 == "COG" & year == 2015 & (is.na(hospd_mdr_dur)|hospd_mdr_dur==0), lead(hospd_mdr_dur,1), hospd_mdr_dur),
         hospd_mdr_prct = ifelse(iso3 == "COG" & year == 2015 & (is.na(hospd_mdr_prct)|hospd_mdr_prct==0), lead(hospd_mdr_prct,1), hospd_mdr_prct),
         hospd_dstb_dur = ifelse(iso3 == "COG" & year == 2014 & (is.na(hospd_dstb_dur)|hospd_dstb_dur==0), lead(hospd_dstb_dur,2), hospd_dstb_dur),
         hospd_dstb_prct = ifelse(iso3 == "COG" & year == 2014 & (is.na(hospd_dstb_prct)|hospd_dstb_prct==0), lead(hospd_dstb_prct,2), hospd_dstb_prct),
         hospd_mdr_dur = ifelse(iso3 == "COG" & year == 2014 & (is.na(hospd_mdr_dur)|hospd_mdr_dur==0), lead(hospd_mdr_dur,2), hospd_mdr_dur),
         hospd_mdr_prct = ifelse(iso3 == "COG" & year == 2014 & (is.na(hospd_mdr_prct)|hospd_mdr_prct==0), lead(hospd_mdr_prct,2), hospd_mdr_prct),
         hospd_dstb_dur = ifelse(iso3 == "COG" & year == 2013 & (is.na(hospd_dstb_dur)|hospd_dstb_dur==0), lead(hospd_dstb_dur,3), hospd_dstb_dur),
         hospd_dstb_prct = ifelse(iso3 == "COG" & year == 2013 & (is.na(hospd_dstb_prct)|hospd_dstb_prct==0), lead(hospd_dstb_prct,3), hospd_dstb_prct),
         hospd_mdr_dur = ifelse(iso3 == "COG" & year == 2013 & (is.na(hospd_mdr_dur)|hospd_mdr_dur==0), lead(hospd_mdr_dur,3), hospd_mdr_dur),
         hospd_mdr_prct = ifelse(iso3 == "COG" & year == 2013 & (is.na(hospd_mdr_prct)|hospd_mdr_prct==0), lead(hospd_mdr_prct,3), hospd_mdr_prct))
         
## DZA
finance_merged <- finance_merged %>%
  mutate(rcvd_tot_domestic = ifelse(iso3 == "DZA" & year == 2014 & is.na(rcvd_tot_domestic), rcvd_tot, rcvd_tot_domestic)) 

## EGY
finance_merged <- finance_merged %>%
  mutate(cf_tot = ifelse(iso3 == "EGY" & (year <= 2014), NA, cf_tot)) 

## ERI
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "ERI" & year >= 2017 & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), rcvd_tot + 20000, rcvd_tot),
         rcvd_fld = ifelse(iso3 == "ERI" & year >= 2017 & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), rcvd_fld + 20000, rcvd_fld),
         rcvd_tot_domestic = ifelse(iso3 == "ERI" & year >= 2017 & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), rcvd_tot_domestic + 20000, rcvd_tot_domestic))

## FSM
finance_merged <- finance_merged %>%
  mutate(cf_tot = ifelse(iso3 == "FSM" & year == 2013 , NA, cf_tot),
         cf_tot_grnt = ifelse(iso3 == "FSM" & year == 2013 , NA, cf_tot_grnt),
         rcvd_tot = ifelse(iso3 == "FSM" & year == 2017 & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), rcvd_tot + 25000, rcvd_tot),
         rcvd_fld = ifelse(iso3 == "FSM" & year == 2017 & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), rcvd_fld + 25000, rcvd_fld),
         rcvd_tot_domestic = ifelse(iso3 == "FSM" & year == 2017 & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), rcvd_tot_domestic + 25000, rcvd_tot_domestic),
         cf_tot = ifelse(iso3 == "FSM" & year == 2018 & (cf_tot_domestic==0|is.na(cf_tot_domestic)), cf_tot + 25000, cf_tot),
         cf_fld = ifelse(iso3 == "FSM" & year == 2018 & (cf_tot_domestic==0|is.na(cf_tot_domestic)), cf_fld + 25000, cf_fld),
         cf_tot_domestic = ifelse(iso3 == "FSM" & year == 2018 & (cf_tot_domestic==0|is.na(cf_tot_domestic)), cf_tot_domestic + 25000, cf_tot_domestic))

finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = ifelse(iso3 == "FSM" & (year %in% c(2013,2019)) & (is.na(hcfvisit_dstb)|hcfvisit_dstb==0), lead(hcfvisit_dstb,1), hcfvisit_dstb),
         hospd_dstb_dur = ifelse(iso3 == "FSM" & (year %in% c(2013,2019)) & (is.na(hospd_dstb_dur)|hospd_dstb_dur==0), lead(hospd_dstb_dur,1), hospd_dstb_dur),
         hospd_dstb_prct = ifelse(iso3 == "FSM" & (year %in% c(2013,2019)) & (is.na(hospd_dstb_prct)|hospd_dstb_prct==0), lead(hospd_dstb_prct,1), hospd_dstb_prct))

finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = ifelse(iso3 == "FSM" & (year %in% c(2019)) & hcfvisit_dstb==0, lead(hcfvisit_dstb,1), hcfvisit_dstb))

## GHA
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "GHA" & year == 2022, rcvd_tot + (lag(rcvd_tot_domestic,1)), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "GHA" & year == 2022, rcvd_tot_domestic + (lag(rcvd_tot_domestic,1)), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "GHA" & year == 2022, rcvd_fld + (lag(rcvd_tot_domestic,1)), rcvd_staff))

## GMB
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "GMB" & (year == 2019|year == 2021) & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), rcvd_tot+18150, rcvd_tot),
         rcvd_fld = ifelse(iso3 == "GMB" & (year == 2019|year == 2021) & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), rcvd_fld+18150, rcvd_fld),
         rcvd_tot_domestic = ifelse(iso3 == "GMB" & (year == 2019|year == 2021) & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), 18150, rcvd_tot_domestic),
         cf_tot = ifelse(iso3 == "GMB" & (year == 2020) & (cf_tot_domestic==0|is.na(cf_tot_domestic)), cf_tot+18150, cf_tot),
         cf_fld = ifelse(iso3 == "GMB" & (year == 2020) & (cf_tot_domestic==0|is.na(cf_tot_domestic)), cf_fld+18150, cf_fld),
         cf_tot_domestic = ifelse(iso3 == "GMB" & (year == 2020) & (cf_tot_domestic==0|is.na(cf_tot_domestic)), 18150, cf_tot_domestic))

## GNB
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "GNB" & (year == 2013|year == 2014) & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), rcvd_tot+100000, rcvd_tot),
         rcvd_fld = ifelse(iso3 == "GNB" & (year == 2013|year == 2014) & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), rcvd_fld+100000, rcvd_fld),
         rcvd_tot_domestic = ifelse(iso3 == "GNB" & (year == 2013|year == 2014) & (rcvd_tot_domestic==0|is.na(rcvd_tot_domestic)), 100000, rcvd_tot_domestic))

## GNQ
finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = ifelse(iso3 == "GNQ" & year == 2013 & (is.na(hcfvisit_dstb)|hcfvisit_dstb==0), lead(hcfvisit_dstb,1), hcfvisit_dstb),
         hcfvisit_mdr = ifelse(iso3 == "GNQ" & year <= 2015 & (is.na(hcfvisit_mdr)|hcfvisit_mdr==0), 5 , hcfvisit_mdr))

## HND
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "HND" & year == 2021, rcvd_tot + (lag(rcvd_tot_domestic,1) + lag(rcvd_tot_domestic,2))/2, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "HND" & year == 2021, rcvd_tot_domestic + (lag(rcvd_tot_domestic,1) + lag(rcvd_tot_domestic,2))/2, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "HND" & year == 2021, rcvd_staff + (lag(rcvd_tot_domestic,1) + lag(rcvd_tot_domestic,2))/2, rcvd_staff),
         rcvd_tot = ifelse(iso3 == "HND" & year == 2022, rcvd_tot + (lag(rcvd_tot_domestic,3) + lag(rcvd_tot_domestic,2))/2, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "HND" & year == 2022, rcvd_tot_domestic + (lag(rcvd_tot_domestic,3) + lag(rcvd_tot_domestic,2))/2, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "HND" & year == 2022, NZ(rcvd_staff) + (lag(rcvd_tot_domestic,3) + lag(rcvd_tot_domestic,2))/2, rcvd_staff)) 

## JOR
finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = ifelse(iso3 == "JOR" & year == 2020, lag(hcfvisit_dstb,1), hcfvisit_dstb),
         hospd_mdr_dur = ifelse(iso3 == "JOR" & year == 2020, lag(hospd_mdr_dur,1), hospd_mdr_dur))


## KGZ
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "KGZ" & year == 2018, rcvd_tot + lead(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "KGZ" & year == 2018, rcvd_tot_domestic + lead(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "KGZ" & year == 2018, rcvd_staff + lead(rcvd_tot_domestic,1), rcvd_staff)) 

# KIR
kir_value <- (finance_merged$rcvd_tot_domestic[finance_merged$iso3=="KIR" & finance_merged$year==2021] + finance_merged$cf_tot_domestic[finance_merged$iso3=="KIR" & finance_merged$year==2022])/2

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "KIR" & (year %in% c(2013,2015:2018,2020)), rcvd_tot + kir_value, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "KIR" & (year %in% c(2013,2015:2018,2020)), rcvd_tot_domestic + kir_value, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "KIR" & (year %in% c(2013,2015:2018,2020)), rcvd_staff + kir_value, rcvd_staff),
         cf_tot = ifelse(iso3 == "KIR" & (year == 2014|year == 2019), cf_tot + kir_value, cf_tot),
         cf_tot_domestic = ifelse(iso3 == "KIR" & (year == 2014|year == 2019), cf_tot_domestic + kir_value, cf_tot_domestic),
         cf_staff = ifelse(iso3 == "KIR" & (year == 2014|year == 2019), cf_staff + kir_value, cf_staff))
         
# LAO
lao_value <- (finance_merged$rcvd_tot_domestic[finance_merged$iso3=="LAO" & finance_merged$year==2018] + finance_merged$cf_tot_domestic[finance_merged$iso3=="LAO" & finance_merged$year==2015])/2

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "LAO" & (year %in% c(2016:2017)), rcvd_tot + lao_value, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "LAO" & (year %in% c(2016:2017)), rcvd_tot_domestic + lao_value, rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "LAO" & (year %in% c(2016:2017)), rcvd_fld + lao_value, rcvd_fld),
         rcvd_tot = ifelse(iso3 == "LAO" & (year %in% c(2013)), rcvd_tot + lead(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "LAO" & (year %in% c(2013)), rcvd_tot_domestic + lead(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "LAO" & (year %in% c(2013)), rcvd_fld + lead(rcvd_tot_domestic,1), rcvd_fld))

## LBR
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "LBR" & (year %in% c(2015)), rcvd_tot + lead(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "LBR" & (year %in% c(2015)), rcvd_tot_domestic + lead(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "LBR" & (year %in% c(2015)), rcvd_fld + lead(rcvd_tot_domestic,1), rcvd_fld),
         rcvd_tot = ifelse(iso3 == "LBR" & (year %in% c(2018,2021,2022)), rcvd_tot + cf_tot_domestic, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "LBR" & (year %in% c(2018,2021,2022)), rcvd_tot_domestic + cf_tot_domestic, rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "LBR" & (year %in% c(2018,2021,2022)), rcvd_fld + cf_tot_domestic, rcvd_fld),
         rcvd_tot = ifelse(iso3 == "LBR" & (year %in% c(2019,2022,2023)), rcvd_tot + lag(cf_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "LBR" & (year %in% c(2019,2022,2023)), NZ(rcvd_tot_domestic) + lag(cf_tot_domestic,1), rcvd_tot_domestic),
         rcvd_fld = ifelse(iso3 == "LBR" & (year %in% c(2019,2022,2023)), rcvd_fld + lag(cf_tot_domestic,1), rcvd_fld))

finance_merged <- finance_merged %>%
  mutate(cf_tot = ifelse(iso3 == "LBR" & (year %in% c(2019)), cf_tot + lead(cf_tot_domestic,1), cf_tot),
         cf_tot_domestic = ifelse(iso3 == "LBR" & (year %in% c(2019)), cf_tot_domestic + lead(cf_tot_domestic,1), cf_tot_domestic),
         cf_staff = ifelse(iso3 == "LBR" & (year %in% c(2019)), cf_staff + lead(cf_tot_domestic,1), cf_staff))

## LSO
lso_value <- (finance_merged$rcvd_tot_domestic[finance_merged$iso3=="LSO" & finance_merged$year==2016] + finance_merged$cf_tot_domestic[finance_merged$iso3=="LSO" & finance_merged$year==2013])/2

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "LSO" & (year %in% c(2014:2015)), rcvd_tot + lso_value, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "LSO" & (year %in% c(2014:2015)), rcvd_tot_domestic + lso_value, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "LSO" & (year %in% c(2014:2015)), rcvd_staff + lso_value, rcvd_staff))

## MAR
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "MAR" & (year %in% c(2022)), rcvd_tot + lag(rcvd_staff,1) + lag(rcvd_prog,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "MAR" & (year %in% c(2022)), rcvd_tot_domestic + lag(rcvd_staff,1) + lag(rcvd_prog,1), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "MAR" & (year %in% c(2022)), NZ(rcvd_staff) + lag(rcvd_staff,1), rcvd_staff),
         rcvd_prog = ifelse(iso3 == "MAR" & (year %in% c(2022)), NZ(rcvd_prog) + lag(rcvd_prog,1), rcvd_prog))

## MDG
mdg_value <- (finance_merged$rcvd_tot_domestic[finance_merged$iso3=="MDG" & finance_merged$year==2017])

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "MDG" & (year %in% c(2018:2020,2022)), rcvd_tot + mdg_value, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "MDG" & (year %in% c(2018:2020,2022)), rcvd_tot_domestic + mdg_value, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "MDG" & (year %in% c(2018:2020,2022)), rcvd_staff + mdg_value, rcvd_staff),
         cf_tot = ifelse(iso3 == "MDG" & (year %in% c(2021)), cf_tot + mdg_value, cf_tot),
         cf_tot_domestic = ifelse(iso3 == "MDG" & (year %in% c(2021)), cf_tot_domestic + mdg_value, cf_tot_domestic),
         cf_staff = ifelse(iso3 == "MDG" & (year %in% c(2021)), cf_staff + mdg_value, cf_staff))


## MHL
mhl_value <- (finance_merged$rcvd_tot_domestic[finance_merged$iso3=="MHL" & finance_merged$year==2019])

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "MHL" & (year %in% c(2016:2017)), rcvd_tot + mhl_value, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "MHL" & (year %in% c(2016:2017)), NZ(rcvd_tot_domestic) + mhl_value, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "MHL" & (year %in% c(2016:2017)), NZ(rcvd_staff) + mhl_value, rcvd_staff))

# MRT
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "MRT" & year == 2019, rcvd_tot + lag(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "MRT" & year == 2019, NZ(rcvd_tot_domestic) + lag(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_prog = ifelse(iso3 == "MRT" & year == 2019, NZ(rcvd_prog) + lag(rcvd_tot_domestic,1), rcvd_prog)) %>%
  mutate(rcvd_tot = ifelse(iso3 == "MRT" & year == 2017, rcvd_tot + lead(rcvd_tot_domestic,1), rcvd_tot),
                rcvd_tot_domestic = ifelse(iso3 == "MRT" & year == 2017, NZ(rcvd_tot_domestic) + lead(rcvd_tot_domestic,1), rcvd_tot_domestic),
                rcvd_prog = ifelse(iso3 == "MRT" & year == 2017, NZ(rcvd_prog) + lead(rcvd_tot_domestic,1), rcvd_prog),
         ) 

# NER
ner_value <- (finance_merged$rcvd_tot_domestic[finance_merged$iso3=="NER" & finance_merged$year==2015])

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "NER" & (year %in% c(2018:2021)), rcvd_tot + ner_value, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "NER" & (year %in% c(2018:2021)), NZ(rcvd_tot_domestic) + ner_value, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "NER" & (year %in% c(2018:2021)), NZ(rcvd_staff) + ner_value, rcvd_staff))

finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = ifelse(iso3 == "NER" & year == 2013 & (is.na(hcfvisit_dstb)|hcfvisit_dstb==0), lead(hcfvisit_dstb,1), hcfvisit_dstb),
         hcfvisit_dstb = ifelse(iso3 == "NER" & year == 2016 & (is.na(hcfvisit_dstb)|hcfvisit_dstb==0), lead(hcfvisit_dstb,2) , hcfvisit_dstb))

# PSE
finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = ifelse(iso3 == "PSE" & year == 2013 & (is.na(hcfvisit_dstb)|hcfvisit_dstb==0), lead(hcfvisit_dstb,1), hcfvisit_dstb))

# SDN
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "SDN" & (year %in% c(2015,2019,2022)), rcvd_tot + lag(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "SDN" & (year %in% c(2015,2019,2022)), rcvd_tot_domestic + lag(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "SDN" & (year %in% c(2015,2019,2022)), rcvd_staff + lag(rcvd_tot_domestic,1), rcvd_staff)) 

# SLE
sle_value <- (finance_merged$rcvd_tot_domestic[finance_merged$iso3=="SLE" & finance_merged$year==2017])

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "SLE" & (year %in% c(2016)), rcvd_tot + sle_value, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "SLE" & (year %in% c(2016)), NZ(rcvd_tot_domestic) + sle_value, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "SLE" & (year %in% c(2016)), NZ(rcvd_staff) + sle_value, rcvd_staff),
         cf_tot = ifelse(iso3 == "SLE" & (year %in% c(2015)), cf_tot + sle_value, cf_tot),
         cf_tot_domestic = ifelse(iso3 == "SLE" & (year %in% c(2015)), NZ(cf_tot_domestic) + sle_value, cf_tot_domestic),
         cf_staff = ifelse(iso3 == "SLE" & (year %in% c(2015)), NZ(cf_staff) + sle_value, cf_staff))

# SOM
finance_merged <- finance_merged %>%
  mutate(hcfvisit_mdr = ifelse(iso3 == "SOM" & year == 2013 & (is.na(hcfvisit_mdr)|hcfvisit_mdr==0), lead(hcfvisit_mdr,1), hcfvisit_mdr))

# SSD
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "SSD" & year >= 2017, rcvd_tot + 60000, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "SSD" & year >= 2017, NZ(rcvd_tot_domestic) + 60000, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "SSD" & year >= 2017, NZ(rcvd_staff) + 60000, rcvd_staff))

finance_merged <- finance_merged %>%
  mutate(hospd_dstb_prct = ifelse(iso3 == "SSD" & year == 2020, lead(hospd_dstb_prct,1), hospd_dstb_prct))

# TCD
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "TCD" & (year %in% c(2016)), rcvd_tot + lag(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "TCD" & (year %in% c(2016)), rcvd_tot_domestic + lag(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "TCD" & (year %in% c(2016)), rcvd_staff + lag(rcvd_tot_domestic,1), rcvd_staff)) 

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "TCD" & (year %in% c(2017)), rcvd_tot + lead(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "TCD" & (year %in% c(2017)), rcvd_tot_domestic + lead(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "TCD" & (year %in% c(2017)), rcvd_staff + lead(rcvd_tot_domestic,1), rcvd_staff)) 

# TKM
tkm_value <- (finance_merged$rcvd_tot_domestic[finance_merged$iso3=="TKM" & finance_merged$year==2020])

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "TKM" & (year %in% c(2014,2018)), rcvd_tot + tkm_value, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "TKM" & (year %in% c(2014,2018)), NZ(rcvd_tot_domestic) + tkm_value, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "TKM" & (year %in% c(2014,2018)), NZ(rcvd_staff) + tkm_value, rcvd_staff),
         cf_tot = ifelse(iso3 == "TKM" & (year %in% c(2015,2017)), cf_tot + tkm_value, cf_tot),
         cf_tot_domestic = ifelse(iso3 == "TKM" & (year %in% c(2015,2017)), NZ(cf_tot_domestic) + tkm_value, cf_tot_domestic),
         cf_staff = ifelse(iso3 == "TKM" & (year %in% c(2015,2017)), NZ(cf_staff) + tkm_value, cf_staff))

# TLS
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "TLS" & (year %in% c(2018,2021)), rcvd_tot + lag(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "TLS" & (year %in% c(2018,2021)), rcvd_tot_domestic + lag(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "TLS" & (year %in% c(2018,2021)), rcvd_staff + lag(rcvd_tot_domestic,1), rcvd_staff)) 

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "TLS" & (year %in% c(2019,2022)), rcvd_tot + lag(rcvd_tot_domestic,2), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "TLS" & (year %in% c(2019,2022)), rcvd_tot_domestic + lag(rcvd_tot_domestic,2), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "TLS" & (year %in% c(2019,2022)), rcvd_staff + lag(rcvd_tot_domestic,2), rcvd_staff)) 

# UGA
uga_value <- (finance_merged$rcvd_tot_domestic[finance_merged$iso3=="UGA" & finance_merged$year==2016])

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "UGA" & (year %in% c(2014,2017)), rcvd_tot + uga_value, rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "UGA" & (year %in% c(2014,2017)), NZ(rcvd_tot_domestic) + uga_value, rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "UGA" & (year %in% c(2014,2017)), NZ(rcvd_staff) + uga_value, rcvd_staff))

# VEN         
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "VEN" & (year %in% c(2017,2019)), rcvd_tot + lag(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "VEN" & (year %in% c(2017,2019)), rcvd_tot_domestic + lag(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "VEN" & (year %in% c(2017,2019)), rcvd_staff + lag(rcvd_tot_domestic,1), rcvd_staff)) 

# VUT
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "VUT" & (year %in% c(2022)), rcvd_tot + lag(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "VUT" & (year %in% c(2022)), rcvd_tot_domestic + lag(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "VUT" & (year %in% c(2022)), rcvd_staff + lag(rcvd_tot_domestic,1), rcvd_staff),
         rcvd_tot_domestic = ifelse(iso3 == "VUT" & (year %in% c(2021)) & rcvd_tot_domestic==102, NA, rcvd_tot_domestic)) 

# ZWE
finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "ZWE" & (year %in% c(2015)), rcvd_tot + lag(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "ZWE" & (year %in% c(2015)), rcvd_tot_domestic + lag(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "ZWE" & (year %in% c(2015)), rcvd_staff + lag(rcvd_tot_domestic,1), rcvd_staff)) 

finance_merged <- finance_merged %>%
  mutate(rcvd_tot = ifelse(iso3 == "ZWE" & (year %in% c(2016)), rcvd_tot + lead(rcvd_tot_domestic,1), rcvd_tot),
         rcvd_tot_domestic = ifelse(iso3 == "ZWE" & (year %in% c(2016)), rcvd_tot_domestic + lead(rcvd_tot_domestic,1), rcvd_tot_domestic),
         rcvd_staff = ifelse(iso3 == "ZWE" & (year %in% c(2016)), rcvd_staff + lead(rcvd_tot_domestic,1), rcvd_staff)) 

# IND/RUS
finance_merged <- finance_merged %>%
  mutate(hcfvisit_dstb = ifelse((iso3 == "IND"|iso3 == "RUS") & year == 2013 & (is.na(hcfvisit_dstb)|hcfvisit_dstb==0), lead(hcfvisit_dstb,1), hcfvisit_dstb))

## ungrouping
finance_merged <- finance_merged %>%
  ungroup()

#--------------------
# 2024 revision 
#--------------------
# Guatemala: drug cost distributions
finance_merged <- finance_merged %>%
  mutate(cf_fld = ifelse(iso3 == "GTM" & cf_fld==1033996 & year == 2024, 836618, cf_fld),
         cf_sld = ifelse(iso3 == "GTM" & cf_fld==1033996 & year == 2024, 141378, cf_sld),
         cf_tpt = ifelse(iso3 == "GTM" & cf_fld==1033996 & year == 2024, 56000, cf_tpt))

# North Makedonia 2016 data
finance_merged <- finance_merged %>%
  mutate(cf_fld = ifelse(iso3 == "GTM" & cf_fld==1033996 & year == 2024, 836618, cf_fld),
         cf_sld = ifelse(iso3 == "GTM" & cf_fld==1033996 & year == 2024, 141378, cf_sld),
         cf_tpt = ifelse(iso3 == "GTM" & cf_fld==1033996 & year == 2024, 56000, cf_tpt))

# Sudan and Chad average drug cost
finance_merged <- finance_merged %>%
  mutate(cf_fld = ifelse((iso3 == "SDN"|iso3 == "TCD") & year == 2023, exp_cpp_dstb/c_notified, exp_cpp_dstb),
         cf_sld = ifelse((iso3 == "SDN"|iso3 == "TCD") & year == 2023, exp_cpp_mdr/mdr_tx, exp_cpp_mdr))

# North Macedonia 2015-2016 data
finance_merged <- finance_merged %>%
  mutate(across(starts_with("cf_"), ~ if_else(iso3 == "MKD" & year == 2016, . / 55.7127, .)),
         across(starts_with("cf_"), ~ if_else(iso3 == "MKD" & (year == 2013 | year == 2014) , . / 45.868, .)),
         across(starts_with("rcvd_"), ~ if_else(iso3 == "MKD" & (year == 2013) , . / 45.868, .)),
         across(starts_with("rcvd_"), ~ if_else(iso3 == "MKD" & (year == 2015) , . / 55.1318, .)),
         across(starts_with("exp_"), ~ if_else(iso3 == "MKD" & (year == 2013 | year == 2014) , . / 45.868, .)),
         across(starts_with("exp_"), ~ if_else(iso3 == "MKD" & (year == 2013 | year == 2014) , . / 45.868, .)))

finance_merged <- finance_merged %>%
  mutate(exp_tot = if_else(iso3 == "MKD" & year == 2023, 208758.461, exp_tot),
         cf_tot = if_else(iso3 == "MKD" & year == 2024, 173965.384, cf_tot))

# RUS income group to be UMC for analysis
finance_merged <- finance_merged %>%
  mutate(g_income = if_else(iso3 == "RUS", "UMC", g_income))

# India 40 million in 2022 for staff: discussed and agreed with KF
# finance_merged <- finance_merged %>% 
#   mutate(rcvd_staff = ifelse(iso3 == "IND" & (year == 2022|year == 2023), rcvd_staff + 40e6, rcvd_staff ),
#          rcvd_nmdr_dot = ifelse(iso3 == "IND" & (year == 2022|year == 2023), rcvd_nmdr_dot + 40e6, rcvd_nmdr_dot ),
#          rcvd_int = ifelse(iso3 == "IND" & (year == 2022|year == 2023), rcvd_int + 40e6, rcvd_int ),
#          rcvd_tot = ifelse(iso3 == "IND" & (year == 2022|year == 2023), rcvd_tot + 40e6, rcvd_tot ))

finance_merged <- finance_merged %>%
  mutate(rcvd_staff = ifelse(iso3 == "IND" & (year == 2022|year == 2023), rcvd_staff + 40e6, rcvd_staff ),
         exp_staff = ifelse(iso3 == "IND" & (year == 2022|year == 2023), exp_staff + 40e6, exp_staff ),
         rcvd_tot = ifelse(iso3 == "IND" & (year == 2022|year == 2023), rcvd_tot + 40e6, rcvd_tot ),
         exp_tot = ifelse(iso3 == "IND" & (year == 2022|year == 2023), exp_tot + 40e6, exp_tot ),
         rcvd_tot_domestic = ifelse(iso3 == "IND" & (year == 2022|year == 2023), rcvd_tot_domestic + 40e6, rcvd_tot_domestic )
  )

# Botswana data for 2022 to be missing as the data is not reliable
finance_merged <- finance_merged %>%
  mutate(across(starts_with("rcvd_"), ~ if_else(iso3 == "BWA" & year == 2022 & rcvd_tot == 890000, NA, .)),
         across(starts_with("exp_"), ~ if_else(iso3 == "BWA" & year == 2022 & exp_tot == 890000, NA, .)))

finance_merged <- finance_merged %>%
  mutate(across(starts_with("rcvd_"), ~ if_else(iso3 == "VUT" & year == 2019 & rcvd_tot == 15261164, NA, .)),
         across(starts_with("exp_"), ~ if_else(iso3 == "VUT" & year == 2019 & exp_tot == 15261164, NA, .)))

finance_merged <- finance_merged %>%
  mutate(hospd_dstb_prct = ifelse(iso3 == "BIH" & (year == 2023) & hospd_dstb_prct == 9, lag(hospd_dstb_prct,1), hospd_dstb_prct ))
         