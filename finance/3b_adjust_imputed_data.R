# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# a new script, for adjusting imputed data. 
# this is based on Stata version of cleaning script.
# Takuya Yamanaka, February 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# this script will be run by "ch4_3_impute_data.R"

#-- Adjustment starts from here --#

finance_merged <- finance_merged %>%
  mutate(
    rcvd_int = if_else(iso3 == "CUB" & is.na(rcvd_int) & is.na(rcvd_tot_gf), rcvd_tot, rcvd_int),
    rcvd_int = if_else(iso3 == "PRK" & year == 2022 & is.na(rcvd_tot), lag(rcvd_int), rcvd_int)
  )


# ZWE: c_clinic_nmdr overly high in 2012 - 2013 since comuptation used previous data (hcfvisit_sp & _sn) which were set at 240 visits instead of 4 - 6 visits for DS)
# finance_merged <- finance_merged %>%
#   arrange(iso3, year) %>%
#   mutate(
#     hcfvisit_sp = ifelse(year > 2010 & year <= 2013 & iso3 == "ZWE", lag(hcfvisit_sp), hcfvisit_sp),
#     hcfvisit_sn = ifelse(year > 2010 & year <= 2013 & iso3 == "ZWE", lag(hcfvisit_sn), hcfvisit_sn)
#   )

# finance_merged <- finance_merged %>%
#   mutate(
#     sh_sp = ifelse(iso3 == "ZWE" & (year >= 2006 & year <= 2013), c_clinic_sp / (c_clinic_sp + c_clinic_sn), NA),
#     sh_sp = ifelse(year > 2010 & year <= 2013 & iso3 == "ZWE" & (is.na(sh_sp) | sh_sp == 0 | sh_sp == 1), lag(sh_sp), sh_sp),
#     c_clinic_sp = ifelse(year > 2010 & year <= 2013 & iso3 == "ZWE", c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp, c_clinic_sp),
#     c_clinic_sn = ifelse(year > 2010 & year <= 2013 & iso3 == "ZWE", c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp), c_clinic_sn)
#   ) %>%
#   select(-sh_sp)

# # //permanent refix of OP cost aggregate since existing value was too high
# finance_merged <- finance_merged %>%
#   mutate(
#     c_clinic_nmdr = ifelse(year > 2010 & year <= 2013 & iso3 == "ZWE", c_clinic_sp + c_clinic_sn, c_clinic_nmdr)
#   )


# AGO: c_hospital_nmdr is exagerrated (33M compared to 2M in neighbor years). Root cause is computational issue in 2012 - 13
# finance_merged <- finance_merged %>%
#   arrange(iso3, year) %>%  # sort by country and year in ascending order
#   mutate(
#     c_clinic_nmdr = ifelse(year > 2010 & year <= 2013 & iso3 == "AGO", lag(c_clinic_nmdr), c_clinic_nmdr),
#     c_hospital_nmdr = ifelse(year > 2010 & year <= 2013 & iso3 == "AGO", lag(c_hospital_nmdr), c_hospital_nmdr)
#   )
# 
# # COG: backfill 2012 and 2013 hospital values using 2014's 
# finance_merged <- finance_merged %>%
#   arrange(iso3, year) %>%  # sort by country and year in ascending order
#   mutate(
#     hospd_sn_dur = ifelse(year > 2011 & year <= 2013 & iso3 == "COG", lag(hospd_sn_dur), hospd_sn_dur),
#     hospd_sp_dur = ifelse(year > 2011 & year <= 2013 & iso3 == "COG", lag(hospd_sp_dur), hospd_sp_dur),
#     hospd_sn_prct = ifelse(year > 2011 & year <= 2013 & iso3 == "COG", lag(hospd_sn_prct), hospd_sn_prct),
#     hospd_sp_prct = ifelse(year > 2011 & year <= 2013 & iso3 == "COG", lag(hospd_sp_prct), hospd_sp_prct)
#   )
# 
# finance_merged <- finance_merged %>%
#   mutate(
#     sh_sp = ifelse(iso3 == "COG" & (year >= 2006 & year <= 2013), c_hospital_sp / (c_hospital_sp + c_hospital_sn), NA),
#     sh_sp = ifelse(year > 2011 & year <= 2013 & iso3 == "COG" & (is.na(sh_sp) | sh_sp == 0 | sh_sp == 1), lag(sh_sp), sh_sp),
#     c_hospital_sp = ifelse(year > 2011 & year <= 2013 & iso3 == "COG", c_notified * hospd_sp_dur * hospd_sp_prct * uc_visit_cur_usd * sh_sp, c_hospital_sp),
#     c_hospital_sn = ifelse(year > 2011 & year <= 2013 & iso3 == "COG", c_notified * hospd_sn_dur * hospd_sn_prct * uc_visit_cur_usd * (1 - sh_sp), c_hospital_sn)
#   ) %>%
#   select(-sh_sp)
# 
# finance_merged <- finance_merged %>%
#   mutate(
#     c_hospital_nmdr = ifelse(year > 2011 & year <= 2013 & iso3 == "COG", c_hospital_sp + c_hospital_sn, c_hospital_nmdr)
#   )


# CIV: backfill from 2015

# finance_merged <- finance_merged %>%
#   arrange(iso3, desc(year)) %>%
#   mutate(
#     c_clinic_nmdr = ifelse(year < 2015 & iso3 == "CIV", lag(c_clinic_nmdr), c_clinic_nmdr),
#     c_hospital_nmdr = ifelse(year < 2015 & iso3 == "CIV", lag(c_hospital_nmdr), c_hospital_nmdr),
#     c_clinic_mdr = ifelse(year < 2015 & iso3 == "CIV", lag(c_clinic_mdr), c_clinic_mdr),
#     c_hospital_mdr = ifelse(year < 2015 & iso3 == "CIV", lag(c_hospital_mdr), c_hospital_mdr)
#   )
# 
# # // Cuba 2012 & 13
# # finance_merged <- finance_merged %>%
# #   arrange(iso3, year) %>%
# #   mutate(
# #     hcfvisit_sp = ifelse(year > 2011 & year <= 2013 & iso3 == "CUB", lag(hcfvisit_sp), hcfvisit_sp),
# #     hcfvisit_sn = ifelse(year > 2011 & year <= 2013 & iso3 == "CUB", lag(hcfvisit_sn), hcfvisit_sn)
# #   )
# 
# finance_merged <- finance_merged %>%
#   mutate(sh_sp = ifelse(iso3 == "CUB" & year >= 2006 & year <= 2013,
#                         c_clinic_sp / (c_clinic_sp + c_clinic_sn),
#                         NA_real_))
# 
# finance_merged <- finance_merged %>%
#   mutate(sh_sp = ifelse(iso3 == "CUB" & (year > 2011 & year <= 2013) & (is.na(sh_sp) | sh_sp == 0 | sh_sp == 1),
#                         lag(sh_sp),
#                         sh_sp))
# 
# finance_merged <- finance_merged %>%
#   mutate(c_clinic_sp = ifelse(year > 2011 & year <= 2013 & iso3 == "CUB",
#                               c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp,
#                               c_clinic_sp),
#          c_clinic_sn = ifelse(year > 2011 & year <= 2013 & iso3 == "CUB",
#                               c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp),
#                               c_clinic_sn)) %>%
#   # Drop sh_sp
#   select(-sh_sp)
# 
# finance_merged <- finance_merged %>%
#   mutate(c_clinic_nmdr = ifelse(year > 2011 & year <= 2013 & iso3 == "CUB",
#                                 c_clinic_sp + c_clinic_sn,
#                                 c_clinic_nmdr))
# 
# 
# # ECuador 2012
# finance_merged <- finance_merged %>% 
#   arrange(iso3, desc(year))
# 
# finance_merged <- finance_merged %>% 
#   mutate(
#     hcfvisit_sp = ifelse(year == 2012 & iso3 == "ECU", lag(hcfvisit_sp), hcfvisit_sp),
#     hcfvisit_sn = ifelse(year == 2012 & iso3 == "ECU", lag(hcfvisit_sn), hcfvisit_sn)
#   )
# 
# finance_merged <- finance_merged %>% 
#   mutate(
#     sh_sp = ifelse(iso3 == "ECU" & year >= 2006 & year <= 2013,
#                    c_clinic_sp / (c_clinic_sp + c_clinic_sn),
#                    NA_real_)
#   )
# 
# finance_merged <- finance_merged %>% 
#   mutate(
#     sh_sp = ifelse(iso3 == "ECU" & year == 2012 & (is.na(sh_sp) | sh_sp == 0 | sh_sp == 1),
#                    lag(sh_sp),
#                    sh_sp)
#   )
# 
# finance_merged <- finance_merged %>% 
#   mutate(
#     c_clinic_sp = ifelse(year == 2012 & iso3 == "ECU",
#                          c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp,
#                          c_clinic_sp),
#     c_clinic_sn = ifelse(year == 2012 & iso3 == "ECU",
#                          c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp),
#                          c_clinic_sn)
#   ) %>%
#   select(-sh_sp)  # Drop sh_sp
# 
# finance_merged <- finance_merged %>% 
#   mutate(
#     c_clinic_nmdr = ifelse(year == 2012 & iso3 == "ECU",
#                            c_clinic_sp + c_clinic_sn,
#                            c_clinic_nmdr)
#   )
# 
# 
# # Guatemala 2010
# finance_merged <- finance_merged %>% 
#   arrange(iso3, year)
# 
# finance_merged <- finance_merged %>% 
#   mutate(
#     hcfvisit_sp = ifelse(year == 2010 & iso3 == "GTM", lag(hcfvisit_sp), hcfvisit_sp),
#     hcfvisit_sn = ifelse(year == 2010 & iso3 == "GTM", lag(hcfvisit_sn), hcfvisit_sn)
#   )
# 
# finance_merged <- finance_merged %>% 
#   mutate(
#     sh_sp = ifelse(iso3 == "GTM" & year >= 2006 & year <= 2013,
#                    c_clinic_sp / (c_clinic_sp + c_clinic_sn),
#                    NA_real_)
#   )
# 
# finance_merged <- finance_merged %>% 
#   mutate(
#     sh_sp = ifelse(iso3 == "GTM" & year == 2010 & (is.na(sh_sp) | sh_sp == 0 | sh_sp == 1),
#                    lag(sh_sp),
#                    sh_sp)
#   )
# 
# finance_merged <- finance_merged %>% 
#   mutate(
#     c_clinic_sp = ifelse(year == 2010 & iso3 == "GTM",
#                          c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp,
#                          c_clinic_sp),
#     c_clinic_sn = ifelse(year == 2010 & iso3 == "GTM",
#                          c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp),
#                          c_clinic_sn)
#   ) %>%
#   select(-sh_sp)  # Drop sh_sp
# 
# finance_merged <- finance_merged %>% 
#   mutate(
#     c_clinic_nmdr = ifelse(year == 2010 & iso3 == "GTM",
#                            c_clinic_sp + c_clinic_sn,
#                            c_clinic_nmdr)
#   )
# 
# # Honduras: backfill from 2014 since previous estimates are unreliable
# finance_merged <- finance_merged %>% 
#   arrange(iso3, desc(year))
# 
# # Replace c_clinic_nmdr, c_hospital_nmdr, c_clinic_mdr, and c_hospital_mdr under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     c_clinic_nmdr = ifelse(year < 2014 & year >= 2008 & iso3 == "HND",
#                            lag(c_clinic_nmdr),
#                            c_clinic_nmdr),
#     c_hospital_nmdr = ifelse(year < 2014 & year >= 2008 & iso3 == "HND",
#                              lag(c_hospital_nmdr),
#                              c_hospital_nmdr),
#     c_clinic_mdr = ifelse(year < 2014 & year >= 2008 & iso3 == "HND",
#                           lag(c_clinic_mdr),
#                           c_clinic_mdr),
#     c_hospital_mdr = ifelse(year < 2014 & year >= 2008 & iso3 == "HND",
#                             lag(c_hospital_mdr),
#                             c_hospital_mdr)
#   )
# 
# # Iran: replace c_clnic and c_hospital costs from 2013 and before (negative hospital costs pre 2014?)
# finance_merged <- finance_merged %>% 
#   arrange(iso3, desc(year))
# 
# # Replace hcfvisit_sp and hcfvisit_sn under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     hcfvisit_sp = ifelse(hcfvisit_sp == . & year <= 2013 & iso3 == "IRN", lag(hcfvisit_sp), hcfvisit_sp),
#     hcfvisit_sn = ifelse(hcfvisit_sn == . & year <= 2013 & iso3 == "IRN", lag(hcfvisit_sn), hcfvisit_sn)
#   )
# 
# # Generate sh_sp and replace its values under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     sh_sp = ifelse(iso3 == "IRN" & (year >= 2006 & year <= 2013),
#                    c_clinic_sp / (c_clinic_sp + c_clinic_sn),
#                    NA_real_),
#     sh_sp = ifelse(iso3 == "IRN" & year == 2010 & (sh_sp == . | sh_sp == 0 | sh_sp == 1),
#                    lag(sh_sp),
#                    sh_sp)
#   )
# 
# # Replace c_clinic_sp, c_clinic_sn, and c_clinic_nmdr under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     c_clinic_sp = ifelse(year == 2010 & iso3 == "IRN",
#                          c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp,
#                          c_clinic_sp),
#     c_clinic_sn = ifelse(year == 2010 & iso3 == "IRN",
#                          c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp),
#                          c_clinic_sn),
#     c_clinic_nmdr = ifelse(year == 2010 & iso3 == "IRN",
#                            c_clinic_sp + c_clinic_sn,
#                            c_clinic_nmdr)
#   )
# 
# # Replace hospd_sp_prct and hospd_sn_prct under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     hospd_sp_prct = ifelse(year <= 2013 & iso3 == "IRN", hospd_dstb_prct, hospd_sp_prct),
#     hospd_sn_prct = ifelse(year <= 2013 & iso3 == "IRN", hospd_dstb_prct, hospd_sn_prct)
#   )
# 
# # Generate sh_sp and replace its values under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     sh_sp = ifelse(iso3 == "IRN" & (year >= 2006 & year <= 2013),
#                    c_hospital_sp / (c_hospital_sp + c_hospital_sn),
#                    NA_real_),
#     sh_sp = ifelse((year > 2011 & year <= 2013) & iso3 == "IRN" & (sh_sp == . | sh_sp == 0 | sh_sp == 1),
#                    lag(sh_sp),
#                    sh_sp)
#   )
# 
# # Replace c_hospital_sp, c_hospital_sn, and c_hospital_nmdr under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     c_hospital_sp = ifelse(year > 2011 & year <= 2013 & iso3 == "IRN",
#                            c_notified * hospd_sp_dur * hospd_sp_prct * uc_visit_cur_usd * sh_sp,
#                            c_hospital_sp),
#     c_hospital_sn = ifelse(year > 2011 & year <= 2013 & iso3 == "IRN",
#                            c_notified * hospd_sn_dur * hospd_sn_prct * uc_visit_cur_usd * (1 - sh_sp),
#                            c_hospital_sn),
#     c_hospital_nmdr = ifelse(year <= 2013 & iso3 == "IRN",
#                              c_hospital_sp + c_hospital_sn,
#                              c_hospital_nmdr)
#   )
# 
# 
# # MDG: 2010 utilixation for OP was faulty. Replace with next year.
# finance_merged <- finance_merged %>% 
#   arrange(iso3, desc(year))
# 
# # Replace hcfvisit_sp and hcfvisit_sn under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     hcfvisit_sp = ifelse(year == 2010 & iso3 == "MDG", lag(hcfvisit_sp), hcfvisit_sp),
#     hcfvisit_sn = ifelse(year == 2010 & iso3 == "MDG", lag(hcfvisit_sn), hcfvisit_sn)
#   )
# 
# # Generate sh_sp and replace its values under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     sh_sp = ifelse(iso3 == "MDG" & (year >= 2006 & year <= 2013),
#                    c_clinic_sp / (c_clinic_sp + c_clinic_sn),
#                    NA_real_),
#     sh_sp = ifelse(iso3 == "MDG" & year == 2010 & (sh_sp == . | sh_sp == 0 | sh_sp == 1),
#                    lag(sh_sp),
#                    sh_sp)
#   )
# 
# # Replace c_clinic_sp, c_clinic_sn, and c_clinic_nmdr under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     c_clinic_sp = ifelse(year == 2010 & iso3 == "MDG",
#                          c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp,
#                          c_clinic_sp),
#     c_clinic_sn = ifelse(year == 2010 & iso3 == "MDG",
#                          c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp),
#                          c_clinic_sn),
#     c_clinic_nmdr = ifelse(year == 2010 & iso3 == "MDG",
#                            c_clinic_sp + c_clinic_sn,
#                            c_clinic_nmdr)
#   )
# 
# # MDV: 2009 and 2010 IP Proporton backfilled with 2011
# finance_merged <- finance_merged %>% 
#   arrange(iso3, desc(year))
# 
# # Generate sh_sp for MDV and replace its values under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     sh_sp = ifelse(iso3 == "MDV" & (year >= 2006 & year <= 2013),
#                    c_clinic_sp / (c_clinic_sp + c_clinic_sn),
#                    NA_real_),
#     sh_sp = ifelse(iso3 == "MDV" & (year >= 2009 & year <= 2010) & (sh_sp == . | sh_sp == 0 | sh_sp == 1),
#                    lag(sh_sp),
#                    sh_sp)
#   )
# 
# # Replace c_clinic_sp, c_clinic_sn, and c_clinic_nmdr under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     c_clinic_sp = ifelse(year >= 2009 & year <= 2010 & iso3 == "MDV",
#                          c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp,
#                          c_clinic_sp),
#     c_clinic_sn = ifelse(year >= 2009 & year <= 2010 & iso3 == "MDV",
#                          c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp),
#                          c_clinic_sn),
#     c_clinic_nmdr = ifelse(year >= 2009 & year <= 2010 & iso3 == "MDV",
#                            c_clinic_sp + c_clinic_sn,
#                            c_clinic_nmdr)
#   )
# 
# # Replace hospd_sp_prct and hospd_sn_prct under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     hospd_sp_prct = ifelse(year >= 2009 & year <= 2010 & iso3 == "MDV",
#                            lag(hospd_sp_prct),
#                            hospd_sp_prct),
#     hospd_sn_prct = ifelse(year >= 2009 & year <= 2010 & iso3 == "MDV",
#                            lag(hospd_sn_prct),
#                            hospd_sn_prct)
#   )
# 
# # Generate sh_sp for c_hospital_sp and replace its values under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     sh_sp = ifelse(iso3 == "MDV" & (year >= 2006 & year <= 2013),
#                    c_hospital_sp / (c_hospital_sp + c_hospital_sn),
#                    NA_real_),
#     sh_sp = ifelse(year > 2010 & year <= 2013 & iso3 == "MDV" & (year >= 2009 & year <= 2010) & (sh_sp == . | sh_sp == 0 | sh_sp == 1),
#                    lag(sh_sp),
#                    sh_sp)
#   )
# 
# # Replace c_hospital_sp, c_hospital_sn, and c_hospital_nmdr under specified conditions
# finance_merged <- finance_merged %>% 
#   mutate(
#     c_hospital_sp = ifelse(year >= 2009 & year <= 2010 & iso3 == "MDV",
#                            c_notified * hospd_sp_dur * hospd_sp_prct * uc_visit_cur_usd * sh_sp,
#                            c_hospital_sp),
#     c_hospital_sn = ifelse(year >= 2009 & year <= 2010 & year <= 2013 & iso3 == "MDV",
#                            c_notified * hospd_sn_dur * hospd_sn_prct * uc_visit_cur_usd * (1 - sh_sp),
#                            c_hospital_sn),
#     c_hospital_nmdr = ifelse(year >= 2009 & year <= 2010 & iso3 == "MDV",
#                              c_hospital_sp + c_hospital_sn,
#                              c_hospital_nmdr)
#   )

# MLI: 2015 and 2014 percent utilisation to be back filled from 2016
finance_merged <- finance_merged %>% 
  mutate(
    c_hospital_nmdr = ifelse(year >= 2014 & year <= 2015 & iso3 == "MLI",
                             lag(c_hospital_nmdr),
                             c_hospital_nmdr)
  )


# PAK: replace 2013 values with 2014
# finance_merged <- finance_merged %>% 
#   arrange(iso3, year) %>% # Sort dataframe by iso3 and year
#   mutate(
#     c_clinic_nmdr = ifelse(iso3 == "PAK" & year == 2013, lead(c_clinic_nmdr), c_clinic_nmdr),
#     c_hospital_nmdr = ifelse(iso3 == "PAK" & year == 2013, lead(c_hospital_nmdr), c_hospital_nmdr)
#   )
# 
# # RWA 2007 - 2012
# finance_data <- finance_data %>%
#   arrange(iso3, desc(year)) %>%  # Sort dataframe by iso3 and year in descending order
#   mutate(
#     hcfvisit_sp = ifelse(year >= 2007 & year <= 2012 & iso3 == "RWA", lag(hcfvisit_sp), hcfvisit_sp),
#     hcfvisit_sn = ifelse(year >= 2007 & year <= 2012 & iso3 == "RWA", lag(hcfvisit_sn), hcfvisit_sn),
#     sh_sp = ifelse(iso3 == "RWA" & (year >= 2006 & year <= 2013), lag(c_clinic_sp / (c_clinic_sp + c_clinic_sn)), sh_sp),
#     c_clinic_sp = ifelse(year >= 2007 & year <= 2012 & iso3 == "RWA", c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp, c_clinic_sp),
#     c_clinic_sn = ifelse(year >= 2007 & year <= 2012 & iso3 == "RWA", c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp), c_clinic_sn),
#     c_clinic_nmdr = ifelse(year >= 2007 & year <= 2012 & iso3 == "RWA", c_clinic_sp + c_clinic_sn, c_clinic_nmdr)
#   )
# 
# 
# # TZA 2012
# # //twoway line c_clinic_nmdr year if iso3 == "TZA" ||  line c_hospital_nmdr year if iso3 == "TZA" || line c_ghs_nmdr year if iso3 == "TZA"
# finance_merged <- finance_merged %>%
#   arrange(iso3, desc(year))
# 
# finance_merged <- finance_merged %>%
#   mutate(
#     hcfvisit_sp = ifelse(year == 2012 & iso3 == "TZA", lag(hcfvisit_sp), hcfvisit_sp),
#     hcfvisit_sn = ifelse(year == 2012 & iso3 == "TZA", lag(hcfvisit_sn), hcfvisit_sn)
#   )
# 
# finance_merged <- finance_merged %>%
#   mutate(
#     sh_sp = ifelse(iso3 == "TZA" & (year >= 2006 & year <= 2013), lag(c_clinic_sp / (c_clinic_sp + c_clinic_sn)), sh_sp),
#     sh_sp = ifelse(year == 2012 & iso3 == "TZA", ifelse(sh_sp == "." | sh_sp == 0 | sh_sp == 1, lag(sh_sp), sh_sp), sh_sp)
#   )
# 
# finance_merged <- finance_merged %>%
#   mutate(
#     c_clinic_sp = ifelse(year == 2012 & iso3 == "TZA", c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp, c_clinic_sp),
#     c_clinic_sn = ifelse(year == 2012 & iso3 == "TZA", c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp), c_clinic_sn)
#   )
# 
# finance_merged <- finance_merged %>%
#   select(-sh_sp)
# 
# finance_merged <- finance_merged %>%
#   mutate(
#     c_clinic_nmdr = ifelse(year == 2012 & iso3 == "TZA", c_clinic_sp + c_clinic_sn, c_clinic_nmdr),
#     c_ghs_nmdr = ifelse(year == 2012 & iso3 == "TZA", c_clinic_nmdr + c_hospital_nmdr, c_ghs_nmdr)
#   )
# 
# # PRK 2014: Vsst = 0 hosp 100, days = as per 2015
# # //twoway line c_clinic_nmdr year if iso3 == "PRK" ||  line c_hospital_nmdr year if iso3 == "PRK" || line c_ghs_nmdr year if iso3 == "PRK"
# # // OP costs nmdr are almost 0 in 20123 - 13
# finance_merged <- finance_merged %>%
#   arrange(iso3, desc(year))
# 
# finance_merged <- finance_merged %>%
#   mutate(
#     hcfvisit_sp = ifelse(year >= 2012 & year <= 2013 & iso3 == "PRK", lag(hcfvisit_sp), hcfvisit_sp),
#     hcfvisit_sn = ifelse(year >= 2012 & year <= 2013 & iso3 == "PRK", lag(hcfvisit_sn), hcfvisit_sn)
#   )
# 
# finance_merged <- finance_merged %>%
#   mutate(
#     sh_sp = ifelse(iso3 == "PRK" & (year >= 2006 & year <= 2013), lag(c_clinic_sp / (c_clinic_sp + c_clinic_sn)), sh_sp),
#     sh_sp = ifelse(year >= 2012 & year <= 2013  & iso3 == "PRK", ifelse(sh_sp == "." | sh_sp == 0 | sh_sp == 1, lag(sh_sp), sh_sp), sh_sp)
#   )
# 
# finance_merged <- finance_merged %>%
#   mutate(
#     c_clinic_sp = ifelse(year >= 2012 & year <= 2013  & iso3 == "PRK", c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp, c_clinic_sp),
#     c_clinic_sn = ifelse(year >= 2012 & year <= 2013  & iso3 == "PRK", c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp), c_clinic_sn)
#   )
# 
# finance_merged <- finance_merged %>%
#   select(-sh_sp)
# 
# finance_merged <- finance_merged %>%
#   mutate(
#     c_clinic_nmdr = ifelse(year >= 2012 & year <= 2013  & iso3 == "PRK", c_clinic_sp + c_clinic_sn, c_clinic_nmdr),
#     c_ghs_nmdr = ifelse(year >= 2012 & year <= 2013 & iso3 == "PRK", c_clinic_nmdr + c_hospital_nmdr, c_ghs_nmdr)
#   )
# 
# ZAF: Note that 2020 TBHIV reported committed funds are 0. No change made 
# li year cf_nmdr cf_nmdr_dot cf_nmdr_dot_fld cf_nmdr_dot_nfld_lab cf_nmdr_ndot_hiv cf_nmdr_ndot_tpt cf_nmdr_ndot_nhiv_oth cf_nmdr_ndot_nhiv_noth if iso3 == "ZAF"

# replace total amounts if missing
finance_merged <- finance_merged %>%
  mutate(
    rcvd_tot = ifelse(iso3 == "ZAF" & year >= 2014 & is.na(rcvd_tot) & year != report_year, cf_tot, rcvd_tot),
    rcvd_int = ifelse(iso3 == "ZAF" & year >= 2014 & is.na(rcvd_int) & year != report_year, cf_int, rcvd_int),
    rcvd_ext_gf = ifelse(iso3 == "ZAF" & year >= 2014 & is.na(rcvd_ext_gf) & year != report_year, cf_ext_gf, rcvd_ext_gf),
    rcvd_ext_ngf = ifelse(iso3 == "ZAF" & year >= 2014 & is.na(rcvd_ext_ngf) & year != report_year, cf_ext_ngf, rcvd_ext_ngf),
    exp_tot = ifelse(iso3 == "ZAF" & year >= 2014 & is.na(exp_tot) & year != report_year, cf_tot, exp_tot)
  )


# Cuba (CUB): change legacy 0s to missing to allow imputation - CUBA was a short form country till 2020.  
# // (Note that the country could not isolate TB budgets or expenses from overall health data in 2020, so reported 0 instead of missing)
# li year budget_tot cf_tot cf_int cf_ext_gf rcvd_ext_gf exp_tot rcvd_tot if iso3 == "CUB" & year > 2010
finance_merged <- finance_merged %>%
  mutate(
    budget_tot = ifelse(budget_tot == 0 & iso3 == "CUB" & year >= report_year - 1, NA, budget_tot),
    cf_tot = ifelse(is.na(budget_tot) & iso3 == "CUB" & year >= 2014, budget_tot, cf_tot),
    exp_tot = ifelse(exp_tot == 0 & iso3 == "CUB" & year >= report_year - 1, NA, exp_tot),
    rcvd_tot = ifelse(rcvd_tot == 0 & iso3 == "CUB" & year >= report_year - 1, NA, rcvd_tot),
    cf_ext_gf = ifelse(cf_ext_gf == 0 & iso3 == "CUB" & year == 2020, NA, cf_ext_gf),
    rcvd_ext_gf = ifelse(rcvd_ext_gf == 0 & iso3 == "CUB" & year >= 2014, NA, rcvd_ext_gf)
  )

# // Calculate GHS
# //NMDR for 2014 onwards (plus SSD's exceptional circumstance)
finance_merged <- finance_merged %>%
  mutate(
    c_clinic_nmdr = ifelse(year >= 2014 | iso3 == "SSD", c_notif_less_mdr * hcfvisit_dstb * uc_visit_cur_usd, c_clinic_nmdr),
    c_hospital_nmdr = ifelse(year >= 2014 | iso3 == "SSD", beddays_nmdr * uc_bedday_nmdr_cur_usd, c_hospital_nmdr),
    c_clinic_mdr = ifelse(year >= 2014 | iso3 == "SSD", mdr_tx * hcfvisit_mdr * uc_visit_cur_usd, c_clinic_mdr),
    c_hospital_mdr = ifelse(year >= 2014 | iso3 == "SSD", beddays_mdr * uc_bedday_mdr_cur_usd, c_hospital_mdr)
  )


#----------------
# 2024
#----------------
# adjustment for the data reported in 2024 will be added here
