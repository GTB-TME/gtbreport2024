# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Produce aggregate TB finance output files to be used in regional 
# and global profiles on the web and in the mobile app
# Takuya Yamanaka, Hazim Timimi June 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

report_year = 2024

load(here::here('finance/output/finance_imputed_constantus_included.Rdata'))

library(dplyr)

# Calculate regional aggregates
group_finance <- finance_merged |> 
  select(year, g_whoregion, rcvd_tot, rcvd_int,	c_ghs,	rcvd_ext_gf,	rcvd_ext_ngf,	cf_int,	cf_ext,	gap_tot) |>
  filter(year <= report_year) |>
  group_by(year, g_whoregion) |>
  summarise_all(sum, na.rm = T) |> 
  mutate(group_type = 'g_whoregion',
         group_name = g_whoregion,
         c_ghs = ifelse(year == report_year, 0, c_ghs),
         rcvd_tot = ifelse(year == report_year, 0, rcvd_tot),
         rcvd_int = ifelse(year == report_year, 0, rcvd_int),
         rcvd_ext_gf = ifelse(year == report_year, 0, rcvd_ext_gf),
         rcvd_ext_ngf = ifelse(year == report_year, 0, rcvd_ext_ngf)
  ) |> 
  ungroup() |> 
  select(group_type, group_name, year, rcvd_tot, rcvd_int,	c_ghs,	rcvd_ext_gf,	rcvd_ext_ngf,	cf_int,	cf_ext,	gap_tot)

# Add global aggregates
group_finance <- finance_merged |> 
  select(year, rcvd_tot, rcvd_int,	c_ghs,	rcvd_ext_gf,	rcvd_ext_ngf,	cf_int,	cf_ext,	gap_tot) |>
  filter(year <= report_year) |>
  group_by(year) |>
  summarise_all(sum, na.rm = T) |> 
  mutate(group_type = 'global',
         group_name = 'global',
         c_ghs = ifelse(year == report_year, 0, c_ghs),
         rcvd_tot = ifelse(year == report_year, 0, rcvd_tot),
         rcvd_int = ifelse(year == report_year, 0, rcvd_int),
         rcvd_ext_gf = ifelse(year == report_year, 0, rcvd_ext_gf),
         rcvd_ext_ngf = ifelse(year == report_year, 0, rcvd_ext_ngf)
  ) |> 
  ungroup() |> 
  select(group_type, group_name, year, rcvd_tot, rcvd_int,	c_ghs,	rcvd_ext_gf,	rcvd_ext_ngf,	cf_int,	cf_ext,	gap_tot) |> 
  rbind(group_finance)

# Add a timestamp to the aggregated finance
attr(group_finance, "timestamp") <- Sys.Date()
save(group_finance, file = here::here('finance/output/group_finance_for_profiles.rda'))



# Produce list of countries for whom finance profiles will be shown
publish_finance_profile <- finance_merged |> 
  filter(year == report_year-1) |> 
  select(iso3) |> 
  mutate(dcyear = report_year)

# Add a timestamp to the aggregated finance
attr(publish_finance_profile, "timestamp") <- Sys.Date()
save(publish_finance_profile, file = here::here('finance/output/countries_publish_finance_profile.rda'))
