# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation/imputation script, Part II imputing unit cost
# Translated from Stata version written by A SiroKa and P Nguhiu
# Takuya Yamanaka, February 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load chapter 4, cleaned data
source(here::here('finance/1_load_data.r'))
load(here::here('finance/local/finance_cleaned.Rda'))


# ** PN / TA 08_2023. Enforced difference in uc estimation across DS and DR 
# Filter the data for observations where year is greater than 2013 and g_income is not "HIC"
filtered_data <- finance_merged %>%
  filter(year > 2013, g_income != "HIC")

# Create a table of year and hosp_type_mdr
tabulated_data <- table(filtered_data$year, filtered_data$hosp_type_mdr)

# Print the tabulated data
print(tabulated_data)


# ** In this script I use WB data instead of IMF data (think the former are more reliable and are consistent across time)
# ** However, WB gdp estimates (in 2022) are available until 2021, without projections. IMF gdp estimates (2023) have projections from 2021 to 2023, 
# ** This section attempts to predict missing WB gdp values using IMF projections

# storage   display    value
# variable name   type    format     label      variable label
# --------------------------------------------------------------------------------------------------------------------------------------------------------
# ny_gdp_pc~pp_cd double  %10.0g                GDP per capita, PPP (current international $)
# ny_gdp_defl_zs  double  %10.0g                GDP deflator (base years vary)
# pa_nus_ppp      double  %10.0g                PPP conversion factor, GDP (LCU per int$)
# imf_ppp_conv    double  %10.0g                Implied PPP conversion rate, National currency per current I$
# off_exch_rate   double  %10.0g                Official exchange rate (LCU per US$)
# imf_deflator    double  %10.0g                

summary_data <- finance_merged %>%
  filter(year == (report_year - 1)) %>%
  summarise(
    ny_gdp_pcap_pp_cd_summary = summary(ny_gdp_pcap_pp_cd),
    ny_gdp_defl_zs_summary = summary(ny_gdp_defl_zs),
    imf_deflator_summary = summary(imf_deflator),
    pa_nus_ppp_summary = summary(pa_nus_ppp),
    imf_ppp_conv_summary = summary(imf_ppp_conv),
    off_exch_rate_summary = summary(off_exch_rate)
  )

# Print the summary data
print(summary_data)
# /* on 29 Jun 2023, missing values in year y-1 were

# **** A. GDP per capita, in current PPP dollars. Recommended authoritative source according to GHED is WB ny_gdp_pcap_pp_cd****
# ** Fill in missing values (mainly 2021 and 2022 wb values pre jul 2023) 
if ("ln_wb_gdp_pc_cur_int" %in% names(finance_merged)) {
  finance_merged$ln_wb_gdp_pc_cur_int <- NULL
}
finance_merged$ln_wb_gdp_pc_cur_int <- log(finance_merged$ny_gdp_pcap_pp_cd)

if ("ln_deflator" %in% names(finance_merged)) {
  finance_merged$ln_deflator <- NULL
}

finance_merged$ln_deflator <- log(finance_merged$imf_deflator)

if ("ln_wb_ppp_conv" %in% names(finance_merged)) {
  finance_merged$ln_wb_ppp_conv <- NULL
}

finance_merged$ln_wb_ppp_conv <- log(finance_merged$pa_nus_ppp)

if ("ln_off_exch_rate" %in% names(finance_merged)) {
  finance_merged$ln_off_exch_rate <- NULL
}

finance_merged$ln_off_exch_rate <- log(finance_merged$off_exch_rate)

# //Probability of facilities being urban or rural in country (ecological, country level)
if ("logit_urb_totl_in_zs" %in% names(finance_merged)) {
  finance_merged$logit_urb_totl_in_zs <- NULL
}
finance_merged$logit_urb_totl_in_zs <- log(finance_merged$sp_urb_totl_in_zs / (100 - finance_merged$sp_urb_totl_in_zs))


# // Each country will have an independent regression done
countries <- unique(finance_merged$iso3)

i <- "PHL"

finance_merged %>%
  filter(iso3==i)

for (i in countries) {
  cat(i, "\n")
  
  # GDP per cap (current PPP)
  tryCatch({
    lm_result <- lm(ln_wb_gdp_pc_cur_int ~ year, data = filter(finance_merged, iso3 == i))
    print(paste(length(lm_result$residuals), "obs for GDP PC PPP"))
    hat <- predict(lm_result)
    finance_merged <- finance_merged %>%
      mutate(ln_wb_gdp_pc_cur_int = ifelse(is.na(ln_wb_gdp_pc_cur_int) & iso3 == i, hat, ln_wb_gdp_pc_cur_int))
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
  })
  
  # DEFLATOR
  tryCatch({
    lm_result <- lm(ln_deflator ~ year, data = filter(finance_merged, iso3 == i))
    print(paste(length(lm_result$residuals), "obs for GDP deflator (LCU)"))
    hat <- predict(lm_result)
    finance_merged <- finance_merged %>%
      mutate(ln_deflator = ifelse(is.na(ln_deflator) & iso3 == i, hat, ln_deflator))
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
  })
  
  # PPP CONVERSION
  tryCatch({
    lm_result <- lm(ln_wb_ppp_conv ~ year, data = filter(finance_merged, iso3 == i))
    print(paste(length(lm_result$residuals), "obs for LCU - PPP conversion"))
    hat <- predict(lm_result)
    finance_merged <- finance_merged %>%
      mutate(ln_wb_ppp_conv = ifelse(is.na(ln_wb_ppp_conv) & iso3 == i, hat, ln_wb_ppp_conv))
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
  })
  
  # Exchange rate
  tryCatch({
    lm_result <- lm(ln_off_exch_rate ~ year, data = filter(finance_merged, iso3 == i))
    print(paste(length(lm_result$residuals), "obs for WB Period-averaged rate LCU-US$"))
    hat <- predict(lm_result)
    finance_merged <- finance_merged %>%
      mutate(ln_off_exch_rate = ifelse(is.na(ln_off_exch_rate) & iso3 == i, hat, ln_off_exch_rate))
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
  })
  
  # Impute urban %
  tryCatch({
    lm_result <- lm(logit_urb_totl_in_zs ~ year, data = filter(finance_merged, iso3 == i))
    print(paste(length(lm_result$residuals), "obs for E[Urban] (ecological)"))
    hat <- predict(lm_result)
    finance_merged <- finance_merged %>%
      mutate(logit_urb_totl_in_zs = ifelse(is.na(logit_urb_totl_in_zs) & iso3 == i, hat, logit_urb_totl_in_zs))
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
  })
}


summary_table <- finance_merged %>%
  filter(year == (report_year - 1) & g_income != "HIC") %>%
  summarise(across(
    .cols = c(ln_wb_gdp_pc_cur_int, ln_deflator, ln_wb_ppp_conv, ln_off_exch_rate),
    .fns = list(
      n_missing = ~sum(is.na(.)),
      n_not_missing = ~sum(!is.na(.)),
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE)
    )
  )) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")


# Print the summary table
print(summary_table)

# // PN June 2023: 2 countries with  missing input variables ( ASM, PRK).
finance_merged %>%
  filter(is.na(ln_wb_gdp_pc_cur_int) & g_income != "HIC" & year > 2012) %>%
  select(iso3, country, year, g_whoregion, ny_gdp_pcap_pp_cd, imf_gdp_pc_cur_usd) %>%
  print(n=100)

finance_merged %>%
  filter(is.na(ln_deflator) & g_income != "HIC" & year > 2012) %>%
  select(iso3, country, year, g_whoregion)  %>%
  print(n=100)

finance_merged %>%
  filter(is.na(ln_wb_ppp_conv) & g_income != "HIC" & year > 2012) %>%
  select(iso3, country, year, g_whoregion)  %>%
  print(n=100)

finance_merged %>%
  filter(is.na(ln_off_exch_rate) & g_income != "HIC" & year > 2012) %>%
  select(iso3, country, year, g_whoregion)  %>%
  print(n=100)

# // Update the input variable values 
finance_merged <- finance_merged %>%
  mutate(flag_gdp_int = is.na(ny_gdp_pcap_pp_cd))

finance_merged <- finance_merged %>%
  mutate(ny_gdp_pcap_pp_cd = if_else(is.na(ny_gdp_pcap_pp_cd) & !is.na(ln_wb_gdp_pc_cur_int),
                                     exp(ln_wb_gdp_pc_cur_int), ny_gdp_pcap_pp_cd))

finance_merged <- finance_merged %>%
  mutate(flag_defl_pct = is.na(imf_deflator))

finance_merged <- finance_merged %>%
  mutate(imf_deflator = if_else(is.na(imf_deflator) & !is.na(ln_deflator),
                                exp(ln_deflator), imf_deflator))

finance_merged <- finance_merged %>%
  mutate(pa_nus_ppp = if_else(is.na(pa_nus_ppp) & !is.na(ln_wb_ppp_conv),
                              exp(ln_wb_ppp_conv), pa_nus_ppp))

finance_merged <- finance_merged %>%
  mutate(off_exch_rate = if_else(is.na(off_exch_rate) & !is.na(ln_off_exch_rate),
                                 exp(ln_off_exch_rate), off_exch_rate))

finance_merged <- finance_merged %>%
  mutate(sp_urb_totl_in_zs = if_else(is.na(sp_urb_totl_in_zs) & !is.na(logit_urb_totl_in_zs),
                                     arm::invlogit(logit_urb_totl_in_zs) * 100, sp_urb_totl_in_zs))

finance_merged %>%
  filter(iso3 == "SSD") %>%
  select(iso3, year, ln_wb_gdp_pc_cur_int, ny_gdp_pcap_pp_cd, imf_deflator, ny_gdp_defl_zs, imf_gdp_pc_cur_usd, starts_with("flag"))

# ***** RE-INDEX imf_deflator to the base year (report_year - 1) *****
#   // The GDP deflator provided by IMF is the ratio of GDP in nominal local currency units, to GDP in real local currency units. 
report_year <- max(finance_merged$year)
finance_merged <- finance_merged %>%
  mutate(deflator = ifelse(year == report_year, 1, deflator))

finance_merged <- finance_merged %>%
  arrange(iso3, desc(year))

# // We use indexing to ensure we are dividing each year's deflator by the current year's deflator value - to make report_year the base currency deflation year
finance_merged <- finance_merged %>%
  group_by(iso3) %>%
  mutate(earliest_year = min(year)) %>%
  ungroup()  # Remove grouping

# // The index here is calculating the record number for each country set of annual records. Countries with records from 2002 to latest year will have
# // [report_year - 2002 + 1] records, etc 
finance_merged <- finance_merged %>%
  group_by(iso3, year) %>%
  mutate(
    deflator = imf_deflator / imf_deflator[which.max(year) - min(year) + 1]
  ) %>%
  ungroup() %>%
  select(-earliest_year)

# * Confirm that all records have a deflator_wb generated.
finance_merged %>%
  filter(is.na(deflator) & year == (report_year - 1) & g_income != "HIC") %>%
  select(iso3, country, g_income, year)

# // Mauritania - new ouguiya was introduced in 2018, replacing the old ouguiya at a rate of 1 new ouguiya = 10 old ouguiya. 
# // WB exchange rate 2020 is redenominated at 1:10. This means the estimates of costs (USD) from before 2015 are all reduced by 0.1
# // Belarus -  In July 2016, a new ruble was introduced (ISO 4217 code BYN), at a rate of 1 BYN = 10,000 BYR. The WB official 
# // exchange rate is not yet adjusted (PN Jul 2022 this is now updated on WB side, so don't need to do the following -  
# // so in the prepare code, we delete the 2018 exchange rate anomaly, and extrapolate previous values)
# 
# 
# ** Note that the IMF reported gdp_pc variable seems to be derived from the constant value LCU gdp and may be misnamed
# //graph twoway scatter  imf_gdp_pc_cur_int year if iso3 == "BLR"  || scatter  ny_gdp_pcap_pp_cd year if iso3 == "BLR" || scatter  ny_gdp_pcap_pp_kd  year if iso3 == "BLR",  mc("blue","brown","gs3")
# ** PN Jul 2020 chose to rely on WB data instead of IMF data (recalling that IMF data wasnt updated in 2020)
# ** PN Jul 2020 comparing IMF and WB deflators, missingness higher in IMF reported deflator (because we've imputed missing values in wb indicator above).
# //li iso3 ny_gdp_defl_zs imf_deflator if year == report_year
# 
# // PN 2021: check countries where the correlation between wb and IMF deflators is less tha 0.7, and ivestigate
# /*
#   levelsof iso3, local(countries)
# foreach country in `countries' {
# 	qui corr deflator deflator_wb if iso3 == "`country'"
# 	//local r = r(rho)
# 	if r(rho) < 0.7 {
# 		print "`country': Corr = `r(rho)'"
# 	}
# } 
# */
# 
# ** PN Jun 2023 DPRK isn't in the IMF dataset, and WB doesn't report inflation (no CPI change data).  We temporarily use US deflator
# ** but to review next year.

finance_merged <- finance_merged %>%
  mutate(deflator = ifelse(iso3 == "PRK", deflator_us, deflator))


# // Create a constant 2017 I$ deflator index to assist with conversion of estimated unit costs to current year LCU
# // To convert current $ of any year to constant $, divide  by the index of that year and multiply  by the index of the base year (eg the index created below) 
# /*
#   gen b17_base = deflator_wb if year == 2017 
# by iso3 (year), sort: egen index_17 = min(b17_base)
# drop b17_base
# 
# gen ppp17_conv = pa_nus_ppp if year == 2017 
# by iso3 (year), sort: egen ppp_conv_2017 = min(ppp17_conv) //Implied PPP conversion rate, National currency per current I$, 2017 
# drop ppp17_conv
# */

# // - latest reported LCU - US$ exchange rate 
finance_merged <- finance_merged %>%
  mutate(exch_latest = ifelse(year == base_year, off_exch_rate, NA_real_))

finance_merged <- finance_merged %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(off_exch_rate_latest = mean(exch_latest, na.rm = TRUE)) %>%
  ungroup() %>%
  select(- exch_latest)


# ** Beginning of Unit Cost estimation using WHO CHOICE model **
#   
#   // Outpatient visits
# 
# // Assumed independent variables, at 80th percentile of IER sample or at country- or TB-specifc values if available
# /* constants come from 80th percentile of IER sample	*/
#   /*
#   PN 2019: Chris' file used the natural log of 2010 per capita GDP measured in current PPP adjusted int $. 
# We use imf_gdp_pc_cur_int variable (obtained from WEO imf data) correspondingly. 
# 
# PN Jun 2020: As documented in Karin et al 2018 
#  - country data doesn't include average outpatient visits (across all health services) or Visits per provider per day, so we use the 80th percentile of the
# data from 2007's sample representing 80% efficiency of operation 
#  - We estimate Outpatient costs at Health centres without beds i.e. Level 1 facilities.
#  - 
# */
finance_merged <- finance_merged %>%
  mutate(lngdp = log(ny_gdp_pcap_pp_cd),  # Natural log of GDP per capita (PPP)- current
         lnvisits = log(67656),             # PN 2020: Natural logarithm of outpatient visits
         lnviz2 = log(8.96),                # PN 2020: Natural log of visits per provider per day
         urban = sp_urb_totl_in_zs / 100)  # Proportion of population living in urban area

finance_merged$urban[is.na(finance_merged$urban)] <- 0.5  # Replace missing values with 0.5 for urban

finance_merged <- finance_merged %>%
  mutate(public = 1,          # Dummy variable for public level
         private = 0,         # Dummy variable for private level
         HC2 = 1,             # Dummy variable for level 2 facilities
         HOSP1 = 0,           # Dummy variable for level 3 facilities
         HOSP2 = 0,           # Dummy variable for level 4 facilities
         DColombia = (iso3 == "COL"),   # Dummy variable for observations in Colombia
         DBrazil = (iso3 == "BRA"),     # Dummy variable for observations in Brazil
         Dbrazil = 0,         # Dummy variables for type 3 facilities in Brazil
         cons = 1)            # Constant

# // Regression results from IER: e(b) e(rmse) and e(V)
out_coeff <- matrix(c(0.865, -0.0142, -0.0412, 0.352, -0.29, 0.0532, 0.208, 0.304, 0.348, 0.628, -1.563, -0.245, -4.534), nrow = 1)
colnames(out_coeff) <- c("lngdp", "lnvisits", "lnviz2", "urban", "public", "private", "HC2", "HOSP1", "HOSP2", "DColombia", "DBrazil", "Dbrazil", "cons")

print(out_coeff)


# lngdp    lnvisits      lnviz2       urban      public     private         HC2       HOSP1       HOSP2   DColombia     DBrazil     Dbrazil        cons
# lngdp   .00040241
# lnvisits  -.00005036   .00004416
# lnviz2   .00004027  -.00004139   .00007187
# urban  -.00042545  -2.674e-06   8.479e-06   .00179749
# public   .00004295  -.00002365   .00001724    .0000124   .00042763
# private  -.00005735   .00001468  -1.281e-06   .00005637   .00021333   .00061054
# HC2   -.0000592   -.0000191   .00005908   .00015438   .00002458   .00001649   .00105547
# HOSP1  -.00026519  -.00005824   .00010337   .00015844   .00006957   .00004942   .00049398   .00216255
# HOSP2   .00008992  -.00010645   .00015066  -9.099e-06   .00010035  -.00002414   .00019368   .00035043   .00123634
# DColombia   -.0002512   .00006216   -.0001277  -.00054799  -.00010882   .00002183  -.00073602  -.00002586  -.00024482   .00189335
# DBrazil  -.00075221    .0000115  -.00001908   .00142067  -8.789e-06   .00007431   .00026872   .00086051  -.00007206   .00036321   .00225241
# Dbrazil   .00033797  -9.013e-06   .00001225  -.00014678   .00015674   5.206e-07  -.00031714  -.00193657   .00004089  -.00025795  -.00099999   .00219149
# cons   -.0024973   .00007105  -.00004095   .00248007   -.0005399   .00006044   .00029098   .00191854  -.00007074   .00159916    .0046487  -.00224007   .01798536

out_covvar <- matrix(0, nrow = 13, ncol = 13)

# Fill in the upper triangle of the matrix
out_covvar[1, 1] <- 0.00040241
out_covvar[2, 2] <- 0.00004416
out_covvar[3, 3] <- 0.00007187
out_covvar[4, 4] <- 0.00179749
out_covvar[5, 5] <- 0.00042763
out_covvar[6, 6] <- 0.00061054
out_covvar[7, 7] <- 0.00105547
out_covvar[8, 8] <- 0.00216255
out_covvar[9, 9] <- 0.00123634
out_covvar[10, 10] <- 0.00189335
out_covvar[11, 11] <- 0.00225241
out_covvar[12, 12] <- 0.00219149
out_covvar[13, 13] <- 0.01798536

# Fill in the lower triangle of the matrix (since it's symmetric)
out_covvar[lower.tri(out_covvar)] <- c(	-0.00005036,	0.00004027,	-0.00042545,	0.00004295,	-0.00005735,	-0.0000592,	-0.00026519,	0.00008992,	-0.0002512,	-0.00075221,	0.00033797,	-0.0024973,
                                       	-0.00004139,	-0.000002674,	-0.00002365,	0.00001468,	-0.0000191,	-0.00005824,	-0.00010645,	0.00006216,	0.0000115,	-0.000009013,	0.00007105,
                                       	0.000008479,	0.00001724,	-0.000001281,	0.00005908,	0.00010337,	0.00015066,	-0.0001277,	-0.00001908,	0.00001225,	-0.00004095,
                                       	0.0000124,	0.00005637,	0.00015438,	0.00015844,	-0.000009099,	-0.00054799,	0.00142067,	-0.00014678,	0.00248007,
                                       	0.00021333,	0.00002458,	0.00006957,	0.00010035,	-0.00010882,	-0.000008789,	0.00015674,	-0.0005399,
                                       	0.00001649,	0.00004942,	-0.00002414,	0.00002183,	0.00007431,	5.206E-07,	0.00006044,
                                       	0.00049398,	0.00019368,	-0.00073602,	0.00026872,	-0.00031714,	0.00029098,
                                       	0.00035043,	-0.00002586,	0.00086051,	-0.00193657,	0.00191854,
                                       	-0.00024482,	-0.00007206,	0.00004089,	-0.00007074,
                                       	0.00036321,	-0.00025795,	0.00159916,
                                       	-0.00099999,	0.0046487,
                                       	-0.00224007
                                       )

rownames(out_covvar) <- c("lngdp", "lnvisits", "lnviz2", "urban", "public", "private", "HC2", "HOSP1", "HOSP2", "DColombia", "DBrazil", "Dbrazil", "cons")
colnames(out_covvar) <- c("lngdp", "lnvisits", "lnviz2", "urban", "public", "private", "HC2", "HOSP1", "HOSP2", "DColombia", "DBrazil", "Dbrazil", "cons")

# Print the matrix
print(out_covvar)
out_rmse <- matrix(.68571297, nrow = 1, ncol = 1)
out_rvar <-  out_rmse*out_rmse

out_X <- finance_merged %>%
  select(lngdp, lnvisits, lnviz2, urban, public, private, HC2, HOSP1, HOSP2, DColombia, DBrazil, Dbrazil, cons) %>%
  as.matrix(dimnames = list(NULL, c("lngdp", "lnvisits", "lnviz2", "urban", "public", "private", "HC2", "HOSP1", "HOSP2", "DColombia", "DBrazil", "Dbrazil", "cons")))

# Matrix product of out_coeff and the transpose of out_X
uc_visit <- as.data.frame(t(out_coeff %*% t(out_X))) %>%
  rename(uc_visit=1)

finance_merged <- finance_merged %>%
  cbind(uc_visit)

# // IER apply a "bias correction factor" of 1.271 and then remove the drug & food? cost component (0.573694903)
finance_merged <- finance_merged %>%
  mutate(uc_visit_cur_int = exp(uc_visit) * 1.271 * (1 - 0.573694903))

# Convert to nominal (current) US$ and report_year constant US$
# a) from nominal PPP to nominal LCU b) convert using each year's official exch rate to USD
finance_merged <- finance_merged %>%
  mutate(uc_visit_cur_usd = uc_visit_cur_int * pa_nus_ppp * (1 / off_exch_rate))

# b) from nominal PPP to nominal LCU b) reindex to base year's estimate using LCU GDP deflator index (1) c) convert using base year's official exch rate
finance_merged <- finance_merged %>%
  mutate(uc_visit_const_usd = uc_visit_cur_int * pa_nus_ppp * (1 / deflator) * (1 / off_exch_rate_latest))

# c) in LCU
finance_merged <- finance_merged %>%
  mutate(uc_visit_cur_lcu = uc_visit_cur_int * pa_nus_ppp)

# // Standard error of the forecast (constant 2007 I$)
n_rows <- nrow(finance_merged)
out_varf <- numeric(n_rows)
i <- 1
# Loop through each row
for (i in 1:n_rows) {
  # Print the current row number
  print(i)
  
  # Calculate the out_varf value for the current row
  out_varf[i] <- out_rvar + out_X[i,] %*% out_covvar %*% out_X[i,]
}
print(out_varf)

finance_merged <- finance_merged %>%
  cbind(out_varf)


finance_merged <- finance_merged %>%
  mutate(
    uc_visit_cur_int_lnvarf = out_varf,
    uc_visit_hi = uc_visit + 1.67 * sqrt(out_varf),
    uc_visit_lo = uc_visit - 1.67 * sqrt(out_varf),
    uc_visit_cur_int_hi = exp(uc_visit_hi) * 1.271 * (1 - 0.573694903),
    uc_visit_cur_int_lo = exp(uc_visit_lo) * 1.271 * (1 - 0.573694903),
    uc_visit_cur_usd_hi = uc_visit_cur_int_hi * pa_nus_ppp * (1 / off_exch_rate),
    uc_visit_cur_usd_lo = uc_visit_cur_int_lo * pa_nus_ppp * (1 / off_exch_rate)
  )

# //twoway scatter uc_visit_cur_int year if iso3== "VEN", mc("brown") || scatter uc_visit_const_usd year if iso3 == "VEN", mc("blue")
#-------------------------------------------------------------------------------------------------------------------------------------------
# // Inpatient stays (DS-TB)
# // Assumed independent variables, using 80pc values for occupancy, ALOS, and admissions
finance_merged <- finance_merged %>%
  arrange(country) %>%
  mutate(
    lngdp = log(ny_gdp_pcap_pp_cd),  # Natural log of GDP per capita (PPP) ** rebased to 2007 constant I$
    lnpctwardbeds = log(0.756),       # Natural log of occupancy rate
    lnalos = log(7.149),              # Natural logarithm of average length of stay (ALOS)
    lnadmissions = log(4971),         # Natural logarithm of total inpatient admissions
    HOSP1 = 1,                        # Dummy variable for level 3 facilities assumes level1 primary
    Dteach = 0,                       # Dummy variable for teaching hospitals
    public = 1,                       # Dummy variable for public level hospitals
    private = 0,                      # Dummy variable for private level hospitals
    DBrazil = ifelse(iso3 == "BRA", 1, 0),  # Dummy variable for observations in Brazil
    cons = 1                          # Constant
  )

# // Regression results from IER: e(b) e(rmse) and e(V)
inp_coeff <- matrix(c(1.1917468,-.02010699,-.59951201,.02521872,-.2035468,.25719725,-.14433241,.10961334,-1.6384874,-4.2770377), nrow = 1)
colnames(inp_coeff) <- c("lngdp", "lnpctwardbeds", "lnalos", "lnadmissions", "HOSP1", "Dteach", "public", "private", "DBrazil", "cons")

print(inp_coeff)


inp_covvar <- matrix(0, nrow = 10, ncol = 10)
inp_covvar[1, 1] <- 0.00169422
inp_covvar[2, 2] <- 0.00005008
inp_covvar[3, 3] <- 0.00064596
inp_covvar[4, 4] <- 0.00010943
inp_covvar[5, 5] <- 0.00134192
inp_covvar[6, 6] <- 0.00231228
inp_covvar[7, 7] <- 0.00036559
inp_covvar[8, 8] <- 0.00038875
inp_covvar[9, 9] <- 0.00081298
inp_covvar[10, 10] <- 0.1493409

# Fill in the lower triangle of the matrix (since it's symmetric)
inp_covvar[lower.tri(inp_covvar)] <- c(	0.00003131,	0.00034551,	-0.000006167,	-0.00007524,	-0.00003248,	-0.00002526,	0.00009554,	-0.00043301,	-0.01532908,
                                       	0.00001815,	-0.00001054,	0.000003047,	-0.00004095,	-0.00002161,	-0.000006621,	0.00007034,	-0.00011467,
                                       	-0.00004283,	-0.00002255,	-0.00017007,	-0.00011937,	0.00012333,	-0.00025965,	-0.00329369,
                                       	0.00007398,	-0.00007272,	0.000004531,	-0.000006074,	0.0000918,	-0.0008943,
                                       	0.00123206,	0.00002472,	0.00013135,	0.00009626,	-0.00120898,
                                       	-0.00007706,	0.00018021,	-0.00025155,	-0.00009635,
                                       	-0.00001696,	0.00026099,	-0.00010748,
                                       	-0.00014978,	-0.00107184,
                                       	0.00297446
                                       
                                       
)

colnames(inp_covvar) <- c("lngdp", "lnpctwardbeds", "lnalos", "lnadmissions", "HOSP1", "Dteach", "public", "private", "DBrazil", "cons")
rownames(inp_covvar) <- c("lngdp", "lnpctwardbeds", "lnalos", "lnadmissions", "HOSP1", "Dteach", "public", "private", "DBrazil", "cons")
inp_rmse <- .43382515

inp_rvar <- inp_rmse*inp_rmse

# // Predicted dependent variable
inp_X <- finance_merged %>%
  select(lngdp,lnpctwardbeds,lnalos,lnadmissions,HOSP1,Dteach,public,private,DBrazil, cons) %>%
  as.matrix()

uc_bedday <- as.data.frame(t(inp_coeff %*% t(inp_X))) %>%
  rename(uc_bedday = 1)

finance_merged <- finance_merged %>%
  cbind(uc_bedday)

# // IER apply a "bias correction factor" of 1.054 and then remove the drug & food cost component (0.573694903)
finance_merged <- finance_merged %>%
  mutate(
    uc_bedday_nmdr_cur_int = exp(uc_bedday) * 1.054 * (1 - 0.573694903),
    uc_bedday_nmdr_cur_usd = uc_bedday_nmdr_cur_int * pa_nus_ppp * (1 / off_exch_rate),
    uc_bedday_nmdr_const_usd = uc_bedday_nmdr_cur_int * pa_nus_ppp * (1 / deflator) * (1 / off_exch_rate_latest),
    uc_bedday_nmdr_cur_lcu = uc_bedday_nmdr_cur_int * pa_nus_ppp
  )

# // Standard error of the forecast
n_rows <- nrow(finance_merged)
inp_varf <- numeric(n_rows)
i <- 1
# Loop through each row
for (i in 1:n_rows) {
  # Print the current row number
  print(i)
  
  # Calculate the out_varf value for the current row
  inp_varf[i] <- inp_rvar + inp_X[i,] %*% inp_covvar %*% inp_X[i,]
}
print(inp_varf)

finance_merged <- finance_merged %>%
  cbind(inp_varf=inp_varf)

# // Standard error of the forecast

finance_merged <- finance_merged %>%
  mutate(
    uc_bedday_nmdr_cur_int_lnvarf = inp_varf,
    uc_bedday_hi = uc_bedday + 1.67 * sqrt(inp_varf),
    uc_bedday_lo = uc_bedday - 1.67 * sqrt(inp_varf),
    uc_bedday_nmdr_cur_int_hi = exp(uc_bedday_hi) * 1.054 * (1 - 0.573694903),
    uc_bedday_nmdr_cur_int_lo = exp(uc_bedday_lo) * 1.054 * (1 - 0.573694903),
    uc_bedday_nmdr_cur_usd_hi = uc_bedday_nmdr_cur_int_hi * pa_nus_ppp * (1 / off_exch_rate),
    uc_bedday_nmdr_cur_usd_lo = uc_bedday_nmdr_cur_int_lo * pa_nus_ppp * (1 / off_exch_rate)
  )


#------------------------------------------------------------------------------
# // Inpatient stays (DR-TB)
# // fill in hosp_type_mdr

any(duplicated(names(finance_merged)))

colnames(finance_merged) <- make.unique(colnames(finance_merged))

finance_merged <- finance_merged %>%
  arrange(iso3, year)

finance_merged <- finance_merged %>%
  group_by(iso3) %>%
  mutate(hosp_type_mdr = ifelse(is.na(hosp_type_mdr), lag(hosp_type_mdr), hosp_type_mdr)) %>%
  mutate(hosp_type_mdr = ifelse(is.na(hosp_type_mdr), lead(hosp_type_mdr), hosp_type_mdr)) %>%
  ungroup()

# // assume otherwise that secondary hospital (level 4)
finance_merged$hosp_type_mdr[is.na(finance_merged$hosp_type_mdr)] <- 141

# /* PN: Updating code to reflect different coefficients for Level 4/5 facilities based on Karin 2018) */
finance_merged <- finance_merged %>%
  mutate(lngdp = log(ny_gdp_pcap_pp_cd)) %>% # // Natural log of GDP per capita (PPP) ** rebased to 2007 constant I$
  mutate(lnpctwardbeds = log(0.810)) %>% # // Natural log of occupancy rate for level 4 hospitals (Table 3, Karin 2018)
  mutate(lnpctwardbeds = ifelse(hosp_type_mdr == 140, log(0.756), lnpctwardbeds)) #//If DR treatment is done in Level 3 hospitals (primary hospitals, like DS-TB assumption), lower value of coefficient.


# Calculate natural logarithm of average length of stay (ALOS)
finance_merged <- finance_merged %>%
  mutate(lnalos = if_else(hosp_type_mdr == 140, log(7.149), log(9.75))) %>% #// Natural logarithm of average length of stay (ALOS), for Level 4 hospitals (Table 3, Karin 2018)
  mutate(lnadmissions = if_else(hosp_type_mdr == 140, log(4971), log(14028))) #// Natural logarithm of total inpatient admissions

finance_merged <- finance_merged %>%
  # Create dummy variable for level 3 facilities
  mutate(HOSP1 = if_else(hosp_type_mdr == 140, 1, 0)) %>%
  # Create dummy variable for teaching hospitals
  mutate(Dteach = if_else(hosp_type_mdr == 142, 1, 0)) %>%
  # Create dummy variable for public level hospitals
  mutate(public = 1) %>%
  # Create dummy variable for private level hospitals
  mutate(private = 0) %>%
  # Create dummy variable for observations in Brazil
  mutate(DBrazil = if_else(iso3 == "BRA", 1, 0)) %>%
  # Create constant variable
  mutate(cons = 1)

# // Regression results from IER: e(b) e(rmse) and e(V)
inp_coeff
inp_covvar
inp_rvar


# // Predicted dependent variable
inp_X <- finance_merged %>%
  select(lngdp,lnpctwardbeds,lnalos,lnadmissions,HOSP1,Dteach,public,private,DBrazil, cons) %>%
  as.matrix()

uc_bedday <- as.data.frame(t(inp_coeff %*% t(inp_X))) %>%
  rename(uc_bedday_mdr = 1)

finance_merged <- finance_merged %>%
  cbind(uc_bedday)

# // IER apply a "bias correction factor" of 1.054 and then remove the drug & food cost component (0.573694903)
finance_merged <- finance_merged %>%
  mutate(
    uc_bedday_mdr_cur_int = exp(uc_bedday_mdr) * 1.054 * (1 - 0.573694903),
    uc_bedday_mdr_cur_usd = uc_bedday_mdr_cur_int * pa_nus_ppp * (1 / off_exch_rate),
    uc_bedday_mdr_const_usd = uc_bedday_mdr_cur_int * pa_nus_ppp * (1 / deflator) * (1 / off_exch_rate_latest),
    uc_bedday_mdr_cur_lcu = uc_bedday_mdr_cur_int * pa_nus_ppp
  )

# // Standard error of the forecast
n_rows <- nrow(finance_merged)
inp_varf <- numeric(n_rows)
i <- 1
# Loop through each row
for (i in 1:n_rows) {
  # Print the current row number
  print(i)
  
  # Calculate the out_varf value for the current row
  inp_varf[i] <- inp_rvar + inp_X[i,] %*% inp_covvar %*% inp_X[i,]
}
print(inp_varf)

finance_merged <- finance_merged %>%
  cbind(inp_varf_mdr=inp_varf)


# // Standard error of the forecast
finance_merged <- finance_merged %>%
  mutate(
    uc_bedday_mdr_cur_int_lnvarf = inp_varf_mdr,
    uc_bedday_hi = uc_bedday + 1.67 * sqrt(inp_varf_mdr),
    uc_bedday_lo = uc_bedday - 1.67 * sqrt(inp_varf_mdr),
    uc_bedday_mdr_cur_int_hi = exp(uc_bedday_hi) * 1.054 * (1 - 0.573694903),
    uc_bedday_mdr_cur_int_lo = exp(uc_bedday_lo) * 1.054 * (1 - 0.573694903),
    uc_bedday_mdr_cur_usd_hi = uc_bedday_mdr_cur_int_hi * pa_nus_ppp * (1 / off_exch_rate),
    uc_bedday_mdr_cur_usd_lo = uc_bedday_mdr_cur_int_lo * pa_nus_ppp * (1 / off_exch_rate)
  )

summary_cols <- c("uc_visit_cur_int", "uc_visit_cur_usd", "uc_bedday_nmdr_cur_int",
                  "uc_bedday_nmdr_cur_usd", "uc_bedday_mdr_cur_int", "uc_bedday_mdr_cur_usd")

summary_result <- summary(finance_merged[, summary_cols])


# // FORCE VALUES FOR THOSE FOR WHO UNIT COST IS STILL MISSING (incl CUB, PSE, SOM, PRK, JPN, SSD)
uc_vars <- c("uc_visit_cur_usd", "uc_bedday_nmdr_cur_usd", 
                     "uc_bedday_mdr_cur_usd", "uc_bedday_nmdr_const_usd", 
                     "uc_bedday_nmdr_const_usd", "uc_bedday_mdr_const_usd")

# Group by year, g_whoregion, and g_income, and then replace missing values with the previous value in each group
fill_na <- function(df, var) {
  df <- df %>%
    group_by(year, g_whoregion, g_income) %>%
    arrange(c_notified) %>%
    ungroup() %>%
    mutate(!!var := if_else(is.na(!!sym(var)), pmin(lead(!!sym(var),1),lag(!!sym(var),1), lead(!!sym(var),2),lag(!!sym(var),2), na.rm=T), !!sym(var))) 
    # mutate(!!var := if_else(is.na(!!sym(var)), pmin(lead(!!sym(var),2),lag(!!sym(var),2), na.rm=T), !!sym(var))) %>%
  return(df)
}

for (var in uc_vars) {
  finance_merged <- fill_na(finance_merged, var)
}

# //Venezuela has issues bc of currency fluctuations
# ** PN Jun 2020: main contributor was ppp_deflator which leads to large USD estimates of UCs. Resolved in prepare do file 
# // foreach var of varlist uc_visit_cur_usd uc_bedday_nmdr_cur_usd  uc_bedday_mdr_cur_usd {
#   //replace `var'=`var'[_n-1] if iso3=="VEN" & year >=2016
# //}

finance_merged <- finance_merged %>%
  mutate(flag = NA) %>%  # Initialize flag column with NA values
  mutate(flag = ifelse(uc_bedday_nmdr_cur_usd < uc_bedday_nmdr_cur_usd, 1, flag))