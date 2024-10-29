# source2 <- function(file, start, end, ...) {
#   file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
#   file.lines.collapsed <- paste(file.lines, collapse='\n')
#   source(textConnection(file.lines.collapsed), ...)
# }
# 

source(here::here("report/ch4-1_prepare_data.R")) # load revised data
# source(here::here("report/ch4_preparations.R")) # load original data

## validation by topic
#---------------------------------------------
# 1-1. The number of DR-TB per country
#---------------------------------------------
mdr <- f4.1.2_4.1.6_data %>%
  select(iso3, country, year, mdr_tx_2024 = mdr_tx) %>%
  left_join(select(f4.1.2_4.1.6_data, iso3, year, mdr_tx_2023 = mdr_tx), by = c("iso3", "year")) %>%
  mutate(diff = mdr_tx_2024-mdr_tx_2023)

writexl::write_xlsx(mdr, here::here("finance/local/mdr_tx.xlsx"))

#---------------------------------------------
# 1-2. Domestic vs external distributions
#---------------------------------------------
# revised
val_country_intext <- f4.1.2_4.1.6_data %>% 
  select(iso3, country, year, g_whoregion, g_income, rcvd_int, c_ghs_nmdr, c_ghs_mdr, rcvd_ext_gf, rcvd_ext_ngf, rcvd_tot, rcvd_imputation) 
val_country_intext$int <- f4.1.2_4.1.6_data %>%  select(rcvd_int, c_ghs_nmdr, c_ghs_mdr) %>%   rowSums(na.rm = T)
val_country_intext$ext <- f4.1.2_4.1.6_data %>%  select(rcvd_ext_gf, rcvd_ext_ngf) %>%     rowSums(na.rm = T)
val_country_intext$tot <- f4.1.2_4.1.6_data %>%  select(rcvd_tot, c_ghs_nmdr, c_ghs_mdr) %>%     rowSums(na.rm = T)

val_country_intext <- val_country_intext %>%
  mutate(tot_sum = int + ext,
         diff = as.integer(tot_sum-tot))

writexl::write_xlsx(val_country_intext, here::here("finance/local/validation_domext.xlsx"))


#---------------------------------------------
# 1-3. Distributions by service
#---------------------------------------------
# revised
val_country_dsdr <- f4.1.2_4.1.6_data %>% 
  select(iso3, country, g_brics, year, g_whoregion, g_income, rcvd_int, rcvd_ext_gf, rcvd_ext_ngf,
         rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv, rcvd_mdr, rcvd_nmdr_ndot_hiv, rcvd_nmdr_ndot_tpt, rcvd_tot, c_ghs_nmdr, c_ghs_mdr, rcvd_imputation)
val_country_dsdr$DSTB <- val_country_dsdr %>%  select(rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv, c_ghs_nmdr) %>% rowSums(na.rm = T)  
val_country_dsdr$MDR <- val_country_dsdr %>%  select(rcvd_mdr, c_ghs_mdr) %>% rowSums(na.rm = T)  
val_country_dsdr$TBHIV <- val_country_dsdr %>% select(rcvd_nmdr_ndot_hiv) %>% unlist() 
val_country_dsdr$TPT <- val_country_dsdr %>%  select(rcvd_nmdr_ndot_tpt) %>% unlist() 
val_country_dsdr$tot <- val_country_dsdr %>%  select(rcvd_tot, c_ghs_mdr, c_ghs_nmdr) %>% rowSums(na.rm = T) 
val_country_dsdr$int <- f4.1.2_4.1.6_data %>%  select(rcvd_int, c_ghs_nmdr, c_ghs_mdr) %>%   rowSums(na.rm = T)
val_country_dsdr$ext <- f4.1.2_4.1.6_data %>%  select(rcvd_ext_gf, rcvd_ext_ngf) %>%     rowSums(na.rm = T)

val_country_dsdr <- val_country_dsdr %>%
  mutate(tot_sum = DSTB + MDR + TBHIV + TPT,
         tot_sum2 = int + ext,
         diff = as.integer(tot_sum-tot),
         diff = as.integer(tot_sum2-tot_sum))

writexl::write_xlsx(val_country_dsdr, here::here("finance/local/validation_dsdr.xlsx"))


#---------------------------------------------
# 2-1. DR per patient cost
#---------------------------------------------
# revised
writexl::write_xlsx(f4.12_data, here::here("finance/local/validation_ppc_dr.xlsx"))

#---------------------------------------------
# GF requested data
#---------------------------------------------
gfc <- c("AFG","ARM","AZE","BGD","BDI","BEN","BLR","BFA","BOL","BTN","KHM","CAF","CIV","CMR","COD","COG","COM",
         "EGY","ERI","ETH","SLV","SWZ","GAB","GMB","GEO","GHA","GTM","GIN","GNB","HTI","HND","IND","IDN","KAZ",
         "KEN","KGZ","LAO","LSO","LBR","MDG","MWI","MLI","MDA","MNG","MAR","MOZ","MMR","NPL","NIC","NER","NGA",
         "PAK","PNG","PER","PHL","RWA","SLB","SOM","ZAF","SSD","LKA","SDN","TJK","TZA","THA","TLS","TGO","TKM",
         "UGA","UKR","UZB","VNM","ZMB","ZWB") # not included - Guyana: HIC, Kosovo: non member states, Zanzibar: part of Tanzania

# 2024
gf_2024_con <- f4.1.2_4.1.6_data %>%
  select(iso3, country, year, g_whoregion, g_income, rcvd_int, c_ghs_nmdr, c_ghs_mdr, rcvd_ext_gf, rcvd_ext_ngf, rcvd_tot, c_ghs_nmdr, c_ghs_mdr, rcvd_imputation) %>%
  filter(year >= 2017, iso3 %in% gfc)
gf_2024_con$int <- gf_2024_con %>%  select(rcvd_int, c_ghs_nmdr, c_ghs_mdr) %>%   rowSums(na.rm = T)
gf_2024_con$ext <- gf_2024_con %>%  select(rcvd_ext_gf, rcvd_ext_ngf) %>%     rowSums(na.rm = T)
gf_2024_con$tot <- gf_2024_con %>%  select(rcvd_tot, c_ghs_nmdr, c_ghs_mdr) %>%     rowSums(na.rm = T)

gf_2024_con <- gf_2024_con %>%
  select(iso3, country, year, g_whoregion, g_income, int, ext, rcvd_ext_gf, rcvd_ext_ngf, tot, rcvd_imputation)

writexl::write_xlsx(gf_2024_con, here::here("finance/local/gf_request_2022constant.xlsx"))

gf_2024_con_sh <- gf_2024_con %>%
  select(!rcvd_ext_gf & !rcvd_ext_ngf & !rcvd_imputation) %>%
  arrange(iso3)

writexl::write_xlsx(gf_2024_con_sh, here::here("finance/local/gf_request_2022constant_share.xlsx"))


# for data check with nominal USD
load(here::here('finance/local/finance_imputed_nomusd.Rdata'))

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
  )

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

gf_2024_nom <- f4.1.2_4.1.6_data %>%
  select(iso3, country, year, g_whoregion, g_income, rcvd_int, c_ghs_nmdr, c_ghs_mdr, rcvd_ext_gf, rcvd_ext_ngf, rcvd_tot, c_ghs_nmdr, c_ghs_mdr, rcvd_imputation) %>%
  filter(year >= 2017, iso3 %in% gfc)
gf_2024_nom$int <- gf_2024_nom %>%  select(rcvd_int, c_ghs_nmdr, c_ghs_mdr) %>%   rowSums(na.rm = T)
gf_2024_nom$ext <- gf_2024_nom %>%  select(rcvd_ext_gf, rcvd_ext_ngf) %>%     rowSums(na.rm = T)
gf_2024_nom$tot <- gf_2024_nom %>%  select(rcvd_tot, c_ghs_nmdr, c_ghs_mdr) %>%     rowSums(na.rm = T)

gf_2024_nom <- gf_2024_nom %>%
  select(iso3, country, year, g_whoregion, g_income, int, ext, rcvd_ext_gf, rcvd_ext_ngf, tot, rcvd_imputation) %>%
  arrange(iso3)

writexl::write_xlsx(gf_2024_nom, here::here("finance/local/gf_request_nomusd.xlsx"))

gf_2024_nom_sh <- gf_2024_nom %>%
  select(!rcvd_ext_gf & !rcvd_ext_ngf & !rcvd_imputation) %>%
  arrange(iso3)

writexl::write_xlsx(gf_2024_nom_sh, here::here("finance/local/gf_request_nomusd_share.xlsx"))


#---------------------------------------------
# comparison with prevalence survey budget
#---------------------------------------------
# for data check with nominal USD
load(here::here('finance/local/finance_imputed_nomusd.Rdata'))
source(here::here("report/ch4-1_prepare_data.R")) # load revised data

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
  )

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

ps_budget <- f4.1.2_4.1.6_data %>% 
  select(iso3, country, year)
ps_budget$int <- f4.1.2_4.1.6_data %>%  select(rcvd_int, c_ghs_nmdr, c_ghs_mdr)  %>%  rowSums(na.rm = T)
ps_budget$ext <- f4.1.2_4.1.6_data %>%  select(rcvd_ext_gf, rcvd_ext_ngf) %>%     rowSums(na.rm = T)
ps_budget$tot <- f4.1.2_4.1.6_data %>%  select(rcvd_tot, c_ghs_nmdr, c_ghs_mdr)  %>%  rowSums(na.rm = T)

ps <- c("BGD","KHM","CHN","PRK","SWZ","ETH","GMB","GHA","IDN","KEN","LAO","LSO","MWI","MNG","MOZ","MMR","NAM","NPL",
        "NGA","PAK","PHL","RWA","ZAF","SDN","THA","UGA","TZA","VNM","ZMB","ZWE","IND","TLS")

ps <- c("GMB")

ps_budget <- ps_budget %>%
  select(iso3, country, year, int, ext, tot) %>%
  filter(year <= report_year-2, iso3 %in% ps)

writexl::write_xlsx(ps_budget, here::here(paste0("./report/local/tbps_funding available_nomusd_", Sys.Date(), ".xlsx")))

#---------------------------------------------
# GF request for their result report
#---------------------------------------------
# 2024
load(here::here('finance/local/finance_imputed_nomusd.Rdata'))

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


gf_2024_nominal <- f4.1.2_4.1.6_data %>%
  select(iso3, country, year, g_whoregion, g_income, rcvd_int, rcvd_ext_gf, c_ghs_nmdr, c_ghs_mdr, rcvd_ext_ngf, rcvd_tot, rcvd_imputation, rcvd_tot_domestic,	rcvd_tot_gf,	rcvd_tot_grnt,	rcvd_tot_usaid
) %>%
  filter(year >= 2017)
gf_2024_nominal$int <- gf_2024_nominal %>%  select(rcvd_int, c_ghs_nmdr, c_ghs_mdr) %>%   rowSums(na.rm = T)
gf_2024_nominal$ext <- gf_2024_nominal %>%  select(rcvd_ext_gf, rcvd_ext_ngf) %>%     rowSums(na.rm = T)
gf_2024_nominal$tot <- gf_2024_nominal %>%  select(rcvd_tot, c_ghs_nmdr, c_ghs_mdr) %>%     rowSums(na.rm = T)

gf_nominal_summary <- gf_2024_nominal %>%
  select(year, int, ext, rcvd_ext_gf, rcvd_ext_ngf, tot, rcvd_tot_domestic,	rcvd_tot_gf,	rcvd_tot_grnt,	rcvd_tot_usaid) %>%
  group_by(year) %>%
  summarise(across(int:rcvd_tot_usaid, sum, na.rm=TRUE)) %>%
  mutate(ext_raw = (rcvd_tot_gf+rcvd_tot_grnt+rcvd_tot_usaid),
         gf_pct_imp = rcvd_ext_gf/ext*100,
         gf_pct_raw = rcvd_tot_gf/ext_raw*100) %>%
  select(year:tot,gf_pct_imp,rcvd_tot_domestic:rcvd_tot_usaid,ext_raw,gf_pct_raw)
  
writexl::write_xlsx(gf_nominal_summary, here::here(paste0("./report/local/gf_pct_trend_nominal_", Sys.Date(), ".xlsx")))

gf_con_country <- gf_2024_con %>%
  select(iso3, country, year, g_income, int, ext, rcvd_ext_gf, rcvd_ext_ngf, tot, rcvd_imputation) %>%
  filter(year == report_year - 1) %>%
  mutate(gf_pct = rcvd_ext_gf/ext*100)

writexl::write_xlsx(gf_con_country, here::here(paste0("./report/local/gf_pct_country_", report_year-1, "_",Sys.Date(), ".xlsx")))

# ----------------------------------------------------------
## cost per person treated - DS-TB comparing past 2 years ##
# ----------------------------------------------------------

f4.1.11_datacheck <- finance_merged
f4.1.11_datacheck <- f4.1.11_datacheck %>%
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
  filter(year >= report_year - 3 & year != report_year & c_notif_less_mdr >= 100 & g_income != "HIC" &
           !is.na(c_pp_dots) & !is.na(imf_gdp_pc_con_usd) &
           !is.na(rcvd_nmdr_dot) & !is.na(c_ghs_nmdr)) %>%
  # In 2024 report, we drop Bhutan as all data are reported in oth
  select(iso3, country, year, imf_gdp_pc_con_usd,log10_gdp_pc_con_usd, c_pp_dots,log10_c_pp_dots,
         c_ghs_nmdr, DSTB, g_whoregion, g_income, g_hb_tb,
         c_notified=c_notif_less_mdr, c_newinc) %>%
  # Sorted in descending order to let larger bubbles be plotted before smaller ones
  arrange(desc(c_notified)) %>%
  select(iso3, country, year, c_pp_dots,DSTB) %>%
  pivot_wider(names_from = year, values_from = c_pp_dots:DSTB) %>%
  mutate(diff_pct = (c_pp_dots_2023/c_pp_dots_2022 - 1)*100,
         flag_50 = ifelse(diff_pct >= 50 | diff_pct <= -50, 1,0)) 

writexl::write_xlsx(f4.1.11_datacheck, here::here(paste0("./finance/local/f4.1.11_datacheck_",Sys.Date(), ".xlsx")))

f4.1.11_VEN <- finance_merged %>%
  filter(iso3 == "VEN") %>%
  select(iso3, year, imf_deflator_us)

f4.1.11_VEN <- finance_merged %>%
  filter(iso3 == "FJI") %>%
  select(iso3, year, c_notif_less_mdr)

# ----------------------------------------------------------
## cost per person treated - DS-TB comparing past 3 years ##
# ----------------------------------------------------------
f4.1.12_datacheck <- finance_merged %>%
  # consistent with Fig 4.4 above, received funds for DSTB (including c_ghs_nmdr) are divided by c_notified, only for countries with c_notified above 100
  mutate(g_hb_mdr = ifelse(is.na(g_hb_mdr),0,g_hb_mdr)) %>%
  
  mutate(rcvd_mdr = ifelse(rcvd_mdr == 0, cf_mdr, rcvd_mdr)) %>%
  mutate(MDR = finance_merged %>% select(rcvd_mdr, c_ghs_mdr) %>% rowSums(na.rm = T),
         c_pp_mdr = MDR / mdr_tx 
  ) %>%
  
  # Restrict to latest year, Exclude countries where c_pp_mdr is empty and where it cannot be computed accurately i.e where rcvd_mdr_sld is zero,
  # or c_ghs_mdr is empty (but keep those with c_ghs_mdr == 0 eg CHN, KAZ, RUS etc)
  filter(year >= report_year - 3 & year != report_year & mdr_tx >= 20 & g_income != "HIC" &
           ((!is.na(c_pp_mdr) & !is.na(imf_gdp_pc_con_usd) &
               !is.na(rcvd_mdr_sld) & rcvd_mdr_sld > 0))) %>% # GTM exceptional: SLD is zero for recent years, but data looks fine
  # In 2024 report, we drop Bhutan as all data are reported in oth
  select(iso3, country, year, g_whoregion, g_income, g_hb_mdr, imf_gdp_pc_con_usd, c_pp_mdr, MDR,  mdr_tx) %>%
  # Sorted in descending order to let larger bubbles be plotted before smaller ones
  arrange(desc(mdr_tx)) %>%
  
  select(iso3, country, year, c_pp_mdr,MDR) %>%
  pivot_wider(names_from = year, values_from = c_pp_mdr:MDR) %>%
  mutate(diff_pct = (c_pp_mdr_2023 /c_pp_mdr_2022 - 1)*100,
         flag_50 = ifelse(diff_pct >= 50 | diff_pct <= -50, 1,0)) 

writexl::write_xlsx(f4.1.12_datacheck, here::here(paste0("./finance/local/f4.1.12_datacheck_",Sys.Date(), ".xlsx")))
