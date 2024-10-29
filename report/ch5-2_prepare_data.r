# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch5-2_and_kendo.rmd
# Takuya Yamanaka, June 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load chapter 5 packages, settings and data
source(here::here('report/ch5_load_data.r'))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.1 ----
# (National surveys of costs faced by TB patients and their households since 2015: progress and plans)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

palatte_fig5.2.1 = c("royalblue4","dodgerblue3","steelblue1","deeppink2","violet")

load(here::here('data/pcs/pcs_progress_data.Rdata'))
  
num <- table(f5.2.1_data$var)
labs <- c(
  paste0("First survey completed (n=",num[1],")"),
  paste0("First survey ongoing (n=",num[2],")"),
  paste0("First survey planned (n=",num[3],")"),
  paste0("Repeat survey ongoing (n=",num[4],")"),
  paste0("Repeat survey planned (n=",num[5],")")
)

f5.2.2_txt <-  f5.2.1_data %>%
  filter(var == "First survey completed"|var == "Repeat survey planned"|var == "Repeat survey ongoing") %>%
  nrow

f5.2.2_txt <- f5.2.1_data %>%
  filter(var == "First survey completed"|var == "Repeat survey planned"|var == "Repeat survey ongoing") %>%
  filter(iso3 %in% iso3_hbc) %>%
  nrow %>% 
  cbind.data.frame(f5.2.2_txt) %>%
  rename(hbc30 = 1, all = 2)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.3 ----
# (Distribution of costs faced by TB patients and their households in 25 national surveys completed since 2016)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
load(here::here('data/pcs/pcs_cost_tot_data.Rdata'))

weo_us <- weo %>% filter(iso3=="USA") %>%
  ungroup() %>%
  select(year,deflator_us)

f5.2.3_const_data <- f5.2.3_data %>%
  left_join(weo_us, by = c("year")) %>%
  mutate(across(c(value:uci), ~ . / deflator_us)) %>%
  mutate(country = ifelse(country=="Nepal","Nepal\u1D9C",country))

f5.2.3_sel_order <- 
  f5.2.3_const_data %>% 
  arrange(value) %>% 
  mutate(country_a = ifelse(iso3=="ZMB","Zambia",
                            ifelse(iso3=="NAM","Namibia",country))) %>% 
  mutate(country = factor(country),
         country_a = factor(country_a)) 

max <- max(f5.2.3_const_data$value, na.rm = T)
min <- min(f5.2.3_const_data$value, na.rm = T)

f5.2.3_txt_num <- f5.2.3_const_data %>%
  filter(!is.na(value))

f5.2.3_txt_min <- f5.2.3_const_data %>%
  filter(value == min)

f5.2.3_txt_max <- f5.2.3_const_data %>%
  filter(value == max)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.4 ----
# (Selected baseline results from national surveys^a^ of costs faced by TB patients and their households, latest year   )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
load(here::here('data/pcs/pcs_catast_data.Rdata'))

# Subset data for All TB to estimate pooled average
notification <- notification %>%
  mutate(c.notified=c_newinc) %>%
  mutate(c_ds=ifelse(year<2020,c.notified-conf_rrmdr,c.notified-(conf_rr_nfqr + conf_rr_fqr))) %>%
  mutate(conf_rrmdr=ifelse(year<2020,conf_rrmdr,conf_rr_nfqr + conf_rr_fqr))

f5.2.4a_data <- f5.2.4_data %>% 
  filter(grp=='overall') %>% 
  mutate(year=ifelse(year>report_year-1,report_year-1,year))  %>%
  mutate(country = ifelse(country=="Nepal","Nepal\u1D48",country))

notification %>% select(iso3,year,c.notified) %>% right_join(f5.2.4a_data,by=c('iso3','year')) -> f5.2.4a_data

fit_all <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = f5.2.4a_data, 
    weights = c.notified
  )

# Save pooled average in the data/pcs folder for importing into the Global Database so that
# it can be included in the global TB profile shown in the mobile app and on the web
pooled_catst_costs_average <- data.frame(
  group_type = "global",
  group_name = "global",
  year_range = paste0("2015-", report_year),
  patient_group = "all",
  catast_pct = as.numeric(fit_all$b),
  catast_pct_lo = fit_all$ci.lb,
  catast_pct_hi = fit_all$ci.ub
)
  
save(pooled_catst_costs_average, file = here::here("data/pcs/pcs_pooled_catast_data.Rdata"))

f5.2.4a_data <- f5.2.4a_data %>% 
  add_row(iso3="AVE",country="Pooled average", 
          grp="ave",
          catast_pct    = as.numeric(fit_all$b),
          catast_pct_lo = fit_all$ci.lb,
          catast_pct_hi = fit_all$ci.ub) 

f5.2.4_sel_order <- 
  f5.2.4a_data %>% 
  arrange(catast_pct) %>% 
  arrange(grp) %>%
  mutate(country_a = ifelse(iso3=="ZMB","Zambia",
                            ifelse(iso3=="NAM","Namibia",country))) %>% 
  mutate(country = factor(country),
         country_a = factor(country_a)) 

## Subset data for DS-TB 
f5.2.4b_data <- f5.2.4_data %>% 
  filter(grp=='TB (first-line treatment)'|iso3=="TLS"|iso3=="SLV"|iso3=="FJI"|iso3=="SLB") %>% 
  mutate(year=ifelse(year>report_year-1,report_year-1,year))  %>%
  mutate(country = ifelse(country=="Nepal","Nepal\u1D48",country))

notification %>% select(iso3,year,c_ds) %>% inner_join(f5.2.4b_data,by=c('iso3','year')) %>% distinct() -> f5.2.4b_data 

fit_all <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = f5.2.4b_data, 
    weights = c_ds
  )

f5.2.4b_data <- f5.2.4b_data %>% 
  add_row(iso3="AVE",country="Pooled average", 
          grp="ave",
          catast_pct    = as.numeric(fit_all$b),
          catast_pct_lo = fit_all$ci.lb,
          catast_pct_hi = fit_all$ci.ub) %>% 
  # mutate(country = ifelse(country=="Namibia","Namibia\u1D9C,\u1D48",country)) %>%
  mutate(country=factor(country,levels=rev(country))) #%>%
  # add_row(iso3="NAM",grp="TB (first-line treatment)",country="Namibia\u1D9C,\u1D48") 
  

f5.2.4b_data <- f5.2.4b_data %>% 
  mutate(grp=ifelse(grp=="overall","TB (first-line treatment)",grp))


## Subset data for DR-TB 
f5.2.4c_data <- f5.2.4_data %>% 
  filter(grp=='Drug-resistant TB') %>% 
  mutate(year=ifelse(year>report_year-1,report_year-1,year))  %>%
  mutate(country = ifelse(country=="Nepal","Nepal\u1D48",country))

notification %>% select(iso3,year,conf_rrmdr) %>% right_join(f5.2.4c_data,by=c('iso3','year')) %>% distinct() -> f5.2.4c_data

fit_all <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = f5.2.4c_data, 
    weights = conf_rrmdr   
  )

f5.2.4c_data <- f5.2.4c_data %>% 
  add_row(iso3="AVE",country="Pooled average", 
          grp="ave",
          catast_pct    = as.numeric(fit_all$b),
          catast_pct_lo = fit_all$ci.lb,
          catast_pct_hi = fit_all$ci.ub) %>%
  # mutate(country = ifelse(country=="Namibia","Namibia\u1D9C,\u1D48",country)) %>% 
  mutate(country=factor(country,levels=rev(country))) %>%  # factorize in the order of rows 
  add_row(iso3="SLB",grp="Drug-resistant TB",country="Solomon Islands") %>%
  add_row(iso3="FJI",grp="Drug-resistant TB",country="Fiji") %>%
  add_row(iso3="TLS",grp="Drug-resistant TB",country="Timor-Leste") %>%
  add_row(iso3="SLV",grp="Drug-resistant TB",country="El Salvador") 
  # add_row(iso3="NAM",grp="Drug-resistant TB",country="Namibia\u1D9C,\u1D48") 


# extract pooled averages for texts
f5.2.4a_txt <- f5.2.4a_data %>%
  subset(iso3=="AVE") %>%
  mutate(grp="Overall\n(End TB Strategy indicator)") 

f5.2.4a_txt_lo <- f5.2.4a_data %>%
  arrange(catast_pct) %>%
  slice(1)

f5.2.4a_txt_hi <- f5.2.4a_data %>%
  arrange(desc(catast_pct)) %>%
  slice(1)

f5.2.4a_txt_num <- f5.2.4a_data %>%
  filter(iso3!="AVE")

f5.2.4c_txt <- f5.2.4c_data %>%
  subset(iso3=="AVE") %>%
  mutate(grp="Drug-resistant TB") 

f5.2.4c_txt_num <- f5.2.4c_data %>%
  filter(iso3!="AVE", !is.na(catast_pct))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.5 ----
# (Distribution of costs faced by TB patients and their households in 25 national surveys completed since 2016)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
palatte_f5.2.5 = c("goldenrod2","dodgerblue1","darkblue")

load(here::here('data/pcs/pcs_cost_driver_data.Rdata'))

f5.2.5_data <- f5.2.5_data %>%
  mutate(country = as.character(country)) %>%
  mutate(country = ifelse(country=="Nepal","Nepal\u1D47",country))

f5.2.5_sel_order <- 
  f5.2.5_data %>% 
  filter(cat == "p_med") %>% 
  arrange(value) %>% 
  mutate(country = factor(country))

f5.2.5_txt_num <- f5.2.5_data %>%
  filter(cat == "p_med")

f5.2.5_txt_med <- f5.2.5_data %>%
  filter(cat == "p_med", value > 20) %>%
  arrange(as.character(country))

f5.2.5_txt_med_list <- f5.2.5_txt_med %>% nrow()

f5.2.5_txt_nmed <- f5.2.5_data %>%
  filter(cat == "p_nmed", value > 50) %>%
  arrange(as.character(country))

f5.2.5_txt_nmed_list <- f5.2.5_txt_nmed %>% nrow()

f5.2.5_txt_indirect <- f5.2.5_data %>%
  filter(cat == "p_indirect", value > 43.6) %>% # find cutoff indirect > nmed
  arrange(as.character(country))

f5.2.5_txt_indirect_list <- f5.2.5_txt_indirect %>% nrow()

# write csv for GTB Database 
f5.2.5_data %>%
  mutate(value = ftb(value)) %>%
  filter(#iso3 == "ZMB" | 
           iso3 == "NAM" ) %>% # filter countries with updates: 2023, Zambia and Namibia
  write_csv(paste0(here::here('report/local/'),"/tbpcs_cost drivers_update",report_year,".csv")) 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.6 ----
# (Model-based estimates of cost faced by TB patients and their households in 135 low- and middle-income countries, WHO regions)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
load(here::here('data/pcs/pcs_modelling_data.Rdata'))

f5.2.6_txt_global <- f5.2.6_data %>%
  filter(entity=="All LMICs")
  
f5.2.6_txt_afro <- f5.2.6_data %>%
  filter(entity=="African Region")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 5.2.7a-f ----
# (Status of social protection for people with TB, 30 high TB burden countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sp_col <- c("free_access_tbdx","free_access_tbtx",
                "enable_tx_adherence","cash_trans",
                "food_security","income_loss")
dnk <- 3
no <- 0

f5.2.7_data <- sp |>
  filter(year==report_year) %>% 
  select(country, iso3, social_protn, free_access_tbdx:enable_tx_adherence,cash_trans,food_security,income_loss)

f5.2.7_txt_sp_num <- f5.2.7_data %>%
  filter(!is.na(social_protn)&social_protn!=0) %>% nrow()

f5.2.7_data <- f5.2.7_data %>%
  mutate(across(all_of(sp_col), ~ ifelse(. == dnk, no, .))) %>%
  mutate(across(free_access_tbdx:income_loss, ~ ifelse(social_protn == 0 & is.na(.), 0, .)))

# produce sub dfs for 6 maps
f5.2.7a_data <- f5.2.7_data %>%
  select(iso3,free_access_tbdx) %>%
  # Assign the categories for the map
  mutate(var = factor(free_access_tbdx,
                    levels = c(1, 0),
                    labels = c("Available", "Not available"))) 
palatte_fig5.2.7a = c("#2171B5","#C6DBEF")

f5.2.7_txt_dx_num <- f5.2.7a_data %>%
  filter(!is.na(free_access_tbdx)&free_access_tbdx!=0) %>% nrow()


f5.2.7b_data <- f5.2.7_data %>%
  select(iso3,free_access_tbtx) %>%
  # Assign the categories for the map
  mutate(var = factor(free_access_tbtx,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.7b = c("#99000D","#FCBBA1")

f5.2.7_txt_tx_num <- f5.2.7b_data %>%
  filter(!is.na(free_access_tbtx)&free_access_tbtx!=0) %>% nrow()

f5.2.7c_data <- f5.2.7_data %>%
  select(iso3,enable_tx_adherence) %>%
  # Assign the categories for the map
  mutate(var = factor(enable_tx_adherence,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.7c = c("#238B45","#C7E9C0")

f5.2.7c_txt_num <- f5.2.7c_data %>%
  filter(!is.na(enable_tx_adherence)&enable_tx_adherence!=0) %>% nrow()

f5.2.7d_data <- f5.2.7_data %>%
  select(iso3,cash_trans) %>%
  # Assign the categories for the map
  mutate(var = factor(cash_trans,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.7d = c("#D94801","#FDD0A2")

f5.2.7d_txt_num <- f5.2.7d_data %>%
  filter(!is.na(cash_trans)&cash_trans!=0) %>% nrow()

f5.2.7e_data <- f5.2.7_data %>%
  select(iso3,food_security) %>%
  # Assign the categories for the map
  mutate(var = factor(food_security,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.7e = c("#6A51A3","#DADAEB")

f5.2.7e_txt_num <- f5.2.7e_data %>%
  filter(!is.na(food_security)&food_security!=0) %>% nrow()

f5.2.7f_data <- f5.2.7_data %>%
  select(iso3,income_loss) %>%
  # Assign the categories for the map
  mutate(var = factor(income_loss,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.7f = c("#CE1256","#D4B9DA")

f5.2.7f_txt_num <- f5.2.7f_data %>%
  filter(!is.na(income_loss)&income_loss!=0) %>% nrow()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 5.2.8 ----
# (Status of national laws and regulations against stigma)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

stigma_col <- c("protect_employment","protect_housing",
            "protect_parenting","protect_movement",
            "protect_association")

f5.2.8_country_data <- sp |>
  filter(year==report_year) %>% 
  select(iso3, g_whoregion, protect_employment:protect_association)

f5.2.8_country_data <- f5.2.8_country_data %>%
  mutate(across(all_of(stigma_col), ~ ifelse(. == dnk, no, .))) 

# n_country <- f5.2.8_country_data %>% 
#   mutate(n_country = 1) %>%
#   select(-iso3) %>%
#   tidyr::gather(variable, category, -g_whoregion) %>%
#   group_by(g_whoregion, variable, category) %>%
#   count() %>%
#   ungroup %>%
#   filter(variable=="n_country") %>%
#   select(g_whoregion, n_country=n)
# 
# f5.2.8_region_data <- f5.2.8_country_data %>% 
#   select(-iso3) %>%
#   tidyr::gather(variable, category, -g_whoregion) %>%
#   group_by(g_whoregion, variable, category) %>%
#   count() %>%
#   ungroup %>% 
#   filter(category==1) %>%
#   select(-category) %>%
#   group_by(g_whoregion, variable) %>%
#   summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
#   # merge with regional names
#   inner_join(who_region_shortnames, by = "g_whoregion") %>%
#   # merge with regional names
#   inner_join(select(n_country, c("g_whoregion","n_country")), by = "g_whoregion") %>%
#   ungroup %>% 
#   select(-g_whoregion)
# 
# # Add global summary to the regional summary
# f5.2.8_global_data <- f5.2.8_region_data %>%
#   select(-n_country) %>%
#   group_by(variable) %>%
#   summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
#   mutate(n_country = 215) %>%
#   mutate(entity="Global")
# 
# 
# f5.2.8_hbc_data <- f5.2.8_country_data %>% 
#   filter(iso3 %in% iso3_hbc) %>%
#   select(-iso3, -g_whoregion) %>%
#   tidyr::gather(variable, category) %>%
#   group_by(variable, category) %>%
#   count() %>%
#   ungroup %>% 
#   filter(category==1) %>%
#   select(-category) %>%
#   group_by(variable) %>%
#   summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
#   ungroup %>% 
#   mutate(entity="High TB burden countries",
#          n_country = 30)
# 
# f5.2.8_data <- rbind(f5.2.8_global_data, f5.2.8_region_data, f5.2.8_hbc_data) %>%
#   mutate(pct = n/n_country*100)
# 
# f5.2.8_data <- f5.2.8_data %>%
#   mutate(variable = factor(variable, levels = c("protect_employment", "protect_housing","protect_parenting", "protect_movement", "protect_association")))
# 
# f5.2.8_global_data <- f5.2.8_global_data %>%
#   mutate(pct = n/n_country*100)

## for map
f5.2.8_txt_num <- f5.2.8_country_data %>%
  filter(!is.na(protect_employment)&!is.na(protect_housing)&!is.na(protect_parenting)&!is.na(protect_movement)&!is.na(protect_association)) %>%
  mutate(protect_n = protect_employment+protect_housing+protect_parenting+protect_movement+protect_association) %>% 
  filter(protect_n != 0) %>%
  nrow()

f5.2.8a_data <- f5.2.8_country_data %>%
  select(iso3,protect_employment) %>%
  # Assign the categories for the map
  mutate(var = factor(protect_employment,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.8a = c("#2171B5","#C6DBEF")

f5.2.8a_txt_num <- f5.2.8a_data %>%
  filter(!is.na(protect_employment)&protect_employment!=0) %>% nrow()

f5.2.8b_data <- f5.2.8_country_data %>%
  select(iso3,protect_housing) %>%
  # Assign the categories for the map
  mutate(var = factor(protect_housing,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.8b = c("#99000D","#FCBBA1")

f5.2.8b_txt_num <- f5.2.8b_data %>%
  filter(!is.na(protect_housing)&protect_housing!=0) %>% nrow()

f5.2.8c_data <- f5.2.8_country_data %>%
  select(iso3,protect_parenting) %>%
  # Assign the categories for the map
  mutate(var = factor(protect_parenting,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.8c = c("#238B45","#C7E9C0")

f5.2.8c_txt_num <- f5.2.8c_data %>%
  filter(!is.na(protect_parenting)&protect_parenting!=0) %>% nrow()

f5.2.8d_data <- f5.2.8_country_data %>%
  select(iso3,protect_movement) %>%
  # Assign the categories for the map
  mutate(var = factor(protect_movement,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.8d = c("#D94801","#FDD0A2")

f5.2.8d_txt_num <- f5.2.8d_data %>%
  filter(!is.na(protect_movement)&protect_movement!=0) %>% nrow()

f5.2.8e_data <- f5.2.8_country_data %>%
  select(iso3,protect_association) %>%
  # Assign the categories for the map
  mutate(var = factor(protect_association,
                      levels = c(1, 0),
                      labels = c("Available", "Not available"))) 
palatte_fig5.2.8e = c("#6A51A3","#DADAEB")

f5.2.8e_txt_num <- f5.2.8e_data %>%
  filter(!is.na(protect_association)&protect_association!=0) %>% nrow()
