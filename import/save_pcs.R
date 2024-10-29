#' # Producing data of PCS key indicators for section 5.2
#'
#' Dependences:
#'  - libraries dplyr and PCS book dataset and excel files for WHO internal monitoring
#'
#' Output: ~/data/pcs folder
library(readxl)
library(dplyr)
library(tidyverse)
library(here)
library(gtbreport)

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))

grpmbr <- load_gtb("grpmbr")

who_region_shortnames <- region_shortnames()

list_iso3_country <-
  grpmbr %>% filter(group_type == 'g_whoregion') %>%
  select(iso3,country)

# list of countries for which survey results needs to be taken from reported data
pcs_reporting <- c("BEN","LSO","MDA","SLV","TLS","ZMB","NAM","KHM","ARG","NPL","COG","SOM","GMB")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.1 ----
# (National surveys of costs faced by TB patients and their households since 2015: progress and plans)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f5.2.1_data <- read_excel(here::here('data/pcs/local/monthly_pcs_monitoring_2024.xlsx'),sheet="for map") %>%
  filter(var!= "Halted"& var!= 0) %>%
  mutate(var = factor(var, labels=c("First survey completed","First survey ongoing","First survey planned", "Repeat survey ongoing", "Repeat survey planned"))) %>%
  mutate(var = factor(var, levels=c("First survey completed","First survey ongoing","First survey planned", "Repeat survey ongoing", "Repeat survey planned"))) %>%
  select(iso3, var)

save(f5.2.1_data, file = here::here('data/pcs/pcs_progress_data.Rdata'))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.3 ----
# (Distribution of costs faced by TB patients and their households in 25 national surveys completed since 2016)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# from PCS book
f5.2.3_data <-  read_excel(here::here('data/pcs/local/TBPCS_ALL_database_v23.xlsx')) %>% #from PCS book
  subset((variable=="cat_total")&
           dr=="overall"&quintile=="all"&sex=="all"&sum_fun=="mean")  %>%
  left_join(list_iso3_country) %>%
  dplyr::select(iso3,country,year, value,lci,uci)

# for addtional 3 countries with standard country profile (COL, NER, ZAF)
f5.2.3_data_add <- read_excel(here::here('data/pcs/local/TBPCS_ALL_database_additional_v1.xlsx')) %>% #from additional 3 countries for Lancet Global Health
  subset((variable=="cat_total")&
           dr=="overall"&quintile=="all"&sex=="all"&sum_fun=="mean")  %>%
  dplyr::select(iso3, year, variable, value, lci, uci) %>%
  left_join(list_iso3_country) %>%
  dplyr::select(iso3,country, year, value,lci,uci)

f5.2.3_data <- rbind.data.frame(f5.2.3_data,f5.2.3_data_add)

# from country reports
f5.2.3_data <- read_excel(here::here('data/pcs/local/catast cost usd 2024_v1.xlsx'))  %>%
  select(-country) %>% left_join(list_iso3_country) %>%
  filter(grp=="all") %>% select(-source,-grp) %>%
  select(iso3,country, year, value,lci,uci) %>%
  subset(iso3 %in% pcs_reporting) %>%
  rbind.data.frame(.,f5.2.3_data) %>%
  arrange(iso3)

save(f5.2.3_data, file = here::here('data/pcs/pcs_cost_tot_data.Rdata'))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.4 ----
# (Selected baseline results from national surveys^a^ of costs faced by TB patients and their households, latest year   )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# PCS book 20 countries
f5.2.4_data <- read_excel(here::here('data/pcs/local/TBPCS_ALL_database_v23.xlsx')) %>% #from PCS book
  subset((variable=="cc1"|variable=="cc2"|variable=="cc3")&
           quintile=="all"&sex=="all") %>%
  left_join(list_iso3_country) %>%
  dplyr::select(iso3,country,year,dr,value,lci,uci) %>%
  dplyr::rename(grp=dr,catast_pct=value,catast_pct_lo=lci,catast_pct_hi=uci) %>%
  mutate_at(c("catast_pct","catast_pct_lo","catast_pct_hi"), ~.*100)

# additional 3 countries (COL, NER, ZAF) that have standard country profile used in PCS book
f5.2.4_data_add <- read_excel(here::here('data/pcs/local/TBPCS_ALL_database_additional_v1.xlsx')) %>%
  subset((variable=="cc1"|variable=="cc2"|variable=="cc3")&
           quintile=="all"&sex=="all") %>%
  left_join(list_iso3_country) %>%
  dplyr::select(iso3,country,year,dr,value,lci,uci) %>%
  dplyr::rename(grp=dr,catast_pct=value,catast_pct_lo=lci,catast_pct_hi=uci) %>%
  mutate_at(c("catast_pct","catast_pct_lo","catast_pct_hi"), ~.*100)

f5.2.4_data <- rbind.data.frame(f5.2.4_data,f5.2.4_data_add)

# other countries that reported results of PCS to WHO
f5.2.4b_data <- read_excel(here::here('data/pcs/local/catast indicator summary 2024_v1.xlsx')) %>%
  select(-country) %>% left_join(list_iso3_country) %>%
  select(iso3, country, year,grp,catast_pct,catast_pct_lo,catast_pct_hi) %>%
  subset(iso3 %in% pcs_reporting) %>%
  mutate(grp=factor(grp,levels=c('all','ds','dr'))) %>%
  mutate(grp=factor(grp,labels=c('overall','TB (first-line treatment)','Drug-resistant TB'))) %>%
  filter(iso3!="MDA") %>%
  arrange(country)

f5.2.4_data <- f5.2.4_data %>%
  rbind.data.frame(.,f5.2.4b_data) %>%
  # replace 95%CI for UGA and MLI with ones without adjustment for sampling effects: agreed with MB in Sep-Oct 2022.
  mutate(catast_pct_lo = ifelse(iso3=="MLI"&grp=="Drug-resistant TB", 80.73638, catast_pct_lo),
         catast_pct_lo = ifelse(iso3=="UGA"&grp=="Drug-resistant TB", 92.01535, catast_pct_lo),
         catast_pct_hi = ifelse(iso3=="UGA"&grp=="Drug-resistant TB", 100.0, catast_pct_hi))

save(f5.2.4_data, file = here::here('data/pcs/pcs_catast_data.Rdata'))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.5 ----
# (Distribution of costs faced by TB patients and their households in 25 national surveys completed since 2016)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# from PCS book
f5.2.5_data <- read_excel(here::here('data/pcs/local/Fig3.3.1.cost drivers_v6_2022-06-14.xlsx')) %>%
  rename(cat=3) %>%
  mutate(value=value*100)

# for addtional 3 countries with standard country profile (COL, NER, ZAF)
f5.2.5_data_add <- read_excel(here::here('data/pcs/local/TBPCS_ALL_database_additional_v1.xlsx')) %>% #from additional 3 countries for Lancet Global Health
  subset((variable=="cat_med"|variable=="cat_nmed"|variable=="cat_indirect")&
           dr=="overall"&quintile=="all"&sex=="all"&sum_fun=="mean")  %>%
  dplyr::select(iso3,variable, value) %>%
  left_join(list_iso3_country,by = "iso3") %>%
  select(iso3,country,variable,value)

f5.2.5_data_add <- tidyr::spread(f5.2.5_data_add, key = variable, value = value)

f5.2.5_data_add <- f5.2.5_data_add %>%
  mutate(tot=cat_med+cat_nmed+cat_indirect)%>%
  mutate(p_med=cat_med/tot) %>%
  mutate(p_nmed=cat_nmed/tot) %>%
  mutate(p_indirect=cat_indirect/tot)

f5.2.5_data_add<- tidyr::gather(f5.2.5_data_add, key = variable, value = value, p_med,p_nmed,p_indirect) %>%
  dplyr::select(iso3,country,cat=variable, value=value) %>%
  mutate(cat=factor(cat,levels=c("p_indirect","p_nmed","p_med"))) %>%
  arrange(iso3)%>%
  mutate(value=value*100)

f5.2.5_data <- rbind.data.frame(f5.2.5_data,f5.2.5_data_add)

# from country reports
f5.2.5_data <- read_excel(here::here('data/pcs/local/catast cost breakdown summary 2024_v1.xlsx'))  %>%
  select(-country) %>% left_join(list_iso3_country) %>%
  filter(grp=="all") %>% select(-source,-grp) %>%
  arrange(pcost_med_pct) %>%
  mutate(country=fct_reorder(country,pcost_med_pct)) %>%
  select(iso3,country,pcost_med_pct,pcost_nmed_pct,pcost_indirect_pct) %>%
  subset(iso3 %in% pcs_reporting) %>%
  rename(p_med=3,p_nmed=4,p_indirect=5) %>%
  pivot_longer(p_med:p_indirect,names_to="cat") %>% rbind.data.frame(.,f5.2.5_data) %>%
  arrange(iso3)

save(f5.2.5_data, file = here::here('data/pcs/pcs_cost_driver_data.Rdata'))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.6 ----
# (Model-based estimates of cost faced by TB patients and their households in 135 low- and middle-income countries, WHO regions)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f5.2.6_data <- read_excel(here::here('data/pcs/local/lancet_gh.xlsx')) %>%
  left_join(who_region_shortnames, by = c("g_whoregion")) %>%
  mutate(entity = ifelse(g_whoregion=="Global","All LMICs",as.character(entity))) %>%
  mutate(entity = factor(entity, levels = c("All LMICs","Western Pacific Region","South-East Asia Region","European Region",
                                            "Eastern Mediterranean Region","Region of the Americas","African Region")))

save(f5.2.6_data, file = here::here('data/pcs/pcs_modelling_data.Rdata'))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: Lancet GH data (modelled estimates) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

df <- read_excel(here::here('data/pcs/local/modelled_cc_list of countries.xlsx')) %>%
  mutate(source = "modelled",
         year = 2021,
         grp = "overall") %>%
  select(source, iso3, country = Country,  year, grp, catast_pct = Overall, ci = `95% credible intervals`, category = `for app`)

extract_numbers <- function(ci) {
  numbers <- as.numeric(unlist(regmatches(ci, gregexpr("\\d+\\.\\d+", ci))))
  return(numbers)
}

df_ci <- do.call(rbind, lapply(df$ci, extract_numbers))
colnames(df_ci) <- c("catast_pct_lo", "catast_pct_hi")

df <- cbind(df, df_ci) %>%
  mutate(catast_pct = catast_pct*100) %>%
  filter(category == "send inquiry") %>%
  select(source, iso3, country,  year, grp, catast_pct, catast_pct_lo, catast_pct_hi) %>%
  filter(iso3 != "RUS" & iso3 != "CUB" & iso3 != "UZB" )

write.csv(df, here::here(paste0('data/pcs/model_cc.csv')), row.names = FALSE)

