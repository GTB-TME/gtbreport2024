library(rsdmx)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(here)

# Establish the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2024
# And the latest year for which OECD data are being displayed in graphics. Always two years older than report year
latest_year <- report_year - 2

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))

# Set the location of the output folder
oecd_folder <- here::here("data/oecd")
gf_folder <- here::here("data/gf")
kff_folder <- here::here("data/kff")

# download country list
grpmbr <- load_gtb("grpmbr")

list_iso3_country <-
  grpmbr %>% filter(group_type == 'g_whoregion') %>%
  select(iso3,country,g_whoregion=group_name)

list_iso3_country_income <-
  grpmbr %>% filter(group_type == 'g_income') %>%
  select(iso3,g_income=group_name)

list_hbcs <-
  grpmbr %>%
  filter(group_type == "g_hb_tb") %>%
  select(iso3,hbc = group_name)

list_iso3_country <- list_iso3_country %>%
  left_join(list_iso3_country_income, by = c("iso3")) %>%
  left_join(list_hbcs, by = c("iso3")) %>%
  mutate(hbc = ifelse(is.na(hbc),0,hbc))

# Load OECD data
# the snapshot data are produced by ./import/save_oecd.R around April/May.
load(here::here(paste0(oecd_folder,"/oecd",".rda")))
load(here::here(paste0(oecd_folder,"/oda",".rda")))
load(here::here(paste0(oecd_folder,"/oda_country",".rda")))

# US contribution reported in KFF
us_kff_2023 <- read.csv(here::here(kff_folder,"./data-y0Him.csv")) %>%
  rename(year = 1, value = 2) %>%
  filter(year < 2015)

us_kff <- read.csv(here::here(kff_folder,"./data-sLGaY.csv")) %>%
  rename(year = 1, value = 2) 

us_kff <- rbind(us_kff_2023,us_kff)

# GF for year 2023-25
gf_23to25 <- read.csv(here::here(gf_folder,"./pledges-contributions-donor_20232025.csv")) %>% 
  rename(donor = 1, pled = 2, cont = 3) 

gf_23to25 <- gf_23to25 %>%
  summarise(total_cont = sum(cont, na.rm = TRUE)) %>%
  cbind(gf_23to25) %>%
  select(donor, pled, cont, total_cont) %>%
  mutate(pcnt = cont/total_cont)

# GF for year 2020-22
gf_20to22 <- read.csv(here::here(gf_folder,"./pledges-contributions-donor_20202022.csv")) %>% 
  rename(donor = 1, pled = 2, cont = 3) 

gf_20to22 <- gf_20to22 %>%
  summarise(total_cont = sum(cont, na.rm = TRUE)) %>%
  cbind(gf_20to22) %>%
  select(donor, pled, cont, total_cont) %>%
  mutate(pcnt = cont/total_cont)

# GF for year 2017-19
gf_17to19 <- read.csv(here::here(gf_folder,"./pledges-contributions-donor_20172019.csv")) %>% 
  rename(donor = 1, pled = 2, cont = 3)

gf_17to19 <- gf_17to19 %>%
  summarise(total_cont = sum(cont, na.rm = TRUE)) %>%
  cbind(gf_17to19) %>%
  select(donor, pled, cont, total_cont) %>%
  mutate(pcnt = cont/total_cont)

# GF for year 2014-16
gf_14to16 <- read.csv(here::here(gf_folder,"./pledges-contributions-donor_20142016.csv")) %>% 
  rename(donor = 1, pled = 2, cont = 3) 

gf_14to16 <- gf_14to16 %>%
  summarise(total_cont = sum(cont, na.rm = TRUE)) %>%
  cbind(gf_14to16) %>%
  select(donor, pled, cont, total_cont) %>%
  mutate(pcnt = cont/total_cont)

# GF for year 2011-13
gf_11to13 <- read.csv(here::here(gf_folder,"./pledges-contributions-donor_20112013.csv")) %>% 
  rename(donor = 1, pled = 2, cont = 3) 

gf_11to13 <- gf_11to13 %>%
  summarise(total_cont = sum(cont, na.rm = TRUE)) %>%
  cbind(gf_11to13) %>%
  select(donor, pled, cont, total_cont) %>%
  mutate(pcnt = cont/total_cont)

# merge GF data
gf <- rbind.data.frame(gf_23to25 %>% mutate(year = 2023), 
                       gf_20to22 %>% mutate(year = 2022), 
                       gf_20to22 %>% mutate(year = 2021),
                       gf_20to22 %>% mutate(year = 2020),
                       gf_17to19 %>% mutate(year = 2019),
                       gf_17to19 %>% mutate(year = 2018),
                       gf_17to19 %>% mutate(year = 2017),
                       gf_14to16 %>% mutate(year = 2016),
                       gf_14to16 %>% mutate(year = 2015),
                       gf_14to16 %>% mutate(year = 2014),
                       gf_11to13 %>% mutate(year = 2013)) %>%
  arrange(donor)


#---------------------------
# fig 1
#---------------------------
Fig1 <- oecd %>%
  pivot_wider(names_from = donor, values_from = value) %>%
  rowwise() %>% mutate(oth = sum(total,usa*(-1),gf*(-1), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(usa = replace_na(usa, 0)) %>%
  select(recipient:year, total, usa, gf, oth, country:g_whoregion) %>%
  pivot_longer(cols = total:oth, values_to = "value", names_to = "donor")

iso3 <- "^[A-Z]{3}$"

Fig1_txt <- Fig1 %>%
  select(recipient, country, g_income) %>%
  distinct() %>%
  filter(grepl(iso3, recipient),recipient!="LDC") %>%
  arrange(recipient) %>% nrow()

Fig1_lmic_txt <- Fig1 %>%
  select(recipient, country, g_income) %>%
  distinct() %>%
  filter(grepl(iso3, recipient),recipient!="LDC") %>%
  arrange(recipient) %>%
  filter(g_income!="HIC") %>% nrow()

plw <- Fig1 %>% filter(recipient=="SYC")

# latest year
all_donation <- Fig1 %>% 
  filter(year == latest_year & donor == "total" & recipient == "DPGC") %>% 
  summarise(sum(value)) %>% unlist() %>% round(0)

gf_donation <- Fig1 %>% 
  filter(year == latest_year & donor == "gf" & recipient == "DPGC") %>% 
  summarise(sum(value)) %>% unlist() %>% round(0)

usa_donation <- Fig1 %>% 
  filter(year == latest_year & donor == "usa" & recipient == "DPGC") %>% 
  summarise(sum(value)) %>% unlist() %>% round(0)

oth_donation <- Fig1 %>% 
  filter(year == latest_year & donor == "oth" & recipient == "DPGC") %>% 
  summarise(sum(value)) %>% unlist() %>% round(0)

# all year
all_donation_all_year <- Fig1 %>% 
  filter(donor == "total" & recipient == "DPGC") %>% 
  summarise(sum(value)) %>% unlist() %>% round(0)

gf_donation_all_year <- Fig1 %>% 
  filter(donor == "gf" & recipient == "DPGC") %>% 
  summarise(sum(value)) %>% unlist() %>% round(0)

gf_contribution <- gf_donation_all_year/all_donation_all_year*100

Fig1 <- Fig1 %>%
  mutate(g_whoregion = ifelse(recipient == "DPGC", "LMICs", g_whoregion)) %>%
  filter(year >= 2013, !is.na(g_whoregion)) %>% 
  # Prepare faceting variable, in factor form
  mutate(g_whoregion = factor(g_whoregion,
                              levels = c(
                                "LMICs", "AFR","AMR", "SEA",
                                "EUR","EMR","WPR"),
                              labels = c( "Developing countriesᵃ",   #\u1d43 for superscript a
                                          "WHO African Region","WHO Region of\nthe Americas", 
                                          "WHO South-East Asia\nRegion","WHO European Region", 
                                          "WHO Eastern Mediterranean\nRegion","WHO Western Pacific\nRegion" ))) %>% 
  #Prepare series variable in factor format, ordered
  mutate(donor = factor(donor,
                        levels = c("total","gf","usa","oth"),
                        labels = c("Total","Global Fund","United States of America","Other"))) %>% 
  group_by(year, donor, g_whoregion) %>% 
  summarise_at(.vars = "value", sum, na.rm = TRUE) %>% 
  ungroup() 

#---------------------------
# fig 2
#---------------------------
Fig2 <- us_kff # only to rename dataframe


#---------------------------
# fig 3
#---------------------------
# overall since 2013
## summarize contributions via GF
oecd_gf <- oecd %>%
    mutate(year = as.numeric(year)) %>%
    filter(year >= 2013 & year <= report_year-1 & donor == "gf" & recipient == "DPGC") 

# reallocate Debt2 funding to country contributions
# gf %>%
#   filter(str_detect(donor, 'Debt2')) 

# gf <- gf %>%
#   mutate(donor= ifelse((str_detect(donor, 'Debt2') & str_detect(donor, 'Germany')), 'Germany', 
#                        ifelse((str_detect(donor, 'Debt2') & str_detect(donor, 'Spain')), 'Spain', 
#                               ifelse((str_detect(donor, 'Debt2') & str_detect(donor, 'Australia')), 'Australia', donor))))

oecd_gf <- gf %>%
  right_join(select(oecd_gf, year,value), by = c("year")) %>%
  mutate(gf_cont = value * pcnt)

Fig3_gf <- oecd_gf %>%
  select(donor, gf_cont) %>%
  group_by(donor) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  arrange(desc(gf_cont)) %>%
  ungroup()

## summarize ODA bilateral contributions
### by donor country, by year
oda_tb <- oda_country %>%
  filter(year >= 2013 & year <= report_year-1 & sector == 12263 & donor != "ALLD") 

### total TB funding, by year
oda_tb_trend <- oda_country %>%
  filter(year >= 2013 & year <= report_year-1 & sector == 12263 & donor == "ALLD") %>%
  select(year, tb_total = value)

### grand total since 2013 until report_year - 2
oda_tb_total <- oda_country %>%
  filter(year >= 2013 & year <= report_year-1 & sector == 12263 & donor == "ALLD") %>%
  select(donor,value) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) 

Fig3_oda <- oda_tb %>%
  select(donor, country, oda_cont = value) %>%
  group_by(donor,country) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  arrange(desc(oda_cont)) %>%
  mutate(country = ifelse(country == "Republic of Korea", "Korea (Republic)", country),
         country = ifelse(country == "United States of America", "United States", country),
         country = ifelse(country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom", country),
         country = ifelse(country == "Netherlands (Kingdom of the)", "Netherlands", country)#,
         # country = ifelse(donor == "9OTH012", "Global Fund", country),
         # country = ifelse(donor == "4EU001", "EU institutions", country),
         # country = ifelse(donor == "1UN026", "World Health Organization", country),
         # country = ifelse(donor == "5WB0", "World Bank", country)
         ) %>%
  ungroup() %>%
  select(donor = country, oda_cont)
  

## merge two datasets
Fig3_all <- Fig3_gf %>%
  full_join(Fig3_oda, by = c("donor"))

## calculate total contributions by entity
Fig3_all <- Fig3_all %>%
  rowwise() %>%
  mutate(entity_total = sum(gf_cont, oda_cont, na.rm = TRUE)) %>%
  arrange(desc(entity_total)) %>%
  ungroup()

entity_grand_total <- Fig3_all %>%
  summarise(entity_total = sum(entity_total, na.rm = TRUE)) 

other_total = oda_tb_total$value - entity_grand_total$entity_total

## calculate overall contributions from all entities
Fig3_all <- Fig3_all %>%
  mutate(grand_total = oda_tb_total$value) %>%
  mutate(pcnt = entity_total/grand_total * 100) 

Fig3_top10 <- Fig3_all %>%
  filter(!is.na(oda_cont)) %>%
  top_n(10)

Fig3_other <- Fig3_all %>%
  filter(!donor %in% Fig3_top10$donor) 

Fig3_other <- Fig3_other %>%
  add_row(donor = "Other reporting entities", entity_total = other_total, grand_total = oda_tb_total$value) %>%
  mutate(pcnt = entity_total/grand_total * 100) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

Fig3 <- plyr::rbind.fill(Fig3_top10,Fig3_other) %>%
  mutate(donor = ifelse(is.na(donor),"Other entities",donor),
         grand_total = ifelse(grand_total != oda_tb_total$value, oda_tb_total$value, grand_total),
         gf_cont = ifelse(donor == "Other entities", NA, gf_cont),
         oda_cont = ifelse(donor == "Other entities", NA, oda_cont)) 

writexl::write_xlsx(Fig3, here::here(paste0("./report/local/oecd_fig3_data",report_year,".xlsx")))

Fig3 <- Fig3 %>%
  mutate(subgroup = ifelse(donor=="United States",2,
                           ifelse(donor=="Other entities",1,3)),
         donor = ifelse(donor == "United States", "United States of America", donor))

Fig3 <- Fig3 %>%
  mutate(donor = factor(donor, levels = c("United States of America","United Kingdom","France",
                                          "Germany","Japan","Canada",
                                          "Australia","Sweden","Norway",
                                          "Netherlands","Other entities")),
         text_col = "white",
         fill_col = ifelse(donor=="United States of America","#00205C", "#A6228C" 
         ))

# US contribution trend for 2013-2021
## summarize contributions via GF
us_oda <- oecd %>%
  filter(year >= 2013 & year <= report_year-1 & donor == "usa" & recipient == "DPGC") %>%
  select(year, oda_cont = value)

us_trend <- oecd_gf %>%
  filter(donor == "United States") %>%
  right_join(us_oda, by = c("year")) %>%
  select(donor, year, pled:total_cont, us_pcnt_gf = pcnt, gf_annual = value, us_gf_cont = gf_cont, us_oda_cont = oda_cont) %>%
  mutate(us_cont_total = us_gf_cont + us_oda_cont) %>%
  right_join(oda_tb_trend, by = c("year")) %>%
  mutate(us_pcnt = us_cont_total/tb_total*100) %>%
  arrange(year)

writexl::write_xlsx(us_trend, here::here(paste0("./report/local/oecd_us_trend_data",report_year,".xlsx"))) # share this with USAID as necessary.

#---------------------------
# fig 4
#---------------------------
Fig4 <- oda %>%
  filter(year >= 2013 & year <= report_year-1 & donor == "ALLD" & recipient == "DPGC") %>%
  filter(sector == 12263 | # TB
           sector == 13040 | # HIV/STI
           sector == 12262 #  Malaria
  ) %>%
  mutate(value = value/1e3L)%>%
  select(sector, year, value)

Fig4_total <- oda %>%
  filter(year >= 2013 & year <= report_year-1 & donor == "ALLD" & recipient == "DPGC") %>%
  filter(sector == 120 | # Health
           sector == 130  # pop policy
  ) %>%
  group_by(year) %>%
  summarise_at(.vars = "value", sum, na.rm = TRUE) %>% 
  mutate(value = value/1e3L) %>%
  ungroup() %>%
  mutate(sector = 100)


Fig4 <- Fig4 %>%
  rbind(Fig4_total) 

tb_pct <- Fig4 %>%
  filter(year == latest_year, (sector == 12263 | sector == 100)) %>%
  pivot_wider(names_from = sector, values_from = value) %>%
  rename(tb=2, total=3) %>%
  mutate(pct = tb/total*100)
  
Fig4 <- Fig4 %>%
  group_by(year) %>% 
  mutate(sector = factor(sector,
                            labels = c("Total health and population programmes","Malaria control","TB control", "STD control including HIV"))) %>%
  mutate(sector = factor(sector,
                            levels = c("TB control","STD control including HIV","Malaria control", "Total health and population programmes"))) %>%
  ungroup()

