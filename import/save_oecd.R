#' # Download the OECD stats for the FT international donor funding
#'
#' data source: https://stats.oecd.org/
#' !!!!!!!!!! RUN this file in April-May, when OECD release the new data !!!!!!!!!!!!!
#' !!!!!!!!!! DO NOT RUN this file after that (OECD will release new data around September-October, but GTBR does not rely on it) !!!!!!!!!!!!!
#'
#' Dependences:
#'  - libraries dplyr, here, rsdmx and tidyr, as well as data using load_gtb function.
#'
#' Output:
#'  - oecd.rda in the ~/data/oecd folder
#'  - oda.rda in the ~/data/oecd folder

library(rsdmx)
library(dplyr)
library(tidyr)
library(here)

report_year <- 2024

# load functions
source(here("import/load_gtb.R"))

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

# Set the location of the output folder
oecd_folder <- here::here("data/oecd")

# Bilateral and multilateral contributions reported to OECD
# oecd_url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/20005+1312+12+302.10100+10045+228+271+238+252+253+259+266+273+279+278+285+231+232+235+287+240+243+244+251+255+260+272+283+740+625+573+580+10046+85+130+142+136+139+233+274+248+282+288+265+225+229+234+268+280+249+236+230+247+241+256+261+269+349+352+342+351+364+428+728+738+745+753+755+765+769+666+630+645+614+635+660+665+640+615+617+540+550+862+866+854+836+860+880+10047+71+86+64+57+93+65+66+63+55+133+257+245+239+227+275+218+338+378+340+381+354+383+384+336+347+358+366+425+431+437+440+446+451+454+457+730+751+764+610+611+612+613+655+616+543+549+555+832+859+870+872.12263.100.100.D.112.100/all?startTime=2007&endTime=",
#                    report_year-1)
# oecd <- as.data.frame(readSDMX(oecd_url)) 
# 
# oecd <- oecd %>% 
#   select(donor = 1, recipient = 2, year = obsTime, value = obsValue) %>%
#   mutate(year = as.numeric(year)) %>%
#   mutate(donor = ifelse(donor == 20005, "total",
#                         ifelse(donor == 302, "usa",
#                                ifelse(donor == 1312, "gf", "uk"))))

# Bilateral and multilateral contributions reported to OECD (NEW API from July 2024!)
oecd_url <- paste0("https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.0/9OTH012+GBR+USA+ALLD..12263.100._T._T.D.Q._T..?startPeriod=2007")
oecd <- as.data.frame(readSDMX(oecd_url)) 

oecd <- oecd %>% 
  select(donor = 1, recipient = 2, year = obsTime, value = obsValue) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(donor = ifelse(donor == "ALLD", "total",
                        ifelse(donor == "USA", "usa",
                               ifelse(donor == "9OTH012", "gf", "uk"))))


# recipient code
recip <- read.csv(here::here(oecd_folder,"./recipient_code.csv")) %>%
  select(country = 2, recipient = 4) %>%
  mutate(recipient = as.character(recipient))

oecd <- oecd %>%
  left_join(recip, by = c("recipient")) %>%
  left_join(select(list_iso3_country, recipient = iso3, g_income, g_whoregion), by = c("recipient")) 


# ODA bilateral contributions only: takes around 2-3 min to download this data.
# oda_url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/20005+801+1+2+301+68+3+18+4+5+40+75+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302.10100+71+86+64+57+93+65+66+63+55+85+89+130+142+133+136+139+228+233+274+271+238+248+252+253+257+259+266+273+279+278+282+285+288+265+225+229+231+232+234+235+245+239+268+227+280+249+275+218+236+287+230+247+240+241+243+244+251+255+256+260+261+269+272+283+298+338+378+340+381+349+354+383+384+352+336+342+347+351+358+364+366+425+428+431+437+440+446+451+454+457+498+728+730+740+738+745+751+753+755+764+765+769+625+610+611+666+630+612+645+613+614+655+635+660+665+640+615+616+617+540+543+549+555+573+550+580+798+832+862+866+854+836+859+860+880+870+872+889+228+233+274+271+238+252+253+259+266+273+279+278+282+285+288+225+231+232+235+268+249+236+287+240+243+244+251+255+256+260+269+272+283+349+728+745+765+625+666+630+635+660+580+866+854+836+872+265+740+57+93+85+142+136+139+248+229+234+280+230+247+241+261+342+347+351+364+428+738+753+755+769+610+612+645+614+665+640+615+617+549+573+550+862+860+71+86+64+65+66+63+55+130+133+257+245+239+227+275+218+338+378+340+381+354+383+384+352+336+358+366+425+431+437+440+446+451+454+457+730+751+764+611+613+655+616+540+543+555+832+859+880+870+228+233+271+238+252+253+259+266+273+278+282+285+288+229+231+232+234+235+268+236+287+247+240+241+243+244+251+255+256+260+269+272+283+349+351+364+428+446+625+93+66+228+238+253+266+279+285+288+265+231+232+227+280+249+287+255+260+428+451+745+753+625+610+611+630+613+614+660+615+616+617+233+257+268+230+244+338+378+340+381+349+354+383+384+352+446+457+765+655+832+862+866+854+836+859+860+880+870+872+133+228+233+274+271+238+248+252+259+273+279+278+282+285+288+265+225+229+231+232+234+235+245+280+249+287+247+240+243+244+251+255+256+260+261+272+283+349+347+351+364+728+740+745+625+666+635+665+615+540+543+573+550+580+862+866+10045+228+271+238+252+253+259+266+273+279+278+285+231+232+235+287+240+243+244+251+255+260+272+283+740+625+573+580+10046+85+130+142+136+139+233+274+248+282+288+265+225+229+234+268+280+249+236+230+247+241+256+261+269+349+352+342+351+364+428+728+738+745+753+755+765+769+666+630+645+614+635+660+665+640+615+617+540+550+862+866+854+836+860+880+10047+71+86+64+57+93+65+66+63+55+133+257+245+239+227+275+218+338+378+340+381+354+383+384+336+347+358+366+425+431+437+440+446+451+454+457+730+751+764+610+611+612+613+655+616+543+549+555+832+859+870+872+10049+258+276+376+385+463+831+856+868+71+86+93+85+610+611+612+613+614+615+616+617+71+86+93+85+610+611+612+613+614+615+616+617+228+233+274+271+238+248+252+253+257+259+266+273+278+282+285+288+265+225+229+231+232+234+235+245+239+268+227+280+249+275+236+287+230+247+240+241+243+244+251+255+256+260+261+269+272+283+378+340+381+349+354+383+384+352+446+457+832+862+866+854+836+880+870+872+232+287+230+240+244+255+256+260+269.120+12262+12263+130+13040.100.100.D.112.100/all?startTime=2001&endTime=",
#                   report_year-1)
# 
# oda <- as.data.frame(readSDMX(oda_url))
# 
# oda <- oda %>% 
#   select(donor = 1, recipient = 2, sector = 3, year = obsTime, value = obsValue) %>%
#   mutate(year = as.numeric(year)) 
# 
# # recipient code
# donor <- read.csv(here::here(oecd_folder,"./donor_code.csv")) %>%
#   select(donor = 1, donor_country = 2) %>%
#   mutate(donor = as.character(donor))
# 
# oda <- oda %>%
#   left_join(donor, by = c("donor")) 

oda_url <- paste0("https://sdmx.oecd.org/dcd-public/rest/data/OECD.DCD.FSD,DSD_CRS@DF_CRS,1.0/4EU001+9OTH024+9OTH023+9OTH021+9OTH020+9OTH019+9OTH018+9OTH017+9OTH016+9OTH015+9OTH013+9OTH012+9OTH011+9OTH009+9OTH006+9OTH005+9OTH004+9OTH003+9OTH002+9OTH001+5WBG002+5WB0+1UN029+1UN028+1UN027+1UN026+1UN025+1UN024+1UN022+1UN021+1UN020+1UN018+1UN019+1UN017+1UN016+1UN015+1UN013+1UN014+1UN012+1UN011+1UN010+1UN008+1UN007+1UN006+1UN005+1UN004+1UN003+1UN002+1UN001+5RDB011+5RDB009+5RDB008+5RDB007+5RDB006+5RDB004+5RDB005+5RDB003+5RDB002+5RDB001+5IDB0+5ASDB0+5AFDB0+5IMF0+ARE+TUR+TLS+THA+SAU+ROU+QAT+MCO+MLT+LIE+KWT+LVA+KAZ+ISR+CYP+HRV+TWN+BGR+AZE+GBR+USA+CHE+ESP+SWE+SVN+SVK+PRT+POL+NOR+NZL+NLD+LUX+LTU+KOR+JPN+ITA+IRL+ISL+HUN+GRC+DEU+FIN+FRA+DNK+EST+CZE+CAN+BEL+AUT+AUS+ALLD.DPGC.13040+12262+12263+130+120.100._T._T.D.Q._T..?startPeriod=2013")

oda <- as.data.frame(readSDMX(oda_url))

oda <- oda %>% 
  select(donor = 1, recipient = 2, sector = 3, year = obsTime, value = obsValue) %>%
  mutate(year = as.numeric(year)) 

oda <- oda %>%
  left_join(select(list_iso3_country, donor = iso3, country), by = c("donor")) 

oda_country <- oda %>%
  filter(!is.na(country)|donor=="ALLD")
  
# recipient code
# donor <- read.csv(here::here(oecd_folder,"./donor_code.csv")) %>%
#   select(donor = 1, donor_country = 2) %>%
#   mutate(donor = as.character(donor))


# Save as .rda files
save(oecd, file = paste0(oecd_folder, "/oecd", ".rda"))
save(oda, file = paste0(oecd_folder, "/oda", ".rda"))
save(oda_country, file = paste0(oecd_folder, "/oda_country", ".rda"))

