#' # Download the IMF World Economic Outlook for the TB finance section
#'
#' Official data source: https://www.imf.org/en/Publications/WEO/
#' We use the indirect data source "DBnomics" to use API to download the dataset: https://db.nomics.world/
#' IMF-WEO is release by IMF twice a year (April and October) and we rely on the April version for Global TB Report.
#' After the release of a new WEO dataset by IMF, it takes 2-3 weeks until the new dataset becomes available in DBnomics.
#' Therefore, please run this code around end of April or begining of May every year - and DO NOT RUN again especially after September!
#'
#' Dependences:
#'  - libraries dplyr, zoo, rdbnomics, tidyverse
#'
#' Output:
#'  - imfweo.rda in the ~/data/imf folder


# install.packages("rdbnomics")
library(rdbnomics)
library(dplyr)
library(zoo)
library(tidyverse)

# Set the location of the output folder
imf_folder <- here::here("data/imf")

report_year <- 2024

# show queries
weo_dataset <- paste0("WEO:",report_year,"-04") # to download and use WEO database as of April, in the report year.

weolist <- rdb_dimensions(provider_code = "IMF", dataset_code = weo_dataset)
weolist$IMF$`WEO:2024-04`

# API to extract necessary variables of IMF-WEO from DBnomics
NGDP_D <- rdb("IMF", paste0("WEO:",report_year,"-04"), query = "Gross domestic product, deflator")
NGDPRPC <- rdb("IMF", paste0("WEO:",report_year,"-04"), query = "Gross domestic product per capita, constant prices")
NGDPPC <- rdb("IMF", paste0("WEO:",report_year,"-04"), query = "Gross domestic product per capita, current prices")
PPPEX <- rdb("IMF", paste0("WEO:",report_year,"-04"), query = "Implied PPP conversion rate")
GGR_NGDP <- rdb("IMF", paste0("WEO:",report_year,"-04"), query = "General government revenue")
GGX_NGDP <- rdb("IMF", paste0("WEO:",report_year,"-04"), query = "General government total expenditure")
GGXWDG_NGDP <- rdb("IMF", paste0("WEO:",report_year,"-04"), query = "General government gross debt")
BCA_NGDPD <- rdb("IMF", paste0("WEO:",report_year,"-04"), query = "Current account balance")
PPPSH <- rdb("IMF", paste0("WEO:",report_year,"-04"), query = "Gross domestic product based on purchasing-power-parity (PPP) share of world total")

# produce a wide dataframe
# weo_long <- rbind.data.frame(NGDP_D,NGDPRPC,NGDPPC,PPPEX,GGX_NGDP,GGXWDG_NGDP,BCA_NGDPD) %>%
#   select(dataset=2,year=5,iso3=14,country=16,varcode=15,variable=17,value)

weo <- rbind.data.frame(NGDP_D,NGDPRPC,NGDPPC,PPPEX,GGX_NGDP,GGXWDG_NGDP,BCA_NGDPD, PPPSH) %>%
  select(dataset=2,year=5,iso3=14,country=16,varcode=15,value) %>%
  pivot_wider(names_from = varcode, values_from = value) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year <= report_year)

weo <- weo %>%
  group_by(iso3) %>%
  mutate(deflator = NGDP_D / NGDP_D[which.max(year) - which.min(year) + 1])


weo <- weo %>%
  mutate(imf_deflator_us = ifelse(iso3 == "USA", NGDP_D, NA_real_)) %>%
  arrange(year, imf_deflator_us) %>%
  group_by(year) %>%
  mutate(imf_deflator_us = zoo::na.locf(imf_deflator_us)) %>%
  ungroup() %>%
  arrange(iso3, year)

weo <- weo %>%
  group_by(iso3) %>%
  mutate(deflator_us = imf_deflator_us / imf_deflator_us[which.max(year) - which.min(year) + 1]) 

# Save as .rda files
save(weo, file = paste0(imf_folder, "/imfweo", ".rda"))
