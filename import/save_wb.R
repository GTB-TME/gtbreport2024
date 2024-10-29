#' # Download the World Bank open data for the TB finance section
#'
#' data source: https://data.worldbank.org/
#'
#' Dependences:
#'  - libraries dplyr and wbstats
#'
#' Output:
#'  - wb.rda in the ~/data/wb folder


# install.packages("wbstats")
library(wbstats)
library(dplyr)

# Set the location of the output folder
wb_folder <- here::here("data/wb")

report_year <- 2024

# Get the up-to-date list of indicators so that the two not in wb_cachelist are not excluded
latest_cache <- wb_cache("en")

# Compile the list of indicators to download:
var_list <- c(
  "FP.CPI.TOTL", # Consumer price index (2010 = 100)
  "NY.GDP.PCAP.KD", # GDP per capita (constant 2015 US$)
  "NY.GDP.PCAP.KN", # GDP per capita (constant LCU)
  "NY.GDP.PCAP.CD", # GDP per capita (current US$)
  "NY.GDP.DEFL.ZS", # GDP deflator (base years vary)
  "NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
  "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2017 international $)
  "NY.GDP.PCAP.PP.CD", # GDP per capita, PPP (current int$)
  "NY.GDP.MKTP.PP.KD", # GDP, PPP (constant 2017 int$)
  "NY.GDP.MKTP.PP.CD", # GDP, PPP (current international $)
  "SI.POV.GINI", # GINI index (World Bank estimate)
  "NY.GDP.DEFL.KD.ZG", # Inflation, GDP deflator (annual %)
  "FP.CPI.TOTL.ZG", # Inflation, consumer prices (annual %)
  "PA.NUS.FCRF", # Official exchange rate (LCU per US$, period average)
  "PE.NUS.FCAE", # Official exchange rate (LCU per US$, end period)
  "PA.NUS.PPP", # PPP conversion factor, GDP (LCU per int$)
  "PA.NUS.PRVT.PP", # PPP conversion factor, private consum(LCU per int$)
  "SP.POP.TOTL", # Population, total
  "PA.NUS.PPPC.RF", # Price level ratio of PPP conversion factor (GDP) to market exchange rate
  "SP.URB.TOTL.IN.ZS" # Urban population (% of total popn)
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# download the data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

wb <-  wb_data(indicator = var_list,
                                country = "countries_only",
                                start_date = 2000,
                                end_date = report_year - 1,
                                return_wide = FALSE,
                                cache = latest_cache,
                                lang = "en")

# Save as .rda files
save(wb, file = paste0(wb_folder, "/wb", ".rda"))
