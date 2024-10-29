#' ---
#' title: IHME data, Update GBD 2021, accessed June 2024
#' author: Mathieu Bastard
#' date: 13/06/2024




# This script was developed to perform data cleaning on IHME csv files
# IHME estimates were updated in 2024 as per GBD 2021
#
# input: IHME files for all causes TB deaths per country and year
#        IHME files for TB deaths per country and year
#        
# Process: Data cleaning and reshaping
# 
# Output: 2 Rda files ihmeall and ihmetb




#'
#' Process IHME files
#'

### Total deaths per country year all causes

library(data.table)

ihme <-  fread('data/ihme/IHME-GBD_2021_DATA-7fd07b1e-1.csv', encoding = 'UTF-8')  # data
cty <- load_gtb("cty",convert_dots = FALSE)




# Keep only metric_name==Number

ihme2=ihme2[metric_name=="Number",]

# Merge iso3

ihme2 <-
  merge(ihme,
        cty[, .(iso3, country)],
        by.x = 'location_name',
        by.y = 'country',
        all.x = TRUE)

# Check iso3 completness and correct if needed
ihme2[is.na(iso3), .(location_name[1]), by = iso3]
ihme2[location_name == 'Netherlands', iso3 := 'NLD']
ihme2[location_name == 'Bolivia', iso3 := 'BOL']
ihme2[location_name == 'Brunei', iso3 := 'BRN']
ihme2[location_name == 'Cape Verde', iso3 := 'CPV']
ihme2[location_name == "Cote d'Ivoire", iso3 := 'CIV']
ihme2[location_name == 'Czech Republic', iso3 := 'CZE']
ihme2[location_name == 'Federated States of Micronesia', iso3 := 'FSM']
ihme2[location_name == 'Iran', iso3 := 'IRN']
ihme2[location_name == 'Laos', iso3 := 'LAO']
ihme2[location_name == 'Macedonia', iso3 := 'MKD']
ihme2[location_name == 'Moldova', iso3 := 'MDA']
ihme2[location_name == 'North Korea', iso3 := 'PRK']
ihme2[location_name == 'Palestine', iso3 := 'PSE']
ihme2[location_name == 'South Korea', iso3 := 'KOR']
ihme2[location_name == 'Swaziland', iso3 := 'SWZ']
ihme2[location_name == 'Syria', iso3 := 'SYR']
ihme2[location_name == 'Virgin Islands, U.S.', iso3 := 'VGB']
ihme2[location_name == 'Vietnam', iso3 := 'VNM']
ihme2[location_name == 'Venezuela', iso3 := 'VEN']
ihme2[location_name == 'United States', iso3 := 'USA']
ihme2[location_name == 'United Kingdom', iso3 := 'GBR']
ihme2[location_name == 'The Gambia', iso3 := 'GMB']
ihme2[location_name == 'The Bahamas', iso3 := 'BHS']
ihme2[location_name == 'Tanzania', iso3 := 'TZA']
ihme2[location_name == 'Taiwan (Province of China)', iso3 := 'TWN']
ihme2[location_name == 'United States Virgin Islands', iso3 := 'TWN']

ihme2[is.na(iso3), .(location_name), by = iso3]

ihme3 <- merge(ihme2, cty[, .(iso3, country)], by = 'iso3', all.x = TRUE)
ihme3[is.na(country), unique(location_name)]


ihme4=ihme3[!is.na(country), ]

#Drop year before 2000
ihme5=ihme4[year>=2000, .(iso3,year,country,measure_name,sex_name,age_name,cause_name,metric_name,val,upper,lower) ]

ihmeall <- copy(ihme5)
setkey(ihmeall, iso3, year)



### IHME TB deaths


ihmetb <-  fread('data/ihme/IHME-GBD_2021_DATA-1be99021-1.csv', encoding = 'UTF-8')  # data

# Keep only metric_name==Number

ihme2=ihmetb[metric_name=="Number",]

# Merge iso3

ihme2 <-
  merge(ihme2,
        cty[, .(iso3, country)],
        by.x = 'location_name',
        by.y = 'country',
        all.x = TRUE)

# Check iso3 completness and correct if needed
ihme2[is.na(iso3), .(location_name[1]), by = iso3]
ihme2[location_name == 'Netherlands', iso3 := 'NLD']
ihme2[location_name == 'Bolivia', iso3 := 'BOL']
ihme2[location_name == 'Brunei', iso3 := 'BRN']
ihme2[location_name == 'Cape Verde', iso3 := 'CPV']
ihme2[location_name == "Cote d'Ivoire", iso3 := 'CIV']
ihme2[location_name == 'Czech Republic', iso3 := 'CZE']
ihme2[location_name == 'Federated States of Micronesia', iso3 := 'FSM']
ihme2[location_name == 'Iran', iso3 := 'IRN']
ihme2[location_name == 'Laos', iso3 := 'LAO']
ihme2[location_name == 'Macedonia', iso3 := 'MKD']
ihme2[location_name == 'Moldova', iso3 := 'MDA']
ihme2[location_name == 'North Korea', iso3 := 'PRK']
ihme2[location_name == 'Palestine', iso3 := 'PSE']
ihme2[location_name == 'South Korea', iso3 := 'KOR']
ihme2[location_name == 'Swaziland', iso3 := 'SWZ']
ihme2[location_name == 'Syria', iso3 := 'SYR']
ihme2[location_name == 'Virgin Islands, U.S.', iso3 := 'VGB']
ihme2[location_name == 'Vietnam', iso3 := 'VNM']
ihme2[location_name == 'Venezuela', iso3 := 'VEN']
ihme2[location_name == 'United States', iso3 := 'USA']
ihme2[location_name == 'United Kingdom', iso3 := 'GBR']
ihme2[location_name == 'The Gambia', iso3 := 'GMB']
ihme2[location_name == 'The Bahamas', iso3 := 'BHS']
ihme2[location_name == 'Tanzania', iso3 := 'TZA']
ihme2[location_name == 'Taiwan (Province of China)', iso3 := 'TWN']
ihme2[location_name == 'United States Virgin Islands', iso3 := 'TWN']

ihme2[is.na(iso3), .(location_name), by = iso3]

ihme3 <- merge(ihme2, cty[, .(iso3, country)], by = 'iso3', all.x = TRUE)
ihme3[is.na(country), unique(location_name)]

ihme4=ihme3[!is.na(country), ]

#Drop year before 2000
ihme5=ihme4[year>=2000, .(iso3,year,country,measure_name,sex_name,age_name,cause_name,metric_name,val,upper,lower) ]

ihmetb <- copy(ihme5)
setkey(ihmetb, iso3, year)

#  save IHME datasets
#

attr(ihmeall, "timestamp") <- Sys.Date() #set date
save(ihmeall, file = 'inc_mort/analysis/ihmeall.rda')

attr(ihmetb, "timestamp") <- Sys.Date() #set date
save(ihmetb, file = 'inc_mort/analysis/ihmetb.rda')



#fwrite(vr, file = paste0('inc_mort/analysis/csv/vr_', Sys.Date(), '.csv'))
