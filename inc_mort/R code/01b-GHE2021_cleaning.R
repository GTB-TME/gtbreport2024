#' ---
#' title: GHE data: data management of the new GHE 2021 estimates
#' author: Mathieu Bastard
#' date: 19/06/2024




# This script was developed to perform data cleaning on GHE 2021 csv files
# Data received from WHO DDI on 19th June 2024
#
# input: csv file estimated number of deaths by country, year and age groups
#        
# Process: Data cleaning and reshaping
# 
# Output: 1 Rda files ghe.rda




#'
#' Process GHE 2021 file
#'

rm(list=ls())

# Load libraries and data
library(data.table)
library(imputeTS)
library(haven) # read_dta
library(readxl)
library(stringr)
library(here)

#Function to import db from GTB databases
source(here("import/load_gtb.R"))



ghe <-  fread('data/mortality/GHE2021_all-cause.csv')
ghe=as.data.table(ghe)
setkey(ghe,iso3)

# Keep only country data
cty <- load_gtb("cty",convert_dots = FALSE)
ghe=merge(ghe,cty,by="iso3")

# Remove duplicate counts of  <1years : Age==0 removed as age==0 = age==0.1 (<1month) +age==0.11 (1-11 months)
ghe=ghe[age!=0,]

# Keep only all sex: sex==3
ghe=ghe[sex==3,]

# Calculate total per year
ghe2=aggregate(cbind(dths=ghe$dths,dths.lo=ghe$dths.low, dths.hi=ghe$dths.up), by=list(iso3=ghe$iso3,year=ghe$year), FUN=sum)
ghe2=as.data.table(ghe2)
setkey(ghe2,iso3)
dim(ghe2)

ghe2021=copy(ghe2)

attr(ghe2021, "timestamp") <- Sys.Date() #set date
save(ghe2021, file = 'inc_mort/analysis/ghe2021.rda')








