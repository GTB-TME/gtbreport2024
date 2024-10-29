#' ---
#' title: Download the under-nutrition indicators from the WHO Global Health Observatory
#' author: Hazim Timimi
#' date: 2024-08-12
#' output:
#'    html_document:
#'      mode: selfcontained
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      number_sections: true
#'      theme: flatly
#'      highlight: zenburn
#'      df_print: paged
#'      code_folding: hide
#' ---

#' (Last updated: `r Sys.Date()`)
#'
#' # Download the under-nutrition indicators from the GHO country level
#' # We use 3 indicators for the different age groups published in GHO:
#' 
#' 1. “Prevalence of underweight among adults, BMI < 18 (crude estimate) (%)” 
#' Indicator Code = "NCD_BMI_18C"
#' https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-underweight-among-adults-bmi-18-(crude-estimate)-(-) 
#' 
#' 2. “Prevalence of thinness among children and adolescents, BMI < -2 standard deviations below the median (crude estimate) (%)”
#' Indicator Code = "NCD_BMI_MINUS2C"
#' https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-thinness-among-children-and-adolescents-bmi--2-standard-deviations-below-the-median-(crude-estimate)-(-)
#'
#' 3. "Wasting prevalence among children under 5 years of age (% weight-for-height <-2 SD), survey-based estimates"
#' Indicator Code = "NUTRITION_WH_2"
#' https://www.who.int/data/gho/data/indicators/indicator-details/GHO/gho-jme-country-children-aged-5-years-wasted-br-(-weight-for-height--2-sd)
#' 
#' Note that this replaces the previous method of downloading CSV files directly from the web pages where the 
#' files were huge. This method gives the same final result, except for minute rounding differences for MRT and TON.
#' 
#' 
#' Dependences:
#'  - libraries dplyr, ghost and here
#'
#' Output:
#'  - prev_malnut_adult.rda file in the ~/data/gho/ folder
#'  - prev_malnut_childado.rda file in the ~/data/gho/ folder 
#'  - prev_malnut_u5.rda file in the ~/data/gho/ folder
#'  


# Set the location of the output folder
#
gho_folder <- here::here("data/gho")

# Set the year of data to download
# 
yr_und <- 2022


# Use the ghost package (https://github.com/gpw13/ghost) to get the GHO indicator data into a dataframe
#
malnutad <- ghost::gho_data("NCD_BMI_18C") |>
  # Restrict to the latest country data and for both sexes combined
  dplyr::filter(SpatialDimType == "COUNTRY" & TimeDim == yr_und & Dim1 == "SEX_BTSX" ) |>
  dplyr::select(iso3 = SpatialDim,
                year = TimeDim,
                val = NumericValue,
                lo = Low,
                hi = High) |> 
  # Annoyingly the CSV download is rounded to 2 decimal places, but the API download isn't. To maintain
  # compatibility will round to 2 decimal places
  dplyr::mutate(val = round(val, 2),
                lo = round(lo, 2),
                hi = round(hi, 2))

malnutchild <- ghost::gho_data("NCD_BMI_MINUS2C") |>
  # Restrict to the latest country data and for both sexes combined and for age group 5 - 19 years
  dplyr::filter(SpatialDimType == "COUNTRY" & TimeDim == yr_und & Dim1 == "SEX_BTSX" & Dim2 == "AGEGROUP_YEARS05-19" ) |>
  dplyr::select(iso3 = SpatialDim,
                year = TimeDim,
                val = NumericValue,
                lo = Low,
                hi = High) |> 
  # Annoyingly the CSV download is rounded to 2 decimal places, but the API download isn't. To maintain
  # compatibility will round to 2 decimal places
  dplyr::mutate(val = round(val, 2),
                lo = round(lo, 2),
                hi = round(hi, 2))

malnutu5 <- ghost::gho_data("NUTRITION_WH_2") |>
  # Restrict to the latest country data and for all under 5 age groups
  dplyr::filter(SpatialDimType == "COUNTRY" & Dim1 == "AGEGROUP_YEARSALL" ) |>
  dplyr::select(iso3 = SpatialDim,
                year = TimeDim,
                val = NumericValue,
                lo = Low,
                hi = High) 


# Add timestamps to the data sets
attr(malnutad, "timestamp") <- Sys.Date()
attr(malnutchild, "timestamp") <- Sys.Date()
attr(malnutu5, "timestamp") <- Sys.Date()

# Save as .rda files
#
save(malnutad, file = paste0(gho_folder, "/prev_malnut_adult", ".rda"))
save(malnutchild, file = paste0(gho_folder, "/prev_malnut_childado", ".rda"))
save(malnutu5, file = paste0(gho_folder, "/prev_malnut_u5", ".rda"))


