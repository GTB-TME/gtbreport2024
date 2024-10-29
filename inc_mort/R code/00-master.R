###
### Global TB report 2024
### Master file
### 11/06/2024
### Author: Mathieu Bastard, 
### Part of the R scripts are adapted from previous scripts written by Philippe Glaziou
###


library(here)


# Run the different scripts to produce the TB disease burden estimates
# All library, functions and files used in the different scripts are loaded directly in the corresponding script

# Cleaning IHME new estimates of mortality
# To run only once or if new IHME estimates are available

source(here('inc_mort/R code/01a-IHME_cleaning.R'))

# Cleaning GHE new estimates of mortality
# To run only once or if new GHE estimates are available

source(here('inc_mort/R code/01b-GHE2021_cleaning.R'))

# Cleaning UNAIDS data
# To run only once or if a new UNAIDS file is available

source(here('inc_mort/R code/01c-UNAIDS_cleaning.R'))

# Cleaning VR data from WHO/DDI and WHO/GTB database
# To run only once or if a new VR file is available

source(here('inc_mort/R code/01d-VR_cleaning.R'))


# Estimating HIV prevalence in TB from WHO/GTB database
# To run each time a new snapshot of the WHO/GTB databases is done

source(here('inc_mort/R code/02-tbhiv_prevalence.R'))

# Estimating TB incidence overall and disaggregated by HIV
# To run each time a new snapshot of the WHO/GTB databases is done
# To run for each new round of estimates

source(here('inc_mort/R code/03-incidence.R'))

# Estimating TB mortality overall and disaggregated by HIV
# To run each time a new snapshot of the WHO/GTB databases is done
# To run for each new round of estimates
# Needs editing to use the last TB incidence estimate dataset (est_inc_most-recent-date.csv)

source(here('inc_mort/R code/04-mortality.R'))

# Estimating TB incidence and mortality overall and disaggregated by HIV at global and regional level
# To run each time a new snapshot of the WHO/GTB databases is done
# To run for each new round of estimates

source(here('inc_mort/R code/05-aggregate.R'))


# Estimating attributable cases per country to the risk factors
# Estimating the global attributable cases to the risk factors
# To run each time a new snapshot of the WHO/GTB databases is done
# To run for each new round of estimates

source(here('inc_mort/R code/06a-attributable_cases_undernut.R'))
source(here('inc_mort/R code/06b-attributable_cases_rf.R'))
source(here('inc_mort/R code/06c-attributable_cases_aggregates.R'))


# Estimating incidence of bact confirmed: the incidence of TB that would be found bacteriologically
# confirmed if all incident cases were tested using recommended modern diagnostics.
# To run each time a new snapshot of the WHO/GTB databases is done
# To run for each new round of estimates

source(here('inc_mort/R code/07-incidence_bact_confirmed.R'))


# Estimating excess of mortality during covid: 2020-2023 using the counter factual
# To run each time a new snapshot of the WHO/GTB databases is done
# To run for each new round of estimates

source(here('inc_mort/R code/08-excess_mortality.R'))






