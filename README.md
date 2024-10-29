---
title: gtbr2024-dev  Git/Github repository
---

# WHO Global Tuberculosis Report 2024

Code and data used to develop the World Health Organization's _Global tuberculosis report 2024_. The report was published on 29 October 2024 at https://www.who.int/teams/global-tuberculosis-programme/tb-reports/global-tuberculosis-report-2024/

This repository brings together a number of linked sub-projects to generate TB burden estimates and to produce the report's web pages and PDF. The sub-projects are linked through the use of common data files, both as inputs and as generated outputs to be used by other sub-projects.

# Folders

* **data**: Datasets used for the report:

  * **datahub**: Selected indicators downloaded from the WHO Health Data Hub, including UHC service index
  
  * **gf**: Global Fund data on pledges from donors
  
  * **gho**: Selected indicators downloaded from the WHO Global Health Observatory, including BCG vaccination coverage and catastrophic health expenditure

  * **gtb**: R binary datasets extracted from the WHO global TB database:

    * **other**: Country lists, population estimates, reference lists, survey results and external indicators
    
    * **snapshot_yyyy-mm-dd**: Snapshot of country-reported data

  * **ihme**: Vital statistics data from the Institute of Health Metrics and Evaluation
  
  * **imf**: Economic data from the International Monitory Fund
  
  * **kff**: Economic data from the Kaiser Family Foundation 

  * **mortality**: Vital statistics data from the WHO Mortality database
  
  * **nhe**: National Health Accounts data from the WHO Global Health Expenditures Database
  
  * **oecd**: Economic data from the Organisation for Economic Co-operation and Development
  
  * **pcs**: Summary results from completed patient cost surveys
  
  * **ps**: Summary results from prevalence surveys not available in the WHO global TB database

  * **unaids**: HIV estimates from UNAIDS
  
  * **unore**: United Nations Operational Rates of Exchange from the United National Treasury
  
  * **wb**: Economic data from the World Bank

* **disaggregation**: Pete Dodd's R scripts to estimate incidence and mortality disaggregated by age group and sex.

* **doc**: A PDF version of the web-based data collection form used by countries to report data to WHO. The PDF also shows database variable names.

* **drtb**: Pete Dodd's R and Stan scripts to estimate drug-resistant TB burden.

* **dynamic**: Nim Arinaminpathy's Matlab code for dynamic modelling of TB incidence and mortality for the period 2020-2023 in selected countries.

* **finance**: Takuya Yamanaka's R code (converted from Stata code developed over many years by many people) for analysing TB financing data.

* **import**: R scripts to download data from external sources and from the WHO global TB database into the relevant `~/data/` folders. There are also scripts to load saved GTB data files into memory.

* **inc_mort**: Mathieu Bastard and Philippe Glaziou's R scripts to produce estimates of TB incidence and mortality

* **lives_saved**: Takuya Yamanaka and Philippe Glaziou's R scripts to produce estimates of the number of death averted by TB treatment and ART since 2005.

* **report**: R Markdown scripts by Mathieu Bastard, Irwin Law, Hazim Timimi and Takuya Yamanaka to generate tables, static figure, interactive Kendo UI charts and text for the 2024 edition of the WHO Global tuberculosis report web pages and the report PDF. 


# Global tuberculosis database data sets

The following sections show the data object names chosen in previous years. If you use the `load_gtb()` function you don't need to know if a data object is part of a snapshot or other, nor does your code have to use the same object name.

For example, the following line loads the most recently saved snapshot of notifications into a dataframe / data table called `notifs`:
```
notifs <- load_gtb("tb")
```

## Snapshot data

* **agg**: TB/HIV indicators with rules to be used to calculate aggregates from `view_TME_master_TBHIV_for_aggregates`

* **covid**: Impact of COVID on services and response to the UNHLM commitments from `view_TME_master_covid_unhlm`

* **drfq**: DRS records used to estimate fluoroquinolone resistance among RR-TB patients from `view_DRS_for_estimation_sldst`

* **drhnew**: DRS records used to estimate HR-TB among new TB patients from `view_DRS_for_estimation_new_INH`

* **drhret**: DRS records used to estimate HR-TB among previously treated TB patients from `view_DRS_for_estimation_ret_INH`

* **drnew**: DRS records used to estimate RR-TB among new TB patients from `view_DRS_for_estimation_new`

* **drret**: DRS records used to estimate RR-TB among previously treated TB patients from `view_DRS_for_estimation_ret`

* **drroutine**: Routine drug resistance surveillance records from `view_TME_master_dr_surveillance`

* **finance**: TB finance and health service utilization from `view_TME_master_budget_expenditure`

* **ltbi**: Estimates of TPT coverage among children (numbers derived from reported data) from `view_TME_estimates_ltbi`

* **monthly**: Provisional monthly or quarterly notifications from `dcf.latest_provisional_c_newinc`

* **sty**: Services, PPM, community engagement, M&E systems from `view_TME_master_strategy`

* **tb**: TB notifications from `view_TME_master_notifications`

* **tpt**: TB preventive treatment from `view_TME_master_contacts_tpt`

* **tx**: Treatment outcomes from `view_TME_master_outcomes`

* **vrgtb**: VR data reported by countries from `dcf.latest_vr`


## Other data

### Explanatory 

* **dic**: Data dictionary from `view_TME_data_dictionary`

* **codes**: Meaning of codes used for categorical variables from `view_TME_data_codes`

### Reference: countries, country groups, population and SDG indicators

* **cty**: Country and area names in 4 languages, their codes and WHO region and status from `view_TME_master_report_country`

* **datacoll**: Options set for the data collection form for each country-data collection year combinations from `view_TME_master_data_collection`

* **grptypes**: Themes by which to group countries from `view_country_group_types`

* **grp**: Country groups within each grouping theme (e.g. the 4 income groups of High, Upper Middle, Lower Middle and Low in the World Bank income classification) from `view_country_groups`

* **grpmbr**: Countries belonging to each country group from `view_country_group_membership`

* **pop**: UN Population Division population estimates from `view_TME_estimates_population`

* **sdg**: SDG indicator data and codes relevant to TB incidence from `external_indicators.view_indicator_data`

* **sdgdef**: Full names of SDG indicators and their sources from `"external_indicators.view_indicator_definition`

### Surveys and survey results

* **svy.cc**: Results of catastrophic costs surveys from `survey.view_catastrophic_costs_survey`

* **svy.prev**: Prevalence estimates resulting from prevalence surveys from `survey.view_prevalence_survey_estimates`

* **svy.prevcases**: Numbers of TB cases found in prevalence surveys from `survey.view_prevalence_survey_cases`

* **svy.prevchar**: Characteristics of prevalence surveys from `survey.view_prevalence_survey`

Codes used in survey records:

* **svy.agegr**: Codes for age groups from `survey.age_group`

* **svy.areatype**: Codes for area types from `survey.area_type`

* **svy.casetype**: Codes for case types from `survey.case_type`

* **svy.patientgr**: Codes for patient groups from `survey.patient_group`

* **svy.screen**: Codes for screening methods from `survey.screen_group`

* **svy.sex**: Codes for sex from `survey.sex`


