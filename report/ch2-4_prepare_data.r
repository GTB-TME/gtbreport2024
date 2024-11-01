# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch2-4.rmd
# Takuya Yamanaka, July 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load chapter 3 and 4 packages, settings and data
source(here::here('report/ch2_load_data.r'))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.1 (previously 2.4.3) ----
# (Bar chart of numbers treated for MDR-TB each year since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.4.1_data <- filter(notification, year >=2010) %>%

  select(year,
         rapid_dx_dr_r,
         conf_mdr,
         unconf_rrmdr_tx,
         conf_rrmdr_tx,
         unconf_rr_nfqr_tx,
         conf_rr_nfqr_tx,
         conf_rr_fqr_tx,
         rrmdr_014_tx,
         rr_nfqr_014_tx) %>%

  group_by(year) %>%
  summarise(across(rapid_dx_dr_r:rr_nfqr_014_tx, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # Calculate total treated (all ages and over-15s). Set all ages to NA when we have numbers for children
  # and set adults to NA when we don;t have children. That way we can have a stacked bar chart that
  # handles the early years without children and the latter with children
  mutate(rrmdr_all_tx = ifelse(rrmdr_014_tx > 0 | rr_nfqr_014_tx > 0, NA,
                               ifelse(year < 2014, rapid_dx_dr_r + conf_mdr,
                                      unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx))) %>%
  mutate(rrmdr_15plus_tx = ifelse(rrmdr_014_tx > 0 | rr_nfqr_014_tx > 0,
                                  unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx - rrmdr_014_tx - rr_nfqr_014_tx,
                                  NA)) %>%
  mutate(rrmdr_014_tx = ifelse(rrmdr_014_tx == 0 & rr_nfqr_014_tx == 0,
                               NA,
                               rrmdr_014_tx + rr_nfqr_014_tx)) %>%

  # restrict to essential variables
  select(year,
         rrmdr_all_tx,
         rrmdr_014_tx,
         rrmdr_15plus_tx) %>%

  # pivot to long format
  pivot_longer(cols = starts_with("rrmdr_"),
               names_to = "age_group",
               names_pattern = "rrmdr_(.*)_tx",
               values_to = "how_many")

f2.4.1_txt <- f2.4.1_data %>%
  group_by(year) %>%
  summarise(across(how_many, sum, na.rm=TRUE)) %>%
  filter(year >=2019) %>%
  pivot_wider(names_from = year, values_from = how_many) %>%
  mutate(pct_increase_2322 = (`2023`/`2022`-1)*100,
         pct_increase_2321 = (`2023`/`2021`-1)*100,
         pct_increase_2220 = (`2022`/`2020`-1)*100
  ) %>%
  rename(rr_2019=`2019`,
         rr_2020=`2020`,
         rr_2021=`2021`,
         rr_2022=`2022`,
         rr_2023=`2023`)


f2.4.1b_data <- filter(notification, year >=2010) %>%
  
  select(year,
         rapid_dx_dr_r,
         conf_mdr,
         unconf_rrmdr_tx,
         conf_rrmdr_tx,
         unconf_rr_nfqr_tx,
         conf_rr_nfqr_tx,
         conf_rr_fqr_tx,
         rrmdr_014_tx,
         rr_nfqr_014_tx) %>%
  
  group_by(year) %>%
  summarise(across(rapid_dx_dr_r:rr_nfqr_014_tx, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  
  # Calculate total treated (all ages and over-15s). Set all ages to NA when we have numbers for children
  # and set adults to NA when we don;t have children. That way we can have a stacked bar chart that
  # handles the early years without children and the latter with children
  mutate(rrmdr_all_tx = ifelse(year < 2014, rapid_dx_dr_r + conf_mdr,
                                      unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx)) %>%
  mutate(rrmdr_15plus_tx = ifelse(rrmdr_014_tx > 0 | rr_nfqr_014_tx > 0,
                                  unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx - rrmdr_014_tx - rr_nfqr_014_tx,
                                  NA)) %>%
  mutate(rrmdr_014_tx = ifelse(rrmdr_014_tx == 0 & rr_nfqr_014_tx == 0,
                               NA,
                               rrmdr_014_tx + rr_nfqr_014_tx)) %>%
  
  # restrict to essential variables
  select(year,
         rrmdr_all_tx,
         rrmdr_014_tx,
         rrmdr_15plus_tx) %>%
  
  # pivot to long format
  pivot_longer(cols = starts_with("rrmdr_"),
               names_to = "age_group",
               names_pattern = "rrmdr_(.*)_tx",
               values_to = "how_many") %>%
  filter(!is.na(how_many)) %>%
  mutate(age_group = factor(age_group, levels = c("all", "15plus", "014" ),
                          labels = c("All ages", "People aged \u226515 years or age not reported", "People aged 0\u201314 years")))


calculate_y_limits <- function(data) {
  data %>%
    group_by(age_group) %>%
    summarize(y_max = max(how_many)) %>%
    mutate(y_upper_limit = ifelse(y_max>10000, ceiling(y_max / 100000) * 100000, ceiling(y_max / 1000) * 1000)) %>%
    select(age_group, y_upper_limit) %>%
    ungroup
}

y_limits <- calculate_y_limits(f2.4.1b_data)
f2.4.1b_data <- f2.4.1b_data %>%
  left_join(y_limits, by = "age_group")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.2 ----
# (Panel of line charts of MDR/RR-TB cases detected, MDR/RR-TB put on treatment
# since 2009 for 30 countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

notification <- notification %>%
  mutate(conf_rr_nfqr = ifelse((is.na(conf_rr_nfqr)&iso3=="BGD"), conf_rr_nfqr_tx, conf_rr_nfqr)) # this is temporal data correction for BGD notification.
  
f2.4.2_data <- filter(notification, year >= 2010 & iso3 %in% hbmdr30$iso3) %>%
  select(country,
         iso3,
         year,
         rapid_dx_dr_r,
         conf_mdr,
         conf_rrmdr,
         conf_rr_nfqr,
         conf_rr_fqr,
         conf_mdr_tx,
         unconf_mdr_tx,
         conf_rrmdr_tx,
         unconf_rrmdr_tx,
         conf_rr_nfqr_tx,
         unconf_rr_nfqr_tx,
         conf_rr_fqr_tx) %>%
  rowwise() %>%
  # Derive total number detected and total enrolled on treatment
  mutate(rr_detected = ifelse(year < 2014,
                              sum(across(rapid_dx_dr_r:conf_mdr), na.rm = TRUE),
                              # the next three are mutually exclusive so can be added
                              sum(across(conf_rrmdr:conf_rr_fqr), na.rm = TRUE)),
         # treatment variables are in mutually exclusive sets so again can be added
         rr_treated = sum(across(conf_mdr_tx:conf_rr_fqr_tx), na.rm = TRUE)) %>%
  ungroup() %>%

  # drop unneeded variables
  select(country,
         iso3,
         year,
         rr_detected,
         rr_treated)

f2.4.2_txt <- f2.4.2_data %>%
  filter(year == report_year-1 & rr_treated>rr_detected)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.3 ----
# (Number of diagnosed with MDR/RR-TB and enrolled on treatment, compared with incident cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# notification
f2.4.3_data <- filter(notification, year >= 2015 ) %>%
  select(country,
         g_whoregion,
         iso3,
         year,
         rapid_dx_dr_r,
         conf_mdr,
         conf_rrmdr,
         conf_rr_nfqr,
         conf_rr_fqr,
         conf_mdr_tx,
         unconf_mdr_tx,
         conf_rrmdr_tx,
         unconf_rrmdr_tx,
         conf_rr_nfqr_tx,
         unconf_rr_nfqr_tx,
         conf_rr_fqr_tx) %>%
  mutate(conf_rr_nfqr=ifelse(conf_rr_nfqr==130124,2938,conf_rr_nfqr)) %>% # tentative correction for Nigeria

  rowwise() %>%
  # Derive total number detected and total enrolled on treatment
  mutate(rr_detected = ifelse(year < 2014,
                              sum(across(rapid_dx_dr_r:conf_mdr), na.rm = TRUE),
                              # the next three are mutually exclusive so can be added
                              sum(across(conf_rrmdr:conf_rr_fqr), na.rm = TRUE)),
         # treatment variables are in mutually exclusive sets so again can be added
         rr_treated = sum(across(conf_mdr_tx:conf_rr_fqr_tx), na.rm = TRUE)) %>%

  # calculate regional aggregates
  group_by(g_whoregion,year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  arrange(entity) %>%
  select(-g_whoregion) %>%

  ungroup() %>%

  # drop unneeded variables
  select(entity,
         year,
         rr_detected,
         rr_treated)

# Add global summary to the regional summary
f2.4.3_global <- f2.4.3_data %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(entity="Global") %>%
  ungroup()

f2.4.3_data <- rbind(f2.4.3_data, f2.4.3_global)

if(show_estimates){
  
f2.4.3_drest <- est_dr_group %>%
  mutate(g_whoregion=group_name) %>%
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  arrange(entity) %>%
  select(-g_whoregion) %>%
  select(entity,
         year,
         e_inc_rr_num,
         e_inc_rr_num_lo,
         e_inc_rr_num_hi)

# Add global summary to the regional summary
f2.4.3_drest_global <- est_dr_group %>%
  mutate(entity=group_name) %>%
  subset(entity=="global") %>%
  mutate(entity="Global") %>%
  select(entity,
         year,
         e_inc_rr_num,
         e_inc_rr_num_lo,
         e_inc_rr_num_hi)

f2.4.3_drest <- rbind(f2.4.3_drest, f2.4.3_drest_global)

# merge notification and estimates
f2.4.3_data <- left_join(f2.4.3_data,f2.4.3_drest, by=c("entity","year"))


f2.4.3_data <- f2.4.3_data %>%
  mutate(entity = factor(entity,
                         levels = c("Global","African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.4 ----
# (Trend of TB treatment coverage for MDR/RR-TB regionally and globally)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if(show_estimates){
  # B. Regions
  # - - - - - - - -
  coverage_inc_region <- filter(est_dr_group, year>=2015 & group_type=="g_whoregion") %>%
    select(g_whoregion = group_name,
           year,
           e_inc_rr_num,
           e_inc_rr_num_lo,
           e_inc_rr_num_hi)

  f2.4.4_region_data <- filter(notification, year>=2015) %>%
    select(g_whoregion,
           year,
           rapid_dx_dr_r,
           conf_mdr,
           conf_rrmdr,
           conf_rr_nfqr,
           conf_rr_fqr,
           conf_mdr_tx,
           unconf_mdr_tx,
           conf_rrmdr_tx,
           unconf_rrmdr_tx,
           conf_rr_nfqr_tx,
           unconf_rr_nfqr_tx,
           conf_rr_fqr_tx)  %>%
    rowwise() %>%
    # Derive total number detected and total enrolled on treatment
    mutate(rr_tx = sum(across(conf_mdr_tx:conf_rr_fqr_tx), na.rm = TRUE)) %>%
    # calculate regional aggregates
    group_by(g_whoregion, year) %>%
    summarise(across(rr_tx, sum, na.rm=TRUE)) %>%
    ungroup() %>%
    
    # merge with incidence estimates
    inner_join(coverage_inc_region, by = c("g_whoregion","year")) %>%

    # Calculate coverage
    mutate(c_rr_coverage = rr_tx * 100 / e_inc_rr_num,
           c_rr_coverage_lo = rr_tx * 100  / e_inc_rr_num_hi,
           c_rr_coverage_hi = rr_tx * 100  / e_inc_rr_num_lo) %>%

    # merge with regional names and simplify to match structure of country table
    right_join(who_region_shortnames, by = "g_whoregion") %>%
    select(entity,
           year,
           c_rr_coverage,
           c_rr_coverage_lo,
           c_rr_coverage_hi) #%>%
    # arrange(desc(c_rr_coverage))

  # C. Global
  # - - - - - - - -
  coverage_inc_global <- filter(est_dr_group, year>=2015 & group_type=="global") %>%
    select(year,
           e_inc_rr_num,
           e_inc_rr_num_lo,
           e_inc_rr_num_hi) %>%
    mutate(entity="Global")

  f2.4.4_global_data <- filter(notification, year>=2015) %>%
    select(           year,
                      rapid_dx_dr_r,
                      conf_mdr,
                      conf_rrmdr,
                      conf_rr_nfqr,
                      conf_rr_fqr,
                      conf_mdr_tx,
                      unconf_mdr_tx,
                      conf_rrmdr_tx,
                      unconf_rrmdr_tx,
                      conf_rr_nfqr_tx,
                      unconf_rr_nfqr_tx,
                      conf_rr_fqr_tx)  %>%
    rowwise() %>%
    # Derive total number detected and total enrolled on treatment
    mutate(rr_tx = sum(across(conf_mdr_tx:conf_rr_fqr_tx), na.rm = TRUE)) %>%
    # calculate global aggregate
    group_by(year) %>%
    summarise(across(rr_tx, sum, na.rm=TRUE)) %>%
    mutate(entity="Global") %>%

    inner_join(coverage_inc_global, by=c("entity","year")) %>%

    # Calculate coverage
    mutate(c_rr_coverage = rr_tx * 100 / e_inc_rr_num,
           c_rr_coverage_lo = rr_tx * 100  / e_inc_rr_num_hi,
           c_rr_coverage_hi = rr_tx * 100  / e_inc_rr_num_lo) %>%
    select(entity,
           year,
           c_rr_coverage,
           c_rr_coverage_lo,
           c_rr_coverage_hi)

  # Simple dataframe of numbers for section text
  f2.4.4_txt <- f2.4.4_global_data  %>%
    filter(year==report_year-1) %>%
    select(c_rr_coverage)

  f2.4.4_txt_list_region <- f2.4.4_region_data  %>%
    filter(year==report_year-1) %>%
    arrange(desc(c_rr_coverage)) %>%
    slice(1) %>%
    arrange(entity)

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.5 ----
# (Trend of TB treatment coverage for MDR/RR-TB in 30 high MDR burden countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if(show_estimates){
  
  # A. Countries
  # - - - - - - - -
  coverage_inc_country <- filter(est_dr_country, year>=2015) %>%
    select(year,
           iso3,
           e_inc_rr_num,
           e_inc_rr_num_lo,
           e_inc_rr_num_hi) %>%
    
    # restrict to high burden countries
    inner_join(hbmdr30, by = "iso3")
  
  f2.4.5_data <- filter(notification, year>=2015) %>%
    select(entity = country,
           g_whoregion,
           iso3,
           year,
           rapid_dx_dr_r,
           conf_mdr,
           conf_rrmdr,
           conf_rr_nfqr,
           conf_rr_fqr,
           conf_mdr_tx,
           unconf_mdr_tx,
           conf_rrmdr_tx,
           unconf_rrmdr_tx,
           conf_rr_nfqr_tx,
           unconf_rr_nfqr_tx,
           conf_rr_fqr_tx) %>%
    rowwise() %>%
    # Derive total number detected and total enrolled on treatment
    mutate(rr_tx = sum(across(conf_mdr_tx:conf_rr_fqr_tx), na.rm = TRUE)) %>%
    # mutate(rr_tx = ifelse(is.na(unconf_rr_nfqr_tx) & is.na(conf_rr_nfqr_tx) & is.na(conf_rr_fqr_tx),
    #                       NA,
    #                       NZ(unconf_rr_nfqr_tx) + NZ(conf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx))) %>%
    right_join(coverage_inc_country, by = c("iso3","year")) %>%
    select(-iso3) %>%
    mutate(c_rr_coverage = rr_tx * 100 / e_inc_rr_num,
           c_rr_coverage_lo = rr_tx * 100  / e_inc_rr_num_hi,
           c_rr_coverage_hi = rr_tx * 100  / e_inc_rr_num_lo,
           # highlight countries with no data
           entity = ifelse(is.na(rr_tx), paste0(entity, "*"), entity )) %>%
    select(entity,
           year,
           c_rr_coverage,
           c_rr_coverage_lo,
           c_rr_coverage_hi) %>%
    arrange(entity)

  f2.4.5_txt_list_hi <- f2.4.5_data  %>%
    filter(year==report_year-1) %>%
    slice(1:30) %>%
    filter(c_rr_coverage > 49.4) %>%
    arrange(as.character(entity)) %>%
    select(entity) 
  
  f2.4.5_txt_list_lo <- f2.4.5_data %>%
    filter(year==report_year-1) %>%
    slice(1:30) %>%
    filter(c_rr_coverage <= 20) %>%
    arrange(as.character(entity)) %>%
    select(entity)
  
  
}



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.6  ----
# (Bubble map of difference between MDR/RR-TB patients starting treatment and
#  estimated MDR/RR-TB incidence for 10 countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if(show_estimates){

  f2.4.6_data  <- filter(est_dr_country, year == report_year - 1) %>%
    select(iso3,
           year,
           e_inc_rr_num) %>%

    # Link to notifications
    inner_join(notification, by = c("iso3", "year")) %>%
    select(iso3,
           country,
           e_inc_rr_num,
           conf_rr_nfqr_tx,
           unconf_rr_nfqr_tx,
           conf_rr_fqr_tx) %>%

    # Filter out countries that have not reported anything yet for the latest year
    filter(!is.na(conf_rr_nfqr_tx) | !is.na(unconf_rr_nfqr_tx) | !is.na(conf_rr_fqr_tx)) %>%

    # Calculate the gap and use that for the bubble sizes
    mutate(size = e_inc_rr_num - (NZ(conf_rr_nfqr_tx) + NZ(unconf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx))) %>%

    # limit to the top 10 by size of gap
    top_n(10, size) %>%

    # sort in descending order so can list the country names in the figure footnote
    arrange(desc(size))

  # Summary number of gaps for the section text
  # Get global incidence
  f2.4.6_txt <- filter(est_dr_group, year == report_year-1 & group_type == "global") %>%
    select(e_inc_rr_num)

  # Add global enrolments
  f2.4.6_txt <- filter(notification, year == report_year-1) %>%
    select(year,
           conf_rr_nfqr_tx,
           unconf_rr_nfqr_tx,
           conf_rr_fqr_tx) %>%
    # calculate global aggregate
    group_by(year) %>%
    summarise(across(conf_rr_nfqr_tx:conf_rr_fqr_tx, sum, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(enrolled = conf_rr_nfqr_tx + unconf_rr_nfqr_tx + conf_rr_fqr_tx) %>%
    cbind(f2.4.6_txt) %>%

    # Calculate the global gap and drop the other variables
    mutate(gap = e_inc_rr_num - enrolled) %>%
    select(gap)

  # Calculate % of global gap contributed by the top 10 countries
  f2.4.6_txt <- f2.4.6_data %>%
    summarise(gap_top_ten = sum(size)) %>%
    cbind(f2.4.6_txt) %>%
    mutate(pct_gap_top_ten = gap_top_ten * 100 / gap)

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.7  ----
# (horizontal bar chart showing TB treatment outcomes globally by year since 2012 for MDR/RR-TB)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.4.7_data <- outcomes %>%

  filter(between(year, 2012, report_year - 3)) %>%
  select(iso2,
         year,
         contains("mdr_")) %>%

  # calculate global aggregates
  group_by(year) %>%
  summarise(across(mdr_coh:c_mdr_neval, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_") %>%

  # Drop the actual numbers and keep percentages, plu sother unwanted variables
  select(-coh, -succ, -fail, -died, -lost, -c_neval, -cur, -cmplt) %>%

  # Add tx group type
  mutate(subgroup = "MDR/RR-TB cases") %>%

  # flip into long format
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")

# Summary for section text
f2.4.7_txt <- filter(f2.4.7_data, year %in% c(2012, report_year-5, report_year-4,  report_year-3) & outcome=="Treatment success") %>%
  select(-outcome) %>%
  pivot_wider(names_from = year,
              names_prefix = "c_tsr_",
              values_from = value)


# high MDR-TB burden countries
f2.4.7b_data <- outcomes %>%
  
  filter(between(year, 2012, report_year - 3), iso3 %in% iso3_hmdrc) %>%
  select(country,
         iso3,
         year,
         contains("mdr_")) %>%
  
  # calculate global aggregates
  group_by(year,country) %>%
  summarise(across(mdr_coh:c_mdr_neval, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  
  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_") %>%
  
  # Drop the actual numbers and keep percentages, plu sother unwanted variables
  select(-coh, -succ, -fail, -died, -lost, -c_neval, -cur, -cmplt) %>%
  
  # Add tx group type
  mutate(subgroup = "MDR/RR-TB cases") %>%
  
  # flip into long format
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.8 ----
# (Horizontal bar chart showing TB treatment outcomes in MDR/RR-TB cases for WHO regions and globally")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Code similar to others so maybe make a reusable function?

# A. Regional aggregates
# - - - - - - - - - - - -
txout_region  <- filter(outcomes, year==report_year - 3) %>%

  select(iso2,
         g_whoregion,
         contains("mdr_")) %>%
  select(-mdr_cur, -mdr_cmplt) %>%

  group_by(g_whoregion) %>%
  summarise(across(mdr_coh:c_mdr_neval, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion) %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_") %>%

  # Sort regions in descending order of success rate
  arrange(desc(`Treatment success`))


# B. Global aggregates
# - - - - - - - - - - - -
txout_global  <- filter(outcomes, year==report_year - 3) %>%

  select(iso2,
         contains("mdr_")) %>%
  select(-mdr_cur, -mdr_cmplt) %>%

  summarise(across(mdr_coh:c_mdr_neval, sum, na.rm=TRUE)) %>%
  ungroup()  %>%
  mutate(entity="Global")  %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_")


# Create a dummy record a gap in the output to separate countries, regions and global parts
txout_dummy <- data.frame(entity = " ", coh = NA, succ = NA, fail = NA,
                          died = NA, lost = NA, c_neval = NA,
                          Failure = NA, Died = NA)

# Had to use mutate to create the next 3 fields because data.frame converted spaces to dots. Grrr
txout_dummy <- txout_dummy %>%
  mutate(`Treatment success` = NA,
         `Lost to follow-up` = NA,
         `Not evaluated` = NA)


# Create combined table in order of countries then regional and global estimates
f2.4.8_data <- rbind(txout_region, txout_dummy, txout_global) %>%

  # Keep record of current order (in reverse) so plot comes out as we want it
  mutate(entity = factor(entity, levels=rev(entity))) %>%

  # Drop the actual numbers and keep percentages
  select(-coh, -succ, -fail, -died, -lost, -c_neval) %>%

  # Flip into long mode for stacked bar plotting
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")

# Summary for section text
f2.4.8_txt <- filter(txout_region, entity %in% c("African Region", "European Region")) %>%
  select(entity,
         c_tsr = `Treatment success`) %>%
  pivot_wider(names_from = entity,
              names_prefix = "c_tsr_",
              values_from = c_tsr) %>%
  # deal with spaces in variable names
  select(c_tsr_AFR = `c_tsr_African Region`,
         c_tsr_EUR = `c_tsr_European Region`)

# tidy up
rm(list = ls(pattern = "^txout_"))

f2.4.8_txt2 <- filter(f2.4.8_data, outcome == "Treatment success" & entity!="Global") %>%
  arrange(desc(value)) %>%
  slice(1,6) %>%
  select(entity,value) %>%
  pivot_wider(names_from = entity,
              values_from = entity:value) %>%
  rename(hi_region = 1, lo_region = 2, hi_region_value = 3, lo_region_value = 4)
  


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.9  ----
# (World map showing which countries used all-oral 6-month (BPaLM/BPaL 6-9 months) treatment regimens)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.4.9_data <- filter(notification, year == report_year-1) %>%
  select(country,
         iso3,
         mdrxdr_bpalm_used) %>%
  
  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdrxdr_bpalm_used = ifelse(mdrxdr_bpalm_used==3, NA, mdrxdr_bpalm_used)) %>%
  
  
  # Assign the categories for the map
  mutate(var = factor(mdrxdr_bpalm_used,
                      levels = c(1, 0),
                      labels = c("Used", "Not used")))


# number of people treated with bpalm/bpal regimens
f2.4.9_txt <-filter(notification, year >= report_year-2) %>%
  
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_tx) %>%
  group_by(year) %>%
  summarise(across(mdrxdr_bpalm_tx, sum, na.rm=TRUE))  %>%
  ungroup() %>%
  pivot_wider(names_from = year,
              names_prefix = "bpalm_tx_",
              values_from = mdrxdr_bpalm_tx)

f2.4.9_txt2 <-  filter(notification, year %in% c(2020, report_year-2, report_year-1)) %>%
  
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_used) %>%
  
  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdrxdr_bpalm_used = ifelse(mdrxdr_bpalm_used==3, NA, mdrxdr_bpalm_used)) %>%
  
  group_by(year) %>%
  summarise(across(mdrxdr_bpalm_used, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  
  pivot_wider(names_from = year,
              names_prefix = "bpalm_used_",
              values_from = mdrxdr_bpalm_used)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.10  ----
# (World map showing which countries used shorter MDR-TB treatment regimens)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.4.10_data <- filter(notification, year == report_year-1) %>%
  select(country,
         iso3,
         mdr_alloral_short_used) %>%

  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdr_alloral_short_used = ifelse(mdr_alloral_short_used==3, NA, mdr_alloral_short_used)) %>%


  # Assign the categories for the map
  mutate(var = factor(mdr_alloral_short_used,
                      levels = c(1, 0),
                      labels = c("Used", "Not used")))


f2.4.10_txt <-  filter(notification, year %in% c( report_year-3, report_year-2, report_year-1)) %>%

  select(country,
         iso3,
         year,
         mdr_alloral_short_used) %>%

  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdr_alloral_short_used = ifelse(mdr_alloral_short_used==3, NA, mdr_alloral_short_used)) %>%

  group_by(year) %>%
  summarise(across(mdr_alloral_short_used, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  pivot_wider(names_from = year,
              names_prefix = "oral_short_used_",
              values_from = mdr_alloral_short_used)

f2.4.10_check <- filter(notification, year >= report_year-3) %>%
  select(country,
         iso3,
         year,
         mdr_alloral_short_used) %>%
  pivot_wider(names_from = year, values_from = mdr_alloral_short_used) %>%
  filter(`2021`==1 & `2022`!=1)

knitr::combine_words(f2.4.10_check$country, oxford_comma=FALSE)


f2.4.9_10_txt <- filter(f2.4.10_data, mdr_alloral_short_used == 1) %>%
  summarise(alloral_short = n()) 

f2.4.9_10_txt <- filter(f2.4.9_data, mdrxdr_bpalm_used == 1) %>%
  summarise(mdrxdr_bpalm = n()) %>%
  cbind(f2.4.9_10_txt)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.11 
# proportion of people treated with each MDR/RR-TB regimens - globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mdr_regimen_country <- filter(notification, year > 2018) %>%
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_used,
         mdr_alloral_short_used,
         mdrxdr_alloral_used) %>%
  
  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdrxdr_bpalm_used = ifelse(mdrxdr_bpalm_used==3, NA, mdrxdr_bpalm_used),
         mdr_alloral_short_used = ifelse(mdr_alloral_short_used==3, NA, mdr_alloral_short_used),
         mdrxdr_alloral_used = ifelse(mdrxdr_alloral_used==3, NA, mdrxdr_alloral_used)
  ) %>%
  
  group_by(year) %>%
  summarise(across(mdrxdr_bpalm_used:mdrxdr_alloral_used, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("mdr"), names_to = "regimen") %>%
  mutate(country = "global")


f2.4.3_treated_data <- f2.4.3_data %>%
  select(entity, year, rr_treated)

f2.4.11_global_data <- filter(notification, year > 2018) %>%
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_tx,
         mdr_alloral_short_tx,
         mdrxdr_alloral_tx)  %>%
  
  group_by(year) %>%
  summarise(across(mdrxdr_bpalm_tx:mdrxdr_alloral_tx, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(entity = "Global") %>%
  left_join(f2.4.3_treated_data, by = c("year","entity")) %>%
  mutate(mdr_unk_tx = rr_treated-(mdrxdr_bpalm_tx+mdr_alloral_short_tx+mdrxdr_alloral_tx)) %>%
  pivot_longer(cols = starts_with("mdr"), names_to = "regimen") 
 
f2.4.11_region_data <- filter(notification, year > 2018) %>%
  select(iso3,
         year,
         g_whoregion,
         mdrxdr_bpalm_tx,
         mdr_alloral_short_tx,
         mdrxdr_alloral_tx)  %>%
  group_by(year,g_whoregion) %>%
  summarise(across(mdrxdr_bpalm_tx:mdrxdr_alloral_tx, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  left_join(f2.4.3_treated_data, by = c("year","entity")) %>%
  mutate(mdr_unk_tx = rr_treated-(mdrxdr_bpalm_tx+mdr_alloral_short_tx+mdrxdr_alloral_tx)) %>%
  pivot_longer(cols = starts_with("mdr"), names_to = "regimen") %>%
  select(!g_whoregion)

f2.4.11_data <- rbind.data.frame(f2.4.11_global_data, f2.4.11_region_data) 


f2.4.11_data <- f2.4.11_data %>%
  mutate(pcnt = value / rr_treated * 100) %>%
  select(year, entity, regimen, pcnt)

f2.4.11_data <- f2.4.11_data %>%
  mutate(pcnt = ifelse((regimen=="mdr_alloral_short_tx"&year==2019)|(regimen=="mdrxdr_bpalm_tx"&year<2022),NA,pcnt)) %>%
  mutate(entity = factor(entity,
                         levels = c("Global","African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))

f2.4.11_txt <- f2.4.11_data %>%
  filter(year == report_year-1 & entity == "Global") %>%
  select(regimen, pcnt) %>%
  pivot_wider(names_from = regimen,
              values_from = pcnt)

f2.4.11_9month_txt <- f2.4.11_data %>%
  filter(year == report_year-1 & entity != "Global" & regimen == "mdr_alloral_short_tx") %>%
  slice(1)

f2.4.11_6month_txt <- f2.4.11_data %>%
  filter(year == report_year-1 & entity != "Global" & regimen == "mdrxdr_bpalm_tx") %>%
  arrange(desc(pcnt)) %>%
  slice(1:2) %>%
  arrange(entity)

# (c) proportion of people treated with each MDR/RR-TB regimens - 30 high MDR/RR-TB burden countries
f2.4.2_country_data <- f2.4.2_data %>%
  select(iso3, year, rr_treated)

f2.4.12_data <- filter(notification, year > 2018) %>%
  select(country,
         iso3,
         year,
         mdrxdr_bpalm_tx,
         mdr_alloral_short_tx,
         mdrxdr_alloral_tx)  %>%
  filter(iso3 %in% hbmdr30$iso3) %>%
  # group_by(country, iso3, year) %>%
  # summarise(across(mdrxdr_bpalm_tx:mdrxdr_alloral_tx, sum, na.rm=TRUE)) %>%
  # ungroup() %>%
  left_join(f2.4.2_country_data, by = c("year","iso3")) %>%
  mutate(mdr_unk_tx = rr_treated-(NZ(mdrxdr_bpalm_tx)+NZ(mdr_alloral_short_tx)+NZ(mdrxdr_alloral_tx))) %>%
  pivot_longer(cols = starts_with("mdr"), names_to = "regimen")


f2.4.12_data <- f2.4.12_data %>%
  mutate(pcnt = value / rr_treated * 100) %>%
  select(country, year, regimen, pcnt) 

f2.4.12_9month_txt <- f2.4.12_data %>%
  filter(year == report_year-1 & pcnt>50 & regimen == "mdr_alloral_short_tx") %>%
  arrange(country)


f2.4.12_6month_txt <- f2.4.12_data %>%
  filter(year == report_year-1 & pcnt>10 & regimen == "mdrxdr_bpalm_tx") %>%
  arrange(country)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.4.13  ----
# (World map showing proportion of drug-resistant TB patients followed up for adverse events)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.4.13_data <- filter(notification, year==report_year-1) %>%

  select(country,
         iso3,
         unconf_rr_nfqr_tx,
         conf_rr_nfqr_tx,
         conf_rr_fqr_tx,
         mdr_tx_adsm) %>%

  # Calculate percent with active follow up
  mutate(adsm_pct = ifelse(NZ(unconf_rr_nfqr_tx) + NZ(conf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx) > 0,
                           mdr_tx_adsm * 100 / (NZ(conf_rr_nfqr_tx) + NZ(conf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx)),
                           NA)) %>%

  # Assign the categories for the map
  mutate(var = cut(adsm_pct,
                   c(0, 25, 50, 75, Inf),
                   c('0\u201324', '25\u201349', '50\u201374', '\u226575'),
                   right=FALSE)) %>%

  # get rid of extra variables
  select(country,
         iso3,
         adsm_pct,
         var)


