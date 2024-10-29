
finance_merged <- finance_merged %>%
  mutate(rcvd_int_sh = ifelse(iso3=="IRN" & (rcvd_int_sh==0|is.na(rcvd_int_sh)) & (year==2015|year==2016), 1, rcvd_int_sh)
)

finance_merged <- finance_merged %>%
  mutate(rcvd_int_sh = ifelse(iso3=="NER"  & (year==2017), lead(rcvd_int_sh,1), rcvd_int_sh)
  )

# finance_merged <- finance_merged %>%
#   mutate(rcvd_int_sh = ifelse(iso3=="VUT"  & (year==2022), lag(rcvd_int_sh,1), rcvd_int_sh)
#   )

finance_merged <- finance_merged %>%
  mutate(rcvd_int_sh = ifelse(iso3=="EGY"  & (year==2020), lead(rcvd_int_sh,1), rcvd_int_sh)
  )