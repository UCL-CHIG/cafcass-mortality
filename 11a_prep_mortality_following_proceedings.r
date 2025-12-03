print("Subsetting cohort")
cafcass_cohort <- deliveries[in_cafcass == T]


print("Calculating status time and variable")
cafcass_cohort[, time_for_model := as.integer(difftime(status_date, cafcass_case_start_date, units = "days"))]
# cafcass_cohort[, time_for_model := person_time_unexposed_days + person_time_exposed_days]

cafcass_cohort[, status_for_model := 0] # censored
cafcass_cohort[!is.na(dod_combined), status_for_model := 1] # dead
cafcass_cohort[, status_for_model := factor(status_for_model)]


print("Age at start of proceedings")
cafcass_cohort[, age_at_proceedings_yrs := as.integer(difftime(cafcass_case_start_date, dob_full, units = "days")) / 365.25]

cafcass_cohort[, age_cat_at_proceedings := factor(NA,
                                                  levels = c("<20",
                                                             "20-24",
                                                             "25-29",
                                                             "30-34",
                                                             "35-39"))]

cafcass_cohort[age_at_proceedings_yrs < 20, age_cat_at_proceedings := "<20"]
cafcass_cohort[age_at_proceedings_yrs >= 20 & age_at_proceedings_yrs <= 24, age_cat_at_proceedings := "20-24"]
cafcass_cohort[age_at_proceedings_yrs >= 25 & age_at_proceedings_yrs <= 29, age_cat_at_proceedings := "25-29"]
cafcass_cohort[age_at_proceedings_yrs >= 30 & age_at_proceedings_yrs <= 34, age_cat_at_proceedings := "30-34"]
cafcass_cohort[age_at_proceedings_yrs >= 35, age_cat_at_proceedings := "35-39"]


print("IMD at start of proceedings")
# table(cafcass_cohort$cafcass_address_imd_quintile, useNA = "always")
cafcass_cohort[, cafcass_address_imd_quintile := as.factor(cafcass_address_imd_quintile)]



