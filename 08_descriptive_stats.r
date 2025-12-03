
print("Outputting descriptive statistics")
vars <-
  c(
    "age_cat_at_delivery",
    "eth_cat",
    "imd_quintile_cat",
    "delivery_fyear_cat",
    "any_health_problem_3yrs_prior_del",
    "charlson_3yrs_prior_del",
    "disabilities_grant_any_3yrs_prior_del",
    "int_dis_3yrs_prior_del",
    "mhbev_any_3yrs_prior_del",
    "adversity_admission_3yrs_prior_del",
    "drug_alc_3yrs_prior_del",
    "self_harm_3yrs_prior_del",
    "violence_3yrs_prior_del",
    "any_health_problem_3yrs_prior_case_start",
    "charlson_3yrs_prior_case_start",
    "disabilities_grant_any_3yrs_prior_case_start",
    "int_dis_3yrs_prior_case_start",
    "mhbev_any_3yrs_prior_case_start",
    "adversity_admission_3yrs_prior_case_start",
    "drug_alc_3yrs_prior_case_start",
    "self_harm_3yrs_prior_case_start",
    "violence_3yrs_prior_case_start",
    "cafcass_final_order"
  )


print("Overall")
write.csv(
  print(
    CreateTableOne(
      vars = vars,
      factorVars = vars,
      strata = "in_cafcass",
      data = deliveries,
      test = F,
      includeNA = T
    ),
    showAllLevels = F
  ),
  file = "output/descriptives_overall.csv"
)


print("Deaths (Comparator)")
write.csv(
  print(
    CreateTableOne(
      vars = vars,
      factorVars = vars,
      strata = "died_within_censor",
      data = deliveries[in_cafcass == F],
      test = F,
      includeNA = T
    ),
    showAllLevels = F
  ),
  file = "output/descriptives_deaths_comparison.csv"
)


print("Deaths (Cafcass)")
write.csv(
  print(
    CreateTableOne(
      vars = vars,
      factorVars = vars,
      strata = "died_within_censor",
      data = deliveries[in_cafcass == T],
      test = F
    ),
   showAllLevels = F
  ),
  file = "output/descriptives_deaths_cafcass.csv"
)

rm(vars)
