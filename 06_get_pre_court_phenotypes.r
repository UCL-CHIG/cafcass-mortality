
years <- 3


print("Loading diagnoses file")
diagnoses_long <- fread(
  paste0(del_data_dir, "/processed/diagnoses_long.csv"),
  stringsAsFactors = F
)

diagnoses_long[, epistart := as.Date(epistart)]
diagnoses_long[, admidate := as.Date(admidate)]

print("Subsetting to those with Cafcass data")
diagnoses_long <- diagnoses_long[tokenid %in% deliveries[!is.na(cafcass_person_id)]$tokenid]


print("Merging in case start date")
diagnoses_long <-
  merge(
    diagnoses_long,
    deliveries[, c("tokenid", "cafcass_case_start_date")],
    by = "tokenid",
    all.x = T
  )

diagnoses_long <- diagnoses_long[order(tokenid, admidate, epistart)]


source("scripts/06a_ari.r")
source("scripts/06b_pearson.r")
source("scripts/06c_charlson.r")
source("scripts/06d_id_sheehan.r")
source("scripts/06e_disability_grant.r")

deliveries[, any_health_problem_3yrs_prior_case_start := charlson_3yrs_prior_case_start | disabilities_grant_any_3yrs_prior_case_start | int_dis_3yrs_prior_case_start |
             mhbev_any_3yrs_prior_case_start | adversity_admission_3yrs_prior_case_start]


rm(diagnoses_long, years)
