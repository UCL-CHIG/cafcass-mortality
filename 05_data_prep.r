
print("Deriving new variables")
print("Age category")
deliveries[, age_cat_at_delivery := factor(NA,
                                           levels = c("<20",
                                                      "20-24",
                                                      "25-29",
                                                      "30-34",
                                                      "35-39"))]

deliveries[startage < 20, age_cat_at_delivery := "<20"]
deliveries[startage >= 20 & startage <= 24, age_cat_at_delivery := "20-24"]
deliveries[startage >= 25 & startage <= 29, age_cat_at_delivery := "25-29"]
deliveries[startage >= 30 & startage <= 34, age_cat_at_delivery := "30-34"]
deliveries[startage >= 35, age_cat_at_delivery := "35-39"]


print("Ethnicity")
deliveries[, eth_cat := factor(NA,
                               levels = c("White",
                                          "Black",
                                          "Mixed",
                                          "Asian",
                                          "Other",
                                          "Unknown"))]

white_val <- c("A", "B", "C", 0)
black_val <- c("M", "N", "P", 1:3)
mixed_val <- c("D", "E", "F", "G")
asian_val <- c("H", "J", "K", "L", 4:6)
other_val <- c("R", "S", 7, 8)
unknown_val <- c("Z", "X", 9, 99, "")

deliveries[ethnos %in% white_val, eth_cat := "White"]
deliveries[ethnos %in% black_val, eth_cat := "Black"]
deliveries[ethnos %in% mixed_val, eth_cat := "Mixed"]
deliveries[ethnos %in% asian_val, eth_cat := "Asian"]
deliveries[ethnos %in% other_val, eth_cat := "Other"]
deliveries[ethnos %in% unknown_val, eth_cat := "Unknown"]

rm(white_val, black_val, mixed_val, asian_val, other_val, unknown_val)

print("IMD")
deliveries[, imd_quintile_cat := integer(0)]
deliveries[imd04_decile_cat %in% 1:2, imd_quintile_cat := 1]
deliveries[imd04_decile_cat %in% 3:4, imd_quintile_cat := 2]
deliveries[imd04_decile_cat %in% 5:6, imd_quintile_cat := 3]
deliveries[imd04_decile_cat %in% 7:8, imd_quintile_cat := 4]
deliveries[imd04_decile_cat %in% 9:10, imd_quintile_cat := 5]
deliveries[, imd_quintile_cat := as.factor(imd_quintile_cat)]

print("Year")
deliveries[, delivery_fyear_cat := factor(NA,
                                          levels = c("2007-2009",
                                                     "2010-2012",
                                                     "2013-2015",
                                                     "2016-2017"))]

deliveries[delivery_fyear %in% 2007:2009, delivery_fyear_cat := "2007-2009"]
deliveries[delivery_fyear %in% 2010:2012, delivery_fyear_cat := "2010-2012"]
deliveries[delivery_fyear %in% 2013:2015, delivery_fyear_cat := "2013-2015"]
deliveries[delivery_fyear %in% 2016:2017, delivery_fyear_cat := "2016-2017"]


print("Health conditions")
deliveries[, any_health_problem_3yrs_prior_del := charlson_3yrs_prior_del | disabilities_grant_any_3yrs_prior_del | int_dis_3yrs_prior_del |
             mhbev_any_3yrs_prior_del | adversity_admission_3yrs_prior_del]

# deliveries[, health_problem_hier := factor(NA, levels = c("None",
#                                                      "Long-term condition (Charlson)",
#                                                      "Physical or sensory disability (Grant)",
#                                                      "Intellectual disability (Sheehan)"))]
# 
# deliveries[int_dis_3yrs_prior_del == T, health_problem_hier := "Intellectual disability (Sheehan)"]
# deliveries[is.na(health_problem_hier) & disabilities_grant_any_3yrs_prior_del == T, health_problem_hier := "Physical or sensory disability (Grant)"]
# deliveries[is.na(health_problem_hier) & charlson_3yrs_prior_del == T, health_problem_hier := "Long-term condition (Charlson)"]
# deliveries[is.na(health_problem_hier), health_problem_hier := "None"]


print("Creating analysis dataset")
cols_to_include <-
  c(
    "tokenid",
    "epikey",
    "dob_full",
    "startage",
    "age_cat_at_delivery",
    "eth_cat",
    "imd_quintile_cat",
    "delivery_date",
    "delivery_fyear",
    "delivery_fyear_cat",
    "delivery_n",
    names(deliveries)[grepl("3yrs_prior_del", names(deliveries))],
    "teenage_mother",
    "teenage_mother_ever",
    "dod_ons",
    "dod_apc",
    "dod_combined",
    names(deliveries)[grepl("cause_of_death", names(deliveries))],
    names(deliveries)[grepl("cafcass_", names(deliveries))]
  )

deliveries <- deliveries[, cols_to_include, with = F]
rm(cols_to_include)


print("Creating censor variable")
deliveries[, date_10_yr_after_birth := delivery_date]
year(deliveries$date_10_yr_after_birth) <- year(deliveries$date_10_yr_after_birth) + 10

# There are some 29/02, of which +10 years do not exist
deliveries[is.na(date_10_yr_after_birth), fix_date := T]
day(deliveries[fix_date == T]$delivery_date) <- day(deliveries[fix_date == T]$delivery_date) - 1
deliveries[fix_date == T, date_10_yr_after_birth := delivery_date]
year(deliveries[fix_date == T]$date_10_yr_after_birth) <- year(deliveries[fix_date == T]$date_10_yr_after_birth) + 10
day(deliveries[fix_date == T]$delivery_date) <- day(deliveries[fix_date == T]$delivery_date) + 1
deliveries[, fix_date := NULL]

deliveries[, birthday_51 := dob_full]
year(deliveries$birthday_51) <- year(deliveries$birthday_51) + 51

deliveries[, censor_date := as.Date("2021-03-31")]

deliveries[, censor_date := min(censor_date, birthday_51, date_10_yr_after_birth), by = 1:nrow(deliveries)]


print("Suppressing Cafcass and death after censor date")
cafcass_cols <- names(deliveries)[grepl("cafcass_", names(deliveries))]
deliveries[cafcass_case_start_date >= censor_date, in_cafcass := F]
deliveries[cafcass_case_start_date >= censor_date, (cafcass_cols) := NA]
rm(cafcass_cols)

deliveries[, in_cafcass := !is.na(cafcass_person_id)]

death_cols <- c("dod_combined", "dod_apc", "dod_ons", names(deliveries)[grepl("cause_of_death", names(deliveries))])
deliveries[dod_combined >= censor_date, (death_cols) := NA]
rm(death_cols)

deliveries[, died_within_censor := F]
deliveries[!is.na(dod_combined), died_within_censor := T]


print("Deriving status variables")
deliveries[, status_date := censor_date]
deliveries[!is.na(dod_combined), status_date := dod_combined]

deliveries[, status_cat := factor("did_not_die",
                              levels = c("did_not_die",
                                         "died_unexposed",
                                         "died_exposed"))]

deliveries[in_cafcass == F & !is.na(dod_combined), status_cat := "died_unexposed"]
deliveries[in_cafcass == T & !is.na(dod_combined), status_cat := "died_exposed"]


print("Calculating person-time")
deliveries[, person_time_unexposed_days := as.integer(difftime(status_date, delivery_date, units = "days"))]
deliveries[in_cafcass == T, person_time_unexposed_days := as.integer(difftime(cafcass_case_start_date, delivery_date, units = "days"))]
deliveries[in_cafcass == T, person_time_exposed_days := as.integer(difftime(status_date, cafcass_case_start_date, units = "days"))]
deliveries[, overall_person_time_days := as.integer(difftime(status_date, delivery_date, units = "days"))]

