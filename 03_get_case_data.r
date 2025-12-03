
print("Merging case data")

print("Mother start age and IMD")

case_adult <- case_adult[adult_id %in% deliveries$cafcass_person_id]
case <- case[case_id %in% case_adult$case_id]

case <-
  merge(
    case,
    case_adult[, c("case_id", "adult_id", "first_app_start_age", "address_imd_quintile")],
    by = "case_id",
    all.x = T
  )

print("Number of hearings and number of courts")
hearing <-
  fread(
    paste0(cafcass_data_dir, "hearing.csv"),
    stringsAsFactors = F
  )

hearing <- hearing[case_id %in% case$case_id]
hearing <- hearing[order(case_id, hearing_date)]
# hearing[, n_hearing := length(unique(hearing_date)), by = case_id]
hearing[, court_count := length(unique(court_id)), by = case_id]
hearing <- hearing[, c("case_id", "court_count")]
hearing <- hearing[!duplicated(hearing)]

case <-
  merge(
    case,
    hearing,
    by = "case_id",
    all.x = T
  )

rm(hearing)


print("Tidying")

case <- case[, c("adult_id",
                 "case_id",
                 "la_id",
                 "first_court_id",
                 "first_dfj_area_id",
                 "first_circuit_id",
                 "last_court_id",
                 "last_dfj_area_id",
                 "last_circuit_id",
                 "case_start_date",
                 "case_start_fyear_starting",
                 "first_app_start_age",
                 "address_imd_quintile",
                 "hearing_first_date",
                 "hearing_last_date",
                 "hearing_count",
                 "court_count",
                 "final_order_date",
                 "any_final_order_dis_ono",
                 "any_final_order_fao_so",
                 "any_final_order_ro_sgo_cao",
                 "any_final_order_co_sao",
                 "any_final_order_po")]

case <- case[order(adult_id, case_start_date)]

case[, end_date := final_order_date]
case[is.na(final_order_date), end_date := hearing_last_date]

case[, case_indx := seq_len(.N), by = adult_id]


print("Cleaning legal output")
case[, final_order := factor(NA,
                             levels = c("Discharge/ONO",
                                        "FAO/SO",
                                        "RO/SGO/CAO",
                                        "CO/SAO",
                                        "PO",
                                        "Not reported"))]

case[any_final_order_dis_ono == 1, final_order := "Discharge/ONO"]
case[is.na(final_order) & any_final_order_fao_so == 1, final_order := "FAO/SO"]
case[is.na(final_order) & any_final_order_ro_sgo_cao == 1, final_order := "RO/SGO/CAO"]
case[is.na(final_order) & any_final_order_co_sao == 1, final_order := "CO/SAO"]
case[is.na(final_order) & any_final_order_po == 1, final_order := "PO"]
case[is.na(final_order), final_order := "Not reported"]

case[, any_final_order_dis_ono := NULL]
case[, any_final_order_fao_so := NULL]
case[, any_final_order_ro_sgo_cao := NULL]
case[, any_final_order_co_sao := NULL]
case[, any_final_order_po := NULL]


print("Creating case index and subsetting to first")
first_case <- case[case_indx == 1]
first_case[, case_indx := NULL]
first_case[, adult_id := as.character(adult_id)]

setnames(first_case, names(first_case), paste0("cafcass_", names(first_case)))


print("Merging into deliveries spine")
deliveries <-
  merge(
    deliveries,
    first_case,
    by.x = "cafcass_person_id",
    by.y = "cafcass_adult_id",
    all.x = T
  )

print("Fixing dates")
deliveries[, cafcass_case_start_date := as.Date(cafcass_case_start_date)]
deliveries[, cafcass_hearing_first_date := as.Date(cafcass_hearing_first_date)]
deliveries[, cafcass_hearing_last_date := as.Date(cafcass_hearing_last_date)]
deliveries[, cafcass_final_order_date := as.Date(cafcass_final_order_date)]
deliveries[, cafcass_end_date := as.Date(cafcass_end_date)]

rm(case, case_adult, case_app, first_case, public_apps)

