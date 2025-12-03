print(" ------ Sheearn intellectual disability ------ ")

int_dis_codes <- fread("codelists/id_sheehan_v1.csv", stringsAsFactors = F)
int_dis_codes[, code := gsub("\\.", "", code)]

diagnoses_long[, int_dis := F]
diagnoses_long[substr(code, 1, 3) %in% int_dis_codes[nchar(code) == 3]$code, int_dis := T]
diagnoses_long[substr(code, 1, 4) %in% int_dis_codes[nchar(code) == 4]$code, int_dis := T]
diagnoses_int_dis <- diagnoses_long[int_dis == T]

diagnoses_long[, int_dis := NULL]
diagnoses_int_dis[, int_dis := NULL]


print("Identifying diagnoses in relevant window")
new_col <- paste0("int_dis_", years, "yrs_prior_case_start")
diagnoses_int_dis[, (new_col) := epistart >= cafcass_case_start_date - 365 * years & epistart <= cafcass_case_start_date]

diagnoses_int_dis <- diagnoses_int_dis[, c("tokenid", new_col), with = F]
diagnoses_int_dis[, (new_col) := max(get(new_col)), by = tokenid]
diagnoses_int_dis <- diagnoses_int_dis[!duplicated(diagnoses_int_dis)]


print("Merging flags into deliveries data")
deliveries <-
  merge(
    deliveries,
    diagnoses_int_dis,
    by = "tokenid",
    all.x = T
  )

deliveries[!is.na(cafcass_person_id) & is.na(get(new_col)), (new_col) := FALSE]

print("Removing temporary data")
rm(int_dis_codes, diagnoses_int_dis, new_col)
