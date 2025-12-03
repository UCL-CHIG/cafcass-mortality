print(" ------ Charlson ------")

charlson_codes <- fread("codelists/charlson_quan_v1.csv", stringsAsFactors = F)
charlson_codes[, code := gsub("\\.", "", code)]

diagnoses_long[, charlson := F]
diagnoses_long[substr(code, 1, 3) %in% charlson_codes[nchar(code) == 3]$code, charlson := T]
diagnoses_long[substr(code, 1, 4) %in% charlson_codes[nchar(code) == 4]$code, charlson := T]
diagnoses_charlson <- diagnoses_long[charlson == T]

diagnoses_long[, charlson := NULL]
diagnoses_charlson[, charlson := NULL]


print("Identifying diagnoses in relevant window")
new_col <- paste0("charlson_", years, "yrs_prior_case_start")
diagnoses_charlson[, (new_col) := epistart >= cafcass_case_start_date - 365 * years & epistart <= cafcass_case_start_date]

diagnoses_charlson <- diagnoses_charlson[, c("tokenid", new_col), with = F]
diagnoses_charlson[, (new_col) := max(get(new_col)), by = tokenid]
diagnoses_charlson <- diagnoses_charlson[!duplicated(diagnoses_charlson)]


print("Merging flags into deliveries data")
deliveries <-
  merge(
    deliveries,
    diagnoses_charlson,
    by = "tokenid",
    all.x = T
  )

deliveries[!is.na(cafcass_person_id) & is.na(get(new_col)), (new_col) := FALSE]


print("Removing temporary data")
rm(charlson_codes, diagnoses_charlson, new_col)
