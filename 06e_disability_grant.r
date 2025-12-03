print(" ------ Grant disability ------ ")
print("Loading and preparing code list")
grant <- fread("codelists/disability_grant_v1.csv", stringsAsFactors = F)
grant[, code := gsub("\\.", "", code)]


print("Flagging episodes")

diagnoses_long[, physical := F]
diagnoses_long[, hearing_impairments := F]
diagnoses_long[, vision_impairments := F]
diagnoses_long[, developmental_disabilities := F]


print("physical")
diagnoses_long[substr(code, 1, 3) %in% grant[nchar(code) == 3 & group == "physical"]$code, physical := T]
diagnoses_long[substr(code, 1, 4) %in% grant[nchar(code) == 4 & group == "physical"]$code, physical := T]


print("hearing_impairments")
diagnoses_long[substr(code, 1, 3) %in% grant[nchar(code) == 3 & group == "hearing_impairments"]$code, hearing_impairments := T]
diagnoses_long[substr(code, 1, 4) %in% grant[nchar(code) == 4 & group == "hearing_impairments"]$code, hearing_impairments := T]


print("vision_impairments")
diagnoses_long[substr(code, 1, 3) %in% grant[nchar(code) == 3 & group == "vision_impairments"]$code, vision_impairments := T]
diagnoses_long[substr(code, 1, 4) %in% grant[nchar(code) == 4 & group == "vision_impairments"]$code, vision_impairments := T]


print("developmental_disabilities")
diagnoses_long[substr(code, 1, 3) %in% grant[nchar(code) == 3 & group == "developmental_disabilities"]$code, developmental_disabilities := T]
diagnoses_long[substr(code, 1, 4) %in% grant[nchar(code) == 4 & group == "developmental_disabilities"]$code, developmental_disabilities := T]



print("Subsetting episodes")
diagnoses_grant <- diagnoses_long[physical | hearing_impairments | vision_impairments | developmental_disabilities]

diagnoses_long[, physical := NULL]
diagnoses_long[, hearing_impairments := NULL]
diagnoses_long[, vision_impairments := NULL]
diagnoses_long[, developmental_disabilities := NULL]


print("Identifying diagnoses in relevant window")
grps <- unique(grant$group)


for (grp in grps) {
  new_col <- paste0("disabilities_grant_", grp, "_", years, "yrs_prior_case_start")
  diagnoses_grant[, (new_col) := epistart >= cafcass_case_start_date - 365 * years & epistart <= cafcass_case_start_date & get(grp) == T]
}


print("Merging flags into deliveries data")
dt_list_tmp <- list()

for (i in 1:length(grps)) {
  
  print(grps[i])
  curr_col <- names(diagnoses_grant)[grepl(paste0("disabilities_grant_", grps[i], "_"), names(diagnoses_grant))]
  dt_list_tmp[[i]] <- diagnoses_grant[, c("tokenid", curr_col), with = F]
  dt_list_tmp[[i]][, (curr_col) := max(get(curr_col)), by = tokenid]
  dt_list_tmp[[i]] <- dt_list_tmp[[i]][!duplicated(dt_list_tmp[[i]])]
  
  deliveries <-
    merge(
      deliveries,
      dt_list_tmp[[i]],
      by = "tokenid",
      all.x = T
    )
  
  deliveries[!is.na(cafcass_person_id) & is.na(get(curr_col)), (curr_col) := FALSE]
  
}

rm(grps, i, grp, curr_col, diagnoses_grant, grant, dt_list_tmp, new_col)

deliveries[, disabilities_grant_any_3yrs_prior_case_start :=
             disabilities_grant_physical_3yrs_prior_case_start |
             disabilities_grant_hearing_impairments_3yrs_prior_case_start |
             disabilities_grant_vision_impairments_3yrs_prior_case_start |
             disabilities_grant_developmental_disabilities_3yrs_prior_case_start]

table(deliveries$disabilities_grant_any_3yrs_prior_case_start, useNA = "always")
table(deliveries$disabilities_grant_physical_3yrs_prior_case_start, useNA = "always")
table(deliveries$disabilities_grant_hearing_impairments_3yrs_prior_case_start, useNA = "always")
table(deliveries$disabilities_grant_vision_impairments_3yrs_prior_case_start, useNA = "always")
table(deliveries$disabilities_grant_developmental_disabilities_3yrs_prior_case_start, useNA = "always")
