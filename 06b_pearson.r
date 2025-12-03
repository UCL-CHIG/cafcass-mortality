print(" ------ Pearson - MH/behavioural ------")
print("Loading and preparing code list")
mhbev_pearson <- fread("codelists/mhbev_pearson_v1.csv", stringsAsFactors = F)
mhbev_pearson[, code := gsub("\\.", "", code)]


print("Flagging episodes")

diagnoses_long[, anx_somat_stress := F]
diagnoses_long[, drug_alc := F]
diagnoses_long[, other_depr := F]
diagnoses_long[, other_pyschiatr := F]
diagnoses_long[, personality := F]
diagnoses_long[, psych_dev_beh_emo := F]
diagnoses_long[, schiz_del := F]
diagnoses_long[, sev_mood_dis := F]


print("anx_somat_stress") # No 4 char codes
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[group == "anx_somat_stress"]$code, anx_somat_stress := T]


print("drug_alc") # No 4 char codes
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[group == "drug_alc"]$code, drug_alc := T]


print("other_depr")
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[nchar(code) == 3 & group == "other_depr"]$code, other_depr := T]
diagnoses_long[substr(code, 1, 4) %in% mhbev_pearson[nchar(code) == 4 & group == "other_depr"]$code, other_depr := T]


print("other_pyschiatr")
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[nchar(code) == 3 & group == "other_pyschiatr"]$code, other_pyschiatr := T]
diagnoses_long[substr(code, 1, 4) %in% mhbev_pearson[nchar(code) == 4 & group == "other_pyschiatr"]$code, other_pyschiatr := T]


print("personality") # No 4 char codes
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[group == "personality"]$code, personality := T]


print("psych_dev_beh_emo") # No 4 char codes
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[group == "psych_dev_beh_emo"]$code, psych_dev_beh_emo := T]


print("schiz_del") # No 4 char codes
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[group == "schiz_del"]$code, schiz_del := T]


print("sev_mood_dis")
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[nchar(code) == 3 & group == "sev_mood_dis"]$code, sev_mood_dis := T]
diagnoses_long[substr(code, 1, 4) %in% mhbev_pearson[nchar(code) == 4 & group == "sev_mood_dis"]$code, sev_mood_dis := T]


print("Subsetting episodes")
diagnoses_mhbev <- diagnoses_long[anx_somat_stress | drug_alc | other_depr | other_pyschiatr |
                                    personality | psych_dev_beh_emo | schiz_del | sev_mood_dis]

diagnoses_long[, anx_somat_stress := NULL]
diagnoses_long[, drug_alc := NULL]
diagnoses_long[, other_depr := NULL]
diagnoses_long[, other_pyschiatr := NULL]
diagnoses_long[, personality := NULL]
diagnoses_long[, psych_dev_beh_emo := NULL]
diagnoses_long[, schiz_del := NULL]
diagnoses_long[, sev_mood_dis := NULL]
 

print("Identifying diagnoses in relevant window")
grps <- unique(mhbev_pearson$group)

for (grp in grps) {
  new_col <- paste0("mhbev_", grp, "_", years, "yrs_prior_case_start")
  diagnoses_mhbev[, (new_col) := epistart >= cafcass_case_start_date - 365 * years & epistart <= cafcass_case_start_date & get(grp) == T]
}


print("Merging flags into deliveries data")
dt_list_tmp <- list()

for (i in 1:length(grps)) {
  
  print(grps[i])
  curr_col <- names(diagnoses_mhbev)[grepl(paste0("mhbev_", grps[i], "_"), names(diagnoses_mhbev))]
  dt_list_tmp[[i]] <- diagnoses_mhbev[, c("tokenid", curr_col), with = F]
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


rm(grps, i, grp, curr_col, diagnoses_mhbev, mhbev_pearson, dt_list_tmp, new_col)


deliveries[, mhbev_any_3yrs_prior_case_start :=
             mhbev_anx_somat_stress_3yrs_prior_case_start |
             mhbev_drug_alc_3yrs_prior_case_start |
             mhbev_other_depr_3yrs_prior_case_start |
             mhbev_other_pyschiatr_3yrs_prior_case_start |
             mhbev_personality_3yrs_prior_case_start |
             mhbev_psych_dev_beh_emo_3yrs_prior_case_start |
             mhbev_schiz_del_3yrs_prior_case_start |
             mhbev_sev_mood_dis_3yrs_prior_case_start]

# table(deliveries[!is.na(cafcass_person_id)]$mhbev_anx_somat_stress_3yrs_prior_case_start, useNA = "always")
# table(deliveries[!is.na(cafcass_person_id)]$mhbev_drug_alc_3yrs_prior_case_start, useNA = "always")
# table(deliveries[!is.na(cafcass_person_id)]$mhbev_other_depr_3yrs_prior_case_start, useNA = "always")
# table(deliveries[!is.na(cafcass_person_id)]$mhbev_other_pyschiatr_3yrs_prior_case_start, useNA = "always")
# table(deliveries[!is.na(cafcass_person_id)]$mhbev_personality_3yrs_prior_case_start, useNA = "always")
# table(deliveries[!is.na(cafcass_person_id)]$mhbev_psych_dev_beh_emo_3yrs_prior_case_start, useNA = "always")
# table(deliveries[!is.na(cafcass_person_id)]$mhbev_schiz_del_3yrs_prior_case_start, useNA = "always")
# table(deliveries[!is.na(cafcass_person_id)]$mhbev_sev_mood_dis_3yrs_prior_case_start, useNA = "always")
# table(deliveries[!is.na(cafcass_person_id)]$mhbev_any_3yrs_prior_case_start, useNA = "always")
