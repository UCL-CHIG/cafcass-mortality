
print(" ------ ARIs/ARAs ------")
print("Loading and preparing code list")

herbert <- fread("codelists/ari_herbert_v1.csv", stringsAsFactors = F)
herbert[, code := gsub("\\.", "", code)]

new_f17 <- data.table(
  code = c("F17.0", paste0("F17.", 2:9)),
  dataset = "hes_apc",
  field = "diag",
  code_type = "icd10",
  description = "smoking_codes",
  group = "drug_alc",
  subgroup = "illict_drugs",
  flag1 = "emergency_admission",
  flag2 = ""
)

herbert <- rbind(herbert, new_f17)
herbert <- herbert[code != "F17"]
rm(new_f17)


print("Flagging episodes")
diagnoses_long[, first_episode := admidate == epistart]

diagnoses_long[, injury_episode := F]
diagnoses_long[, adversity_episode := F]
diagnoses_long[, accident_episode := F]

diagnoses_long[substr(code, 1, 3) %in% herbert[group == "injuries"]$code, injury_episode := T]
diagnoses_long[injury_episode == T & first_episode == F, injury_episode := F]

diagnoses_long[substr(code, 1, 3) %in% herbert[group == "drug_alc" & nchar(code) == 3]$code, drug_alc_episode := T]
diagnoses_long[substr(code, 1, 4) %in% herbert[group == "drug_alc" & nchar(code) == 4]$code, drug_alc_episode := T]

diagnoses_long[substr(code, 1, 3) %in% herbert[group == "self-harm" & nchar(code) == 3]$code, self_harm_episode := T]
diagnoses_long[substr(code, 1, 4) %in% herbert[group == "self-harm" & nchar(code) == 4]$code, self_harm_episode := T]

diagnoses_long[substr(code, 1, 3) %in% herbert[group == "violence" & nchar(code) == 3]$code, violence_episode := T]
diagnoses_long[substr(code, 1, 4) %in% herbert[group == "violence" & nchar(code) == 4]$code, violence_episode := T]

diagnoses_long[, adversity_episode := drug_alc_episode | self_harm_episode | violence_episode]

diagnoses_long[substr(code, 1, 3) %in% herbert[group == "accidents"]$code, accident_episode := T]


print("Subsetting episodes")
em_adm <- fread("codelists/emergency_admissions_v1.csv")$code
diagnoses_herbert <- diagnoses_long[(injury_episode | adversity_episode | accident_episode) & admimeth %in% em_adm]

diagnoses_long[, first_episode := NULL]
diagnoses_long[, injury_episode := NULL]
diagnoses_long[, adversity_episode := NULL]
diagnoses_long[, drug_alc_episode := NULL]
diagnoses_long[, self_harm_episode := NULL]
diagnoses_long[, violence_episode := NULL]
diagnoses_long[, accident_episode := NULL]
rm(em_adm)


diagnoses_herbert[, injury_admission := as.logical(max(injury_episode)), by = .(tokenid, admidate)]
diagnoses_herbert[, adversity_admission := as.logical(max(adversity_episode)), by = .(tokenid, admidate)]
diagnoses_herbert[, accident_admission := as.logical(max(accident_episode)), by = .(tokenid, admidate)]
diagnoses_herbert[, drug_alc_admission := as.logical(max(drug_alc_episode)), by = .(tokenid, admidate)]
diagnoses_herbert[, self_harm_admission := as.logical(max(self_harm_episode)), by = .(tokenid, admidate)]
diagnoses_herbert[, violence_admission := as.logical(max(violence_episode)), by = .(tokenid, admidate)]
diagnoses_herbert[, adversity_admission := as.logical(max(adversity_episode)), by = .(tokenid, admidate)]

diagnoses_herbert[, adversity_injury_admission :=
                    injury_admission &
                    adversity_admission]

diagnoses_herbert[, accident_injury_admission :=
                    injury_admission &
                    !adversity_admission &
                    accident_admission]

diagnoses_herbert <- diagnoses_herbert[adversity_injury_admission == T |
                                         accident_injury_admission == T |
                                         adversity_admission == T]


print("Identifying diagnoses in relevant window")
new_col <- paste0("adversity_admission_", years, "yr_prior_case_start")
diagnoses_herbert[, (new_col) := epistart >= cafcass_case_start_date - (365 * years) & epistart <= cafcass_case_start_date & adversity_admission == T]

new_col <- paste0("drug_alc_admission_", years, "yr_prior_case_start")
diagnoses_herbert[, (new_col) := epistart >= cafcass_case_start_date - (365 * years) & epistart <= cafcass_case_start_date & drug_alc_admission == T]

new_col <- paste0("self_harm_admission_", years, "yr_prior_case_start")
diagnoses_herbert[, (new_col) := epistart >= cafcass_case_start_date - (365 * years) & epistart <= cafcass_case_start_date & self_harm_admission == T]

new_col <- paste0("violence_admission_", years, "yr_prior_case_start")
diagnoses_herbert[, (new_col) := epistart >= cafcass_case_start_date - (365 * years) & epistart <= cafcass_case_start_date & violence_admission == T]

new_col <- paste0("adversity_injury_admission_", years, "yr_prior_case_start")
diagnoses_herbert[, (new_col) := epistart >= cafcass_case_start_date - (365 * years) & epistart <= cafcass_case_start_date & adversity_injury_admission == T]

new_col <- paste0("accident_injury_admission_", years, "yr_prior_case_start")
diagnoses_herbert[, (new_col) := epistart >= cafcass_case_start_date - (365 * years) & epistart <= cafcass_case_start_date & accident_injury_admission == T]

rm(new_col)


print("Merging flags into deliveries data")
diagnoses_herbert_adv_inj <- diagnoses_herbert[, c("tokenid", names(diagnoses_herbert)[grepl("adversity_injury_admission_", names(diagnoses_herbert))]), with = F]
diagnoses_herbert_adv_adm <- diagnoses_herbert[, c("tokenid", names(diagnoses_herbert)[grepl("adversity_admission_", names(diagnoses_herbert))]), with = F]
diagnoses_herbert_acc_inj <- diagnoses_herbert[, c("tokenid", names(diagnoses_herbert)[grepl("accident_injury_admission_", names(diagnoses_herbert))]), with = F]
diagnoses_herbert_drug_alc <- diagnoses_herbert[, c("tokenid", names(diagnoses_herbert)[grepl("drug_alc_admission_", names(diagnoses_herbert))]), with = F]
diagnoses_herbert_self_harm <- diagnoses_herbert[, c("tokenid", names(diagnoses_herbert)[grepl("self_harm_admission_", names(diagnoses_herbert))]), with = F]
diagnoses_herbert_violence <- diagnoses_herbert[, c("tokenid", names(diagnoses_herbert)[grepl("violence_admission_", names(diagnoses_herbert))]), with = F]

diagnoses_herbert_adv_inj[, adversity_injury_admission_3yr_prior_case_start := max(adversity_injury_admission_3yr_prior_case_start), by = tokenid]
diagnoses_herbert_adv_adm[, adversity_admission_3yr_prior_case_start := max(adversity_admission_3yr_prior_case_start), by = tokenid]
diagnoses_herbert_acc_inj[, accident_injury_admission_3yr_prior_case_start := max(accident_injury_admission_3yr_prior_case_start), by = tokenid]
diagnoses_herbert_drug_alc[, drug_alc_admission_3yr_prior_case_start := max(drug_alc_admission_3yr_prior_case_start), by = tokenid]
diagnoses_herbert_self_harm[, self_harm_admission_3yr_prior_case_start := max(self_harm_admission_3yr_prior_case_start), by = tokenid]
diagnoses_herbert_violence[, violence_admission_3yr_prior_case_start := max(violence_admission_3yr_prior_case_start), by = tokenid]

diagnoses_herbert_adv_inj <- diagnoses_herbert_adv_inj[!duplicated(diagnoses_herbert_adv_inj)]
diagnoses_herbert_adv_adm <- diagnoses_herbert_adv_adm[!duplicated(diagnoses_herbert_adv_adm)]
diagnoses_herbert_acc_inj <- diagnoses_herbert_acc_inj[!duplicated(diagnoses_herbert_acc_inj)]
diagnoses_herbert_drug_alc <- diagnoses_herbert_drug_alc[!duplicated(diagnoses_herbert_drug_alc)]
diagnoses_herbert_self_harm <- diagnoses_herbert_self_harm[!duplicated(diagnoses_herbert_self_harm)]
diagnoses_herbert_violence <- diagnoses_herbert_violence[!duplicated(diagnoses_herbert_violence)]

setnames(diagnoses_herbert_adv_inj, "adversity_injury_admission_3yr_prior_case_start", "adversity_injury_3yrs_prior_case_start")
setnames(diagnoses_herbert_adv_adm, "adversity_admission_3yr_prior_case_start", "adversity_admission_3yrs_prior_case_start")
setnames(diagnoses_herbert_acc_inj, "accident_injury_admission_3yr_prior_case_start", "accident_injury_3yrs_prior_case_start")
setnames(diagnoses_herbert_drug_alc, "drug_alc_admission_3yr_prior_case_start", "drug_alc_3yrs_prior_case_start")
setnames(diagnoses_herbert_self_harm, "self_harm_admission_3yr_prior_case_start", "self_harm_3yrs_prior_case_start")
setnames(diagnoses_herbert_violence, "violence_admission_3yr_prior_case_start", "violence_3yrs_prior_case_start")

deliveries <-
  merge(
    deliveries,
    diagnoses_herbert_adv_inj,
    by = "tokenid",
    all.x = T
  )

deliveries[!is.na(cafcass_person_id) &
             is.na(get(paste0("adversity_injury_", years, "yrs_prior_case_start"))),
           (paste0("adversity_injury_", years, "yrs_prior_case_start")) := FALSE]


deliveries <-
  merge(
    deliveries,
    diagnoses_herbert_adv_adm,
    by = "tokenid",
    all.x = T
  )

deliveries[!is.na(cafcass_person_id) &
             is.na(get(paste0("adversity_admission_", years, "yrs_prior_case_start"))),
           (paste0("adversity_admission_", years, "yrs_prior_case_start")) := FALSE]


deliveries <-
  merge(
    deliveries,
    diagnoses_herbert_acc_inj,
    by = "tokenid",
    all.x = T
  )

deliveries[!is.na(cafcass_person_id) &
             is.na(get(paste0("accident_injury_", years, "yrs_prior_case_start"))),
           (paste0("accident_injury_", years, "yrs_prior_case_start")) := FALSE]


deliveries <-
  merge(
    deliveries,
    diagnoses_herbert_drug_alc,
    by = "tokenid",
    all.x = T
  )

deliveries[!is.na(cafcass_person_id) &
             is.na(get(paste0("drug_alc_", years, "yrs_prior_case_start"))),
           (paste0("drug_alc_", years, "yrs_prior_case_start")) := FALSE]


deliveries <-
  merge(
    deliveries,
    diagnoses_herbert_self_harm,
    by = "tokenid",
    all.x = T
  )

deliveries[!is.na(cafcass_person_id) &
             is.na(get(paste0("self_harm_", years, "yrs_prior_case_start"))),
           (paste0("self_harm_", years, "yrs_prior_case_start")) := FALSE]


deliveries <-
  merge(
    deliveries,
    diagnoses_herbert_violence,
    by = "tokenid",
    all.x = T
  )

deliveries[!is.na(cafcass_person_id) &
             is.na(get(paste0("violence_", years, "yrs_prior_case_start"))),
           (paste0("violence_", years, "yrs_prior_case_start")) := FALSE]



print("Removing temporary data")
rm(herbert, diagnoses_herbert, diagnoses_herbert_adv_inj, diagnoses_herbert_adv_adm,
   diagnoses_herbert_acc_inj)
