
print("Coding causes of death")
mortality_codes <- fread("codelists/mortality_code_list.csv", stringsAsFactors = F)
mortality_codes[, code := gsub("\\.", "", code)]

cause_cols <- names(deliveries)[grepl("cause_of_death", names(deliveries))]

causes <- deliveries[, c("epikey", cause_cols), with = F]
setnames(causes, "cause_of_death", "cause_of_death_0")

causes <-
  melt(
    causes,
    id.vars = "epikey",
    variable.name = "cause_n",
    value.name = "code"
  )

causes <- causes[code != ""]

causes[, include := F]
causes[, diag_for_link := code]

causes[substr(code, 1, 3) %in% mortality_codes[nchar(code) == 3]$code, include := T]
causes[substr(code, 1, 3) %in% mortality_codes[nchar(code) == 3]$code, diag_for_link := substr(code, 1, 3)]

causes[substr(code, 1, 4) %in% mortality_codes[nchar(code) == 4]$code, include := T]
causes[substr(code, 1, 4) %in% mortality_codes[nchar(code) == 4]$code, diag_for_link := substr(code, 1, 4)]

causes <- causes[include == T]

causes <-
  merge(
    causes,
    mortality_codes,
    by.x = "diag_for_link",
    by.y = "code",
    all.x = T
  )

deliveries[!is.na(dod_ons), cause_of_death_suicide := epikey %in% causes[group2 == "suicide"]$epikey]
deliveries[!is.na(dod_ons), cause_of_death_homicide := epikey %in% causes[group2 == "homicide"]$epikey]
deliveries[!is.na(dod_ons), cause_of_death_alcohol := epikey %in% causes[group2 == "alcohol"]$epikey]
deliveries[!is.na(dod_ons), cause_of_death_drug := epikey %in% causes[group2 == "drug"]$epikey]
deliveries[!is.na(dod_ons), cause_of_death_accidental := epikey %in% causes[group2 == "accidental"]$epikey]

deliveries[!is.na(dod_ons), cause_of_death_medical_other := T]
deliveries[cause_of_death_medical_other == T & (cause_of_death_suicide | cause_of_death_homicide | cause_of_death_alcohol |
             cause_of_death_drug | cause_of_death_accidental), cause_of_death_medical_other := F]

deliveries[, cause_of_death_adversity := NA]
deliveries[!is.na(dod_ons), cause_of_death_adversity := F]
deliveries[cause_of_death_suicide | cause_of_death_homicide | cause_of_death_alcohol |
             cause_of_death_drug, cause_of_death_adversity := T]

deliveries[, cause_of_death_cat := factor(NA,
                                          levels = c("adversity",
                                                     "accident",
                                                     "medical/other/not recorded"))]

deliveries[!is.na(dod_ons) | (is.na(dod_ons) & !is.na(dod_apc)), cause_of_death_cat := "medical/other/not recorded"]
deliveries[!is.na(dod_ons) & cause_of_death_accidental & !cause_of_death_adversity, cause_of_death_cat := "accident"]
deliveries[!is.na(dod_ons) & cause_of_death_adversity, cause_of_death_cat := "adversity"]

deliveries[, cause_of_death_cat_detailed := factor(NA,
                                                   levels = c("suicide",
                                                              "homicide",
                                                              "drugs/alcohol",
                                                              "accident",
                                                              "medical/other/not recorded"))]

deliveries[!is.na(dod_ons) | (is.na(dod_ons) & !is.na(dod_apc)), cause_of_death_cat_detailed := "medical/other/not recorded"]
deliveries[!is.na(dod_ons) & cause_of_death_accidental & !cause_of_death_adversity, cause_of_death_cat_detailed := "accident"]
deliveries[!is.na(dod_ons) & (cause_of_death_drug | cause_of_death_alcohol), cause_of_death_cat_detailed := "drugs/alcohol"]
deliveries[!is.na(dod_ons) & cause_of_death_homicide, cause_of_death_cat_detailed := "homicide"]
deliveries[!is.na(dod_ons) & cause_of_death_suicide, cause_of_death_cat_detailed := "suicide"]

rm(mortality_codes, cause_cols, causes)

