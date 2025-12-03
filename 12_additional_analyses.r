
print("Deaths with missing cause of death")
table(!is.na(deliveries$dod_apc) & is.na(deliveries$dod_ons))
table(!is.na(deliveries$dod_combined))
deliveries[, c("tokenid", "dod_combined", "dod_apc", "dod_ons", names(deliveries)[grepl("cause_of_death", names(deliveries))]), with = F]
deliveries[!is.na(dod_apc) & is.na(dod_ons),
           c("tokenid", "dod_combined", "dod_apc", "dod_ons", names(deliveries)[grepl("cause_of_death", names(deliveries))]), with = F]


print("Deaths with missing COD due to Coroners")
print("Re-establishing SQL server connection")
sql_channel <-
  dbConnect(
    RMySQL::MySQL(),
    username = "rmjlmaj",
    password = .rs.askForPassword("log in CPRU SQL server"),
    host = "dsh-00922msq01.idhs.ucl.ac.uk",
    port = 3306,
    dbname = "rekggth_hes_ons"
  )

ons_mort_data <-
  data.table(
    dbGetQuery(
      sql_channel,
      paste0(
        "select tokenid, record_id, ",
        "dod, dor, cause_of_death, subsequent_activity, ",
        paste0("cause_of_death_non_neonatal_", 1:15, collapse = ", "), " ",
        "from ons_full_2022"
      )
    )
  )

dbDisconnect(sql_channel)

ons_mort_data <- ons_mort_data[, c("tokenid", "record_id", "dod", "dor", "cause_of_death")]

# lt <- as.POSIXlt(ons_mort_data$dod)
# ons_mort_data[, death_fyear := lt$year + (lt$mo >= 3) + 1900 - 1] # -1 to make it fyear starting
# rm(lt)

ons_mort_data[, death_cyear := format(as.Date(dod), format = "%Y")]

ons_mort_data <- ons_mort_data[death_cyear <= 2020]

table(ons_mort_data$death_cyear, is.na(ons_mort_data$cause_of_death))


print("Time to first hearing")
cafcass_cohort[, yrs_to_case := as.integer(difftime(cafcass_case_start_date, delivery_date, units = "days")) / 365.25]
quantile(cafcass_cohort$yrs_to_case, probs = c(0.25, 0.50, 0.75))


print("Median follow-up time")
quantile(deliveries$overall_person_time_days / 365.25, probs = c(0.25, 0.50, 0.75))
quantile(deliveries[in_cafcass == F]$overall_person_time_days / 365.25, probs = c(0.25, 0.50, 0.75))
quantile(deliveries[in_cafcass == T]$overall_person_time_days / 365.25, probs = c(0.25, 0.50, 0.75))


print("Median age at death")
deliveries[, age_at_death := as.integer(difftime(dod_combined, dob_full, units = "days")) / 365.25]
quantile(deliveries[!is.na(dod_combined)]$age_at_death, probs = c(0.25, 0.50, 0.75))
quantile(deliveries[!is.na(dod_combined) & in_cafcass == F]$age_at_death, probs = c(0.25, 0.50, 0.75))
quantile(deliveries[!is.na(dod_combined) & in_cafcass == T]$age_at_death, probs = c(0.25, 0.50, 0.75))
