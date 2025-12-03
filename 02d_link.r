
print("Identifying eligible cases")
case <-
  fread(
    paste0(cafcass_data_dir, "case.csv"),
    stringsAsFactors = F
  )

case_app <-
  fread(
    paste0(cafcass_data_dir, "case_app.csv"),
    stringsAsFactors = F
  )

lt <- as.POSIXlt(case$case_start_date)
case[, case_start_fyear_starting := lt$year + (lt$mo >= 3) + 1900 - 1] # -1 to make it fyear starting
rm(lt)

# table(case$case_start_fyear_starting)
# case <- case[case_start_fyear_starting >= 2007 & case_start_fyear_starting <= 2021]

public_apps <- c("Care (s31)",
                 "Child Assessment (s43)",
                 "EPO (s44)",
                 "Inherent Jurisdiction/Wardship",
                 "Supervision (s31)")
case_app[, public_app := app_type_name %in% public_apps]
case[, has_public_app := case_id %in% case_app[public_app == T]$case_id]

case <- case[has_public_app == T & case_status == "Completed"]


case_adult <-
  fread(
    paste0(cafcass_data_dir, "case_adult.csv"),
    stringsAsFactors = F
  )

case_adult <- case_adult[case_id %in% case$case_id]
case_adult <- case_adult[first_app_start_age >= 15 & first_app_start_age <= 50]


print("Merging linkage key for mothers with relevant cases")

linkage <- linkage[person_id %in% case_adult$adult_id]

setnames(linkage, "person_id", "cafcass_person_id")

deliveries <-
  merge(
    deliveries,
    linkage,
    by = "tokenid",
    all.x = T
  )

rm(linkage)
