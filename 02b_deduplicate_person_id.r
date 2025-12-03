print("Deduplicating linkage spine based on Cafcass person ID")
dups <- linkage[person_id %in% linkage[duplicated(person_id)]$person_id]


print("Getting Cafcass demographic data")
case <-
  fread(
    paste0(cafcass_data_dir, "case.csv"),
    stringsAsFactors = F
  )

case_adult <-
  fread(
    paste0(cafcass_data_dir, "case_adult.csv"),
    stringsAsFactors = F
  )

case_adult <- case_adult[, c("case_id", "adult_id", "first_app_start_age")]
case_adult <- case_adult[adult_id %in% dups$person_id]

case <- case[, c("case_id", "case_start_date")]
case <- case[case_id %in% case_adult$case_id]

case_adult <-
  merge(
    case_adult,
    case,
    by = "case_id",
    all.x = T
  )

rm(case)

lt <- as.POSIXlt(case_adult$case_start_date)
lt$year <- lt$year - case_adult$first_app_start_age
case_adult[, dob_approx := as.Date(lt)]
rm(lt)

case_adult[, yob_approx := format(dob_approx, format = "%Y")]
case_adult <- case_adult[, c("adult_id", "yob_approx")]
case_adult <- case_adult[order(adult_id)]
case_adult <- case_adult[!duplicated(case_adult)]
case_adult <- case_adult[!(adult_id %in% case_adult[duplicated(adult_id)]$adult_id)]
case_adult[, adult_id := as.character(adult_id)]
setnames(case_adult, names(case_adult), c("person_id", "yob_approx_cafcass"))

dups <-
  merge(
    dups,
    case_adult,
    by = "person_id",
    all.x = T
  )

rm(case_adult)

dups <- dups[!is.na(yob_approx_cafcass)]


print("Getting HES demographic data")
dups <-
  merge(
    dups,
    deliveries[, c("tokenid", "dob_full")],
    by = "tokenid",
    all.x = T
  )

dups[, yob_approx_hes := format(dob_full, format = "%Y")]
dups <- dups[order(person_id, tokenid), c("person_id", "tokenid", "yob_approx_cafcass", "yob_approx_hes")]


print("Comparing and dropping")
dups[, match := yob_approx_cafcass == yob_approx_hes]

dups <- dups[person_id %in% dups[match == T]$person_id]
dups[, n_true := sum(match, na.rm = T), by = person_id]
dups <- dups[n_true == 1 & match == T]
dups <- dups[, c("person_id", "tokenid")]
setnames(dups, "tokenid", "tokenid_dedup")

linkage <-
  merge(
    linkage,
    dups,
    by = "person_id",
    all.x = T
  )

rm(dups)

linkage[!is.na(tokenid_dedup), tokenid := tokenid_dedup]
linkage[, tokenid_dedup := NULL]

linkage <- linkage[!duplicated(linkage)]
drops_hes <- unique(linkage[person_id %in% linkage[duplicated(person_id)]$person_id]$tokenid)
linkage <- linkage[!(person_id %in% linkage[duplicated(person_id)]$person_id)]
deliveries <- deliveries[!(tokenid %in% drops_hes)]

rm(drops_hes)
