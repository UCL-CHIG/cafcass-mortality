print("Loading all deliveries")
deliveries <-
  fread(
    paste0(del_data_dir, "/processed/deliveries_cohort_full.csv"),
    stringsAsFactors = F
  )

print("Subsetting to first live births in window")
lt <- as.POSIXlt(deliveries$delivery_date)
deliveries[, delivery_fyear := lt$year + (lt$mo >= 3) + 1900 - 1] # -1 to make it fyear starting
rm(lt)

deliveries <- deliveries[first_live_birth == T &
                           startage >= 15 & startage <= 39 &
                           delivery_fyear >= 2007 & delivery_fyear <= 2017]


print("Dropping exclusions")
deliveries <- deliveries[exclude == F]

