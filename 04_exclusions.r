
print("Dropping where DOD < case start")
deliveries[, drop := !is.na(dod_combined) & !is.na(cafcass_case_start_date) & dod_combined < cafcass_case_start_date]
deliveries <- deliveries[drop == F]
deliveries[, drop := NULL]


print("Dropping where case start < delivery date")
deliveries[, drop := !is.na(cafcass_case_start_date) & cafcass_case_start_date < delivery_date]
deliveries <- deliveries[drop == F]
deliveries[, drop := NULL]
