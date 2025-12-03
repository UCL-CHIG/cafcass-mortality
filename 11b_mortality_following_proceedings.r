
print("subsetting cohort")

get_cuminc <- function(binary_exp = F, exposure = NULL) {
  
  out_list <- list()
  
  if (is.null(exposure)) {
    
    out_list[[1]] <- as.integer(NA) # n_deaths
    out_list[[2]] <- as.double(NA) # estimate
    out_list[[3]] <- as.double(NA) # se
    out_list[[4]] <- as.double(NA) # lcl
    out_list[[5]] <- as.double(NA) # ucl
    out_list[[6]] <- as.character(NA) # string
    out_list[[7]] <- as.character(NA) # name
    
    names(out_list) <- c("n_deaths", "estimate", "se", "lcl", "ucl", "string", "group")
    
    cuminc_obj <- cuminc(Surv(time_for_model, status_for_model) ~ 1, data = cafcass_cohort)
    n_deaths <- max(cuminc_obj$tidy$cum.event)
    max_time <- max(cuminc_obj$tidy$time)
    est <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$estimate
    se <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$std.error
    lcl <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$conf.low
    ucl <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$conf.high
    out_list[[1]][1] <- n_deaths
    out_list[[2]][1] <- est
    out_list[[3]][1] <- se
    out_list[[4]][1] <- lcl
    out_list[[5]][1] <- ucl
    out_list[[6]][1] <- paste0(round(est * 100, 2), " (", round(lcl * 100, 2), ", ", round(ucl * 100, 2), ")")
    out_list[[7]][1] <- "all"
    
  } else {
   
    exposure_dt <- cafcass_cohort[, get(exposure)]
    
    if (binary_exp == T) {
      
      out_list[[1]] <- as.integer(NA) # n_deaths
      out_list[[2]] <- as.double(NA) # estimate
      out_list[[3]] <- as.double(NA) # se
      out_list[[4]] <- as.double(NA) # lcl
      out_list[[5]] <- as.double(NA) # ucl
      out_list[[6]] <- as.character(NA) # string
      out_list[[7]] <- as.character(NA) # name
      
      names(out_list) <- c("n_deaths", "estimate", "se", "lcl", "ucl", "string", "group")
      
      cuminc_obj <- cuminc(Surv(time_for_model, status_for_model) ~ 1, data = cafcass_cohort[get(exposure) == F])
      n_deaths <- max(cuminc_obj$tidy$cum.event)
      max_time <- max(cuminc_obj$tidy$time)
      est <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$estimate
      se <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$std.error
      lcl <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$conf.low
      ucl <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$conf.high
      out_list[[1]][1] <- n_deaths
      out_list[[2]][1] <- est
      out_list[[3]][1] <- se
      out_list[[4]][1] <- lcl
      out_list[[5]][1] <- ucl
      out_list[[6]][1] <- paste0(round(est * 100, 2), " (", round(lcl * 100, 2), ", ", round(ucl * 100, 2), ")")
      out_list[[7]][1] <- paste0(exposure, " == F")
      
      cuminc_obj <- cuminc(Surv(time_for_model, status_for_model) ~ 1, data = cafcass_cohort[get(exposure) == T])
      n_deaths <- max(cuminc_obj$tidy$cum.event)
      max_time <- max(cuminc_obj$tidy$time)
      est <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$estimate
      se <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$std.error
      lcl <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$conf.low
      ucl <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$conf.high
      out_list[[1]][2] <- n_deaths
      out_list[[2]][2] <- est
      out_list[[3]][2] <- se
      out_list[[4]][2] <- lcl
      out_list[[5]][2] <- ucl
      out_list[[6]][2] <- paste0(round(est * 100, 2), " (", round(lcl * 100, 2), ", ", round(ucl * 100, 2), ")")
      out_list[[7]][2] <- paste0(exposure, " == T")
      
    } else {
      
      exp_lv <- levels(exposure_dt)
      
      out_list[[1]] <- as.integer(NA) # n_deaths
      out_list[[2]] <- as.double(NA) # estimate
      out_list[[3]] <- as.double(NA) # se
      out_list[[4]] <- as.double(NA) # lcl
      out_list[[5]] <- as.double(NA) # ucl
      out_list[[6]] <- as.character(NA) # string
      out_list[[7]] <- as.character(NA) # name
      
      names(out_list) <- c("n_deaths", "estimate", "se", "lcl", "ucl", "string", "group")
      
      for (i in 1:length(exp_lv)) {
        
        cuminc_obj <- cuminc(Surv(time_for_model, status_for_model) ~ 1, data = cafcass_cohort[get(exposure) == exp_lv[i]])
        n_deaths <- max(cuminc_obj$tidy$cum.event)
        max_time <- max(cuminc_obj$tidy$time)
        est <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$estimate
        se <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$std.error
        lcl <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$conf.low
        ucl <- cuminc_obj$tidy[cuminc_obj$tidy$time == max_time, ]$conf.high
        
        out_list[[1]][i] <- n_deaths
        out_list[[2]][i] <- est
        out_list[[3]][i] <- se
        out_list[[4]][i] <- lcl
        out_list[[5]][i] <- ucl
        out_list[[6]][i] <- paste0(round(est * 100, 2), " (", round(lcl * 100, 2), ", ", round(ucl * 100, 2), ")")
        out_list[[7]][i] <- exp_lv[i]
      }
      
    }
    
     
  }
  
  return(out_list)
  
}


print("Creating output matrix")
rs <- (
  1 +
    length(levels(cafcass_cohort$age_cat_at_proceedings)) + 
    length(levels(cafcass_cohort$cafcass_address_imd_quintile)) +
    length(levels(as.factor(cafcass_cohort$any_health_problem_prior_case_start))) +
    length(levels(as.factor(cafcass_cohort$int_dis_3yrs_prior_case_start))) +
    length(levels(as.factor(cafcass_cohort$disabilities_grant_any_3yrs_prior_case_start))) +
    length(levels(as.factor(cafcass_cohort$charlson_3yrs_prior_case_start))) +
    length(levels(as.factor(cafcass_cohort$mhbev_any_3yrs_prior_case_start))) +
    length(levels(as.factor(cafcass_cohort$adversity_admission_3yrs_prior_case_start))) +
    length(levels(as.factor(cafcass_cohort$drug_alc_3yrs_prior_case_start))) +
    length(levels(as.factor(cafcass_cohort$self_harm_3yrs_prior_case_start))) +
    length(levels(as.factor(cafcass_cohort$violence_3yrs_prior_case_start))) +
    length(levels(cafcass_cohort$cafcass_final_order))
)



out_matrix <- as.data.frame(
  matrix(
    rep(NA, rs * 7),
    nrow = rs,
    ncol = 7
  )
)

rm(rs)

colnames(out_matrix) <- c("n_deaths", "estimate", "se", "lcl", "ucl", "string", "group")


print("Writing data")
print("All")
out_matrix[1, ] <- get_cuminc()

print("age_cat_at_proceedings")
curr_rows <- 2:6
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = F, exposure = "age_cat_at_proceedings")
out_matrix[2:6, ]$group <- paste0("age_cat_at_proceedings == ", out_matrix[2:6, ]$group)

print("cafcass_address_imd_quintile")
curr_rows <- 7:11
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = F, exposure = "cafcass_address_imd_quintile")
out_matrix[7:11, ]$group <- paste0("cafcass_address_imd_quintile == ", out_matrix[7:11, ]$group)

print("any_health_problem_3yrs_prior_case_start")
curr_rows <- 12:13
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = T, exposure = "any_health_problem_3yrs_prior_case_start")

print("charlson_3yrs_prior_case_start")
curr_rows <- 14:15
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = T, exposure = "charlson_3yrs_prior_case_start")

print("disabilities_grant_any_3yrs_prior_case_start")
curr_rows <- 16:17
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = T, exposure = "disabilities_grant_any_3yrs_prior_case_start")

print("int_dis_3yrs_prior_case_start")
curr_rows <- 18:19
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = T, exposure = "int_dis_3yrs_prior_case_start")

print("mhbev_any_3yrs_prior_case_start")
curr_rows <- 20:21
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = T, exposure = "mhbev_any_3yrs_prior_case_start")

print("adversity_admission_3yrs_prior_case_start")
curr_rows <- 22:23
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = T, exposure = "adversity_admission_3yrs_prior_case_start")

print("drug_alc_3yrs_prior_case_start")
curr_rows <- 24:25
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = T, exposure = "drug_alc_3yrs_prior_case_start")

print("self_harm_3yrs_prior_case_start")
curr_rows <- 26:27
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = T, exposure = "self_harm_3yrs_prior_case_start")

print("violence_3yrs_prior_case_start")
curr_rows <- 28:29
# out_matrix[curr_rows, ] <- get_cuminc(binary_exp = T, exposure = "violence_3yrs_prior_case_start")
out_matrix[curr_rows, 1:6] <- "suppressed"
out_matrix[curr_rows, 7] <- c("violence_3yrs_prior_case_start == F", "violence_3yrs_prior_case_start == T")

print("cafcass_final_order")
curr_rows <- 30:35
out_matrix[curr_rows, ] <- get_cuminc(binary_exp = F, exposure = "cafcass_final_order")
out_matrix[30:35, ]$group <- paste0("cafcass_final_order == ", out_matrix[30:35, ]$group)

row.names(out_matrix) <- out_matrix$group
out_matrix$group <- NULL

print("Saving")
write.csv(out_matrix, file = "output/mortality_following_proceedings.csv", row.names = T)
rm(get_cuminc, out_matrix)
