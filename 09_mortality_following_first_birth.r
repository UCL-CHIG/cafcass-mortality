
print("Calculating mortality rates")

print("Create matrix")
out_matrix <- as.data.frame(matrix(NA, nrow = length(levels(deliveries$age_cat_at_delivery)), ncol = 10))
colnames(out_matrix) <- c("unexp_person_years", "unexp_deaths", "unexp_rate", "unexp_rate_per_1000_95_ci",
                          "exp_person_years", "exp_deaths", "exp_rate", "exp_rate_per_1000_95_ci",
                          "crude_mortality_ratio", "crude_mortality_ratio_95_ci")
rownames(out_matrix) <- levels(deliveries$age_cat_at_delivery)


print("By age group")
out_matrix[, "unexp_person_years"] <- round(aggregate(person_time_unexposed_days / 365.25 ~ age_cat_at_delivery, data = deliveries, FUN = sum)[, 2], 0)
out_matrix[, "unexp_deaths"] <- table(deliveries$age_cat_at_delivery, deliveries$status_cat)[, "died_unexposed"]

r <- pois.approx(out_matrix[, "unexp_deaths"], out_matrix[, "unexp_person_years"])$rate
lcl <- pois.approx(out_matrix[, "unexp_deaths"], out_matrix[, "unexp_person_years"])$lower
ucl <- pois.approx(out_matrix[, "unexp_deaths"], out_matrix[, "unexp_person_years"])$upper

out_matrix[, "unexp_rate"] <- r

out_matrix[, "unexp_rate_per_1000_95_ci"] <-
  paste0(
    round(out_matrix[, "unexp_rate"] * 1000, 2), " (",
    round(lcl * 1000, 2), ", ",
    round(ucl * 1000, 2), ")"
  )

out_matrix[, "exp_person_years"] <- round(aggregate(person_time_exposed_days / 365.25 ~ age_cat_at_delivery, data = deliveries[in_cafcass == T], FUN = sum)[, 2], 0)
out_matrix[, "exp_deaths"] <- table(deliveries$age_cat_at_delivery, deliveries$status_cat)[, "died_exposed"]

r <- pois.approx(out_matrix[, "exp_deaths"], out_matrix[, "exp_person_years"])$rate
lcl <- pois.approx(out_matrix[, "exp_deaths"], out_matrix[, "exp_person_years"])$lower
ucl <- pois.approx(out_matrix[, "exp_deaths"], out_matrix[, "exp_person_years"])$upper

out_matrix[, "exp_rate"] <- r

out_matrix[, "exp_rate_per_1000_95_ci"] <-
  paste0(
    round(out_matrix[, "exp_rate"] * 1000, 2), " (",
    round(lcl * 1000, 2), ", ",
    round(ucl * 1000, 2), ")"
  )



print("Add overall")
out_matrix[nrow(out_matrix) + 1, ] <- rep(NA, 10)
n <- nrow(out_matrix)
rownames(out_matrix)[n] <- "all"

out_matrix[n, "unexp_person_years"] <- round(sum(deliveries$person_time_unexposed_days / 365.25), 0)
out_matrix[n, "unexp_deaths"] <- table(deliveries$status_cat)["died_unexposed"]

r <- pois.approx(out_matrix[n, "unexp_deaths"], out_matrix[n, "unexp_person_years"])$rate
lcl <- pois.approx(out_matrix[n, "unexp_deaths"], out_matrix[n, "unexp_person_years"])$lower
ucl <- pois.approx(out_matrix[n, "unexp_deaths"], out_matrix[n, "unexp_person_years"])$upper

out_matrix[n, "unexp_rate"] <- r

out_matrix[n, "unexp_rate_per_1000_95_ci"] <-
  paste0(
    round(out_matrix[n, "unexp_rate"] * 1000, 2), " (",
    round(lcl * 1000, 2), ", ",
    round(ucl * 1000, 2), ")"
  )

out_matrix[n, "exp_person_years"] <- round(sum(deliveries[in_cafcass == T]$person_time_exposed_days / 365.25), 0)
out_matrix[n, "exp_deaths"] <- table(deliveries$status_cat)["died_exposed"]
out_matrix[n, "exp_rate"] <- out_matrix[n, "exp_deaths"] / out_matrix[n, "exp_person_years"]

r <- pois.approx(out_matrix[n, "exp_deaths"], out_matrix[n, "exp_person_years"])$rate
lcl <- pois.approx(out_matrix[n, "exp_deaths"], out_matrix[n, "exp_person_years"])$lower
ucl <- pois.approx(out_matrix[n, "exp_deaths"], out_matrix[n, "exp_person_years"])$upper

out_matrix[n, "exp_rate"] <- r

out_matrix[n, "exp_rate_per_1000_95_ci"] <-
  paste0(
    round(out_matrix[n, "exp_rate"] * 1000, 2), " (",
    round(lcl * 1000, 2), ", ",
    round(ucl * 1000, 2), ")"
  )

rm(n, lcl, ucl, r)



print("Mortality ratios")
tmp_dt <- out_matrix[, 1:2]
setnames(tmp_dt, c("unexp_person_years", "unexp_deaths"), c("person_time", "deaths"))
tmp_dt$in_cafcass <- 0
tmp_dt$age <- rownames(tmp_dt)

tmp_dt2 <- out_matrix[, 5:6]
setnames(tmp_dt2, c("exp_person_years", "exp_deaths"), c("person_time", "deaths"))
tmp_dt2$in_cafcass <- 1
tmp_dt2$age <- rownames(tmp_dt2)

tmp_dt <- rbind(tmp_dt, tmp_dt2); rm(tmp_dt2)

get_irr <- function(age_grps) {
  
  out_list <- list()
  out_list[[1]] <- as.integer(rep(NA, length(age_grps)))
  out_list[[2]] <- as.character(rep(NA, length(age_grps)))
  
  for (i in 1:length(age_grps)) {
    
    m <- summary(glm(deaths ~ in_cafcass + offset(log(person_time)), data = tmp_dt[tmp_dt$age == age_grps[i], ], family = poisson))
    r <- m$coefficients[, "Estimate"]["in_cafcass"]
    se <- m$coefficients[, "Std. Error"]["in_cafcass"]
  
    out_list[[1]][i] <- exp(r)
    out_list[[2]][i] <-
      paste0(
        round(exp(r), 2),
        " (",
        round(exp(r - 1.96 * se), 2),
        ", ",
        round(exp(r + 1.96 * se), 2),
        ")"
      )
    
  }
  
  return(out_list)

}

out_matrix[, c("crude_mortality_ratio", "crude_mortality_ratio_95_ci")] <- get_irr(tmp_dt$age[1:6])

print("Saving")
write.csv(out_matrix, file = "output/mortality_rates.csv", row.names = T)



print("Age-standardising")
pop_ps <- prop.table(table(deliveries$age_cat_at_delivery))

std_matrix_unexp <- out_matrix[1:5, c("unexp_person_years", "unexp_deaths", "unexp_rate")]

std_matrix_unexp$unexp_wtd_rate <- std_matrix_unexp$unexp_rate * pop_ps
std_matrix_unexp$unexp_wtd_var <- (std_matrix_unexp$unexp_deaths / std_matrix_unexp$unexp_person_years ^ 2) * pop_ps

std_matrix_unexp$unexp_std_rate <- sum(std_matrix_unexp$unexp_wtd_rate)
std_matrix_unexp$unexp_wtd_var_sum <- sum(std_matrix_unexp$unexp_wtd_var)

se <- sqrt(unique(std_matrix_unexp$unexp_wtd_var_sum))
lcl <- std_matrix_unexp$unexp_std_rate - 1.96 * se
ucl <- std_matrix_unexp$unexp_std_rate + 1.96 * se

std_matrix_unexp$unexp_std_rate_string <-
  paste0(
    round(std_matrix_unexp$unexp_std_rate * 1000, 2),
    " (",
    round(lcl * 1000, 2),
    ", ",
    round(ucl * 1000, 2),
    ")"
  )


std_matrix_exp <- out_matrix[1:5, c("exp_person_years", "exp_deaths", "exp_rate")]

std_matrix_exp$exp_wtd_rate <- std_matrix_exp$exp_rate * pop_ps
std_matrix_exp$exp_wtd_var <- (std_matrix_exp$exp_deaths / std_matrix_exp$exp_person_years ^ 2) * pop_ps

std_matrix_exp$exp_std_rate <- sum(std_matrix_exp$exp_wtd_rate)
std_matrix_exp$exp_wtd_var_sum <- sum(std_matrix_exp$exp_wtd_var)

se <- sqrt(unique(std_matrix_exp$exp_wtd_var_sum))
lcl <- std_matrix_exp$exp_std_rate - 1.96 * se
ucl <- std_matrix_exp$exp_std_rate + 1.96 * se

std_matrix_exp$exp_std_rate_string <-
  paste0(
    round(std_matrix_exp$exp_std_rate * 1000, 2),
    " (",
    round(lcl * 1000, 2),
    ", ",
    round(ucl * 1000, 2),
    ")"
  )


std_matrix <- cbind(std_matrix_unexp, std_matrix_exp)
rm(std_matrix_unexp, std_matrix_exp, lcl, ucl, se, tmp_dt)


print("Calculating SMR")
std_matrix$smr_actual <- std_matrix$exp_std_rate / std_matrix$unexp_std_rate


# print("Confidence interval using the delta method")
# se_ln_smr <- unique(
#   sqrt((std_matrix$exp_wtd_var_sum / std_matrix$exp_std_rate ^ 2) +
#          (std_matrix$unexp_wtd_var_sum / std_matrix$unexp_std_rate ^ 2))
# )
# lcl <- log(unique(std_matrix$smr_actual)) - 1.96 * se_ln_smr
# ucl <- log(unique(std_matrix$smr_actual)) + 1.96 * se_ln_smr
# 
# std_matrix$smr_string_delta <-
#   paste0(
#     round(std_matrix$smr_actual, 2),
#     " (",
#     round(exp(lcl), 2),
#     ", ",
#     round(exp(ucl), 2),
#     ")"
#   )


print("Confidence interval using simulation")
draw_unexp_rate <- rnorm(10000000, mean = unique(std_matrix$unexp_std_rate), sd = sqrt(sum(std_matrix$unexp_wtd_var)))
draw_exp_rate <- rnorm(10000000, mean = unique(std_matrix$exp_std_rate), sd = sqrt(sum(std_matrix$exp_wtd_var)))
draw_ratio <- draw_exp_rate / draw_unexp_rate
summary(draw_ratio)
quantile(draw_ratio, probs = c(0.025, 0.975))

std_matrix$smr_simulated <- mean(draw_ratio)
std_matrix$smr_string_simulated <-
  paste0(
    round(mean(draw_ratio), 2), " (",
    round(quantile(draw_ratio, probs = 0.025), 2),
    ", ",
    round(quantile(draw_ratio, probs = 0.975), 2),
    ")"
  )


write.csv(std_matrix, file = "output/std_mortality_rates.csv", row.names = T)

rm(pop_ps, out_matrix, std_matrix, get_irr, draw_exp_rate, draw_ratio, draw_unexp_rate,
   lcl, ucl, se_ln_smr)


