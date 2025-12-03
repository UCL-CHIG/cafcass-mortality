
print("Outputting percentages of causes of death")

print("Create matrix")
out_matrix <- as.data.frame(matrix(NA, nrow = length(levels(deliveries$age_cat_at_delivery)), ncol = 6))
colnames(out_matrix) <- c("exp_lt_25", "exp_25_plus", "exp_total",
                          "unexp_lt_25", "unexp_25_plus", "unexp_total")
rownames(out_matrix) <- levels(deliveries$cause_of_death_cat_detailed)


n <- table(deliveries[in_cafcass == T & startage < 25]$cause_of_death_cat_detailed)
p <- round(prop.table(n) * 100, 1)
out_matrix[, "exp_lt_25"] <- paste0(n, " (", p, "%)")

n <- table(deliveries[in_cafcass == T & startage >= 25]$cause_of_death_cat_detailed)
p <- round(prop.table(n) * 100, 1)
out_matrix[, "exp_25_plus"] <- paste0(n, " (", p, "%)")

n <- table(deliveries[in_cafcass == T]$cause_of_death_cat_detailed)
p <- round(prop.table(n) * 100, 1)
out_matrix[, "exp_total"] <- paste0(n, " (", p, "%)")


n <- table(deliveries[in_cafcass == F & startage < 25]$cause_of_death_cat_detailed)
p <- round(prop.table(n) * 100, 1)
out_matrix[, "unexp_lt_25"] <- paste0(n, " (", p, "%)")

n <- table(deliveries[in_cafcass == F & startage >= 25]$cause_of_death_cat_detailed)
p <- round(prop.table(n) * 100, 1)
out_matrix[, "unexp_25_plus"] <- paste0(n, " (", p, "%)")

n <- table(deliveries[in_cafcass == F]$cause_of_death_cat_detailed)
p <- round(prop.table(n) * 100, 1)
out_matrix[, "unexp_total"] <- paste0(n, " (", p, "%)")


print("Saving")
write.csv(out_matrix, file = "output/cause_of_death.csv", row.names = T)
rm(out_matrix, n, p)
