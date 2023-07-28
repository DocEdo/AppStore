# Re-creating success_induced ----

# RF check
promotion_cols <- c("rf2", "rf4", "rf6")
prevention_cols <- c("rf3", "rf5", "rf7")

# Combining selected columns into separate data frames
promotion_focus_data <- join_survey[, promotion_cols]
prevention_focus_data <- join_survey[, prevention_cols]

# FA for promotion focus
promotion_factor <- principal(promotion_focus_data, nfactors = 1)
# FA for prevention focus
prevention_factor <- principal(prevention_focus_data, nfactors = 1)

promotion_loadings <- promotion_factor$loadings
prevention_loadings <- prevention_factor$loadings

promotion_loadings
prevention_loadings

# Average variance extracted: AVE = (Sum of squared factor loadings) / (Sum of squared factor loadings + Sum of error variances)

# Calculate Composite Reliability (CR) for promotion focus
promotion_squared_loadings <- promotion_loadings^2
promotion_cr <- sum(promotion_squared_loadings) / (sum(promotion_squared_loadings) + promotion_factor$uniquenesses)

# Calculate Average Variance Extracted (AVE) for promotion focus
promotion_ave <- sum(promotion_squared_loadings) / (sum(promotion_squared_loadings) + sum(promotion_factor$uniquenesses))

# Calculate Composite Reliability (CR) for prevention focus
prevention_squared_loadings <- prevention_loadings^2
prevention_cr <- sum(prevention_squared_loadings) / (sum(prevention_squared_loadings) + prevention_factor$uniquenesses)

# Calculate Average Variance Extracted (AVE) for prevention focus
prevention_ave <- sum(prevention_squared_loadings) / (sum(prevention_squared_loadings) + sum(prevention_factor$uniquenesses))

promotion_cr
promotion_ave
prevention_cr
prevention_ave

# Mean of each CR and AVE
mean_prom_cr <- mean(promotion_cr)
mean_prom_ave <- mean(promotion_ave)
mean_preven_cr <- mean(prevention_cr)
mean_preven_ave <- mean(prevention_ave)

mean_prom_cr
mean_prom_ave
mean_preven_cr
mean_preven_ave

# Difference between average of promotion and prevention items

# Columns for promotion focus and prevention focus
promotion_cols <- c("rf2", "rf4", "rf6")
prevention_cols <- c("rf3", "rf5", "rf7")

# Separate data frames
promotion_focus_data <- join_survey[, promotion_cols]
prevention_focus_data <- join_survey[, prevention_cols]

# Calculating the average scores for promotion focus and prevention focus
promotion_average <- rowMeans(promotion_focus_data, na.rm = TRUE)
prevention_average <- rowMeans(prevention_focus_data, na.rm = TRUE)

mean_difference <- mean(regulatory_focus_difference)
mean_difference
# If mean_difference is positive, it indicates an overall tendency towards promotion focus, 
# and if it is negative, it indicates an overall tendency towards prevention focus.

# Regulatory focus difference score
regulatory_focus_difference <- promotion_average - prevention_average
regulatory_focus_difference

hist(regulatory_focus_difference, main = "Regulatory Focus Difference",
     xlab = "Difference (Promotion - Prevention)")

barplot(regulatory_focus_difference, main = "Regulatory Focus Difference",
        xlab = "Respondents", ylab = "Difference (Promotion - Prevention)")

# One-way anova
join_survey <- mutate(join_survey, 
                      regulatory_focus = factor(regulatory_focus))

anova_result <- aov(
  regulatory_focus_difference ~ regulatory_focus, data = join_survey)

summary(anova_result)

tukey_hsd <- TukeyHSD(anova_result)
tukey_hsd

# Success induced

join_survey$regulatory_focus_difference <- regulatory_focus_difference

# Create a new column 'success_induced' and categorize based on regulatory_focus_difference
join_survey$success_induced <- ifelse(join_survey$regulatory_focus_difference > 0, "yes", "no")

# Print the first few rows of the updated data frame to check the new columns
head(join_survey$success_induced)

sum(join_survey$success_induced == "yes") # new calculation using 
sum(purchases$success_induced == "yes") # original 'collapsed' data set







