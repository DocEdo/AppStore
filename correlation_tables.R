# Correlation/similarity for binary variables

# Packages ----
require("corrr")
require("proxy")
require("polycor")
source("data_preparation.R")
#surveysource("data_preparation.R")

# Jaccard similarity ----
binary_data <- as.matrix(surveysub[, c("detail", "review")])

binary_data <- data.frame(
  details = as.logical(as.numeric(surveysub$detail) - 1),
  reviews = as.logical(as.numeric(surveysub$review) - 1)
)

# Compute the Jaccard dissimilarity
jaccard_dissimilarity <- as.matrix(dist(binary_data, method = "binary"))
mean(jaccard_dissimilarity)
# The variables are not highly similar, and there are differences in their binary patterns across the dataset

# Convert Jaccard dissimilarity to Jaccard similarity
jaccard_similarity <- 1 - jaccard_dissimilarity
mean(jaccard_similarity)
# There is some level of similarity between the two binary variables, but they are not entirely the same (range is 0 to 1)


# Phi coefficient ----
binary_data2 <- surveysub[, c("detail", "review")]

phi_plot <- assoc(binary_data2, method = "phi")
#phi_coefficient <- polycor::phi(binary_data2)
#phi_coefficient <- phik(binary_data2)

# 2x2 contingency table for the binary variables
contingency_table <- table(binary_data2)

# Compute the Phi coefficient
phi_coefficient <- sqrt(chisq.test(contingency_table)$statistic / sum(contingency_table))

phi_coefficient
# Low value: A chi-squared statistic of 0.331096 suggests a relatively weak association between the 'details' and 'reviews' variables.

# Effect size: The Phi coefficient, which ranges from 0 to 1, can provide a measure of effect size. A higher Phi coefficient indicates a stronger association between the binary variables.

phi_plot

# Chi-squared test to test the significance of the phi coeff
chi_squared_result <- chisq.test(contingency_table)

p_value <- chi_squared_result$p.value
p_value

significance_level <- 0.05
p_value < significance_level

# It means that there is a significant relationship or dependence between these two binary variables. In this context, the statistically significant association suggests that the presence (1) or absence (0) of one variable (e.g., 'detail') is related to the presence or absence of the other variable (e.g., 'review').


