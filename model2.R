# Model 2

# Packages ----
require("nnet")
require("mlogit")
source("data_preparation.R")

# Model 2 ----

# Our Model Exploration starts here

# DV: purchased_ratings

model_2 <- list()
model_2[["base"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore,
  data = surveysub)

model_2[["det_rev"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + review + detail +
    regulatory_focus * review + regulatory_focus * detail,
  data = surveysub)

model_2[["regfocus_prev"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + review + detail +
    regulatory_focus * previous_experience,
  data = surveysub)

model_2[["regfocus_visit"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + review + detail +
    regulatory_focus * visit_frequency,
  data = surveysub)

model_2[["regfocus_direct"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + review + detail +
    regulatory_focus * directlyPurchase,
  data = surveysub)

model_2[["regfocus_app"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + review + detail +
    regulatory_focus * app_expense,
  data = surveysub)

model_2[["regfocus_detailp"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + review + detail +
    regulatory_focus * detail,
  data = surveysub)

model_2[["regfocus_reviewp"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + review + detail +
    regulatory_focus * review,
  data = surveysub)

# Read either detail/review
model_2[["regfocus_read"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore +
    regulatory_focus,
  data = surveysub)

model_2[["regfocus_read_rf"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore +
    regulatory_focus * explore,
  data = surveysub)

# Read both detail/review
model_2[["regfocus_readboth"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + readboth +
    regulatory_focus,
  data = surveysub)

model_2[["regfocus_readboth_rf"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + readboth +
    regulatory_focus * readboth,
  data = surveysub)

# Read either detail/review
model_2[["regfocus_read"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore,
  data = surveysub)

# Explore either read/detail
model_2[["regfocus_explore_rf"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore +
    regulatory_focus * explore,
  data = surveysub)

# model_2[["combined_four"]] <- multinom(
#   purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + 
#     regulatory_focus * det_rev,
#   data = surveysub)
# 
# model_2[["combined_maj"]] <- multinom(
#   purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + 
#     regulatory_focus * det_rev_maj,
#   data = surveysub)

# Results ----

# Function to report model results
multinom_pvalues <- function(est_model) {
  summary(est_model)
  
  z <- summary(est_model)$coefficients/summary(est_model)$standard.errors
  
  # return p-values
  ( (1 - pnorm(abs(z), 0, 1)) * 2) |> round(3)
}

# Model results
summary(model_2$regfocus_explore_rf)
summary(model_2$base)

# p-values
model_2_pvalues <- lapply(model_2, FUN = multinom_pvalues)

model_2_pvalues


# Exponentiate results
round(exp(coef(model_2$regfocus_explore_rf)), 3)


# Experimenting csv outputs

model_2out <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore +
    regulatory_focus * explore,
  data = surveysub)

summary_model2 <- summary(model_2out)

# Extract coefficients and standard errors
coefs <- as.data.frame(summary_model2$coefficients)
stderrs <- as.data.frame(summary_model2$standard.errors)

# Fail: Combine and save to CSV
output_nnet <- cbind(coefs, stderrs)
colnames(output_nnet) <- c("Coefficient", "StandardError")
write.csv(output_nnet, "temporary_files/model2_summary.csv", row.names = TRUE)




