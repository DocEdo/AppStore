# Model Exploration

# Packages ----
require("nnet")
require("mlogit")
source("data_preparation.R")

# Model 2 ----

# Our Model Exploration starts here

# DV: purchased_ratings

model_2 <- list()
model_2[["base"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + review + detail,
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

# Function to report model results

multinom_pvalues <- function(est_model) {
  summary(est_model)
  
  z <- summary(est_model)$coefficients/summary(est_model)$standard.errors
  
  # return p-values
  ( (1 - pnorm(abs(z), 0, 1)) * 2) |> round(3)
}

# p-values
model_2_pvalues <- lapply(model_2, FUN=multinom_pvalues)





