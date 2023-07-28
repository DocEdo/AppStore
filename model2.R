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
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + read +
    regulatory_focus,
  data = surveysub)

model_2[["regfocus_read_rf"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + read +
    regulatory_focus * read,
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


model_2[["combined_four"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + 
    regulatory_focus * det_rev,
  data = surveysub)

model_2[["combined_maj"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + 
    regulatory_focus * det_rev_maj,
  data = surveysub)

# Summary
model_2$combined_maj

# Function to report model results
multinom_pvalues <- function(est_model) {
  summary(est_model)
  
  z <- summary(est_model)$coefficients/summary(est_model)$standard.errors
  
  # return p-values
  ( (1 - pnorm(abs(z), 0, 1)) * 2) |> round(3)
}

# p-values
model_2_pvalues <- lapply(model_2, FUN=multinom_pvalues)

model_2_pvalues



