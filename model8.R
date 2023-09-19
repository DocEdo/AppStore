# Model Exploration

# Packages ----
require("nnet")
require("mlogit")
source("data_preparation.R")

# Model 8 ----

# Our Model Exploration starts here

# DV: purchased_ratings

model_8 <- list()

model_8[["base"]] <- glm(
  read ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + shape + numRating,
  family = "binomial",
  data = surveysub)

model_8[["prev"]] <- glm(
  read ~ numRating + shape + age + gender +income + visit_frequency + app_expense + previous_experience +regulatory_focus + platform_preference + involvement + regulatory_focus * previous_experience,
  family = "binomial",
  data = surveysub)

model_8[["visit"]] <- glm(
  read ~ numRating + shape + age + gender +income + visit_frequency + app_expense + previous_experience +regulatory_focus + platform_preference + involvement + regulatory_focus * visit_frequency,
  family = "binomial",
  data = surveysub)

model_8[["app"]] <- glm(
  read ~ numRating + shape + age + gender +income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + regulatory_focus * app_expense,
  family = "binomial",
  data = surveysub)

# Results ----

# Function to print summary
models_summaries <- function(model_list) {
  invisible(lapply(model_list, function(model) {
    print(summary(model))
  }))
}

# Function to report model results
summary_exp <- function(model_list) {
  summary(model_list) # print the list
  
  # extract and round the coefficients
  coef_values <- lapply(model_list, function(model) {
    round(exp(coef(model)), 3)
  })
  
  # return the coefficients
  return(coef_values)
}

# Results ----
# Summary
models_summaries(model_8)

# Exponential
summary_exp(model_8)











