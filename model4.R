# Model Exploration

# Packages ----
require("nnet")
require("mlogit")
source("data_preparation.R")

# Model 4 ----

model_4 <- list()

# DV: numRating

model_4[["base"]] <- glm(
  numRating ~ shape +age + gender +income + visit_frequency + app_expense + previous_experience +regulatory_focus + platform_preference + involvement + review + detail,
  family = "binomial",
  data = surveysub)

model_4[["prev"]] <- glm(
  numRating ~ shape +age + gender +income + visit_frequency + app_expense + previous_experience +regulatory_focus + platform_preference + involvement + review + detail + regulatory_focus * previous_experience,
  family = "binomial",
  data = surveysub)

model_4[["visit"]] <- glm(
  numRating ~ shape +age + gender +income + visit_frequency + app_expense + previous_experience +regulatory_focus + platform_preference + involvement + review + detail + regulatory_focus * visit_frequency,
  family = "binomial",
  data = surveysub)

model_4[["app"]] <- glm(
  numRating ~ shape +age + gender +income + visit_frequency + app_expense + previous_experience +regulatory_focus + platform_preference + involvement + review + detail + regulatory_focus * app_expense,
  family = "binomial",
  data = surveysub)

# Functions ----
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
models_summaries(model_4)

# Exponential
summary_exp(model_4)



