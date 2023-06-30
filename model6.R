# Model Exploration

# Packages ----
require("nnet")
require("mlogit")
source("data_preparation.R")

# Model 6 ----

model_6 <- list()

# DV: details

model_6[["base"]] <- glm(
  detail ~ numRating +shape +age + gender +income + visit_frequency + app_expense + previous_experience +regulatory_focus + platform_preference + involvement + review,
  family = "binomial",
  data = surveysub)

model_6[["prev"]] <- glm(
  detail ~ numRating +shape +age + gender +income + visit_frequency + app_expense + previous_experience +regulatory_focus + platform_preference + involvement + review + regulatory_focus * previous_experience,
  family = "binomial",
  data = surveysub)

model_6[["visit"]] <- glm(
  detail ~ numRating +shape +age + gender +income + visit_frequency + app_expense + previous_experience +regulatory_focus + platform_preference + involvement + review + regulatory_focus * visit_frequency,
  family = "binomial",
  data = surveysub)

model_6[["app"]] <- glm(
  detail ~ numRating + shape + age + gender +income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + review + regulatory_focus * app_expense,
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
models_summaries(model_6)

# Exponential
summary_exp(model_6)


