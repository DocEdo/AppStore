# Model Exploration

# Packages ----
require("nnet")
require("mlogit")
source("data_preparation.R")

# Model 3 ----

model_3 <- list()

# DV: shape

model_3[["base"]] <- glm(
  shape ~ numRating + age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore,
  family = "binomial",
  data = surveysub)

model_3[["prev"]] <- glm(
  shape ~ numRating + age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore + regulatory_focus * previous_experience,
  family = "binomial",
  data = surveysub)

model_3[["visit"]] <- glm(
  shape ~ numRating + age + gender +income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore + regulatory_focus * visit_frequency,
  family = "binomial",
  data = surveysub)

model_3[["num"]] <- glm(
  shape ~ numRating + age + gender +income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore + regulatory_focus * numRating,
  family = "binomial",
  data = surveysub)

# Functions ----
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

# Function to print summary
models_summaries <- function(model_list) {
  invisible(lapply(model_list, function(model) {
    print(summary(model))
  }))
}

# Reults ----
# Summary
models_summaries(model_3)

# Exponential
summary_exp(model_3)



