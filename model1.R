# Model Exploration

# Packages ----
require("nnet")
require("mlogit")
source("data_preparation.R")

# Model 1 ----

# Original Model with multinom

# DV: purchased_rating 

model_1 <- multinom( # Multinom
  purchased_ratings ~ 
    factor(regulatory_focus) + 
    gender + 
    age + 
    income + 
    platform_preference + 
    visit_frequency + 
    app_expense + 
    previous_experience + 
    involvement, 
  data = surveysub)

summary(model_1)

summary(model_1)$coefficients/summary(model_1)$standard.errors

exp(coef(model_1))
