# Model Exploration

# Packages ----
require("nnet")
require("mlogit")
source("data_preparation.R")

# Model 9 ---- Discarded!

# Mean-Centered Terms


# Residuals of Interaction


# Our Model Exploration starts here

# DV: read ---> Orthogonalized Interaction

model_9 <- list()

model_9[["base"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + review + detail,
  data = surveysub)

model_9[["shape_rating"]] <- multinom(
  purchased_ratings ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + read +
    regulatory_focus * read,
  data = surveysub)

# Results ----

# Function to report model results
multinom_pvalues <- function(est_model) {
  summary(est_model)
  
  z <- summary(est_model)$coefficients/summary(est_model)$standard.errors
  
  # return p-values
  ( (1 - pnorm(abs(z), 0, 1)) * 2) |> round(3)
}

# Model results
summary(model_2$regfocus_readboth)
summary(model_2$regfocus_readboth_rf)
summary(model_2$regfocus_read)
summary(model_2$regfocus_read_rf)

# p-values
model_2_pvalues <- lapply(model_2, FUN = multinom_pvalues)

model_2_pvalues


# Exponentiate results
round(exp(coef(model_2$regfocus_readboth)), 3)
round(exp(coef(model_2$regfocus_readboth_rf)), 3)
round(exp(coef(model_2$regfocus_read)), 3)
round(exp(coef(model_2$regfocus_read_rf)), 3)









