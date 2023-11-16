# Model Exploration

# Packages ----
require("nnet")
require("mlogit")
source("data_preparation.R")


# Model 11 -> DV: purchased_ratings Highu vs else ----

# Recode purchased_ratings to a binary of HighU vs the rest
surveysub$purchased_binary <- as.numeric(surveysub$purchased_ratings == "HighU")

# Logit: Base
model_base <- glm(purchased_binary ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore, 
                   data = surveysub, 
                   family = binomial)

summary(model_base)

# Logit: Interaction term - RF * explore
model_int <- glm(purchased_binary ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + explore + explore*regulatory_focus, 
                  data = surveysub, 
                  family = binomial)

summary(model_int)







