# Model Exploration

# Packages ----
require("nnet")
require("mlogit")
require("car")
source("data_preparation.R")


# Model 11 -> DV: purchased_ratings Highu vs rest ----

# Recoding purchased_ratings to a binary of HighU vs the rest
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

# Model 12 -> DV: purchased_ratings Highu vs Highj ----

# Recoding purchased_ratings to a binary variable (HighU vs HighJ)
surveysub$highju <- ifelse(surveysub$purchased_ratings == "HighU", 1, 
                                             ifelse(surveysub$purchased_ratings == "HighJ", 0, NA))

# Trying something during meeting
surveysub$regulatory_focus <- 
  relevel(factor(surveysub$regulatory_focus), ref = "Promotion")

# Filtering out NA values (LowJ and LowU)
surveysub_filtered <- surveysub %>% filter(!is.na(highju))

# Logit HighU vs HighJ
base_ju <- glm(highju ~ 
                 age + 
                 gender + 
                 income + 
                 visit_frequency + 
                 app_expense + 
                 previous_experience + 
                 regulatory_focus + 
                 platform_preference + 
                 involvement + 
                 explore, 
               data = surveysub_filtered, 
               family = binomial)

summary(base_ju)

# Interaction

int_ju <- glm(highju ~ 
                 age + 
                 gender + 
                 income + 
                 visit_frequency + 
                 app_expense + 
                 previous_experience + 
                 regulatory_focus + 
                 platform_preference + 
                 involvement + 
                 explore +
               regulatory_focus * explore, 
               data = surveysub_filtered, 
               family = binomial)

summary(int_ju)

# VIF -----

# for HighU vs rest
round(vif(model_base), 3)

# for HighU vs HighJ 
round(vif(base_ju), 3)






















