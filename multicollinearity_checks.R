# Multicollinearity Checks

# Packages ----
source("data_preparation.R")
library("car")
library("dwtest")

# Tests for Multinom ----

# Tests start here

# VIF for multinom

# Identify the levels of the DV
levels(surveysub$purchased_ratings)

# HighU vs rest
# LowJ vs rest
# LowU vs rest

# VIF for "Base" multinom -----

# HighU vs rest
surveysub$response_highu <- ifelse(surveysub$purchased_ratings == "HighU", 1, 0)

highu_rest <- glm(
  response_highu ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + read, data = surveysub, family = "binomial")

round(vif(highu_rest), 3)

# LowJ vs rest
surveysub$response_lowj <- ifelse(surveysub$purchased_ratings == "LowJ", 1, 0)

lowj_rest <- glm(
  response_lowj ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + read, data = surveysub, family = "binomial")

round(vif(lowj_rest), 3)

# LowU vs rest
surveysub$response_lowu <- ifelse(surveysub$purchased_ratings == "LowU", 1, 0)

lowu_rest <- glm(
  response_lowu ~ age + gender + income + visit_frequency + app_expense + previous_experience + regulatory_focus + platform_preference + involvement + read, data = surveysub, family = "binomial")

round(vif(lowu_rest), 3)

# VIF for "RF * Read" multinom ----

# Create Interaction Term as a Separate Variable since the VIF function suggests using 'predictor' but only for un-weighted models
surveysub$regfocus_read <- interaction(surveysub$regulatory_focus, surveysub$read)

highu_resti <- glm(
  response_highu ~ age + gender + income + visit_frequency + app_expense + previous_experience + platform_preference + involvement + regfocus_read, data = surveysub, family = "binomial")

highu_rest2 <- vif(highu_resti)

round(highu_rest2, 3)

# LowJ vs rest

lowj_resti <- glm(
  response_lowj ~ age + gender + income + visit_frequency + app_expense + previous_experience + platform_preference + involvement + regfocus_read, data = surveysub, family = "binomial")

lowj_rest2 <- vif(lowj_resti)

round(lowj_rest2, 3)

# LowU vs rest

lowu_resti <- glm(
  response_lowu ~ age + gender + income + visit_frequency + app_expense + previous_experience + platform_preference + involvement + regfocus_read, data = surveysub, family = "binomial")

lowu_rest2 <- vif(lowu_resti)

round(lowu_rest2, 3)



















































