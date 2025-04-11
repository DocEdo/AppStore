# Tests for endogeneity

source("data_preparation.R")
library("tidyverse")


# Reference:
# HighU
# highu_rest_orthogonal <- glm(highu_rest ~
#                                reg_focus_dummy +  # Use the single dummy variable
#                                expHighJ_only +
#                                expHighU_only +
#                                exp_both_high +
#                                int_expHighJ_rf_orthogonal +
#                                int_expHighU_rf_orthogonal +
#                                int_expBoth_rf_orthogonal +
#                                age + 
#                                gender + 
#                                income + 
#                                visit_frequency + 
#                                app_expense + 
#                                previous_experience + 
#                                platform_preference + 
#                                involvement + 
#                                appname_purchased + 
#                                apporder_purchased, 
#                              data = surveysub, 
#                              family = binomial)
# 
# summary(highu_rest_orthogonal)

# 2-SRI endogenous approach

# Linear model to predict the endogenous variable
stage1 <- lm(expHighU_only ~ reg_focus_dummy + platform_preference + age + gender + income + 
               visit_frequency + app_expense + previous_experience + involvement, data = surveysub)

surveysub$resid_expHighU <- residuals(stage1)

# Add residual to the logistic model
model_2sri_clean <- glm(highu_rest ~
                          reg_focus_dummy +
                          expHighJ_only +
                          expHighU_only +
                          exp_both_high +
                          int_expHighJ_rf_orthogonal +
                          int_expHighU_rf_orthogonal +
                          int_expBoth_rf_orthogonal +
                          age + gender + income + visit_frequency + 
                          app_expense + previous_experience + platform_preference +
                          involvement + appname_purchased + apporder_purchased +
                          resid_expHighU, # Residual
                        data = surveysub,
                        family = binomial)

summary(model_2sri_clean)




# Linear model to predict the endogenous variable
stage1J <- lm(expHighJ_only ~ reg_focus_dummy + platform_preference + age + gender + income + 
               visit_frequency + app_expense + previous_experience + involvement, data = surveysub)

surveysub$resid_expHighJ <- residuals(stage1J)

# Add residual to the logistic model
model_2sriJ_clean <- glm(highu_rest ~
                          reg_focus_dummy +
                          expHighJ_only +
                          expHighU_only +
                          exp_both_high +
                          int_expHighJ_rf_orthogonal +
                          int_expHighU_rf_orthogonal +
                          int_expBoth_rf_orthogonal +
                          age + gender + income + visit_frequency + 
                          app_expense + previous_experience + platform_preference +
                          involvement + appname_purchased + apporder_purchased +
                           resid_expHighJ, # Residual
                        data = surveysub,
                        family = binomial)

summary(model_2sriJ_clean)




# 2-SRI IV Model - F<0.9

# First stage for each model
# Using IV: previous_experience
first_stage <- lm(expHighU_only ~ 
                    previous_experience +  # (Z)
                    reg_focus_dummy + age + gender + income + visit_frequency + 
                    app_expense + platform_preference + involvement + 
                    appname_purchased + apporder_purchased,
                  data = surveysub)

summary(first_stage)

# Save residuals from first stage (û)
surveysub$expHighU_residuals <- resid(first_stage)

# The residual from first stage: expHighU_residuals (correction term for endogeneity)
logit_1 <- glm(highu_rest ~ 
                    reg_focus_dummy + 
                    expHighU_only +                      # Endogenous regressor (X)
                    expHighU_residuals +                     # Residual from Stage 1 (û)
                    expHighJ_only + 
                    exp_both_high + 
                    int_expHighJ_rf_orthogonal + 
                    int_expHighU_rf_orthogonal + 
                    int_expBoth_rf_orthogonal + 
                    age + gender + income + visit_frequency + app_expense + 
                    platform_preference + involvement + 
                    appname_purchased + apporder_purchased,
                  data = surveysub,
                  family = binomial)


summary(logit_1)





# HighJ

first_stage2 <- lm(expHighJ_only ~ 
                    previous_experience + 
                    reg_focus_dummy + age + gender + income + visit_frequency + 
                    app_expense + platform_preference + involvement + 
                    appname_purchased + apporder_purchased,
                  data = surveysub)

summary(first_stage2)

logit_1 <- glm(highu_rest ~ 
                 reg_focus_dummy + 
                 expHighU_only +                      # Endogenous regressor (X)
                 expHighU_residuals +                     # Residual from Stage 1 (û)
                 expHighJ_only + 
                 exp_both_high + 
                 int_expHighJ_rf_orthogonal + 
                 int_expHighU_rf_orthogonal + 
                 int_expBoth_rf_orthogonal + 
                 age + gender + income + visit_frequency + app_expense + 
                 platform_preference + involvement + 
                 appname_purchased + apporder_purchased,
               data = surveysub,
               family = binomial)


summary(logit_1)


first_stage3 <- lm(exp_both_high ~ 
                     previous_experience +  # (Z)
                     reg_focus_dummy + age + gender + income + visit_frequency + 
                     app_expense + platform_preference + involvement + 
                     appname_purchased + apporder_purchased,
                   data = surveysub)

summary(first_stage3)




# Model A: Naive
model_naive <- glm(highu_rest ~ 
                     reg_focus_dummy + expHighU_only + expHighJ_only + exp_both_high +
                     int_expHighJ_rf_orthogonal + int_expHighU_rf_orthogonal + int_expBoth_rf_orthogonal +
                     age + gender + income + visit_frequency + app_expense + previous_experience +
                     platform_preference + involvement + appname_purchased + apporder_purchased,
                   data = surveysub,
                   family = binomial)

# Model B: 2SRI-style (with residual)
model_resids <- glm(highu_rest ~ 
                    reg_focus_dummy + expHighU_only + expHighJ_only + exp_both_high +
                    int_expHighJ_rf_orthogonal + int_expHighU_rf_orthogonal + int_expBoth_rf_orthogonal +
                    age + gender + income + visit_frequency + app_expense + previous_experience +
                    platform_preference + involvement + appname_purchased + apporder_purchased +
                    resid_expHighU,
                  data = surveysub,
                  family = binomial)



# Compare the coefficient for expHighU_only
coef_naive <- coef(model_naive)["expHighU_only"]
coef_2sri  <- coef(model_resids)["expHighU_only"]

diff_coef <- coef_naive - coef_2sri
diff_coef
































