# Journal Manuscript Models

source("data_preparation.R")
library("tidyverse")
library("openxlsx")
library("pscl")
library("caret")
library("broom")
library("knitr")

# Data Preparation: -----

# Rename Appnames -- 
survey$appnames <- factor(recode(survey$appname,
                                 `1` = "JogStats",
                                 `2` = "Map My Walk",
                                 `3` = "FITAPP",
                                 `4` = "Running Watch"))

surveysub$highU_appname = factor(survey[survey$purchased_ratings == "HighU", "appnames"])
surveysub$highJ_appname = factor(survey[survey$purchased_ratings == "HighJ", "appnames"])
surveysub$lowU_appname = factor(survey[survey$purchased_ratings == "LowU", "appnames"])
surveysub$lowJ_appname = factor(survey[survey$purchased_ratings == "HighJ", "appnames"])

surveysub$apporder <- factor(surveysub$apporder)

surveysub$highU_apporder = factor(survey[survey$purchased_ratings == "HighU", "apporder"])
surveysub$highJ_apporder = factor(survey[survey$purchased_ratings == "HighJ", "apporder"])
surveysub$lowU_apporder = factor(survey[survey$purchased_ratings == "LowU", "apporder"])
surveysub$lowJ_apporder = factor(survey[survey$purchased_ratings == "HighJ", "apporder"])

# Categorical variables for App Name (recode) and App Position --

# Recode appname to appname_purchased
surveysub$appname_purchased <- recode(surveysub$appname,
                                      `1` = "JogStats",
                                      `2` = "Map My Walk",
                                      `3` = "FITAPP",
                                      `4` = "Running Watch")

# Convert to factor
surveysub$appname_purchased <- as.factor(surveysub$appname_purchased)

# Rename apporder and covert to factor 
surveysub$apporder_purchased <- surveysub$apporder

surveysub$apporder_purchased <- as.factor(surveysub$apporder_purchased)


# New exploration variables --

# HighU only explored (1 if explored HighU and not HighJ, 0 otherwise)
surveysub$expHighU_only <- ifelse(surveysub$highU_explored == "TRUE" & surveysub$highJ_explored == "FALSE", 1, 0)
surveysub$expHighU_only

# HighJ only explored (1 if explored HighJ and not HighU, 0 otherwise)
surveysub$expHighJ_only <- ifelse(surveysub$highJ_explored == "TRUE" & surveysub$highU_explored == "FALSE", 1, 0)
surveysub$expHighJ_only

# Both HighU and HighJ explored (1 if explored both, 0 otherwise)
surveysub$exp_both_high <- ifelse(surveysub$highU_explored == "TRUE" & surveysub$highJ_explored == "TRUE", 1, 0)
surveysub$exp_both_high

# Let's compare all three
explore_table <- surveysub %>%
  summarise(
    HighU_Only_Explored = sum(expHighU_only, na.rm = TRUE),
    HighJ_Only_Explored = sum(expHighJ_only, na.rm = TRUE),
    Both_HighU_HighJ_Explored = sum(exp_both_high, na.rm = TRUE)
  )

explore_table

# Regulatory Focus Dummy
surveysub$reg_focus_dummy <- ifelse(surveysub$regulatory_focus == "Prevention", 1, 0)
surveysub$reg_focus_dummy



# Logit Models with App-name and -order (w/ & w/o Interaction)----

# Model: Purchase_HighU_apps ~ RF

filter_highu_apps <- surveysub

# Rest_HighU Base
filter_highu_apps$regulatory_focus <- 
  relevel(factor(filter_highu_apps$regulatory_focus), ref = "Promotion")

hirest_rfhighu_base <- glm(highu_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             appname_purchased +
                             apporder_purchased +
                             highU_explored, 
                           data = filter_highu_apps, 
                           family = binomial)

summary(hirest_rfhighu_base)

# Summary into a data frame
purchase_highu_base <- tidy(hirest_rfhighu_base)

# Round the numeric columns to three decimal places
purchase_highu_base$estimate <- round(purchase_highu_base$estimate, 2)
purchase_highu_base$std.error <- round(purchase_highu_base$std.error, 2)
purchase_highu_base$statistic <- round(purchase_highu_base$statistic, 2)
purchase_highu_base$p.value <- round(purchase_highu_base$p.value, 2)

write.xlsx(purchase_highu_base, file = "temporary_files/logit_highu_base.xlsx")

# Rest_HighU w/ Interaction
filter_highu_apps$regulatory_focus <- 
  relevel(factor(filter_highu_apps$regulatory_focus), ref = "Promotion")

hirest_rfhighu_int <- glm(highu_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             appname_purchased +
                             apporder_purchased +
                             highU_explored + 
                             highU_explored * regulatory_focus, 
                           data = filter_highu_apps, 
                           family = binomial)

summary(hirest_rfhighu_int)

# Summary into a data frame
purchase_highu_int <- tidy(hirest_rfhighu_int)

# Round the numeric columns to three decimal places
purchase_highu_int$estimate <- round(purchase_highu_int$estimate, 2)
purchase_highu_int$std.error <- round(purchase_highu_int$std.error, 2)
purchase_highu_int$statistic <- round(purchase_highu_int$statistic, 2)
purchase_highu_int$p.value <- round(purchase_highu_int$p.value, 2)

write.xlsx(purchase_highu_int, file = "temporary_files/logt_highu_int.xlsx")

# Model: Purchase_HighJ ~ RF
filter_highj_apps <- surveysub

# Rest_HighJ
filter_highj_apps$regulatory_focus <- 
  relevel(factor(filter_highj_apps$regulatory_focus), ref = "Promotion")

hirest_rfhighj_base <- glm(highj_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             appname_purchased +
                             apporder_purchased +
                             highJ_explored, 
                           data = filter_highj_apps, 
                           family = binomial)

summary(hirest_rfhighj_base)

# Summary into a data frame
purchase_highj_base <- tidy(hirest_rfhighj_base)

# Round the numeric columns to three decimal places
purchase_highj_base$estimate <- round(purchase_highj_base$estimate, 2)
purchase_highj_base$std.error <- round(purchase_highj_base$std.error, 2)
purchase_highj_base$statistic <- round(purchase_highj_base$statistic, 2)
purchase_highj_base$p.value <- round(purchase_highj_base$p.value, 2)

write.xlsx(purchase_highj_base, file = "temporary_files/logit_highj_base.xlsx")

# Rest_HighJ w/ Interaction
filter_highj_apps$regulatory_focus <- 
  relevel(factor(filter_highj_apps$regulatory_focus), ref = "Promotion")

hirest_rfhighj_int <- glm(highj_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             appname_purchased +
                             apporder_purchased +
                             highJ_explored + 
                             highJ_explored * regulatory_focus, 
                           data = filter_highj_apps, 
                           family = binomial)

summary(hirest_rfhighj_int)

# Summary into a data frame
purchase_highj_int <- tidy(hirest_rfhighj_int)

# Round the numeric columns to three decimal places
purchase_highj_int$estimate <- round(purchase_highj_int$estimate, 2)
purchase_highj_int$std.error <- round(purchase_highj_int$std.error, 2)
purchase_highj_int$statistic <- round(purchase_highj_int$statistic, 2)
purchase_highj_int$p.value <- round(purchase_highj_int$p.value, 2)

write.xlsx(purchase_highj_int, file = "temporary_files/logit_highj_int.xlsx")

# BIC for both HighU and HighJ models ----

bic_highu_base <- BIC(hirest_rfhighu_base)
bic_highu_int <- BIC(hirest_rfhighu_int)

bic_highu <- bic_highu_base - bic_highu_int
bic_highu

bic_highj_base <- BIC(hirest_rfhighj_base)
bic_highj_int <- BIC(hirest_rfhighj_int)

bic_highj <- bic_highj_base - bic_highj_int
bic_highj

# Pseudo R^2 (Nagelkerke)
PseudoR2(hirest_rfhighu_base, which = NULL)

pr2_highubase <- pR2(hirest_rfhighu_base)
pr2_highubase

pr2_highuint <- pR2(hirest_rfhighu_int)
pr2_highuint

pr2_highjbase <- pR2(hirest_rfhighj_base)
pr2_highjbase

pr2_highjint <- pR2(hirest_rfhighj_int)
pr2_highjint

# (in-sample) Classification Accuracy

# predicted probabilities from each model
predict_probs1 <- predict(hirest_rfhighu_base, type = "response")  # Type response gives probabilities

# converting probabilities into binary predictions (using 0.5 as a threshold)
predict_class1 <- ifelse(predict_probs1 > 0.5, 1, 0)

# calculate the accuracy by comparing predicted class with actual class
actual_class1 <- surveysub$highu_rest

accuracy_highubase <- mean(predict_class1 == actual_class1)

paste("Classification Accuracy for Model 1:", accuracy_highubase)

# HighU w/ Interaction

predict_probs2 <- predict(hirest_rfhighu_int, type = "response")

predict_class2 <- ifelse(predict_probs2 > 0.5, 1, 0)

actual_class2 <- surveysub$highu_rest

accuracy_highuint <- mean(predict_class2 == actual_class2)

paste("Classification Accuracy for Model 2:", accuracy_highuint)

# HighJ Base

predict_probs3 <- predict(hirest_rfhighj_base, type = "response")

predict_class3 <- ifelse(predict_probs3 > 0.5, 1, 0)

actual_class3 <- surveysub$highj_rest

accuracy_highjbase <- mean(predict_class3 == actual_class3)

paste("Classification Accuracy for Model 3:", accuracy_highjbase)

# HighJ w/ Interaction

predict_probs4 <- predict(hirest_rfhighj_int, type = "response")

predict_class4 <- ifelse(predict_probs4 > 0.5, 1, 0)

actual_class4 <- surveysub$highj_rest

accuracy_highjint <- mean(predict_class4 == actual_class4)

paste("Classification Accuracy for Model 4:", accuracy_highjint)

# (in-sample) Classification Accuracy - Confusion Matrix

cm_highubase <- confusionMatrix(as.factor(predict_class1), as.factor(actual_class1))
cm_highubase

cm_highuint <- confusionMatrix(as.factor(predict_class2), as.factor(actual_class2))
cm_highuint

cm_highjbase <- confusionMatrix(as.factor(predict_class3), as.factor(actual_class3))
cm_highjbase

cm_highjint <- confusionMatrix(as.factor(predict_class4), as.factor(actual_class4))
cm_highjint

# Purchase_HighU and Purchase_HighJ models w/o Explore

# Rest_HighJ w/o Explore

hijrest_noexp <- glm(highj_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             appname_purchased + 
                             apporder_purchased, 
                           data = filter_highj_apps, 
                           family = binomial) 

summary(hijrest_noexp)

# Summary into a data frame
purchase_highj_noexp <- tidy(hijrest_noexp)

# Round the numeric columns to three decimal places
purchase_highj_noexp$estimate <- round(purchase_highj_noexp$estimate, 2)
purchase_highj_noexp$std.error <- round(purchase_highj_noexp$std.error, 2)
purchase_highj_noexp$statistic <- round(purchase_highj_noexp$statistic, 2)
purchase_highj_noexp$p.value <- round(purchase_highj_noexp$p.value, 2)

write.xlsx(purchase_highj_noexp, file = "temporary_files/logit_highj_noexp.xlsx")

# Rest_HighU w/o Explore
hiurest_noexp <- glm(highu_rest ~
                      age + 
                      gender + 
                      income + 
                      visit_frequency + app_expense + 
                      previous_experience + 
                      regulatory_focus + 
                      platform_preference + 
                      involvement + 
                      appname_purchased +
                      apporder_purchased, 
                    data = filter_highu_apps, 
                    family = binomial)

summary(hiurest_noexp)

# Summary into a data frame
purchase_highu_noexp <- tidy(hiurest_noexp)

# Round the numeric columns to three decimal places
purchase_highu_noexp$estimate <- round(purchase_highu_noexp$estimate, 2)
purchase_highu_noexp$std.error <- round(purchase_highu_noexp$std.error, 2)
purchase_highu_noexp$statistic <- round(purchase_highu_noexp$statistic, 2)
purchase_highu_noexp$p.value <- round(purchase_highu_noexp$p.value, 2)

write.xlsx(purchase_highu_noexp, file = "temporary_files/logit_highu_noexp.xlsx")


# Rest_HighJ w/ two Explore variables

highj_rest_twoexp <- glm(highj_rest ~
                       regulatory_focus +
                       highU_explored +
                       highJ_explored +
                       age + 
                       gender + 
                       income + 
                       visit_frequency + app_expense + 
                       previous_experience + 
                       platform_preference + 
                       involvement + 
                       appname_purchased + 
                       apporder_purchased, 
                     data = filter_highj_apps, 
                     family = binomial) 

summary(highj_rest_twoexp)

# Summary into a data frame
purchase_highj_twoexp <- tidy(highj_rest_twoexp)

# Round the numeric columns to three decimal places
purchase_highj_twoexp$estimate <- round(purchase_highj_twoexp$estimate, 2)
purchase_highj_twoexp$std.error <- round(purchase_highj_twoexp$std.error, 2)
purchase_highj_twoexp$statistic <- round(purchase_highj_twoexp$statistic, 2)
purchase_highj_twoexp$p.value <- round(purchase_highj_twoexp$p.value, 2)

write.xlsx(purchase_highj_twoexp, file = "temporary_files/logit_highj_twoexp.xlsx")

# Rest_HighU w/ two Explore variables
highu_rest_twoexp <- glm(highu_rest ~
                      regulatory_focus +
                      highU_explored +
                      highJ_explored +
                       age + 
                       gender + 
                       income + 
                       visit_frequency + app_expense + 
                       previous_experience + 
                       regulatory_focus + 
                       platform_preference + 
                       involvement + 
                       appname_purchased +
                       apporder_purchased, 
                     data = filter_highu_apps, 
                     family = binomial)

summary(highu_rest_twoexp)

# Summary into a data frame
purchase_highu_twoexp <- tidy(highu_rest_twoexp)

# Round the numeric columns to three decimal places
purchase_highu_twoexp$estimate <- round(purchase_highu_twoexp$estimate, 2)
purchase_highu_twoexp$std.error <- round(purchase_highu_twoexp$std.error, 2)
purchase_highu_twoexp$statistic <- round(purchase_highu_twoexp$statistic, 2)
purchase_highu_twoexp$p.value <- round(purchase_highu_twoexp$p.value, 2)

write.xlsx(purchase_highu_twoexp, file = "temporary_files/logit_highu_twoexp.xlsx")



# New Logits with new three Explore Variables ----

filter_highu_apps <- surveysub
filter_highj_apps <- surveysub


# Rest_HighU w/ two Explore variables

highu_rest_threeexp <- glm(highu_rest ~
                             regulatory_focus +
                             expHighJ_only +
                             expHighU_only +
                             exp_both_high +
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             platform_preference + 
                             involvement + 
                             appname_purchased + 
                             apporder_purchased, 
                           data = filter_highu_apps, 
                           family = binomial) 

summary(highu_rest_threeexp)

# Summary into a data frame
purchase_highu_three <- tidy(highu_rest_threeexp)

# Round the numeric columns to three decimal places
purchase_highu_three$estimate <- round(purchase_highu_three$estimate, 2)
purchase_highu_three$std.error <- round(purchase_highu_three$std.error, 2)
purchase_highu_three$statistic <- round(purchase_highu_three$statistic, 2)
purchase_highu_three$p.value <- round(purchase_highu_three$p.value, 2)

write.xlsx(purchase_highu_three, file = "temporary_files/logit_highu_three.xlsx")

# Rest_HighJ w/ two Explore variables

highj_rest_threeexp <- glm(highj_rest ~
                           regulatory_focus +
                           expHighJ_only +
                           expHighU_only +
                          exp_both_high +
                           age + 
                           gender + 
                           income + 
                           visit_frequency + app_expense + 
                           previous_experience + 
                           platform_preference + 
                           involvement + 
                           appname_purchased + 
                           apporder_purchased, 
                         data = filter_highj_apps, 
                         family = binomial) 

summary(highj_rest_threeexp)

# Summary into a data frame
purchase_highj_three <- tidy(highj_rest_threeexp)

# Round the numeric columns to three decimal places
purchase_highj_three$estimate <- round(purchase_highj_three$estimate, 2)
purchase_highj_three$std.error <- round(purchase_highj_three$std.error, 2)
purchase_highj_three$statistic <- round(purchase_highj_three$statistic, 2)
purchase_highj_three$p.value <- round(purchase_highj_three$p.value, 2)

write.xlsx(purchase_highj_three, file = "temporary_files/logit_highj_three.xlsx")



highu_rest_temp <- glm(highu_rest ~
                             regulatory_focus +
                             expHighJ_only +
                             expHighU_only +
                             exp_both_high +
                             expHighJ_only * regulatory_focus +
                             expHighU_only * regulatory_focus +
                             exp_both_high * regulatory_focus +
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             platform_preference + 
                             involvement + 
                             appname_purchased + 
                             apporder_purchased, 
                           data = filter_highu_apps, 
                           family = binomial) 

summary(highu_rest_temp)

# Summary into a data frame
purchase_highu_temp <- tidy(highu_rest_temp)

# Round the numeric columns to three decimal places
purchase_highu_temp$estimate <- round(purchase_highu_temp$estimate, 2)
purchase_highu_temp$std.error <- round(purchase_highu_temp$std.error, 2)
purchase_highu_temp$statistic <- round(purchase_highu_temp$statistic, 2)
purchase_highu_temp$p.value <- round(purchase_highu_temp$p.value, 2)

write.xlsx(purchase_highu_temp, file = "temporary_files/logit_highu_temp.xlsx")

highj_rest_temp <- glm(highj_rest ~
                         regulatory_focus +
                         expHighJ_only +
                         expHighU_only +
                         exp_both_high +
                         expHighJ_only * regulatory_focus +
                         expHighU_only * regulatory_focus +
                         exp_both_high * regulatory_focus +
                         age + 
                         gender + 
                         income + 
                         visit_frequency + app_expense + 
                         previous_experience + 
                         platform_preference + 
                         involvement + 
                         appname_purchased + 
                         apporder_purchased, 
                       data = filter_highj_apps, 
                       family = binomial) 

summary(highj_rest_temp)

# Summary into a data frame
purchase_highj_temp <- tidy(highj_rest_temp)

# Round the numeric columns to three decimal places
purchase_highj_temp$estimate <- round(purchase_highj_temp$estimate, 2)
purchase_highj_temp$std.error <- round(purchase_highj_temp$std.error, 2)
purchase_highj_temp$statistic <- round(purchase_highj_temp$statistic, 2)
purchase_highj_temp$p.value <- round(purchase_highj_temp$p.value, 2)

write.xlsx(purchase_highj_temp, file = "temporary_files/logit_highj_temp.xlsx")


# BIC for both HighU and HighJ models ----

bic_highu_exp_base <- BIC(highu_rest_threeexp)
bic_highu_exp_base
bic_highu_exp_int <- BIC(highu_rest_temp)
bic_highu_exp_int

bic_highu_exp <- bic_highu_exp_base - bic_highu_exp_int
bic_highu_exp

bic_highj_exp_base <- BIC(highj_rest_threeexp)
bic_highj_exp_int <- BIC(highj_rest_temp)

bic_highj_exp <- bic_highj_exp_base - bic_highj_exp_int
bic_highj_exp

# Pseudo R^2 (Nagelkerke)
PseudoR2(hirest_rfhighu_base, which = NULL)

pr2_highu_exp_base <- pR2(highu_rest_threeexp)
round(pr2_highu_exp_base, 3)

pr2_highu_exp_int <- pR2(highu_rest_temp)
round(pr2_highu_exp_int, 3)

pr2_highj_exp_base <- pR2(highj_rest_threeexp)
round(pr2_highj_exp_base, 3)

pr2_highj_exp_int <- pR2(highj_rest_temp)
round(pr2_highj_exp_int, 3)

# Chi-square

highu_rest_threeexp

null_highu_base <- glm(highu_rest ~ 1, data = filter_highu_apps, family = binomial)

chi_highu_base <- null_highu_base$deviance - highu_rest_threeexp$deviance

df_highu_base <- null_highu_base$df.residual - highu_rest_threeexp$df.residual

p_highu_base <- pchisq(chi_highu_base, df_highu_base, lower.tail = FALSE)

round(chi_highu_base, 2)

# Output of Chi-square statistic and p-value
cat("Chi-square statistic:", chi_highu_base, "\n")
cat("Degrees of freedom:", df_highu_base, "\n")
cat("P-value:", p_highu_base, "\n")

highu_rest_temp

null_highu_int <- glm(highu_rest ~ 1, data = filter_highu_apps, family = binomial)

chi_highu_int <- null_highu_int$deviance - highu_rest_temp$deviance

df_highu_int <- null_highu_int$df.residual - highu_rest_temp$df.residual

p_highu_int <- pchisq(chi_highu_int, df_highu_int, lower.tail = FALSE)

round(chi_highu_int, 2)

# Output of Chi-square statistic and p-value
cat("Chi-square statistic:", chi_highu_int, "\n")
cat("Degrees of freedom:", df_highu_int, "\n")
cat("P-value:", p_highu_int, "\n")

null_highj_base <- glm(highj_rest ~ 1, data = filter_highj_apps, family = binomial)

chi_highj_base <- null_highj_base$deviance - highj_rest_threeexp$deviance

df_highj_base <- null_highj_base$df.residual - highj_rest_threeexp$df.residual

p_highj_base <- pchisq(chi_highj_base, df_highj_base, lower.tail = FALSE)

round(chi_highj_base, 2)

# Output of Chi-square statistic and p-value
cat("Chi-square statistic:", chi_highj_base, "\n")
cat("Degrees of freedom:", df_highj_base, "\n")
cat("P-value:", p_highj_base, "\n")

null_highj_int <- glm(highj_rest ~ 1, data = filter_highj_apps, family = binomial)

chi_highj_int <- null_highj_int$deviance - highj_rest_temp$deviance

df_highj_int <- null_highj_int$df.residual - highj_rest_temp$df.residual

p_highj_int <- pchisq(chi_highj_int, df_highj_int, lower.tail = FALSE)

round(chi_highj_int, 2)

# Output of Chi-square statistic and p-value
cat("Chi-square statistic:", chi_highj_int, "\n")
cat("Degrees of freedom:", df_highj_int, "\n")
cat("P-value:", p_highj_int, "\n")


# Orthogonalization of Intx terms ----
surveysub$reg_focus_dummy <- ifelse(surveysub$regulatory_focus == "Prevention", 1, 0)
surveysub$reg_focus_dummy

# Interaction terms using a dummy variable (every variable has to be numeric)
surveysub$int_expHighJ_rf <- surveysub$expHighJ_only * surveysub$reg_focus_dummy
surveysub$int_expHighU_rf <- surveysub$expHighU_only * surveysub$reg_focus_dummy
surveysub$int_expBoth_rf  <- surveysub$exp_both_high * surveysub$reg_focus_dummy

# Orthogonalize the interaction terms
int_expHighJ_rf_reg <- lm(int_expHighJ_rf ~ expHighJ_only + reg_focus_dummy, data = surveysub)
surveysub$int_expHighJ_rf_orthogonal <- residuals(int_expHighJ_rf_reg)

int_expHighU_rf_reg <- lm(int_expHighU_rf ~ expHighU_only + reg_focus_dummy, data = surveysub)
surveysub$int_expHighU_rf_orthogonal <- residuals(int_expHighU_rf_reg)

int_expBoth_rf_reg <- lm(int_expBoth_rf ~ exp_both_high + reg_focus_dummy, data = surveysub)
surveysub$int_expBoth_rf_orthogonal <- residuals(int_expBoth_rf_reg)

# HighU
highu_rest_orthogonal <- glm(highu_rest ~
                                    reg_focus_dummy +  # Use the single dummy variable
                                    expHighJ_only +
                                    expHighU_only +
                                    exp_both_high +
                                    int_expHighJ_rf_orthogonal +
                                    int_expHighU_rf_orthogonal +
                                    int_expBoth_rf_orthogonal +
                                    age + 
                                    gender + 
                                    income + 
                                    visit_frequency + 
                                    app_expense + 
                                    previous_experience + 
                                    platform_preference + 
                                    involvement + 
                                    appname_purchased + 
                                    apporder_purchased, 
                                  data = surveysub, 
                                  family = binomial)

summary(highu_rest_orthogonal)

# Summary into a data frame
purchase_highu_orthogonal<- tidy(highu_rest_orthogonal)

# Round the numeric columns to three decimal places
purchase_highu_orthogonal$estimate <- round(purchase_highu_orthogonal$estimate, 2)
purchase_highu_orthogonal$std.error <- round(purchase_highu_orthogonal$std.error, 2)
purchase_highu_orthogonal$statistic <- round(purchase_highu_orthogonal$statistic, 2)
purchase_highu_orthogonal$p.value <- round(purchase_highu_orthogonal$p.value, 2)

write.xlsx(purchase_highu_orthogonal, file = "temporary_files/logit_highu_orthogonal.xlsx")

# Compare to original model:
summary(highu_rest_temp)

# HighJ
highj_rest_orthogonal <- glm(highj_rest ~
                                    reg_focus_dummy +  # Use the single dummy variable
                                    expHighJ_only +
                                    expHighU_only +
                                    exp_both_high +
                                    int_expHighJ_rf_orthogonal +  # Orthogonalized interaction
                                    int_expHighU_rf_orthogonal +  # Orthogonalized interaction
                                    int_expBoth_rf_orthogonal +   # Orthogonalized interaction
                                    age + 
                                    gender + 
                                    income + 
                                    visit_frequency + 
                                    app_expense + 
                                    previous_experience + 
                                    platform_preference + 
                                    involvement + 
                                    appname_purchased + 
                                    apporder_purchased, 
                                  data = surveysub, 
                                  family = binomial)

summary(highj_rest_orthogonal)

# Summary into a data frame
purchase_highj_orthogonal<- tidy(highj_rest_orthogonal)

# Round the numeric columns to three decimal places
purchase_highj_orthogonal$estimate <- round(purchase_highj_orthogonal$estimate, 2)
purchase_highj_orthogonal$std.error <- round(purchase_highj_orthogonal$std.error, 2)
purchase_highj_orthogonal$statistic <- round(purchase_highj_orthogonal$statistic, 2)
purchase_highj_orthogonal$p.value <- round(purchase_highj_orthogonal$p.value, 2)

write.xlsx(purchase_highj_orthogonal, file = "temporary_files/logit_highj_orthogonal.xlsx")

# Compare to original model:
summary(highj_rest_temp)


# Mean-centered Purchased Models ----

mean_reg_focus <- mean(as.numeric(surveysub$reg_focus_dummy))
mean_expHighJ <- mean(as.numeric(surveysub$expHighJ_only))
mean_expHighU <- mean(as.numeric(surveysub$expHighU_only))
mean_exp_both_high <- mean(as.numeric(surveysub$exp_both_high))

# Center each predictor by subtracting its mean
surveysub$centered_reg_focus <- as.numeric(surveysub$reg_focus_dummy) - mean_reg_focus
surveysub$centered_expHighJ <- as.numeric(surveysub$expHighJ_only) - mean_expHighJ
surveysub$centered_expHighU <- as.numeric(surveysub$expHighU_only) - mean_expHighU
surveysub$centered_exp_both_high <- as.numeric(surveysub$exp_both_high) - mean_exp_both_high

# Centered interaction terms
surveysub$centered_int_expHighJ_rf <- surveysub$centered_expHighJ * surveysub$centered_reg_focus
surveysub$centered_int_expHighU_rf <- surveysub$centered_expHighU * surveysub$centered_reg_focus
surveysub$centered_int_expBoth_rf <- surveysub$centered_exp_both_high * surveysub$centered_reg_focus

# HighU Mean-Centered
highu_rest_centered <- glm(highu_rest ~
                             centered_reg_focus + 
                             centered_expHighJ + 
                             centered_expHighU + 
                             centered_exp_both_high + 
                             centered_int_expHighJ_rf + 
                             centered_int_expHighU_rf + 
                             centered_int_expBoth_rf + 
                             age + 
                             gender + 
                             income + 
                             visit_frequency + 
                             app_expense + 
                             previous_experience + 
                             platform_preference + 
                             involvement + 
                             appname_purchased + 
                             apporder_purchased, 
                           data = surveysub, 
                           family = binomial)

summary(highu_rest_centered)

# Data frame
purchase_highu_centered<- tidy(highu_rest_centered)

# Round the numeric columns to three decimal places
purchase_highu_centered$estimate <- round(purchase_highu_centered$estimate, 2)
purchase_highu_centered$std.error <- round(purchase_highu_centered$std.error, 2)
purchase_highu_centered$statistic <- round(purchase_highu_centered$statistic, 2)
purchase_highu_centered$p.value <- round(purchase_highu_centered$p.value, 2)

write.xlsx(purchase_highu_centered, file = "temporary_files/logit_highu_centered.xlsx")

# HighJ Mean-Centered
highj_rest_centered <- glm(highj_rest ~
                             centered_reg_focus + 
                             centered_expHighJ + 
                             centered_expHighU + 
                             centered_exp_both_high + 
                             centered_int_expHighJ_rf + 
                             centered_int_expHighU_rf + 
                             centered_int_expBoth_rf + 
                             age + 
                             gender + 
                             income + 
                             visit_frequency + 
                             app_expense + 
                             previous_experience + 
                             platform_preference + 
                             involvement + 
                             appname_purchased + 
                             apporder_purchased, 
                           data = surveysub, 
                           family = binomial)

summary(highj_rest_centered)

# Data frame
purchase_highj_centered<- tidy(highj_rest_centered)

# Round the numeric columns to three decimal places
purchase_highj_centered$estimate <- round(purchase_highj_centered$estimate, 2)
purchase_highj_centered$std.error <- round(purchase_highj_centered$std.error, 2)
purchase_highj_centered$statistic <- round(purchase_highj_centered$statistic, 2)
purchase_highj_centered$p.value <- round(purchase_highj_centered$p.value, 2)

write.xlsx(purchase_highj_centered, file = "temporary_files/logit_highj_centered.xlsx")


# Effect coding with purchase models:

effect_sub <- surveysub

effect_sub$expHighU_only <- factor(effect_sub$expHighU_only, levels = c(0, 1), labels = c("NotExpU", "ExpU"))
effect_sub$expHighJ_only <- factor(effect_sub$expHighJ_only, levels = c(0, 1), labels = c("NotExpJ", "ExpJ"))
effect_sub$exp_both_high <- factor(effect_sub$exp_both_high, levels = c(0, 1), labels = c("NotBoth", "Both"))

# Effect coding for main categorical variables
contrasts(effect_sub$expHighU_only) <- contr.sum(2)
contrasts(effect_sub$expHighJ_only) <- contr.sum(2)
contrasts(effect_sub$exp_both_high) <- contr.sum(2)
contrasts(effect_sub$regulatory_focus) <- contr.sum(2)

# HighU
effect_codedU <- glm(highu_rest ~
                            regulatory_focus * expHighJ_only +  
                            regulatory_focus * expHighU_only +  
                            regulatory_focus * exp_both_high +
                            age +                                
                            gender + 
                            income + 
                            visit_frequency + 
                            app_expense + 
                            previous_experience + 
                            platform_preference + 
                            involvement + 
                            appname_purchased + 
                            apporder_purchased,
                          family = binomial,
                          data = effect_sub)

summary(effect_codedU)

purchase_highu_ec<- tidy(effect_codedU)

# Round the numeric columns to three decimal places
purchase_highu_ec$estimate <- round(purchase_highu_ec$estimate, 2)
purchase_highu_ec$std.error <- round(purchase_highu_ec$std.error, 2)
purchase_highu_ec$statistic <- round(purchase_highu_ec$statistic, 2)
purchase_highu_ec$p.value <- round(purchase_highu_ec$p.value, 2)

write.xlsx(purchase_highu_ec, file = "temporary_files/logit_highu_ec.xlsx")

# HighJ
effect_codedJ <- glm(highj_rest ~
                            regulatory_focus * expHighJ_only +  
                            regulatory_focus * expHighU_only +  
                            regulatory_focus * exp_both_high +
                            age +                                
                            gender + 
                            income + 
                            visit_frequency + 
                            app_expense + 
                            previous_experience + 
                            platform_preference + 
                            involvement + 
                            appname_purchased + 
                            apporder_purchased,
                          family = binomial,
                          data = effect_sub)

summary(effect_codedJ)

purchase_highj_ec<- tidy(effect_codedJ)

# Round the numeric columns to three decimal places
purchase_highj_ec$estimate <- round(purchase_highj_ec$estimate, 2)
purchase_highj_ec$std.error <- round(purchase_highj_ec$std.error, 2)
purchase_highj_ec$statistic <- round(purchase_highj_ec$statistic, 2)
purchase_highj_ec$p.value <- round(purchase_highj_ec$p.value, 2)

write.xlsx(purchase_highj_ec, file = "temporary_files/logit_highj_ec.xlsx")







# Models for final buy table ----

# Base: RF_Only
# HighU
purchase_highu_base <- glm(highu_rest ~
                               reg_focus_dummy +
                               age + 
                               gender + 
                               income + 
                               visit_frequency + 
                               app_expense + 
                               previous_experience + 
                               platform_preference + 
                               involvement + 
                               appname_purchased + 
                               apporder_purchased, 
                             data = surveysub, 
                             family = binomial)

summary(purchase_highu_base)

# Summary into a data frame
purchase_highu_base <- tidy(purchase_highu_base)

# Round the numeric columns to two decimal places
purchase_highu_base$estimate <- round(purchase_highu_base$estimate, 2)
purchase_highu_base$std.error <- round(purchase_highu_base$std.error, 2)
purchase_highu_base$statistic <- round(purchase_highu_base$statistic, 2)
purchase_highu_base$p.value <- round(purchase_highu_base$p.value, 2)

write.xlsx(purchase_highu_base, file = "temporary_files/logit_highu_base.xlsx")

# HighJ
purchase_highj_base <- glm(highj_rest ~
                               reg_focus_dummy +
                               age + 
                               gender + 
                               income + 
                               visit_frequency + 
                               app_expense + 
                               previous_experience + 
                               platform_preference + 
                               involvement + 
                               appname_purchased + 
                               apporder_purchased, 
                             data = surveysub, 
                             family = binomial)

summary(purchase_highj_base)

# Summary into a data frame
purchase_highj_base <- tidy(purchase_highj_base)

# Round the numeric columns to two decimal places
purchase_highj_base$estimate <- round(purchase_highj_base$estimate, 2)
purchase_highj_base$std.error <- round(purchase_highj_base$std.error, 2)
purchase_highj_base$statistic <- round(purchase_highj_base$statistic, 2)
purchase_highj_base$p.value <- round(purchase_highj_base$p.value, 2)

write.xlsx(purchase_highj_base, file = "temporary_files/logit_highj_base.xlsx")


# Exp: RF_Only + Explorations

# HighU Exploration
purchase_highu_exp <- glm(highu_rest ~
                             reg_focus_dummy +
                             expHighJ_only +
                             expHighU_only +
                             exp_both_high +
                             age + 
                             gender + 
                             income + 
                             visit_frequency + 
                             app_expense + 
                             previous_experience + 
                             platform_preference + 
                             involvement + 
                             appname_purchased + 
                             apporder_purchased, 
                           data = surveysub, 
                           family = binomial)

summary(purchase_highu_exp)

# Summary into a data frame
purchase_highu_exp <- tidy(purchase_highu_exp)

# Round the numeric columns to two decimal places
purchase_highu_exp$estimate <- round(purchase_highu_exp$estimate, 2)
purchase_highu_exp$std.error <- round(purchase_highu_exp$std.error, 2)
purchase_highu_exp$statistic <- round(purchase_highu_exp$statistic, 2)
purchase_highu_exp$p.value <- round(purchase_highu_exp$p.value, 2)

write.xlsx(purchase_highu_exp, file = "temporary_files/logit_highu_exp.xlsx")

# HighJ Exploration
purchase_highj_exp <- glm(highj_rest ~
                             reg_focus_dummy +
                             expHighJ_only +
                             expHighU_only +
                             exp_both_high +
                             age + 
                             gender + 
                             income + 
                             visit_frequency + 
                             app_expense + 
                             previous_experience + 
                             platform_preference + 
                             involvement + 
                             appname_purchased + 
                             apporder_purchased, 
                           data = surveysub, 
                           family = binomial)

summary(purchase_highj_exp)

# Summary into a data frame
purchase_highj_exp <- tidy(purchase_highj_exp)

# Round the numeric columns to two decimal places
purchase_highj_exp$estimate <- round(purchase_highj_exp$estimate, 2)
purchase_highj_exp$std.error <- round(purchase_highj_exp$std.error, 2)
purchase_highj_exp$statistic <- round(purchase_highj_exp$statistic, 2)
purchase_highj_exp$p.value <- round(purchase_highj_exp$p.value, 2)

write.xlsx(purchase_highj_exp, file = "temporary_files/logit_highj_exp.xlsx")


# Buy Models with Promotion instead ----

# RegFocus base term
surveysub$reg_focus_dummy2 <- ifelse(surveysub$regulatory_focus == "Promotion", 1, 0)
surveysub$reg_focus_dummy2

# Base: RF_Only
# HighU
purchase_highu_prom <- glm(highu_rest ~
                             reg_focus_dummy2 +
                             age + 
                             gender + 
                             income + 
                             visit_frequency + 
                             app_expense + 
                             previous_experience + 
                             platform_preference + 
                             involvement + 
                             appname_purchased + 
                             apporder_purchased, 
                           data = surveysub, 
                           family = binomial)

summary(purchase_highu_prom)

# Summary into a data frame
purchase_highu_prom <- tidy(purchase_highu_prom)

# Round the numeric columns to two decimal places
purchase_highu_prom$estimate <- round(purchase_highu_prom$estimate, 2)
purchase_highu_prom$std.error <- round(purchase_highu_prom$std.error, 2)
purchase_highu_prom$statistic <- round(purchase_highu_prom$statistic, 2)
purchase_highu_prom$p.value <- round(purchase_highu_prom$p.value, 2)

write.xlsx(purchase_highu_prom, file = "temporary_files/logit_highu_prom.xlsx")

# HighJ
purchase_highj_prom <- glm(highj_rest ~
                             reg_focus_dummy2 +
                             age + 
                             gender + 
                             income + 
                             visit_frequency + 
                             app_expense + 
                             previous_experience + 
                             platform_preference + 
                             involvement + 
                             appname_purchased + 
                             apporder_purchased, 
                           data = surveysub, 
                           family = binomial)

summary(purchase_highj_prom)

# Summary into a data frame
purchase_highj_prom <- tidy(purchase_highj_prom)

# Round the numeric columns to two decimal places
purchase_highj_prom$estimate <- round(purchase_highj_prom$estimate, 2)
purchase_highj_prom$std.error <- round(purchase_highj_prom$std.error, 2)
purchase_highj_prom$statistic <- round(purchase_highj_prom$statistic, 2)
purchase_highj_prom$p.value <- round(purchase_highj_prom$p.value, 2)

write.xlsx(purchase_highj_prom, file = "temporary_files/logit_highj_prom.xlsx")


# Exp: RF_Only + Explorations

# HighU Exploration
purchase_highu_exp_prom <- glm(highu_rest ~
                                 reg_focus_dummy2 +
                                 expHighJ_only +
                                 expHighU_only +
                                 exp_both_high +
                                 age + 
                                 gender + 
                                 income + 
                                 visit_frequency + 
                                 app_expense + 
                                 previous_experience + 
                                 platform_preference + 
                                 involvement + 
                                 appname_purchased + 
                                 apporder_purchased, 
                               data = surveysub, 
                               family = binomial)

summary(purchase_highu_exp_prom)

# Summary into a data frame
purchase_highu_exp_prom <- tidy(purchase_highu_exp_prom)

# Round the numeric columns to two decimal places
purchase_highu_exp_prom$estimate <- round(purchase_highu_exp_prom$estimate, 2)
purchase_highu_exp_prom$std.error <- round(purchase_highu_exp_prom$std.error, 2)
purchase_highu_exp_prom$statistic <- round(purchase_highu_exp_prom$statistic, 2)
purchase_highu_exp_prom$p.value <- round(purchase_highu_exp_prom$p.value, 2)

write.xlsx(purchase_highu_exp_prom, file = "temporary_files/logit_highu_exp_prom.xlsx")

# HighJ Exploration
purchase_highj_exp_prom <- glm(highj_rest ~
                                 reg_focus_dummy2 +
                                 expHighJ_only +
                                 expHighU_only +
                                 exp_both_high +
                                 age + 
                                 gender + 
                                 income + 
                                 visit_frequency + 
                                 app_expense + 
                                 previous_experience + 
                                 platform_preference + 
                                 involvement + 
                                 appname_purchased + 
                                 apporder_purchased, 
                               data = surveysub, 
                               family = binomial)

summary(purchase_highj_exp_prom)

# Summary into a data frame
purchase_highj_exp_prom <- tidy(purchase_highj_exp_prom)

# Round the numeric columns to two decimal places
purchase_highj_exp_prom$estimate <- round(purchase_highj_exp_prom$estimate, 2)
purchase_highj_exp_prom$std.error <- round(purchase_highj_exp_prom$std.error, 2)
purchase_highj_exp_prom$statistic <- round(purchase_highj_exp_prom$statistic, 2)
purchase_highj_exp_prom$p.value <- round(purchase_highj_exp_prom$p.value, 2)

write.xlsx(purchase_highj_exp_prom, file = "temporary_files/logit_highj_exp_prom.xlsx")

# Interaction terms using a dummy variable (every variable has to be numeric)
surveysub$int_expHighJ_rf2 <- surveysub$expHighJ_only * surveysub$reg_focus_dummy2
surveysub$int_expHighU_rf2 <- surveysub$expHighU_only * surveysub$reg_focus_dummy2
surveysub$int_expBoth_rf2  <- surveysub$exp_both_high * surveysub$reg_focus_dummy2

# Orthogonalize the interaction terms
int_expHighJ_rf_reg2 <- lm(int_expHighJ_rf2 ~ expHighJ_only + reg_focus_dummy2, data = surveysub)
surveysub$int_expHighJ_rf_orthogonal2 <- residuals(int_expHighJ_rf_reg2)

int_expHighU_rf_reg2 <- lm(int_expHighU_rf2 ~ expHighU_only + reg_focus_dummy2, data = surveysub)
surveysub$int_expHighU_rf_orthogonal2 <- residuals(int_expHighU_rf_reg2)

int_expBoth_rf_reg2 <- lm(int_expBoth_rf2 ~ exp_both_high + reg_focus_dummy2, data = surveysub)
surveysub$int_expBoth_rf_orthogonal2 <- residuals(int_expBoth_rf_reg2)

# HighU
highu_rest_orthogonal_prom <- glm(highu_rest ~
                                    reg_focus_dummy2 +  # Use the single dummy variable
                                    expHighJ_only +
                                    expHighU_only +
                                    exp_both_high +
                                    int_expHighJ_rf_orthogonal2 +
                                    int_expHighU_rf_orthogonal2 +
                                    int_expBoth_rf_orthogonal2 +
                                    age + 
                                    gender + 
                                    income + 
                                    visit_frequency + 
                                    app_expense + 
                                    previous_experience + 
                                    platform_preference + 
                                    involvement + 
                                    appname_purchased + 
                                    apporder_purchased, 
                                  data = surveysub, 
                                  family = binomial)

summary(highu_rest_orthogonal_prom)

# Summary into a data frame
purchase_highu_orthogonal_prom <- tidy(highu_rest_orthogonal_prom)

# Round the numeric columns to three decimal places
purchase_highu_orthogonal_prom$estimate <- round(purchase_highu_orthogonal_prom$estimate, 2)
purchase_highu_orthogonal_prom$std.error <- round(purchase_highu_orthogonal_prom$std.error, 2)
purchase_highu_orthogonal_prom$statistic <- round(purchase_highu_orthogonal_prom$statistic, 2)
purchase_highu_orthogonal_prom$p.value <- round(purchase_highu_orthogonal_prom$p.value, 2)

write.xlsx(purchase_highu_orthogonal_prom, file = "temporary_files/logit_highu_orthogonal_prom.xlsx")

# HighJ
highj_rest_orthogonal_prom <- glm(highj_rest ~
                                    reg_focus_dummy2 +
                                    expHighJ_only +
                                    expHighU_only +
                                    exp_both_high +
                                    int_expHighJ_rf_orthogonal2 +
                                    int_expHighU_rf_orthogonal2 +
                                    int_expBoth_rf_orthogonal2 +
                                    age + 
                                    gender + 
                                    income + 
                                    visit_frequency + 
                                    app_expense + 
                                    previous_experience + 
                                    platform_preference + 
                                    involvement + 
                                    appname_purchased + 
                                    apporder_purchased, 
                                  data = surveysub, 
                                  family = binomial)

summary(highj_rest_orthogonal_prom)

# Summary into a data frame
purchase_highj_orthogonal_prom <- tidy(highj_rest_orthogonal_prom)

# Round the numeric columns to three decimal places
purchase_highj_orthogonal_prom$estimate <- round(purchase_highj_orthogonal_prom$estimate, 2)
purchase_highj_orthogonal_prom$std.error <- round(purchase_highj_orthogonal_prom$std.error, 2)
purchase_highj_orthogonal_prom$statistic <- round(purchase_highj_orthogonal_prom$statistic, 2)
purchase_highj_orthogonal_prom$p.value <- round(purchase_highj_orthogonal_prom$p.value, 2)

write.xlsx(purchase_highj_orthogonal_prom, file = "temporary_files/logit_highj_orthogonal_prom.xlsx")











# Models for final Exploration behavior table ----

explored_highu_apps <- glm(highU_explored ~
                             age +
                             gender +
                             income +
                             visit_frequency +
                             app_expense +
                             previous_experience +
                             regulatory_focus +
                             platform_preference +
                             involvement +
                             factor(highU_apporder) +
                             factor(highU_appname),
                           family = binomial,
                           data = surveysub)

summary(explored_highu_apps)

explored_highu_apps <- tidy(explored_highu_apps)

explored_highu_apps$estimate <- round(explored_highu_apps$estimate, 2)
explored_highu_apps$std.error <- round(explored_highu_apps$std.error, 2)
explored_highu_apps$statistic <- round(explored_highu_apps$statistic, 2)
explored_highu_apps$p.value <- round(explored_highu_apps$p.value, 2)

write.xlsx(explored_highu_apps, file = "temporary_files/explored_highu.xlsx")

explored_highj_apps <- glm(highJ_explored ~
                             age +
                             gender +
                             income +
                             visit_frequency +
                             app_expense +
                             previous_experience +
                             regulatory_focus +
                             platform_preference +
                             involvement +
                             factor(highJ_apporder) +
                             factor(highJ_appname),
                           family = binomial,
                           data = surveysub)

summary(explored_highj_apps)

explored_highj_apps <- tidy(explored_highj_apps)

explored_highj_apps$estimate <- round(explored_highj_apps$estimate, 2)
explored_highj_apps$std.error <- round(explored_highj_apps$std.error, 2)
explored_highj_apps$statistic <- round(explored_highj_apps$statistic, 2)
explored_highj_apps$p.value <- round(explored_highj_apps$p.value, 2)

write.xlsx(explored_highj_apps, file = "temporary_files/explored_highj.xlsx")


# AIC for final table ----

summary(purchase_highu_exp)
summary(purchase_highj_exp)

summary(highu_rest_orthogonal)
summary(highj_rest_orthogonal)

summary(explored_highu_apps)
summary(explored_highj_apps)

# Chi-square ----

# ANOVA - compare nested models and see if the predictors improve the model. Similar to BIC AIC

# HighU
# Compare Base Model vs Exploration Model
anova(purchase_highu_base, purchase_highu_exp, test = "Chisq")

# Compare Exploration Model vs Interaction Model
anova(purchase_highu_exp, highu_rest_orthogonal, test = "Chisq")

# HighJ
# Compare Base Model vs Exploration Model
anova(purchase_highj_base, purchase_highj_exp, test = "Chisq")

# Compare Exploration Model vs Interaction Model
anova(purchase_highj_exp, highj_rest_orthogonal, test = "Chisq")


# # NULL approach - compare the full model to a null model that has no predictors
# 
# # HighU
# null_highu <- glm(highu_rest ~ 1, data = surveysub, family = binomial)
# 
# highu_rest_orthogonal
# 
# chi_sq_highu <- null_highu$deviance - highu_rest_orthogonal$deviance
# chi_sq_highu
# 
# df_highu <- null_highu$df.residual - highu_rest_orthogonal$df.residual
# df_highu
# 
# p_highu <- pchisq(chi_sq_highu, df_highu, lower.tail = FALSE)
# p_highu
# 
# # HighJ
# null_highj <- glm(highj_rest ~ 1, data = surveysub, family = binomial)
# 
# highj_rest_orthogonal
# 
# chi_sq_highj <- null_highj$deviance - highj_rest_orthogonal$deviance
# chi_sq_highj
# 
# df_highj <- null_highj$df.residual - highj_rest_orthogonal$df.residual
# df_highj
# 
# p_highj <- pchisq(chi_sq_highj, df_highj, lower.tail = FALSE)
# p_highj

# Null approach

# HighU
chi_sq_highu <- purchase_highu_base$deviance - purchase_highu_exp$deviance
round(chi_sq_highu, 2)

df_highu <- purchase_highu_base$df.residual - purchase_highu_exp$df.residual
df_highu

p_highu <- pchisq(chi_sq_highu, df_highu, lower.tail = FALSE)
round(p_highu, 2)

chi_sq_highu2 <- purchase_highu_exp$deviance - highu_rest_orthogonal$deviance
round(chi_sq_highu2, 2)

df_highu2 <- purchase_highu_exp$df.residual - highu_rest_orthogonal$df.residual
df_highu2

p_highu2 <- pchisq(chi_sq_highu2, df_highu2, lower.tail = FALSE)
round(p_highu2, 2)

# HighJ
chi_sq_highj <- purchase_highj_base$deviance - purchase_highj_exp$deviance
round(chi_sq_highj, 2)

df_highj <- purchase_highj_base$df.residual - purchase_highj_exp$df.residual
df_highj

p_highj <- pchisq(chi_sq_highj, df_highj, lower.tail = FALSE)
round(p_highj, 2)

chi_sq_highj2 <- purchase_highj_exp$deviance - highj_rest_orthogonal$deviance
round(chi_sq_highj2, 2)

df_highj2 <- purchase_highj_exp$df.residual - highj_rest_orthogonal$df.residual
df_highj2

p_highj2 <- pchisq(chi_sq_highj2, df_highj2, lower.tail = FALSE)
round(p_highj2, 2)


# Chi-square Null for three purchase models and exploration behavior

# Purhcase models: purchase_highu_base purchase_highu_exp highu_rest_orthogonal

# HighU
# Base
null_highu <- glm(highu_rest ~ 1, data = surveysub, family = binomial)

chi_sq_highu <- null_highu$deviance - purchase_highu_base$deviance
chi_sq_highu

df_highu <- null_highu$df.residual - purchase_highu_base$df.residual
df_highu

p_highu <- pchisq(chi_sq_highu, df_highu, lower.tail = FALSE)
p_highu

# Base + Explore
chi_sq_highuexp <- null_highu$deviance - purchase_highu_exp$deviance
chi_sq_highuexp

df_highuexp <- null_highu$df.residual - purchase_highu_exp$df.residual
df_highuexp

p_highuexp <- pchisq(chi_sq_highuexp, df_highuexp, lower.tail = FALSE)
p_highuexp

# Full w/ interactions
chi_sq_highuint <- null_highu$deviance - highu_rest_orthogonal$deviance
chi_sq_highuint

df_highuint <- null_highu$df.residual - highu_rest_orthogonal$df.residual
df_highuint

p_highuint <- pchisq(chi_sq_highuint, df_highuint, lower.tail = FALSE)
p_highuint

# HighJ
# Base
null_highj <- glm(highj_rest ~ 1, data = surveysub, family = binomial)

chi_sq_highj <- null_highj$deviance - purchase_highj_base$deviance
chi_sq_highj

df_highj <- null_highj$df.residual - purchase_highj_base$df.residual
df_highj

p_highj <- pchisq(chi_sq_highj, df_highj, lower.tail = FALSE)
p_highj

# Base + Explore
chi_sq_highjexp <- null_highj$deviance - purchase_highj_exp$deviance
chi_sq_highjexp

df_highjexp <- null_highj$df.residual - purchase_highj_exp$df.residual
df_highjexp

p_highjexp <- pchisq(chi_sq_highjexp, df_highjexp, lower.tail = FALSE)
p_highjexp

# Full w/ interactions
chi_sq_highjint <- null_highj$deviance - highj_rest_orthogonal$deviance
chi_sq_highjint

df_highjint <- null_highj$df.residual - highj_rest_orthogonal$df.residual
df_highjint

p_highjint <- pchisq(chi_sq_highjint, df_highjint, lower.tail = FALSE)
p_highjint

# Exploration logits

# HighU
null_exp_highu <- glm(highU_explored ~ 1, data = surveysub, family = binomial)

chi_sq_expu <- null_exp_highu$deviance - explored_highu_apps$deviance
chi_sq_expu

df_expu <- null_exp_highu$df.residual - explored_highu_apps$df.residual
df_expu

p_expu <- pchisq(chi_sq_expu, df_expu, lower.tail = FALSE)
p_expu

# HighJ
null_exp_highj <- glm(highJ_explored ~ 1, data = surveysub, family = binomial)

chi_sq_expj <- null_exp_highj$deviance - explored_highj_apps$deviance
chi_sq_expj

df_expj <- null_exp_highj$df.residual - explored_highj_apps$df.residual
df_expj

p_highj <- pchisq(chi_sq_expj, df_expj, lower.tail = FALSE)
p_highj



# Summary of stats ----

data <- surveysub

# Summarize continuous and categorical variables by assignment (if applicable)
summary_table <- data %>%
  group_by(regulatory_focus) %>%  # Change this to your assignment variable, if any
  summarize(
    Age_Mean = mean(age, na.rm = TRUE),
    Age_SD = sd(age, na.rm = TRUE),
    Gender_Female = sum(gender == "Female", na.rm = TRUE),
    Gender_Male = sum(gender == "Male", na.rm = TRUE),
    Income_Mean = mean(income, na.rm = TRUE),
    Income_SD = sd(income, na.rm = TRUE),
    Visit_Freq_Mean = mean(visit_frequency, na.rm = TRUE),
    Visit_Freq_SD = sd(visit_frequency, na.rm = TRUE),
    App_Expense_Mean = mean(app_expense, na.rm = TRUE),
    App_Expense_SD = sd(app_expense, na.rm = TRUE),
    Previous_Experience_Count = sum(previous_experience == "Yes", na.rm = TRUE),
    Platform_Pref_Android = sum(platform_preference == "Android", na.rm = TRUE),
    Platform_Pref_iOS = sum(platform_preference == "iOS", na.rm = TRUE),
  )

kable(summary_table, caption = "Table of Respondent Breakdown by Assignment")


# Summarize data into a tidy long-format table
summary_table2 <- data %>%
  group_by(regulatory_focus) %>%
  summarize(
    `Age Mean` = round(mean(age, na.rm = TRUE), 1),
    `Age SD` = round(sd(age, na.rm = TRUE), 1),
    `Gender Female Count` = sum(gender == "Female", na.rm = TRUE),
    `Gender Male Count` = sum(gender == "Male", na.rm = TRUE),
    `Income Mean` = round(mean(income, na.rm = TRUE), 1),
    `Income SD` = round(sd(income, na.rm = TRUE), 1),
    `Visit Frequency Mean` = round(mean(visit_frequency, na.rm = TRUE), 1),
    `Visit Frequency SD` = round(sd(visit_frequency, na.rm = TRUE), 1),
    `App Expense Mean` = round(mean(app_expense, na.rm = TRUE), 1),
    `App Expense SD` = round(sd(app_expense, na.rm = TRUE), 1),
    `Previous Experience Count` = sum(previous_experience == "Yes", na.rm = TRUE),
    `Platform Android Count` = sum(platform_preference == "Android", na.rm = TRUE),
    `Platform iOS Count` = sum(platform_preference == "iOS", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -regulatory_focus, names_to = "Statistic", values_to = "Value")  # Convert to long format

# Export to Excel
write.xlsx(summary_table2, "respondent_breakdown2.xlsx", rowNames = FALSE)



# Summarize all variables
summary_table <- surveysub %>%
  group_by(regulatory_focus) %>%
  summarize(
    # Numerical variables: Mean, SD, and Count
    `Age Mean` = round(mean(age, na.rm = TRUE), 1),
    `Age SD` = round(sd(age, na.rm = TRUE), 1),
    `Age Count` = sum(!is.na(age)),
    
    `Income Mean` = round(mean(income, na.rm = TRUE), 1),
    `Income SD` = round(sd(income, na.rm = TRUE), 1),
    `Income Count` = sum(!is.na(income)),
    
    `Visit Frequency Mean` = round(mean(visit_frequency, na.rm = TRUE), 1),
    `Visit Frequency SD` = round(sd(visit_frequency, na.rm = TRUE), 1),
    `Visit Frequency Count` = sum(!is.na(visit_frequency)),
    
    `App Expense Mean` = round(mean(app_expense, na.rm = TRUE), 1),
    `App Expense SD` = round(sd(app_expense, na.rm = TRUE), 1),
    `App Expense Count` = sum(!is.na(app_expense)),
    
    `Previous Experience Mean` = round(mean(previous_experience, na.rm = TRUE), 1),
    `Previous Experience SD` = round(sd(previous_experience, na.rm = TRUE), 1),
    `Previous Experience Count` = sum(!is.na(previous_experience)),
    
    # Categorical variables: Counts and Proportions
    `Gender Female Count` = sum(gender == "Female", na.rm = TRUE),
    `Gender Female Proportion` = round(mean(gender == "Female", na.rm = TRUE) * 100, 1),
    `Gender Male Count` = sum(gender == "Male", na.rm = TRUE),
    `Gender Male Proportion` = round(mean(gender == "Male", na.rm = TRUE) * 100, 1),
    
    `Platform GooglePlay Count` = sum(platform_preference == "GooglePlay", na.rm = TRUE),
    `Platform GooglePlay Proportion` = round(mean(platform_preference == "GooglePlay", na.rm = TRUE) * 100, 1),
    `Platform AppStore Count` = sum(platform_preference == "AppStore", na.rm = TRUE),
    `Platform AppStore Proportion` = round(mean(platform_preference == "AppStore", na.rm = TRUE) * 100, 1)
  ) %>%
  pivot_longer(
    cols = -regulatory_focus, 
    names_to = c("Variable", ".value"),  # Splits names into separate columns
    names_pattern = "(.*) (.*)"
  )

write.xlsx(summary_table, "respondent_breakdown.xlsx", rowNames = FALSE)

cat("Summary table exported to 'respondent_breakdown.xlsx'.")

# - Summary of statistics between regfocus groups -
# Summarize data
summary_table <- surveysub %>%
  group_by(regulatory_focus) %>%
  summarize(
    # Numerical variables: Mean, SD, and Count
    `Age Mean` = round(mean(age, na.rm = TRUE), 1),
    `Age SD` = round(sd(age, na.rm = TRUE), 1),
    `Age Count` = sum(!is.na(age)),
    
    `Income Mean` = round(mean(income, na.rm = TRUE), 1),
    `Income SD` = round(sd(income, na.rm = TRUE), 1),
    `Income Count` = sum(!is.na(income)),
    
    `Visit Frequency Mean` = round(mean(visit_frequency, na.rm = TRUE), 1),
    `Visit Frequency SD` = round(sd(visit_frequency, na.rm = TRUE), 1),
    `Visit Frequency Count` = sum(!is.na(visit_frequency)),
    
    `App Expense Mean` = round(mean(app_expense, na.rm = TRUE), 1),
    `App Expense SD` = round(sd(app_expense, na.rm = TRUE), 1),
    `App Expense Count` = sum(!is.na(app_expense)),
    
    `Previous Experience Mean` = round(mean(previous_experience, na.rm = TRUE), 1),
    `Previous Experience SD` = round(sd(previous_experience, na.rm = TRUE), 1),
    `Previous Experience Count` = sum(!is.na(previous_experience)),
    
    # Categorical variables: Counts and Proportions
    `Gender Female Count` = sum(gender == "Female", na.rm = TRUE),
    `Gender Female Proportion` = round(mean(gender == "Female", na.rm = TRUE) * 100, 1),
    `Gender Male Count` = sum(gender == "Male", na.rm = TRUE),
    `Gender Male Proportion` = round(mean(gender == "Male", na.rm = TRUE) * 100, 1),
    
    `Platform GooglePlay Count` = sum(platform_preference == "GooglePlay", na.rm = TRUE),
    `Platform GooglePlay Proportion` = round(mean(platform_preference == "GooglePlay", na.rm = TRUE) * 100, 1),
    `Platform AppStore Count` = sum(platform_preference == "AppStore", na.rm = TRUE),
    `Platform AppStore Proportion` = round(mean(platform_preference == "AppStore", na.rm = TRUE) * 100, 1),
    
    # Dummy variables: Counts and Proportions
    `Reg Focus Dummy Count` = sum(reg_focus_dummy == 1, na.rm = TRUE),
    `Reg Focus Dummy Proportion` = round(mean(reg_focus_dummy, na.rm = TRUE) * 100, 1),
    
    `Exp HighJ Only Count` = sum(expHighJ_only == 1, na.rm = TRUE),
    `Exp HighJ Only Proportion` = round(mean(expHighJ_only, na.rm = TRUE) * 100, 1),
    
    `Exp HighU Only Count` = sum(expHighU_only == 1, na.rm = TRUE),
    `Exp HighU Only Proportion` = round(mean(expHighU_only, na.rm = TRUE) * 100, 1),
    
    `Exp Both High Count` = sum(exp_both_high == 1, na.rm = TRUE),
    `Exp Both High Proportion` = round(mean(exp_both_high, na.rm = TRUE) * 100, 1)
  ) %>%
  pivot_longer(
    cols = -regulatory_focus, 
    names_to = c("Variable", ".value"),  # Splits names into columns
    names_pattern = "(.*) (.*)"
  )

# excel file
write.xlsx(summary_table, "respondent_breakdown_with_dummies.xlsx", rowNames = FALSE)



# - Overall summary of statistics -

# Summarize data
summary_table3 <- surveysub %>%
  summarize(
    # Numerical variables: Mean, SD, and Count
    `Age Mean` = round(mean(age, na.rm = TRUE), 1),
    `Age SD` = round(sd(age, na.rm = TRUE), 1),
    `Age Count` = sum(!is.na(age)),
    
    `Income Mean` = round(mean(income, na.rm = TRUE), 1),
    `Income SD` = round(sd(income, na.rm = TRUE), 1),
    `Income Count` = sum(!is.na(income)),
    
    `Visit Frequency Mean` = round(mean(visit_frequency, na.rm = TRUE), 1),
    `Visit Frequency SD` = round(sd(visit_frequency, na.rm = TRUE), 1),
    `Visit Frequency Count` = sum(!is.na(visit_frequency)),
    
    `App Expense Mean` = round(mean(app_expense, na.rm = TRUE), 1),
    `App Expense SD` = round(sd(app_expense, na.rm = TRUE), 1),
    `App Expense Count` = sum(!is.na(app_expense)),
    
    `Previous Experience Mean` = round(mean(previous_experience, na.rm = TRUE), 1),
    `Previous Experience SD` = round(sd(previous_experience, na.rm = TRUE), 1),
    `Previous Experience Count` = sum(!is.na(previous_experience)),
    
    # Categorical variables: Counts and Proportions
    `Gender Female Count` = sum(gender == "Female", na.rm = TRUE),
    `Gender Female Proportion` = round(mean(gender == "Female", na.rm = TRUE) * 100, 1),
    `Gender Male Count` = sum(gender == "Male", na.rm = TRUE),
    `Gender Male Proportion` = round(mean(gender == "Male", na.rm = TRUE) * 100, 1),
    
    `Platform GooglePlay Count` = sum(platform_preference == "GooglePlay", na.rm = TRUE),
    `Platform GooglePlay Proportion` = round(mean(platform_preference == "GooglePlay", na.rm = TRUE) * 100, 1),
    `Platform AppStore Count` = sum(platform_preference == "AppStore", na.rm = TRUE),
    `Platform AppStore Proportion` = round(mean(platform_preference == "AppStore", na.rm = TRUE) * 100, 1),
    
    # Dummy variables: Counts and Proportions
    `Reg Focus Dummy Count` = sum(reg_focus_dummy == 1, na.rm = TRUE),
    `Reg Focus Dummy Proportion` = round(mean(reg_focus_dummy, na.rm = TRUE) * 100, 1),
    
    `Exp HighJ Only Count` = sum(expHighJ_only == 1, na.rm = TRUE),
    `Exp HighJ Only Proportion` = round(mean(expHighJ_only, na.rm = TRUE) * 100, 1),
    
    `Exp HighU Only Count` = sum(expHighU_only == 1, na.rm = TRUE),
    `Exp HighU Only Proportion` = round(mean(expHighU_only, na.rm = TRUE) * 100, 1),
    
    `Exp Both High Count` = sum(exp_both_high == 1, na.rm = TRUE),
    `Exp Both High Proportion` = round(mean(exp_both_high, na.rm = TRUE) * 100, 1),
    
    `Explored Neither Count` = sum(expHighJ_only == 0 & expHighU_only == 0 & exp_both_high == 0, na.rm = TRUE),
    `Explored Neither Proportion` = round(mean(expHighJ_only == 0 & expHighU_only == 0 & exp_both_high == 0, na.rm = TRUE) * 100, 1)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "(.*) (.*)"
  )

summary_table3

# Excel
write.xlsx(summary_table3, "overall_summary.xlsx", rowNames = FALSE)

rf_table <- join_survey %>%
  group_by(regulatory_focus) %>%
  summarize(
    RFpromo_Mean = round(mean(PromotionMean, na.rm = TRUE), 2),
    RFprev_Mean = round(mean(PreventionMean, na.rm = TRUE), 2),
    RFdiff_Mean = round(mean(rf_differ, na.rm = TRUE), 2)
  )
rf_table


# Manipulation Check ----

# Shirley's code:
rfcheck <- read.csv("fa+anova2.csv", header=T) # missing differ

oneway.test(rf_differ ~ factor(join_survey$regulatory_focus), var.equal=TRUE)

summary(rfcheck)

# # What is rf_differ
# rf_differ = join_survey$PromotionMean - join_survey$PreventionMean
# a <- aov(rf_differ ~ factor(join_survey$regulatory_focus), data=rfcheck)
# a
# TukeyHSD(a)
# 
# table(true=rfcheck$regulatory_focus, mani=rfcheck$X.0)
# chisq.test(table(true=rfcheck$regulatory_focus, mani=rfcheck$X.0))

# Manipulation check from join_survey
join_survey$PromotionMean <- rowMeans(join_survey[, c("rf2", "rf4", "rf6")])
join_survey$PreventionMean <- rowMeans(join_survey[, c("rf1", "rf3", "rf7")])

# Calculate the Difference
join_survey$rf_differ <- join_survey$PromotionMean - join_survey$PreventionMean

# Perform One-Way ANOVA
a <- aov(rf_differ ~ factor(regulatory_focus), data = join_survey)
summary(a)

# Optional: Check for group means
aggregate(rf_differ ~ regulatory_focus, data = join_survey, mean)


# 
overall_mean <- mean(c(join_survey$PromotionMean, join_survey$PreventionMean))
join_survey$rf_differ_alt <- (join_survey$PromotionMean - join_survey$PreventionMean) - overall_mean

# Perform ANOVA with the alternative `rf_differ`
a_alt <- aov(rf_differ_alt ~ factor(regulatory_focus), data = join_survey)
summary(a_alt)


# Cronbach's alpha for Promotion items

promotion_items <- join_survey %>% select(rf2, rf4, rf6)
prevention_items <- join_survey %>% select(rf1, rf3, rf5)

promotion_alpha <- psych::alpha(promotion_items)
promotion_alpha

# Calculate Cronbach's alpha for Prevention items
prevention_alpha <- psych::alpha(prevention_items)
prevention_alpha


# New Manipulation Checks with rescaled rf items ----

# Rescale rf items to 0-6
join_survey_rescaled <- join_survey %>%
  mutate(across(c(rf1, rf2, rf3, rf4, rf5, rf6), ~ . - 1))

join_survey_rescaled$rf1

# Calculate PromotionMean and PreventionMean plus their difference (rf_differ)
join_survey_rescaled <- join_survey_rescaled %>%
  mutate(
    PromotionMean_rescaled = rowMeans(across(c(rf2, rf4, rf6)), na.rm = TRUE),
    PreventionMean_rescaled = rowMeans(across(c(rf1, rf3, rf5)), na.rm = TRUE),
    rf_differ_rescaled = PromotionMean_rescaled - PreventionMean_rescaled
  )

# Aggregate means by rf
aggregate_results_rescaled <- join_survey_rescaled %>%
  group_by(regulatory_focus) %>%
  summarize(
    PromotionMean = round(mean(PromotionMean_rescaled, na.rm = TRUE), 2),
    PreventionMean = round(mean(PreventionMean_rescaled, na.rm = TRUE), 2),
    RFdiff_Mean = round(mean(rf_differ_rescaled, na.rm = TRUE), 2)
  )

aggregate_results_rescaled

# Anova on rf_differ
anova_result_rescaled <- aov(rf_differ_rescaled ~ factor(regulatory_focus), 
                             data = join_survey_rescaled)

summary(anova_result_rescaled)


# Rescaling rf items to -3 to 3 

join_survey_rescaled_3 <- join_survey %>%
  mutate(across(c(rf1, rf2, rf3, rf4, rf5, rf6), ~ (. - 4)))

join_survey_rescaled_3$rf1

# Calculate PromotionMean, PreventionMean, and their difference (rf_differ) in the rescaled (-3 to 3) dataset
join_survey_rescaled_3 <- join_survey_rescaled_3 %>%
  mutate(
    PromotionMean_rescaled_3 = rowMeans(across(c(rf2, rf4, rf6)), na.rm = TRUE),
    PreventionMean_rescaled_3 = rowMeans(across(c(rf1, rf3, rf5)), na.rm = TRUE),
    rf_differ_rescaled_3 = PromotionMean_rescaled_3 - PreventionMean_rescaled_3
  )

# Aggregate means by regulatory focus (rescaled -3 to 3 dataset)
aggregate_results_rescaled_3 <- join_survey_rescaled_3 %>%
  group_by(regulatory_focus) %>%
  summarize(
    PromotionMean = round(mean(PromotionMean_rescaled_3, na.rm = TRUE), 2),
    PreventionMean = round(mean(PreventionMean_rescaled_3, na.rm = TRUE), 2),
    RFdiff_Mean = round(mean(rf_differ_rescaled_3, na.rm = TRUE), 2)
  )

aggregate_results_rescaled_3

# Perform ANOVA on rf_differ (rescaled -3 to 3 dataset)
anova_result_rescaled_3 <- aov(rf_differ_rescaled_3 ~ factor(regulatory_focus), data = join_survey_rescaled_3)

summary(anova_result_rescaled_3)


# Regression results using Δ_RFsurvey_questions ----

join_survey_rescaled <- join_survey_rescaled %>%
  mutate(
    delta_rf = PromotionMean - PreventionMean  # Difference score
  )

head(join_survey_rescaled$delta_rf, 50)

join_survey_rescaled_3 <- join_survey_rescaled_3 %>%
  mutate(
    delta_rf = PromotionMean - PreventionMean  # Difference score
  )

head(join_survey_rescaled_3$delta_rf, 50)


hist(join_survey_rescaled_3$delta_rf, 
     main="Distribution of Delta RegFocus", 
     xlab="Delta RegFocus", 
     breaks=20)

summary(join_survey_rescaled_3$delta_rf)


# Add delta_rf into surveysub
join_survey_rescaled_3 <- join_survey_rescaled_3 %>%
  mutate(age = as.integer(age)) 

# Filter rows in join_survey_rescaled_3 based on matching 'age' column in surveysub
join_survey_filtered <- join_survey_rescaled_3 %>%
  filter(age %in% surveysub$age) %>%
  select(age, delta_rf)

# Adding delta_rf column to surveysub
surveysub <- surveysub %>%
  mutate(delta_rf = join_survey_filtered$delta_rf)

# Check if the rows matched
age_equal <- all(join_survey_filtered$age == surveysub$age)
age_equal

# Models: 

# HighU
highu_rest_base_delta <- glm(highu_rest ~
                          delta_rf +
                          expHighJ_only +
                          expHighU_only +
                          exp_both_high +
                          age + 
                          gender + 
                          income + 
                          visit_frequency + 
                          app_expense + 
                          previous_experience + 
                          platform_preference + 
                          involvement + 
                          appname_purchased + 
                          apporder_purchased, 
                        data = surveysub, 
                        family = binomial)

summary(highu_rest_base_delta)

# HighU
highu_rest_delta <- glm(highu_rest ~
                               delta_rf +
                               expHighJ_only +
                               expHighU_only +
                               exp_both_high +
                               expHighJ_only * delta_rf +
                               expHighU_only * delta_rf +
                               exp_both_high * delta_rf +
                               age + 
                               gender + 
                               income + 
                               visit_frequency + 
                               app_expense + 
                               previous_experience + 
                               platform_preference + 
                               involvement + 
                               appname_purchased + 
                               apporder_purchased, 
                             data = surveysub, 
                             family = binomial)

summary(highu_rest_delta)


# HighJ Base
highj_rest_base_delta <- glm(highj_rest ~
                          delta_rf +
                          expHighJ_only +
                          expHighU_only +
                          exp_both_high +
                          age + 
                          gender + 
                          income + 
                          visit_frequency + 
                          app_expense + 
                          previous_experience + 
                          platform_preference + 
                          involvement + 
                          appname_purchased + 
                          apporder_purchased, 
                        data = surveysub, 
                        family = binomial)

summary(highj_rest_base_delta)


# HighJ
highj_rest_delta <- glm(highj_rest ~
                               delta_rf +
                               expHighJ_only +
                               expHighU_only +
                               exp_both_high +
                               expHighJ_only * delta_rf +
                               expHighU_only * delta_rf +
                               exp_both_high * delta_rf +
                               age + 
                               gender + 
                               income + 
                               visit_frequency + 
                               app_expense + 
                               previous_experience + 
                               platform_preference + 
                               involvement + 
                               appname_purchased + 
                               apporder_purchased, 
                             data = surveysub, 
                             family = binomial)

summary(highj_rest_delta)


