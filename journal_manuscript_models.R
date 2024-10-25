# Journal Manuscript Models

source("data_preparation.R")
library("tidyverse")
library("openxlsx")
library("pscl")
library("caret")
library("broom")

# Data Preparation: -----

# Rename Appnames
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

# Categorical variables for App Name (recode) and App Position ----

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



# New exploration variables ----

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


# New Logits with new three Explore Variables

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









