# Journal Manuscript Models

source("data_preparation.R")
library("tidyverse")
library("openxlsx")
library("pscl")
library("caret")

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






