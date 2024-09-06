# Meeting August 23rd

source("data_preparation.R")
library(tidyverse)
library(openxlsx)

# Categorical variables for App Name (recode) and App Position ----

# Recode appname to appname_purchased
surveysub$appname_purchased <- recode(surveysub$appname,
                                      `1` = "JogStats",
                                      `2` = "Map My Walk",
                                      `3` = "FITAPP",
                                      `4` = "Running Watch")

# Convert to factor
surveysub$appname_purchased <- as.factor(surveysub$appname_purchased)
unique(surveysub$appname_purchased)

# Rename apporder and covert to factor 
surveysub$apporder_purchased <- surveysub$apporder

surveysub$apporder_purchased <- as.factor(surveysub$apporder_purchased)
unique(surveysub$apporder_purchased)
class(surveysub$apporder_purchased)

# Logits with App name and order ----

# Model: Purchase_HighU_apps ~ RF:Explore_HighU

filter_highu_apps <- surveysub

# Promotion Rest_HighU
filter_highu_apps$regulatory_focus <- 
  relevel(factor(filter_highu_apps$regulatory_focus), ref = "Promotion")

hirest_rfhighu_apps <- glm(highu_rest ~
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

summary(hirest_rfhighu_apps)

# Summary into a data frame
purchase_highu_apps <- tidy(hirest_rfhighu_apps)

# Round the numeric columns to three decimal places
purchase_highu_apps$estimate <- round(purchase_highu_apps$estimate, 3)
purchase_highu_apps$std.error <- round(purchase_highu_apps$std.error, 3)
purchase_highu_apps$statistic <- round(purchase_highu_apps$statistic, 3)
purchase_highu_apps$p.value <- round(purchase_highu_apps$p.value, 3)

write.xlsx(purchase_highu_apps, file = "temporary_files/highu_apps.xlsx")

# Model: Purchase_HighJ ~ RF:Explore_HighJ
filter_highj_apps <- surveysub

# Prevention Rest_HighJ
filter_highj_apps$regulatory_focus <- 
  relevel(factor(filter_highj_apps$regulatory_focus), ref = "Prevention")

hirest_rfhighj_apps <- glm(highj_rest ~
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

summary(hirest_rfhighj_apps)

# Summary into a data frame
purchase_highj_apps <- tidy(hirest_rfhighj_apps)

# Round the numeric columns to three decimal places
purchase_highj_apps$estimate <- round(purchase_highj_apps$estimate, 3)
purchase_highj_apps$std.error <- round(purchase_highj_apps$std.error, 3)
purchase_highj_apps$statistic <- round(purchase_highj_apps$statistic, 3)
purchase_highj_apps$p.value <- round(purchase_highj_apps$p.value, 3)

write.xlsx(purchase_highj_apps, file = "temporary_files/highj_apps.xlsx")

# Model: Purchase_LowU ~ RF:Explore_LowU
filter_lowu_apps <- surveysub

# Promotion Rest_LowU
filter_lowu_apps$regulatory_focus <- 
  relevel(factor(filter_lowu_apps$regulatory_focus), ref = "Promotion")

hirest_rflowu_apps <- glm(lowu_rest ~
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
                            lowU_explored + 
                            lowU_explored * regulatory_focus, 
                          data = filter_lowu_apps, 
                          family = binomial)

summary(hirest_rflowu_apps)

# Summary into a data frame
purchase_lowu_apps <- tidy(hirest_rflowu_apps)

# Round the numeric columns to three decimal places
purchase_lowu_apps$estimate <- round(purchase_lowu_apps$estimate, 3)
purchase_lowu_apps$std.error <- round(purchase_lowu_apps$std.error, 3)
purchase_lowu_apps$statistic <- round(purchase_lowu_apps$statistic, 3)
purchase_lowu_apps$p.value <- round(purchase_lowu_apps$p.value, 3)

write.xlsx(purchase_lowu_apps, file = "temporary_files/lowu_apps.xlsx")

# Model: Purchase_LowJ ~ RF:Explore_LowJ
filter_lowj_apps <- surveysub

# Promotion Rest_LowJ
filter_lowj_apps$regulatory_focus <- 
  relevel(factor(filter_lowj_apps$regulatory_focus), ref = "Promotion")

hirest_rflowj_apps <- glm(lowj_rest ~
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
                            lowJ_explored + 
                            lowJ_explored * regulatory_focus, 
                          data = filter_lowj_apps, 
                          family = binomial)

summary(hirest_rflowj_apps)


# Summary into a data frame
purchase_lowj_apps <- tidy(hirest_rflowj_apps)

# Round the numeric columns to three decimal places
purchase_lowj_apps$estimate <- round(purchase_lowj_apps$estimate, 3)
purchase_lowj_apps$std.error <- round(purchase_lowj_apps$std.error, 3)
purchase_lowj_apps$statistic <- round(purchase_lowj_apps$statistic, 3)
purchase_lowj_apps$p.value <- round(purchase_lowj_apps$p.value, 3)

write.xlsx(purchase_lowj_apps, file = "temporary_files/lowj_apps.xlsx")


# Exploration models with Explored App Name ----

# Tables of all Explore behavior logits with AppName

survey$appnames <- factor(recode(survey$appname,
                                 `1` = "JogStats",
                                 `2` = "Map My Walk",
                                 `3` = "FITAPP",
                                 `4` = "Running Watch"))

# surveysub$appnames <- as.factor(survey$appnames)

surveysub$highU_appname = factor(survey[survey$purchased_ratings == "HighU", "appnames"])
surveysub$highJ_appname = factor(survey[survey$purchased_ratings == "HighJ", "appnames"])
surveysub$lowU_appname = factor(survey[survey$purchased_ratings == "LowU", "appnames"])
surveysub$lowJ_appname = factor(survey[survey$purchased_ratings == "HighJ", "appnames"])

surveysub$apporder <- factor(surveysub$apporder)

surveysub$highU_apporder = factor(survey[survey$purchased_ratings == "HighU", "apporder"])
surveysub$highJ_apporder = factor(survey[survey$purchased_ratings == "HighJ", "apporder"])
surveysub$lowU_apporder = factor(survey[survey$purchased_ratings == "LowU", "apporder"])
surveysub$lowJ_apporder = factor(survey[survey$purchased_ratings == "HighJ", "apporder"])


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

explored_highu_apps$estimate <- round(explored_highu_apps$estimate, 3)
explored_highu_apps$std.error <- round(explored_highu_apps$std.error, 3)
explored_highu_apps$statistic <- round(explored_highu_apps$statistic, 3)
explored_highu_apps$p.value <- round(explored_highu_apps$p.value, 3)

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

explored_highj_apps$estimate <- round(explored_highj_apps$estimate, 3)
explored_highj_apps$std.error <- round(explored_highj_apps$std.error, 3)
explored_highj_apps$statistic <- round(explored_highj_apps$statistic, 3)
explored_highj_apps$p.value <- round(explored_highj_apps$p.value, 3)

write.xlsx(explored_highj_apps, file = "temporary_files/explored_highj.xlsx")

explored_lowu_apps <- glm(lowU_explored ~
                            age +
                            gender +
                            income +
                            visit_frequency +
                            app_expense +
                            previous_experience +
                            regulatory_focus +
                            platform_preference +
                            involvement +
                            factor(lowU_apporder) +
                            factor(lowU_appname),
                          family = binomial,
                          data = surveysub)

summary(explored_lowu_apps)

explored_lowu_apps <- tidy(explored_lowu_apps)

explored_lowu_apps$estimate <- round(explored_lowu_apps$estimate, 3)
explored_lowu_apps$std.error <- round(explored_lowu_apps$std.error, 3)
explored_lowu_apps$statistic <- round(explored_lowu_apps$statistic, 3)
explored_lowu_apps$p.value <- round(explored_lowu_apps$p.value, 3)

write.xlsx(explored_lowu_apps, file = "temporary_files/explored_lowu.xlsx")

explored_lowj_apps <- glm(lowJ_explored ~
                            age +
                            gender +
                            income +
                            visit_frequency +
                            app_expense +
                            previous_experience +
                            regulatory_focus +
                            platform_preference +
                            involvement +
                            factor(lowJ_apporder) +
                            factor(lowJ_appname),
                          family = binomial,
                          data = surveysub)

summary(explored_lowj_apps)

explored_lowj_apps <- tidy(explored_lowj_apps)

explored_lowj_apps$estimate <- round(explored_lowj_apps$estimate, 3)
explored_lowj_apps$std.error <- round(explored_lowj_apps$std.error, 3)
explored_lowj_apps$statistic <- round(explored_lowj_apps$statistic, 3)
explored_lowj_apps$p.value <- round(explored_lowj_apps$p.value, 3)

write.xlsx(explored_lowj_apps, file = "temporary_files/explored_lowj.xlsx")


# Visualizations ----

# HighU
surveysub_clean1 <- surveysub %>%
  filter(!is.na(highju))

# Counts of each highju(1) within each group
grouped_counts <- surveysub_clean1 %>%
  group_by(regulatory_focus, highU_explored) %>%
  summarise(
    Purchase_Count = sum(highju == 1, na.rm = TRUE),
    No_Purchase_Count = sum(highju == 0, na.rm = TRUE)
  ) %>%
  ungroup()

# Combine purchase and no purchase counts into one dataset
grouped_counts_long <- grouped_counts %>%
  pivot_longer(cols = c(Purchase_Count, No_Purchase_Count), 
               names_to = "Purchase_Status", 
               values_to = "Count") %>%
  filter(Purchase_Status == "Purchase_Count")

# Plot
count_plot <- ggplot(grouped_counts_long, aes(x = regulatory_focus, y = Count, fill = highU_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_brewer(palette="Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +
  labs(y = "Count of Purchases", x = "Regulatory Focus (RF)", fill = "HighU Explored") +
  theme_minimal() +
  theme(legend.position = "bottom")

count_plot

# HighJ
surveysub_clean2 <- surveysub %>%
  filter(!is.na(highju))

# Counts of each highju(1) within each group
grouped_counts2 <- surveysub_clean2 %>%
  group_by(regulatory_focus, highJ_explored) %>%
  summarise(
    Purchase_Count = sum(highju == 0, na.rm = TRUE),
    No_Purchase_Count = sum(highju == 1, na.rm = TRUE)
  ) %>%
  ungroup()

# Combine purchase and no purchase counts into one dataset
grouped_counts_long2 <- grouped_counts2 %>%
  pivot_longer(cols = c(Purchase_Count, No_Purchase_Count), 
               names_to = "Purchase_Status", 
               values_to = "Count") %>%
  filter(Purchase_Status == "Purchase_Count")

# Plot
count_plot2 <- ggplot(grouped_counts_long2, aes(x = regulatory_focus, y = Count, fill = highJ_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_brewer(palette="Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +
  labs(y = "Count of Purchases", x = "Regulatory Focus (RF)", fill = "HighJ Explored") +
  theme_minimal() +
  theme(legend.position = "bottom")

count_plot2

table(surveysub$highju, useNA = "ifany")

# Low U
# Filter out NAs
surveysub_clean3 <- surveysub %>%
  filter(!is.na(lowju))

# Counts of each lowju(1) within each group
grouped_counts_lowu <- surveysub_clean3 %>%
  group_by(regulatory_focus, lowU_explored) %>%
  summarise(
    Purchase_Count = sum(lowju == 1, na.rm = TRUE),
    No_Purchase_Count = sum(lowju == 0, na.rm = TRUE)
  ) %>%
  ungroup()

# Combine purchase and no purchase counts into one dataset
counts_long_lowu <- grouped_counts_lowu %>%
  pivot_longer(cols = c(Purchase_Count, No_Purchase_Count), 
               names_to = "Purchase_Status", 
               values_to = "Count") %>%
  filter(Purchase_Status == "Purchase_Count")

# Plot
count_plot_lowu <- ggplot(counts_long_lowu, aes(x = regulatory_focus, y = Count, fill = lowU_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_brewer(palette="Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +
  labs(y = "Count of Purchases", x = "Regulatory Focus (RF)", fill = "LowU Explored") +
  theme_minimal() +
  theme(legend.position = "bottom")

count_plot_lowu

# LowJ
# Filter out NAs
surveysub_clean4 <- surveysub %>%
  filter(!is.na(lowju))

# Counts of each lowju(0) within each group
grouped_counts_lowj <- surveysub_clean4 %>%
  group_by(regulatory_focus, lowJ_explored) %>%
  summarise(
    Purchase_Count = sum(lowju == 0, na.rm = TRUE),
    No_Purchase_Count = sum(lowju == 1, na.rm = TRUE)
  ) %>%
  ungroup()

# Combine purchase and no purchase counts into one dataset
counts_long_lowj <- grouped_counts_lowj %>%
  pivot_longer(cols = c(Purchase_Count, No_Purchase_Count), 
               names_to = "Purchase_Status", 
               values_to = "Count") %>%
  filter(Purchase_Status == "Purchase_Count")

# Plot
count_plot_lowj <- ggplot(counts_long_lowj, aes(x = regulatory_focus, y = Count, fill = lowJ_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_brewer(palette="Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +
  labs(y = "Count of Purchases", x = "Regulatory Focus (RF)", fill = "LowJ Explored") +
  theme_minimal() +
  theme(legend.position = "bottom")

count_plot_lowj



