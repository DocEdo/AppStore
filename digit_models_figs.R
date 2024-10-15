# DIGIT Submission Models and Figures:
source("data_preparation.R")
library("tidyverse")
library("ggvenn")

# Rename Appnames ----
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
unique(surveysub$appname_purchased)
class(surveysub$appname_purchased)

# Rename apporder and covert to factor 
surveysub$apporder_purchased <- surveysub$apporder

surveysub$apporder_purchased <- as.factor(surveysub$apporder_purchased)
unique(surveysub$apporder_purchased)
class(surveysub$apporder_purchased)

# HighU and HighJ Models:

# Logits for DIGIT Submission with Interaction Only ----

# Model: Purchase_HighU ~ RF:Explore_HighU

filter_highu <- surveysub

# Promotion Rest_HighU
filter_highu$regulatory_focus <- 
  relevel(factor(filter_highu$regulatory_focus), ref = "Promotion")

hirest_rfhighu <- glm(highu_rest ~
                        age + 
                        gender + 
                        income + 
                        visit_frequency + app_expense + 
                        previous_experience + 
                        regulatory_focus + 
                        platform_preference + 
                        involvement +
                        highU_explored + 
                        highU_explored * regulatory_focus, 
                      data = filter_highu, 
                      family = binomial)

summary(hirest_rfhighu)

# Summary into a data frame
purchase_highu <- tidy(hirest_rfhighu)

# Round the numeric columns to three decimal places
purchase_highu$estimate <- round(purchase_highu$estimate, 3)
purchase_highu$std.error <- round(purchase_highu$std.error, 3)
purchase_highu$statistic <- round(purchase_highu$statistic, 3)
purchase_highu$p.value <- round(purchase_highu$p.value, 3)

write.xlsx(purchase_highu, file = "digit/purchase_highu.xlsx")

# Model: Purchase_HighJ ~ RF:Explore_HighJ
filter_highj <- surveysub

# Prevention Rest_HighJ
filter_highj$regulatory_focus <- 
  relevel(factor(filter_highj$regulatory_focus), ref = "Prevention")

hirest_rfhighj <- glm(highj_rest ~
                        age + 
                        gender + 
                        income + 
                        visit_frequency + app_expense + 
                        previous_experience + 
                        regulatory_focus + 
                        platform_preference + 
                        involvement +
                        highJ_explored + 
                        highJ_explored * regulatory_focus, 
                      data = filter_highj, 
                      family = binomial)

summary(hirest_rfhighj)

# Summary into a data frame
purchase_highj <- tidy(hirest_rfhighj)

# Round the numeric columns to three decimal places
purchase_highj$estimate <- round(purchase_highj$estimate, 3)
purchase_highj$std.error <- round(purchase_highj$std.error, 3)
purchase_highj$statistic <- round(purchase_highj$statistic, 3)
purchase_highj$p.value <- round(purchase_highj$p.value, 3)

write.xlsx(purchase_highj, file = "digit/purchase_highj.xlsx")





# Logits for DIGIT Submission with App-name and -order ----

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

write.xlsx(purchase_highu_apps, file = "digit/highu_apps.xlsx")

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

write.xlsx(purchase_highj_apps, file = "digit/highj_apps.xlsx")

# Visualizations for DIGIT Submission ----

# HighU DIGIT Submission Dodged Count Plot
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
  theme(
    legend.position = "bottom",
    text = element_text(family = "Georgia", face = "bold")
    )

count_plot

ggsave("digit/HighU_dodged.jpg", plot = count_plot, dpi = 300)

# HighJ DIGIT Submission Dodged Count Plot

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
  theme(
    legend.position = "bottom",
    text = element_text(family = "Georgia", face = "bold")
  )

count_plot2
ggsave("digit/HighJ_dodged.jpg", plot = count_plot2, dpi = 300)

# Exploration Venn Diagram for DIGIT Submission

# All explorations for each subject
survey_sum <- survey %>%
  group_by(subject) %>%
  summarise(
    highU_explored = any(purchased_ratings == "HighU" & (review == "Read" | detail == "Read")),
    highJ_explored = any(purchased_ratings == "HighJ" & (review == "Read" | detail == "Read")),
    lowU_explored = any(purchased_ratings == "LowU" & (review == "Read" | detail == "Read")),
    lowJ_explored = any(purchased_ratings == "LowJ" & (review == "Read" | detail == "Read"))
  )

# Data for venn
venn_data <- list(
  HighU = survey_sum$subject[survey_sum$highU_explored == TRUE],
  HighJ = survey_sum$subject[survey_sum$highJ_explored == TRUE],
  LowU = survey_sum$subject[survey_sum$lowU_explored == TRUE],
  LowJ = survey_sum$subject[survey_sum$lowJ_explored == TRUE]
)

# Check vector length
length_highU <- length(venn_data$HighU)
length_highJ <- length(venn_data$HighJ)
length_lowU <- length(venn_data$LowU)
length_lowJ <- length(venn_data$LowJ)

c(HighU = length_highU, HighJ = length_highJ, LowU = length_lowU, LowJ = length_lowJ)

# Count unique subjects in the Venn diagram
total_unique_subjects_venn <- length(unique(c(venn_data$HighU, venn_data$HighJ, venn_data$LowU, venn_data$LowJ)))

total_unique_subjects_venn

venn_plot<- ggvenn(venn_data, 
                   fill_color = c("red", "blue", "green", "purple"), 
                   text_size = 4.2)

venn_plot

ggsave("digit/Explore_Venn.jpg", plot = venn_plot, dpi = 300)


# Explore behavior for DIGIT ----

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

write.xlsx(explored_highu_apps, file = "digit/explored_highu.xlsx")

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

write.xlsx(explored_highj_apps, file = "digit/explored_highj.xlsx")






# Visualization of Interaction Plot with Average purchase ----

surveysub_clean3 <- surveysub %>%
  filter(!is.na(highju))

# Calculate means and standard deviations for each group
grouped_stats3 <- surveysub_clean3 %>%
  group_by(regulatory_focus, highU_explored) %>%
  summarise(
    Avg_Purchase = mean(highju, na.rm = TRUE),
    SD = sd(highju, na.rm = TRUE),
    SE = SD / sqrt(n()),
    n = n()
  ) %>%
  ungroup()

# Add a column for the upper and lower bounds of the error bars (mean +/- 1.96*SE for 95% CI)
grouped_stats3 <- grouped_stats3 %>%
  mutate(
    lower = Avg_Purchase - 1.96 * SE,
    upper = Avg_Purchase + 1.96 * SE
  )

avg_purch_highu_plot <- ggplot(grouped_statsish, aes(x = regulatory_focus, y = Avg_Purchase, fill = highU_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7), 
    width = 0.2
  ) +
  scale_fill_brewer(palette="Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +  # ColorBrewer palette
  labs(y = "Avg: Purchase (HighU 0/1)", x = "Regulatory Focus (RF)", fill = "HighU Explored") +
  theme_minimal() +
  theme(legend.position = "bottom")

avg_purch_highu_plot


# Visualization of Interaction with Percentage ----

surveysub_clean4 <- surveysub %>%
  filter(!is.na(highju))

# Average purchase rate (proportion of people who purchased)
grouped_stats4 <- surveysub_clean4 %>%
  group_by(regulatory_focus, highU_explored) %>%
  summarise(
    Avg_Purchase = mean(highju, na.rm = TRUE),  # Calculate average purchase (mean of 0/1)
    SD = sd(highju, na.rm = TRUE),              # Standard deviation
    SE = SD / sqrt(n()),                        # Standard error
    n = n()                                     # Count of people in the group
  ) %>%
  ungroup()

# Convert the average purchase likelihood to a percentage
grouped_stats4 <- grouped_stats4 %>%
  mutate(
    Avg_Purchase_Percent = Avg_Purchase * 100,  # Convert average to percentage
    lower_percent = (Avg_Purchase - 1.96 * SE) * 100,  # Convert lower bound of CI to percentage
    upper_percent = (Avg_Purchase + 1.96 * SE) * 100   # Convert upper bound of CI to percentage
  )

percentage_plot <- ggplot(grouped_stats4, aes(x = regulatory_focus, y = Avg_Purchase_Percent, fill = highU_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = lower_percent, ymax = upper_percent),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  scale_fill_brewer(palette = "Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +
  labs(y = "% Likelihood of Purchase", x = "Regulatory Focus (RF)", fill = "HighU Explored") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Georgia", face = "bold")
  )

percentage_plot


# Prof. Ray's approach
surveysub_clean5 <- surveysub %>%
  filter(!is.na(highju))

# Calculate the percentage likelihood of purchase for each group
grouped_percentages <- surveysub_clean5 %>%
  group_by(regulatory_focus, highU_explored) %>%
  summarise(
    Purchase_Count = sum(highju == 1, na.rm = TRUE),  # Count of purchases (highju == 1)
    Total_Count = n()  # Total number of people in each group
  ) %>%
  mutate(
    Purchase_Percent = (Purchase_Count / Total_Count) * 100  # Calculate the percentage likelihood
  ) %>%
  ungroup()

percentage_plot2 <- ggplot(grouped_percentages, aes(x = regulatory_focus, y = Purchase_Percent, fill = highU_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_brewer(palette = "Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +
  labs(y = "% Likelihood of Purchase", x = "Regulatory Focus (RF)", fill = "HighU Explored") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Georgia", face = "bold")
  )

percentage_plot2


# test highu:

surveysub_clean5 <- surveysub %>%
  filter(!is.na(highju))

grouped_percentages3 <- surveysub_clean5 %>%
  group_by(regulatory_focus, highU_explored) %>%
  summarise(
    Total_Count = n(),                               # Total number of people in each group
    Purchase_Count = sum(highju == 1, na.rm = TRUE)  # Number of people who made a purchase
  ) %>%
  mutate(
    Purchase_Percent = (Purchase_Count / Total_Count) * 100  # Percentage likelihood of purchase
  ) %>%
  ungroup()

percentage_plot3 <- ggplot(grouped_percentages3, aes(x = regulatory_focus, y = Purchase_Percent, fill = highU_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_brewer(palette = "Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +
  labs(y = "% Likelihood of Purchase", x = "Regulatory Focus (RF)", fill = "HighU Explored") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Georgia", face = "bold", size = 20),   # Change overall text size
    axis.title = element_text(size = 24),      # Increase axis titles size
    axis.text = element_text(size = 20),       # Increase axis text size
    legend.text = element_text(size = 20),     # Increase legend text size
    legend.title = element_text(size = 22)     # Increase legend title size
  )

# percentage_plot3 <- ggplot(grouped_percentages3, aes(x = regulatory_focus, y = Purchase_Percent, fill = highU_explored)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
#   scale_fill_brewer(palette = "Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +
#   labs(y = "% Likelihood of Purchase", x = "Regulatory Focus (RF)", fill = "HighU Explored") +
#   theme_minimal() +
#   scale_y_continuous(breaks = seq(0, 100, by = 5)) +  # Custom breaks
#   theme(
#     legend.position = "bottom",
#     text = element_text(family = "Georgia", face = "bold")
#   )

percentage_plot3

# test highj:

surveysub_clean6 <- surveysub %>%
  filter(!is.na(highju))

grouped_percentages4 <- surveysub_clean6 %>%
  group_by(regulatory_focus, highU_explored) %>%
  summarise(
    Total_Count = n(),                               # Total number of people in each group
    Purchase_Count = sum(highju == 0, na.rm = TRUE)  # Number of people who made a purchase
  ) %>%
  mutate(
    Purchase_Percent = (Purchase_Count / Total_Count) * 100  # Percentage likelihood of purchase
  ) %>%
  ungroup()

percentage_plot4 <- ggplot(grouped_percentages4, aes(x = regulatory_focus, y = Purchase_Percent, fill = highU_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_brewer(palette = "Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +
  labs(y = "% Likelihood of Purchase", x = "Regulatory Focus (RF)", fill = "HighJ Explored") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Georgia", face = "bold", size = 20),   # Change overall text size
    axis.title = element_text(size = 24),      # Increase axis titles size
    axis.text = element_text(size = 20),       # Increase axis text size
    legend.text = element_text(size = 20),     # Increase legend text size
    legend.title = element_text(size = 22)     # Increase legend title size
  )

percentage_plot4











