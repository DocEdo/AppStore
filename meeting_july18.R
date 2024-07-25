source("data_preparation.R")

# Visualizations ----

# Filter out NAs
surveysub_clean2 <- surveysub %>%
  filter(!is.na(highju))

# Counts of each highju(1) within each group
grouped_counts <- surveysub_clean2 %>%
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

table(surveysub$highju, useNA = "ifany")

# Logits ----

# Model: Purchase_HighJ ~ RF:Explore_HighJ

filter_highj <- surveysub

# Promotion Rest_HighJ
filter_highj$regulatory_focus <- 
  relevel(factor(filter_highj$regulatory_focus), ref = "Promotion")

hirest_rfhighj_prom <- glm(highj_rest ~
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

summary(hirest_rfhighj_prom)

# Summary into a data frame
purchase_highj <- tidy(hirest_rfhighj_prom)

# Round the numeric columns to three decimal places
purchase_highj$estimate <- round(purchase_highj$estimate, 3)
purchase_highj$std.error <- round(purchase_highj$std.error, 3)
purchase_highj$statistic <- round(purchase_highj$statistic, 3)
purchase_highj$p.value <- round(purchase_highj$p.value, 3)

write.xlsx(purchase_highj, file = "purchase_highj.xlsx")

# Model: Purchase_LowU ~ RF:Explore_LowU

filter_lowu <- surveysub

# Promotion Rest_LowU
filter_lowu$regulatory_focus <- 
  relevel(factor(filter_lowu$regulatory_focus), ref = "Promotion")

hirest_rflowu_prom <- glm(lowu_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             lowU_explored + 
                             lowU_explored * regulatory_focus, 
                           data = filter_lowu, 
                           family = binomial)

summary(hirest_rflowu_prom)

# Summary into a data frame
purchase_lowu <- tidy(hirest_rflowu_prom)

# Round the numeric columns to three decimal places
purchase_lowu$estimate <- round(purchase_lowu$estimate, 3)
purchase_lowu$std.error <- round(purchase_lowu$std.error, 3)
purchase_lowu$statistic <- round(purchase_lowu$statistic, 3)
purchase_lowu$p.value <- round(purchase_lowu$p.value, 3)

write.xlsx(purchase_lowu, file = "purchase_lowu.xlsx")

# Model: Purchase_LowJ ~ RF:Explore_LowJ

filter_lowj <- surveysub

# Promotion Rest_LowJ
filter_lowj$regulatory_focus <- 
  relevel(factor(filter_lowj$regulatory_focus), ref = "Promotion")

hirest_rflowj_prom <- glm(lowj_rest ~
                            age + 
                            gender + 
                            income + 
                            visit_frequency + app_expense + 
                            previous_experience + 
                            regulatory_focus + 
                            platform_preference + 
                            involvement + 
                            lowJ_explored + 
                            lowJ_explored * regulatory_focus, 
                          data = filter_lowj, 
                          family = binomial)

summary(hirest_rflowj_prom)


# Summary into a data frame
purchase_lowj <- tidy(hirest_rflowj_prom)

# Round the numeric columns to three decimal places
purchase_lowj$estimate <- round(purchase_lowj$estimate, 3)
purchase_lowj$std.error <- round(purchase_lowj$std.error, 3)
purchase_lowj$statistic <- round(purchase_lowj$statistic, 3)
purchase_lowj$p.value <- round(purchase_lowj$p.value, 3)

write.xlsx(purchase_lowj, file = "purchase_lowj.xlsx")









