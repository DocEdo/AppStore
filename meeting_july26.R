source("data_preparation.R")
library(venn)
library(ggvenn)
library(tidyverse)
library(broom)
library(openxlsx)


# Visualization: 2x2 grid of High vs. Low, U vs. J. with purchase counts in the quadrants

# Column for the 2x2 categorization based on purchased_ratings
surveysub2 <- surveysub %>%
  mutate(Category = case_when(
    purchased_ratings == "HighU" ~ "HighU",
    purchased_ratings == "HighJ" ~ "HighJ",
    purchased_ratings == "LowU" ~ "LowU",
    purchased_ratings == "LowJ" ~ "LowJ",
    TRUE ~ NA_character_
  ))

surveysub2_clean <- surveysub2 %>%
  filter(!is.na(Category))

# COunt of number of purchases in each category
purchase_counts <- surveysub2_clean %>%
  group_by(Category) %>%
  summarise(Count = n()) %>%
  ungroup()

# Plot
count_plot <- ggplot(purchase_counts, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_brewer(palette="Set1") +
  labs(y = "Count of Purchases", x = "Category") +
  theme_minimal() +
  theme(legend.position = "none")

count_plot

# Tables of all Explore behavior logits

explored_highu <- glm(highU_explored ~ 
                       age + 
                       gender + 
                       income + 
                       visit_frequency + 
                       app_expense + 
                       previous_experience + 
                       regulatory_focus + 
                       platform_preference + 
                       involvement,
                     family = binomial,
                     data = surveysub)

summary(explored_highu)

explored_highu <- tidy(explored_highu)

explored_highu$estimate <- round(explored_highu$estimate, 3)
explored_highu$std.error <- round(explored_highu$std.error, 3)
explored_highu$statistic <- round(explored_highu$statistic, 3)
explored_highu$p.value <- round(explored_highu$p.value, 3)

write.xlsx(explored_highu, file = "explored_highu.xlsx")

explored_highj <- glm(highJ_explored ~ 
                       age + 
                       gender + 
                       income + 
                       visit_frequency + 
                       app_expense + 
                       previous_experience + 
                       regulatory_focus + 
                       platform_preference + 
                       involvement,
                     family = binomial,
                     data = surveysub)

summary(explored_highj)

explored_highj <- tidy(explored_highj)

explored_highj$estimate <- round(explored_highj$estimate, 3)
explored_highj$std.error <- round(explored_highj$std.error, 3)
explored_highj$statistic <- round(explored_highj$statistic, 3)
explored_highj$p.value <- round(explored_highj$p.value, 3)

write.xlsx(explored_highj, file = "explored_highj.xlsx")

explored_lowu <- glm(lowU_explored ~ 
                        age + 
                        gender + 
                        income + 
                        visit_frequency + 
                        app_expense + 
                        previous_experience + 
                        regulatory_focus + 
                        platform_preference + 
                        involvement,
                      family = binomial,
                      data = surveysub)

summary(explored_lowu)

explored_lowu <- tidy(explored_lowu)

explored_lowu$estimate <- round(explored_lowu$estimate, 3)
explored_lowu$std.error <- round(explored_lowu$std.error, 3)
explored_lowu$statistic <- round(explored_lowu$statistic, 3)
explored_lowu$p.value <- round(explored_lowu$p.value, 3)

write.xlsx(explored_lowu, file = "explored_lowu.xlsx")

explored_lowj <- glm(lowJ_explored ~ 
                       age + 
                       gender + 
                       income + 
                       visit_frequency + 
                       app_expense + 
                       previous_experience + 
                       regulatory_focus + 
                       platform_preference + 
                       involvement,
                     family = binomial,
                     data = surveysub)

summary(explored_lowj)

explored_lowj <- tidy(explored_lowj)

explored_lowj$estimate <- round(explored_lowj$estimate, 3)
explored_lowj$std.error <- round(explored_lowj$std.error, 3)
explored_lowj$statistic <- round(explored_lowj$statistic, 3)
explored_lowj$p.value <- round(explored_lowj$p.value, 3)

write.xlsx(explored_lowj, file = "explored_lowj.xlsx")

# Venn attempt

survey_sum <- survey %>%
  group_by(subject) %>%
  summarise(
    highU_explored = any(purchased_ratings == "HighU" & (review == "Read" | detail == "Read")),
    highJ_explored = any(purchased_ratings == "HighJ" & (review == "Read" | detail == "Read")),
    lowU_explored = any(purchased_ratings == "LowU" & (review == "Read" | detail == "Read")),
    lowJ_explored = any(purchased_ratings == "LowJ" & (review == "Read" | detail == "Read"))
  )

# Prepare data for Venn
venn_data <- list(
  HighU = survey_sum$subject[survey_sum$highU_explored == TRUE],
  HighJ = survey_sum$subject[survey_sum$highJ_explored == TRUE],
  LowU = survey_sum$subject[survey_sum$lowU_explored == TRUE],
  LowJ = survey_sum$subject[survey_sum$lowJ_explored == TRUE]
)

# Plot
venn(venn_data)



# Summarize exploration status for each subject
survey_sum2 <- survey %>%
  group_by(subject) %>%
  summarise(
    highU_explored = any(purchased_ratings == "HighU" & (review == "Read" | detail == "Read")),
    highJ_explored = any(purchased_ratings == "HighJ" & (review == "Read" | detail == "Read")),
    lowU_explored = any(purchased_ratings == "LowU" & (review == "Read" | detail == "Read")),
    lowJ_explored = any(purchased_ratings == "LowJ" & (review == "Read" | detail == "Read"))
  )

# List for Venn Diagram
venn_data2 <- list(
  HighU = survey_sum2$subject[survey_sum2$highU_explored == TRUE],
  HighJ = survey_sum2$subject[survey_sum2$highJ_explored == TRUE],
  LowU = survey_sum2$subject[survey_sum2$lowU_explored == TRUE],
  LowJ = survey_sum2$subject[survey_sum2$lowJ_explored == TRUE]
)

# Plot
ggvenn(venn_data2, fill_color = c("red", "blue", "green", "purple"))


summary_counts <- survey_sum2 %>%
  summarise(
    total_highU = sum(highU_explored),
    total_highJ = sum(highJ_explored),
    total_lowU = sum(lowU_explored),
    total_lowJ = sum(lowJ_explored)
  )

summary_counts



