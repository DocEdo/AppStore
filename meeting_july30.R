# meeting_july30

source("data_preparation.R")


# All explorations for each subject
survey_sum <- survey %>%
  group_by(subject) %>%
  summarise(
    highU_explored = any(purchased_ratings == "HighU" & (review == "Read" | detail == "Read")),
    highJ_explored = any(purchased_ratings == "HighJ" & (review == "Read" | detail == "Read")),
    lowU_explored = any(purchased_ratings == "LowU" & (review == "Read" | detail == "Read")),
    lowJ_explored = any(purchased_ratings == "LowJ" & (review == "Read" | detail == "Read"))
  )

# Summarize exploration data
summary_counts <- survey_sum %>%
  summarise(
    total_highU = sum(highU_explored),
    total_highJ = sum(highJ_explored),
    total_lowU = sum(lowU_explored),
    total_lowJ = sum(lowJ_explored),
    total_subjects = n_distinct(subject)
  )
summary_counts

# Unique subjects #432
all_subjects <- unique(survey$subject)

venn_data_unique <- list(
  HighU = unique(survey_sum$subject[survey_sum$highU_explored == TRUE]),
  HighJ = unique(survey_sum$subject[survey_sum$highJ_explored == TRUE]),
  LowU = unique(survey_sum$subject[survey_sum$lowU_explored == TRUE]),
  LowJ = unique(survey_sum$subject[survey_sum$lowJ_explored == TRUE])
)

# Subjects in the Venn diagram
subjects_in_venn <- unique(c(
  venn_data_unique$HighU,
  venn_data_unique$HighJ,
  venn_data_unique$LowU,
  venn_data_unique$LowJ
))

# Identify subjects not included in the Venn diagram 
missing_subjects <- setdiff(all_subjects, subjects_in_venn)
# (setdiff returns the data in A that is not in B, with no repetitions)

missing_subjects
length(missing_subjects)

# Check exploration status of missing subjects
missing_subjects_exploration <- survey %>%
  filter(subject %in% missing_subjects) %>%
  group_by(subject) %>%
  summarise(
    highU_explored = any(purchased_ratings == "HighU" & (review == "Read" | detail == "Read")),
    highJ_explored = any(purchased_ratings == "HighJ" & (review == "Read" | detail == "Read")),
    lowU_explored = any(purchased_ratings == "LowU" & (review == "Read" | detail == "Read")),
    lowJ_explored = any(purchased_ratings == "LowJ" & (review == "Read" | detail == "Read"))
  )

missing_subjects_exploration

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

print(c(HighU = length_highU, HighJ = length_highJ, LowU = length_lowU, LowJ = length_lowJ))

# Count unique subjects in the Venn diagram
total_unique_subjects_venn <- length(unique(c(venn_data$HighU, venn_data$HighJ, venn_data$LowU, venn_data$LowJ)))

total_unique_subjects_venn

# Plot
ggvenn(venn_data, fill_color = c("red", "blue", "green", "purple"))

# With no exploration

# No Exploration category
survey_sum2 <- survey_sum %>%
  mutate(no_exploration = !highU_explored & !highJ_explored & !lowU_explored & !lowJ_explored)

venn_data_all <- list(
  HighU = survey_sum2$subject[survey_sum2$highU_explored == TRUE],
  HighJ = survey_sum2$subject[survey_sum2$highJ_explored == TRUE],
  LowU = survey_sum2$subject[survey_sum2$lowU_explored == TRUE],
  LowJ = survey_sum2$subject[survey_sum2$lowJ_explored == TRUE],
  NoExploration = survey_sum2$subject[survey_sum2$no_exploration == TRUE]
)

# Plot the Venn diagram
ggvenn(venn_data_all, fill_color = c("red", "blue", "green", "purple", "yellow"))


# ----

# Create a new column for the quadrants based on `purchased_ratings`
surveysub2 <- surveysub %>%
  mutate(Quadrant = case_when(
    purchased_ratings == "HighU" ~ "HighU",
    purchased_ratings == "HighJ" ~ "HighJ",
    purchased_ratings == "LowU" ~ "LowU",
    purchased_ratings == "LowJ" ~ "LowJ",
    TRUE ~ NA_character_
  ))

# Summarize the data to count subjects in each quadrant
purchase_counts <- surveysub2 %>%
  group_by(Quadrant) %>%
  summarise(Count = n()) %>%
  ungroup()

purchase_counts

# Create a simple table
purchase_counts_table <- purchase_counts %>%
  pivot_wider(names_from = Quadrant, values_from = Count, values_fill = 0)

# Display the table
purchase_counts_table

# ---- HighJ Barplot

# Filter out NAs
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


# Check AIC of HighU and HighJ logits, with and without interaction ----

# Rest_HighU

filter_highu1 <- surveysub

filter_highu1$regulatory_focus <- 
  relevel(factor(filter_highu1$regulatory_focus), ref = "Promotion")

hirest_rfhighu_prom1 <- glm(highu_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             highU_explored, 
                           data = filter_highu1, 
                           family = binomial)

summary(hirest_rfhighu_prom1)

# With interaction

filter_highu2 <- surveysub

filter_highu2$regulatory_focus <- 
  relevel(factor(filter_highu2$regulatory_focus), ref = "Promotion")

hirest_rfhighu_prom2 <- glm(highu_rest ~
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
                            data = filter_highu2, 
                            family = binomial)

summary(hirest_rfhighu_prom2)

# Rest_HighJ

filter_highj1 <- surveysub

filter_highj1$regulatory_focus <- 
  relevel(factor(filter_highj1$regulatory_focus), ref = "Promotion")

hirest_rfhighj_prom1 <- glm(highj_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             highJ_explored, 
                           data = filter_highj1, 
                           family = binomial)

summary(hirest_rfhighj_prom1)

# With interaction

filter_highj2 <- surveysub

filter_highj2$regulatory_focus <- 
  relevel(factor(filter_highj2$regulatory_focus), ref = "Promotion")

hirest_rfhighj_prom2 <- glm(highj_rest ~
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
                           data = filter_highj2, 
                           family = binomial)

summary(hirest_rfhighj_prom2)

# Meeting Aug 2 ----

# Delta BIC and AIC

# BIC for HighU and HighJ Models
bic_highu1 <- BIC(hirest_rfhighu_prom1)
bic_highu2 <- BIC(hirest_rfhighu_prom2)
bic_highj1 <- BIC(hirest_rfhighj_prom1)
bic_highj2 <- BIC(hirest_rfhighj_prom2)

# Delta BIC
delta_bicu <- bic_highu1 - bic_highu2
delta_bicu

delta_bicj <- bic_highj1 - bic_highj2
delta_bicj

# AIC for HighU and HighJ Models

aic_highu1 <- AIC(hirest_rfhighu_prom1)
aic_highu2 <- AIC(hirest_rfhighu_prom2)
aic_highj1 <- AIC(hirest_rfhighj_prom1)
aic_highj2 <- AIC(hirest_rfhighj_prom2)

delta_aicu <- aic_highu1 - aic_highu2
delta_aicj <- aic_highj1 - aic_highj2

delta_aicu
delta_aicj


# Create separate logit results for High J, with RF based on Prevention

# With interaction

filter_highj3 <- surveysub

filter_highj3$regulatory_focus <- 
  relevel(factor(filter_highj3$regulatory_focus), ref = "Prevention")

hirest_rfhighj_prev <- glm(highj_rest ~
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
                            data = filter_highj3, 
                            family = binomial)

summary(hirest_rfhighj_prev)

hirest_rfhighj_prev <- tidy(hirest_rfhighj_prev)

hirest_rfhighj_prev$estimate <- round(hirest_rfhighj_prev$estimate, 3)
hirest_rfhighj_prev$std.error <- round(hirest_rfhighj_prev$std.error, 3)
hirest_rfhighj_prev$statistic <- round(hirest_rfhighj_prev$statistic, 3)
hirest_rfhighj_prev$p.value <- round(hirest_rfhighj_prev$p.value, 3)

write.xlsx(hirest_rfhighj_prev, file = "hirest_highjprev.xlsx")


# HighJ Dodged Bar plot try 2 ----

# Recode to HighJ in Highju
surveysub$highju_highj <- ifelse(surveysub$purchased_ratings == "HighJ", 1, 
                                 ifelse(surveysub$purchased_ratings == "HighU", 0, NA))

# Filter NAs
surveysub_clean_highj <- surveysub %>%
  filter(!is.na(highju_highj))

# Count each highju_highj(1) within each group
grouped_counts_highj <- surveysub_clean_highj %>%
  group_by(regulatory_focus, highJ_explored) %>%
  summarise(
    Purchase_Count = sum(highju_highj == 1, na.rm = TRUE),
    No_Purchase_Count = sum(highju_highj == 0, na.rm = TRUE)
  ) %>%
  ungroup()

# Combined purchase and no purchase counts into one dataset
grouped_counts_long_highj <- grouped_counts_highj %>%
  pivot_longer(cols = c(Purchase_Count, No_Purchase_Count), 
               names_to = "Purchase_Status", 
               values_to = "Count") %>%
  filter(Purchase_Status == "Purchase_Count")


count_plot_highj <- ggplot(grouped_counts_long_highj, aes(x = regulatory_focus, y = Count, fill = highJ_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_brewer(palette="Set1", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +
  labs(y = "Count of Purchases", x = "Regulatory Focus (RF)", fill = "HighJ Explored") +
  theme_minimal() +
  theme(legend.position = "bottom")

count_plot_highj








