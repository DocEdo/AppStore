# Packages -----
source("data_preparation.R")
source("model2.R")
source("meeting_models.R")
library("tidyverse")
library("tidytext")
library("vcd")
library("ggvenn")
library("VennDiagram") # works but no percentages
library("ggVennDiagram")


# Visit frequency visualization ----

# Visualizations of visit_frequency
hist(surveysub$visit_frequency, breaks = 20, col = "cadetblue2",
     xlab = "Visit Frequency", ylab = "Frequency Count",
     main = "Histogram of Visit Frequency")

visit_density <- density(surveysub$visit_frequency)
plot(visit_density, col = "cadetblue2", lwd = 2, main = "Density Plot of Visit Frequency")

visit_counts <- table(surveysub$visit_frequency)
barplot(visit_counts,
        col = "cadetblue2",
        main = "Bar Plot of Visit Frequency Categories")


# Visualizations of purchased_ratings vs explore behavior

# Visualizations purchased_ratings ----
# Dodged (side by side) bar plot
dodged_plot <- ggplot(surveysub, aes(x = as.factor(explore), fill = regulatory_focus)) +
  geom_bar(position = "dodge", width = 0.6) + 
  facet_wrap(~purchased_ratings, scales = "free_y") +
  labs(title = "Read Behavior by Regulatory Focus (Dodged Bars)",
       x = "Read Behavior",
       y = "Count",
       fill = "Regulatory Focus") +
  theme_minimal()

# Stacked bar plot
stacked_plot <- ggplot(surveysub, aes(x = as.factor(explore), fill = regulatory_focus)) +
  geom_bar(position = "stack", width = 0.6) + 
  facet_wrap(~purchased_ratings, scales = "free_y") +
  labs(title = "Read Behavior by Regulatory Focus (Stacked Bars)",
       x = "Read Behavior",
       y = "Count",
       fill = "Regulatory Focus") +
  theme_minimal()

dodged_plot
stacked_plot

# Mosaic plot ----
# Age
filtered_data <- surveysub %>%
  filter(purchased_ratings %in% c("HighU", "HighJ"))

filtered_data_low <- surveysub %>%
  filter(purchased_ratings %in% c("LowU", "LowJ"))

mosaic(~ explore + purchased_ratings, data = filtered_data, shade = TRUE)

# Regulatory Focus
mosaic(~ explore + purchased_ratings + regulatory_focus, data = filtered_data, shade = TRUE)


# Stacked bar plot ----
# Age and High
age_data <- filtered_data %>%
  group_by(purchased_ratings, explore, age) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(age_data, aes(x = explore, y = percentage, fill = as.factor(age))) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) + # stack keeps different ages stacked
  facet_grid(purchased_ratings ~ .) +
  labs(y = "Percentage of Ages", x = "Read Behavior", fill = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Regulatory Focus and High
rf_data <- filtered_data %>%
  group_by(purchased_ratings, explore, regulatory_focus) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(rf_data, aes(x = explore, y = count, fill = regulatory_focus)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  facet_grid(purchased_ratings ~ .) +
  labs(y = "Count", x = "Read Behavior", fill = "Regulatory Focus") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Regulatory Focus and Low
rf_data2 <- filtered_data_low%>%
  group_by(purchased_ratings, explore, regulatory_focus) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(rf_data2, aes(x = explore, y = count, fill = regulatory_focus)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  facet_grid(purchased_ratings ~ .) +
  labs(y = "Count", x = "Read Behavior", fill = "Regulatory Focus") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dodged bar plot ----
# Age and High
ggplot(age_data, aes(x = explore, y = percentage, fill = as.factor(age))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + # dodge places bars for different ages side by side
  facet_grid(. ~ purchased_ratings) +
  labs(y = "Percentage of Ages", x = "Read Behavior", fill = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Regulatory Focus and High - using rf_data
ggplot(rf_data, aes(x = explore, y = count, fill = regulatory_focus)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_grid(. ~ purchased_ratings) +
  labs(y = "Count", x = "Read Behavior", fill = "Regulatory Focus") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Regulatory Focus and High - using rf_data
ggplot(rf_data2, aes(x = explore, y = count, fill = regulatory_focus)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_grid(. ~ purchased_ratings) +
  labs(y = "Count", x = "Read Behavior", fill = "Regulatory Focus") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Qualitative thematic analysis ----
# File = joinxlsx

join_theme <- read_excel("data_join_(432_valid).xlsx", sheet = "subjectinfo+survey")

# Install or load necessary packages
library(wordcloud)
library(tm)

# Assuming join_theme is your dataframe and 'why' is the column of interest
text <- unlist(strsplit(tolower(as.character(join_theme$why)), split = " "))

# Create a corpus and clean the data
corpus <- Corpus(VectorSource(text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
word_data <- data.frame(word = names(word_freqs), freq = word_freqs)

# Plot the word cloud
wordcloud(words = word_data$word, freq = word_data$freq, min.freq = 1,
          max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Visualization purchased_ratings
plot <- ggplot(filtered_data, aes(x = as.factor(explore), fill = regulatory_focus)) +
  geom_bar(width = 0.6) + # use position="fill" to normalize the bars
  facet_wrap(~purchased_ratings, scales = "free_y") + # separate plots for HighU and HighJ
  labs(title = "Read Behavior by Regulatory Focus",
       x = "Read Behavior",
       y = "Proportion",
       fill = "Regulatory Focus") +
  theme_minimal()

plot


# TF-IDF Analysis ----
# Term frequency-inverse document frequency, highlights the words that are frequent in a specific response but not common accross all responses. We can identify unique terms

# Convert dataframe into a tidy format using 'id' column
tidy_data <- join_theme %>%
  select(id, why) %>% # Select only the id and why columns
  unnest_tokens(word, why) 

# TF-IDF
tf_idf <- tidy_data %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n)

# Filter-out stopwords and arrange by importance
important_terms <- tf_idf %>%
  filter(!word %in% stopwords("en")) %>%
  arrange(desc(tf_idf))

# Most significant terms
top_terms <- important_terms %>%
  group_by(word) %>%
  summarize(total_tfidf = sum(tf_idf)) %>%
  top_n(10, total_tfidf) %>%
  arrange(-total_tfidf)

ggplot(top_terms, aes(y = reorder(word, total_tfidf), x = total_tfidf)) +
  geom_bar(stat = "identity") +
  labs(title = "Top terms by TF-IDF score", x = "Cumulative TF-IDF score", y = "")

# Most significant terms as bubbles
ggplot(top_terms, aes(x = reorder(word, total_tfidf), y = total_tfidf, size = total_tfidf)) +
  geom_point(aes(color = total_tfidf), alpha = 0.6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top terms by TF-IDF score", x = "", y = "Cumulative TF-IDF score", size = "TF-IDF Score", color = "TF-IDF Score")


# Dodged bar plot for long dataset 'survey'

survey$explore_num <- as.numeric(survey$explore)

dodged_plot <- ggplot(survey, aes(x = as.factor(explore_num), fill = regulatory_focus, group = regulatory_focus)) +
  geom_bar(position = "dodge", width = 0.6) + 
  facet_wrap(~purchased_ratings, scales = "free_y") +
  labs(title = "Explore Behavior by Regulatory Focus (Dodged Bars)",
       x = "Explore Behavior",
       y = "Count",
       fill = "Regulatory Focus") +
  theme_minimal()

dodged_plot

# Consistent count
dodged_plot <- ggplot(survey, aes(x = as.factor(explore_num), fill = regulatory_focus, group = regulatory_focus)) +
  geom_bar(position = "dodge", width = 0.6) + 
  facet_wrap(~purchased_ratings, scales = "free_x") +
  labs(title = "Explore Behavior by Regulatory Focus (Dodged Bars)",
       x = "Explore Behavior",
       y = "Count",
       fill = "Regulatory Focus") + 
   # + ylim(0, 300)
  theme_minimal()

dodged_plot


verify_counts <- survey %>%
  group_by(as.factor(explore_num), regulatory_focus, purchased_ratings) %>%
  summarise(Count = n()) %>%
  ungroup()

# Check if the sum of all counts is 1728
sum(verify_counts$Count)

# Assuming that 'numRating' is a numeric variable that needs to be categorized into 'High' and 'Low'
survey$rating_category <- ifelse(survey$numRating > threshold, "High", "Low") # Replace 'threshold' with the appropriate value
survey$rating_category <- factor(survey$rating_category, levels = c("High", "Low"))

# Now, create the dodged bar plot
dodged_bar_plot <- ggplot(survey, aes(x = shape, fill = regulatory_focus)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  facet_wrap(~rating_category) + # Facets for 'High' and 'Low' numRating
  labs(title = "Distribution of Shapes by Regulatory Focus and NumRating",
       x = "Shape",
       y = "Count",
       fill = "Regulatory Focus") +
  theme_minimal()

dodged_bar_plot


# Visualization 
new_dodged <- ggplot(survey, aes(x = shape)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  facet_wrap(~numRating) + # Facets for 'High' and 'Low' numRating
  labs(title = "Distribution of Shapes by NumRating",
       x = "Shape",
       y = "Count") +
  theme_minimal()

new_dodged

bar_plot <- ggplot(survey, aes(x = shape, y = purchased_ratings, fill = numRating)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Purchase Count by Shape and NumRating",
       x = "Shape",
       y = "Purchase Count",
       fill = "NumRating") +
  theme_minimal()

bar_plot

library(ggplot2)


purchase_summary <- survey %>%
  group_by(shape, numRating) %>%
  summarise(purchase_count = n())


quadrant_bar_plot <- ggplot(purchase_summary, aes(x = shape, y = purchase_count, fill = numRating)) +
  geom_col(position = position_dodge(width = 0.7)) +
  facet_grid(numRating ~ shape) + # Create a 2x2 grid layout for the quadrants
  labs(title = "Purchase Count by Shape and NumRating",
       x = "Shape",
       y = "Purchase Count",
       fill = "NumRating") +
  theme_minimal() +
  theme(strip.text.x = element_blank(), # Remove facet labels on top
        strip.text.y = element_blank()) # Remove facet labels on right

quadrant_bar_plot


# Average Purchase HighU - Interaction Plot ----
surveysub_clean <- surveysub %>%
  filter(!is.na(highju))

# Dodged Bar Plot

# Compute the average purchase of HighU for each group of regulatory focus and exploration status
avg_purchase <- surveysub_clean %>%
  group_by(regulatory_focus, highU_explored) %>%
  summarise(Avg_Purchase = mean(highju, na.rm = TRUE)) %>%
  ungroup()

ggplot(avg_purchase, aes(x = as.factor(regulatory_focus), y = Avg_Purchase, fill = as.factor(highU_explored))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Avg_Purchase, 2), 
                group = highU_explored),
            position = position_dodge(width = 0.7), 
            vjust = -0.25, # Adjust labels above bars
            size = 3.5) +
  scale_fill_manual(values = c("brown2", "lightgreen"), labels = c("E=0", "E=1")) +
  labs(y = "Avg: Purchase (HighU 0/1)", x = "Regulatory Focus (RF)", fill = "Explored HighU (E)") +
  theme_minimal()

# With error bars to represent std deviations
# To represent one standard deviation around the mean, we need to calculate the standard deviation for each group first.

# Calculate means and standard deviations for each group
grouped_stats <- surveysub_clean %>%
  group_by(regulatory_focus, highU_explored) %>%
  summarise(
    Avg_Purchase = mean(highju, na.rm = TRUE),
    SD = sd(highju, na.rm = TRUE),
    SE = SD / sqrt(n()),
      n = n()
  ) %>%
  ungroup()

# Add a column for the upper and lower bounds of the error bars (mean +/- 1.96*SE for 95% CI)
grouped_stats <- grouped_stats %>%
  mutate(
    lower = Avg_Purchase - 1.96 * SE,
    upper = Avg_Purchase + 1.96 * SE
  )

# Plot with error bars -> Overlapping problem
ggplot(grouped_stats, aes(x = as.factor(regulatory_focus), y = Avg_Purchase, fill = as.factor(highU_explored))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.7), 
                width = 0.25) +
  labs(y = "Avg: Purchase (HighU 0/1)", x = "Regulatory Focus (RF)", fill = "Explored HighU (E)") +
  theme_minimal()

# Same plot with error bars but narrower bars

avg_purch_highu_plot <- ggplot(grouped_stats, aes(x = regulatory_focus, y = Avg_Purchase, fill = highU_explored)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7), 
    width = 0.2
  ) +
  scale_fill_brewer(palette="Set2", labels = c("Not Explored" = "FALSE", "Explored" = "TRUE")) +  # ColorBrewer palette
  labs(y = "Avg: Purchase (HighU 0/1)", x = "Regulatory Focus (RF)", fill = "HighU Explored") +
  theme_minimal() +
  theme(legend.position = "bottom")

avg_purch_highu_plot

# Average Purchase HighU with flipped x axis and grouping ----

grouped_stats <- surveysub %>%
  group_by(highU_explored, regulatory_focus) %>%
  summarise(
    Avg_Purchase = mean(highu_rest, na.rm = TRUE), # Ensure to use highu_rest
    SD = sd(highu_rest, na.rm = TRUE), # Ensure to use highu_rest
    SE = SD / sqrt(n())
  ) %>%
  ungroup() %>%
  mutate(
    lower = Avg_Purchase - 1.96 * SE,
    upper = Avg_Purchase + 1.96 * SE
  )

# Plot with error bars but narrower bars
ggplot(grouped_stats, aes(x = highU_explored, y = Avg_Purchase, fill = regulatory_focus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper, group = regulatory_focus),
    position = position_dodge(width = 0.7), 
    width = 0.2
  ) +
  scale_fill_manual(values = c("brown2", "lightgreen"), labels = c("Prevention" = "Prevention", "Promotion" = "Promotion")) +
  labs(y = "Avg: Purchase (HighU 0/1)", x = "HighU Explored", fill = "Regulatory Focus") +
  theme_minimal() +
  theme(legend.position = "bottom")

# FOUR group visualization ----

# Average purchase rate for each group
group_means <- surveysub %>%
  group_by(highU_explored, regulatory_focus) %>%
  summarise(Avg_Purchase = mean(highu_rest, na.rm = TRUE)) %>%
  ungroup()

# Plot the average purchase rate for each group as points
ggplot(group_means, aes(x = as.factor(regulatory_focus), y = Avg_Purchase, group = highU_explored, color = as.factor(highU_explored))) +
  geom_line(aes(linetype = as.factor(highU_explored)), position = position_dodge(width = 0.2)) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("brown2", "lightgreen"), labels = c("Not Explored", "Explored")) +
  labs(y = "Avg: Purchase (HighU 0/1)", x = "Regulatory Focus (RF)", color = "HighU Explored") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Viz: logit RestHighU ~ . + ExploreHighU + RF:ExploreHighU ----

model_coef <- tidy(hirest_rfhighu)

# Coefficient plot
ggplot(model_coef, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  coord_flip() +
  xlab("Variables") +
  ylab("Estimates") +
  ggtitle("Coefficient Plot of the Logistic Regression Model") +
  theme_minimal()

# newdata
newdata <- with(surveysub, expand.grid(
  age = mean(age, na.rm = TRUE),
  gender = levels(gender)[1],
  income = mean(income, na.rm = TRUE),
  visit_frequency = mean(visit_frequency, na.rm = TRUE),
  app_expense = mean(app_expense, na.rm = TRUE),
  previous_experience = mean(previous_experience, na.rm = TRUE),
  regulatory_focus = levels(regulatory_focus),
  platform_preference = levels(platform_preference)[1],
  involvement = levels(involvement)[1],
  highU_explored = levels(highU_explored)
))

# Predict probabilities
newdata$predicted_prob <- predict(hirest_rfhighu, newdata = newdata, type = "response")

# Interaction plot
ggplot(newdata, aes(x = regulatory_focus, y = predicted_prob, color = highU_explored, group = highU_explored)) +
  geom_line(aes(linetype = highU_explored), size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("brown2", "lightgreen"),
                     labels = c("HighU Not Explored", "HighU Explored")) +
  labs(y = "Predicted Probability of Choosing HighU",
       x = "Regulatory Focus",
       color = "HighU Explored") +
  theme_minimal() +
  theme(legend.position = "bottom")

# How many people explored each app (using survey) ----

# Create a new character column based on 'purchased_ratings'
survey$purchased_ratings_char <- as.character(survey$purchased_ratings)

# General Exploration
general_explored <- survey %>%
  filter(review == "Read" | detail == "Read") %>%
  group_by(subject) %>%
  summarize(explored_rating_types = list(unique(purchased_ratings_char)))
general_explored

# HighU Exploration
highU_explored <- survey %>%
  filter(purchased_ratings == "HighU", (review == "Read" | detail == "Read")) %>%
  group_by(subject) %>%
  summarize(explored_highU_types = list(unique(purchased_ratings_char)))

# Merging the datasets to see them together
combined_explored <- merge(general_explored, highU_explored, by = "subject", all = TRUE)

combined_explored

# For general exploration summary
general_explored_summary <- combined_explored %>%
  mutate(num_explored = lengths(explored_rating_types)) %>%
  select(subject, num_explored, explored_rating_types)
general_explored_summary

# Visualizing count
rating_type_distribution <- general_explored %>%
  unnest(explored_rating_types) %>%
  count(explored_rating_types) %>%
  arrange(desc(n))

# Plot the distribution
ggplot(rating_type_distribution, aes(x = explored_rating_types, y = n, fill = explored_rating_types)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) + 
  theme_minimal() +
  labs(x = "Ratings Type", y = "Count", title = "Distribution of Explored App Types") +
  scale_fill_viridis_d()















# Count of groups in average purchase of HighU Visualization ----

surveysub_clean <- surveysub %>%
  filter(!is.na(highju))

group_counts <- surveysub_clean %>%
  group_by(regulatory_focus, highU_explored) %>%
  summarise(count = n(), .groups = 'drop')

group_counts

group_count_plot <- ggplot(group_counts, aes(x = regulatory_focus, y = count, fill = highU_explored)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(y = "Count of People", x = "Regulatory Focus", fill = "HighU Explored") +
  theme_minimal() +
  theme(legend.position = "bottom")

group_count_plot


# Users who explored HighU -> What else did they explored? ----

survey <- survey %>%
  mutate(exploration = (review == "Read" | detail == "Read"))


user_explorations <- survey %>%
  filter(exploration) %>% # Filter rows where exploration happened
  group_by(subject) %>%
  summarise(
    explored_HighU = any(purchased_ratings == "HighU"),
    explored_HighJ = any(purchased_ratings == "HighJ"),
    explored_LowJ = any(purchased_ratings == "LowJ"),
    explored_LowU = any(purchased_ratings == "LowU")
  ) %>%
  ungroup()

# Categorize if they explored HighU alone and other things
exploration_patterns <- user_explorations %>%
  mutate(
    exploration_category = case_when(
      explored_HighU & !explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU Alone",
      explored_HighU & explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU+HighJ",
      explored_HighU & !explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+LowJ",
      explored_HighU & !explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+LowU",
      TRUE ~ "Explored Multiple"
    )
  )

# Visualize the patterns
ggplot(exploration_patterns, aes(x = exploration_category, fill = exploration_category)) +
  geom_bar(stat = "count") +
  labs(title = "Exploration Patterns Based on App Types",
       x = "Exploration Category",
       y = "Number of Subjects") +
  theme_minimal() +
  theme(legend.position = "bottom")

# People who purchased HighU and HighJ: What else did they explore? ----
survey <- survey %>%
  mutate(
    explored_HighU = if_else(purchased_ratings == "HighU" & (review == "Read" | detail == "Read"), 1, 0),
    explored_HighJ = if_else(purchased_ratings == "HighJ" & (review == "Read" | detail == "Read"), 1, 0),
    explored_LowJ = if_else(purchased_ratings == "LowJ" & (review == "Read" | detail == "Read"), 1, 0),
    explored_LowU = if_else(purchased_ratings == "LowU" & (review == "Read" | detail == "Read"), 1, 0)
  )

highU_purchasers <- survey %>%
  filter(purchased_ratings == "HighU" & purchase == 1)

highJ_purchasers <- survey %>%
  filter(purchased_ratings == "HighJ" & purchase == 1)


# Aggregate exploration for highu and highj purchases
aggregated_explorations <- survey %>%
  group_by(subject) %>%
  summarise(
    purchase_type = first(purchased_ratings[purchase == 1]),
    explored_HighU = max(explored_HighU),
    explored_HighJ = max(explored_HighJ),
    explored_LowJ = max(explored_LowJ),
    explored_LowU = max(explored_LowU)
  ) %>%
  filter(purchase_type %in% c("HighU", "HighJ")) %>%
  ungroup()

# Categorize the patterns
aggregated_explorations <- aggregated_explorations %>%
  mutate(
    exploration_category = case_when(
      explored_HighU == 1 & explored_HighJ == 0 & explored_LowJ == 0 & explored_LowU == 0 ~ "Only HighU",
      explored_HighU == 0 & explored_HighJ == 1 & explored_LowJ == 0 & explored_LowU == 0 ~ "Only HighJ",
      explored_HighU == 1 & explored_HighJ == 1 ~ "HighU+HighJ",
      TRUE ~ "Explored Multiple"
    )
  )

ggplot(aggregated_explorations, aes(x = purchase_type, fill = exploration_category)) +
  geom_bar(position = "dodge") +
  labs(title = "Exploration Patterns for HighU vs. HighJ Purchasers",
       x = "Purchase Type",
       y = "Count of Users",
       fill = "Exploration Category") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Attempt two
survey <- survey %>%
  mutate(
    pp_HighU = (review == "Read" | detail == "Read") & purchased_ratings == "HighU",
    pp_HighJ = (review == "Read" | detail == "Read") & purchased_ratings == "HighJ",
    pp_LowJ = (review == "Read" | detail == "Read") & purchased_ratings == "LowJ",
    pp_LowU = (review == "Read" | detail == "Read") & purchased_ratings == "LowU"
  )

aggregated_explorations <- survey %>%
  group_by(subject) %>%
  summarise(
    purchase_type = first(purchased_ratings[purchase == 1]),
    explored_HighU = max(pp_HighU),
    explored_HighJ = max(pp_HighJ),
    explored_LowJ = max(pp_LowJ),
    explored_LowU = max(pp_LowU)
  ) %>%
  filter(purchase_type %in% c("HighU", "HighJ")) %>%
  ungroup()

aggregated_explorations <- aggregated_explorations %>%
  mutate(
    exploration_category = case_when(
      explored_HighU & !explored_HighJ & !explored_LowJ & !explored_LowU ~ "Only HighU",
      !explored_HighU & explored_HighJ & !explored_LowJ & !explored_LowU ~ "Only HighJ",
      explored_HighU & explored_HighJ ~ "HighU+HighJ",
      TRUE ~ "Explored Multiple"
    )
  )

ggplot(aggregated_explorations, aes(x = purchase_type, fill = exploration_category)) +
  geom_bar(position = "dodge") +
  labs(title = "Post-Purchase Exploration Patterns for HighU vs. HighJ Purchasers",
       x = "Purchase Type",
       y = "Count of Users",
       fill = "Exploration Category") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "bottom")


# ANALYSIS: Breakdown of 8 combinations - explored HighU: (highu-only; highu+highj; highu+lowu; highu+lowj; highu+.) ----

survey <- survey %>%
  mutate(
    explored_HighU = (review == "Read" | detail == "Read") & purchased_ratings == "HighU",
    explored_HighJ = (review == "Read" | detail == "Read") & purchased_ratings == "HighJ",
    explored_LowJ = (review == "Read" | detail == "Read") & purchased_ratings == "LowJ",
    explored_LowU = (review == "Read" | detail == "Read") & purchased_ratings == "LowU"
  )

# Seven combinations of HighU and rest
survey <- survey %>%
  mutate(
    exploration_category = case_when(
      explored_HighU & !explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU only",
      explored_HighU & explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU+HighJ",
      explored_HighU & !explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+LowJ",
      explored_HighU & !explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+LowU",
      explored_HighU & explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+HighJ+LowJ",
      explored_HighU & explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+HighJ+LowU",
      explored_HighU & !explored_HighJ & explored_LowJ & explored_LowU ~ "HighU+LowJ+LowU",
      # Do not create a category for users who explored all types (HighU+All)
      TRUE ~ "Other"  # Users who do not fit the above categories
    )
  )

# Aggregate 
exploration_counts <- survey %>%
  filter(exploration_category != "Other") %>%
  group_by(exploration_category) %>%
  summarise(count = n()) %>%
  ungroup()

# Plot
highu_seven_combs <- ggplot(
  exploration_counts, aes(x = exploration_category, y = count, fill = exploration_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flipped coordinates
  labs(title = "Exploration Combinations Among HighU Explorers",
       x = "Exploration Category",
       y = "Number of Users") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "none")

highu_seven_combs

# 8 Combinations of HighU
survey <- survey %>%
  mutate(
    exploration_category = case_when(
      explored_HighU & !explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU only",
      explored_HighU & explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU+HighJ",
      explored_HighU & !explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+LowJ",
      explored_HighU & !explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+LowU",
      explored_HighU & explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+HighJ+LowJ",
      explored_HighU & explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+HighJ+LowU",
      explored_HighU & !explored_HighJ & explored_LowJ & explored_LowU ~ "HighU+LowJ+LowU",
      explored_HighU & explored_HighJ & explored_LowJ & explored_LowU ~ "HighU+All",
      TRUE ~ "No category"
    )
  )

exploration_counts <- survey %>%
  group_by(exploration_category) %>%
  summarise(count = n()) %>%
  ungroup()

highu_eight_combs <- ggplot(
  exploration_counts, aes(x = exploration_category, y = count, fill = exploration_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flipped coordinates
  labs(title = "Exploration Combinations Among HighU Explorers",
       x = "Exploration Category",
       y = "Number of Users") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "bottom")

highu_eight_combs

# Cross check by adding up - change explore multiple to other combinations

# How many people did NOT look at HighU at all? ----

survey <- survey %>%
  mutate(
    highU_interaction = purchased_ratings == "HighU",
    explored_HighU = highU_interaction & (review == "Read" | detail == "Read")
  )

user_exploration <- survey %>%
  group_by(subject) %>%
  summarise(did_not_explore_HighU = !any(explored_HighU))

non_highu_explorers_count <- user_exploration %>%
  filter(did_not_explore_HighU) %>%
  summarise(count = n())

non_highu_explorers_count

# What did they buy?
did_not_explore_HighU <- survey %>%
  group_by(subject) %>%
  summarise(did_not_explore_HighU = all(!(review == "Read" | detail == "Read") | purchased_ratings != "HighU")) %>%
  ungroup()

purchases_non_highu_explorers <- survey %>%
  inner_join(did_not_explore_HighU, by = "subject") %>%
  filter(did_not_explore_HighU) %>%
  filter(purchase == 1) %>%
  distinct(subject, purchased_ratings)

# Aggregate data for non-HighU explorers
purchase_non_highu_agg <- purchases_non_highu_explorers %>%
  group_by(purchased_ratings) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(purchase_non_highu_agg, aes(x = purchased_ratings, y = count, fill = purchased_ratings)) +
  geom_bar(stat = "identity") +
  labs(
       x = "Purchased App Type",
       y = "Number of Users") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "bottom")


# How many of the people in the 8 combinations of HighU actually purchased HighU?
survey <- survey %>%
  mutate(
    explored_HighU = (review == "Read" | detail == "Read") & purchased_ratings == "HighU",
    explored_HighJ = (review == "Read" | detail == "Read") & purchased_ratings == "HighJ",
    explored_LowJ = (review == "Read" | detail == "Read") & purchased_ratings == "LowJ",
    explored_LowU = (review == "Read" | detail == "Read") & purchased_ratings == "LowU"
  )

subject_exploration <- survey %>%
  group_by(subject) %>%
  mutate(
    purchased_HighU = purchased_ratings == "HighU" & purchase == 1
  ) %>%
  summarise(
    purchased_HighU = max(purchased_HighU),
    explored_HighU = max(explored_HighU),
    explored_HighJ = max(explored_HighJ),
    explored_LowJ = max(explored_LowJ),
    explored_LowU = max(explored_LowU),
    exploration_category = case_when(
      explored_HighU & !explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU only",
      explored_HighU & explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU+HighJ",
      explored_HighU & !explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+LowJ",
      explored_HighU & !explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+LowU",
      explored_HighU & explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+HighJ+LowJ",
      explored_HighU & explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+HighJ+LowU",
      explored_HighU & !explored_HighJ & explored_LowJ & explored_LowU ~ "HighU+LowJ+LowU",
      explored_HighU & explored_HighJ & explored_LowJ & explored_LowU ~ "HighU+All"
    )
  ) %>%
  ungroup()

purchase_by_group <- subject_exploration %>%
  group_by(exploration_category) %>%
  summarise(
    Total = n(),
    Purchased_HighU = sum(purchased_HighU)
  ) %>%
  ungroup()

ggplot(purchase_by_group, aes(x = exploration_category, y = Purchased_HighU, fill = exploration_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "HighU Purchases within Exploration Categories",
       x = "Exploration Category",
       y = "Number of HighU Purchases") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "bottom")






















# Final Plots: ----

# (Works) What did non-HighU explorers buy? ----
survey <- survey %>%
  mutate(
    highU_interaction = purchased_ratings == "HighU",
    explored_HighU = highU_interaction & (review == "Read" | detail == "Read")
  )

user_exploration <- survey %>%
  group_by(subject) %>%
  summarise(did_not_explore_HighU = !any(explored_HighU))

non_highu_explorers_count <- user_exploration %>%
  filter(did_not_explore_HighU) %>%
  summarise(count = n())

non_highu_explorers_count

# What did they buy?
did_not_explore_HighU <- survey %>%
  group_by(subject) %>%
  summarise(did_not_explore_HighU = all(!(review == "Read" | detail == "Read") | purchased_ratings != "HighU")) %>%
  ungroup()

purchases_non_highu_explorers <- survey %>%
  inner_join(did_not_explore_HighU, by = "subject") %>%
  filter(did_not_explore_HighU) %>%
  filter(purchase == 1) %>%
  distinct(subject, purchased_ratings)

# Aggregate data for non-HighU explorers
purchase_non_highu_agg <- purchases_non_highu_explorers %>%
  group_by(purchased_ratings) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(purchase_non_highu_agg, aes(x = purchased_ratings, y = count, fill = purchased_ratings)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Purchased App Type",
    y = "Number of Users") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "bottom")

# (Works) Exploration patterns of HighU & HighJ Purchasers ----
survey <- survey %>%
  mutate(
    pp_HighU = (review == "Read" | detail == "Read") & purchased_ratings == "HighU",
    pp_HighJ = (review == "Read" | detail == "Read") & purchased_ratings == "HighJ",
    pp_LowJ = (review == "Read" | detail == "Read") & purchased_ratings == "LowJ",
    pp_LowU = (review == "Read" | detail == "Read") & purchased_ratings == "LowU"
  )

aggregated_explorations <- survey %>%
  group_by(subject) %>%
  summarise(
    purchase_type = first(purchased_ratings[purchase == 1]),
    explored_HighU = max(pp_HighU),
    explored_HighJ = max(pp_HighJ),
    explored_LowJ = max(pp_LowJ),
    explored_LowU = max(pp_LowU)
  ) %>%
  filter(purchase_type %in% c("HighU", "HighJ")) %>%
  ungroup()

aggregated_explorations <- aggregated_explorations %>%
  mutate(
    exploration_category = case_when(
      explored_HighU & !explored_HighJ & !explored_LowJ & !explored_LowU ~ "Only HighU",
      !explored_HighU & explored_HighJ & !explored_LowJ & !explored_LowU ~ "Only HighJ",
      explored_HighU & explored_HighJ ~ "HighU+HighJ",
      TRUE ~ "Explored Multiple"
    )
  )

ggplot(aggregated_explorations, aes(x = purchase_type, fill = exploration_category)) +
  geom_bar(position = "dodge") +
  labs(
       x = "Purchase Type",
       y = "Count of Users",
       fill = "Exploration Category") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "bottom")


# (Works) Interaction plot ----

surveysub_clean <- surveysub %>%
  filter(!is.na(highju))

# Calculate means and standard deviations for each group
grouped_stats <- surveysub_clean %>%
  group_by(regulatory_focus, highU_explored) %>%
  summarise(
    Avg_Purchase = mean(highju, na.rm = TRUE),
    SD = sd(highju, na.rm = TRUE),
    SE = SD / sqrt(n()),
    n = n()
  ) %>%
  ungroup()

# Add a column for the upper and lower bounds of the error bars (mean +/- 1.96*SE for 95% CI)
grouped_stats <- grouped_stats %>%
  mutate(
    lower = Avg_Purchase - 1.96 * SE,
    upper = Avg_Purchase + 1.96 * SE
  )

avg_purch_highu_plot <- ggplot(grouped_stats, aes(x = regulatory_focus, y = Avg_Purchase, fill = highU_explored)) +
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


# (Works) All 8 Combinations of HighU Explorations----
# Define exploration flags
survey <- survey %>%
  mutate(
    explored_HighU = (review == "Read" | detail == "Read") & purchased_ratings == "HighU",
    explored_HighJ = (review == "Read" | detail == "Read") & purchased_ratings == "HighJ",
    explored_LowJ = (review == "Read" | detail == "Read") & purchased_ratings == "LowJ",
    explored_LowU = (review == "Read" | detail == "Read") & purchased_ratings == "LowU"
  )

# Aggregate at the subject level and categorize exploration
subject_exploration <- survey %>%
  group_by(subject) %>%
  summarise(
    explored_HighU = max(explored_HighU),
    explored_HighJ = max(explored_HighJ),
    explored_LowJ = max(explored_LowJ),
    explored_LowU = max(explored_LowU)
  ) %>%
  mutate(
    exploration_category = case_when(
      explored_HighU & !explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU only",
      explored_HighU & explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU+HighJ",
      explored_HighU & !explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+LowJ",
      explored_HighU & !explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+LowU",
      explored_HighU & explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+HighJ+LowJ",
      explored_HighU & explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+HighJ+LowU",
      explored_HighU & !explored_HighJ & explored_LowJ & explored_LowU ~ "HighU+LowJ+LowU",
      explored_HighU & explored_HighJ & explored_LowJ & explored_LowU ~ "HighU+All",
      TRUE ~ "Other"
    )
  )

# Count the number of subjects in each exploration category
exploration_counts <- subject_exploration %>%
  group_by(exploration_category) %>%
  summarise(count = n()) %>%
  ungroup()

# Visualize the counts of each exploration category
highu_exploration_plot <- ggplot(exploration_counts, aes(x = exploration_category, y = count, fill = exploration_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
       x = "Exploration Category",
       y = "Number of Subjects") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "bottom")

highu_exploration_plot

# (Works) All 7 Combinations of HighU Explorations ----
subject_exploration_no_other <- survey_explore %>%
  group_by(subject) %>%
  summarise(
    explored_HighU = max(explored_HighU),
    explored_HighJ = max(explored_HighJ),
    explored_LowJ = max(explored_LowJ),
    explored_LowU = max(explored_LowU)
  ) %>%
  mutate(
    exploration_category_no_other = case_when(
      explored_HighU & !explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU only",
      explored_HighU & explored_HighJ & !explored_LowJ & !explored_LowU ~ "HighU+HighJ",
      explored_HighU & !explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+LowJ",
      explored_HighU & !explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+LowU",
      explored_HighU & explored_HighJ & explored_LowJ & !explored_LowU ~ "HighU+HighJ+LowJ",
      explored_HighU & explored_HighJ & !explored_LowJ & explored_LowU ~ "HighU+HighJ+LowU",
      explored_HighU & !explored_HighJ & explored_LowJ & explored_LowU ~ "HighU+LowJ+LowU",
      explored_HighU & explored_HighJ & explored_LowJ & explored_LowU ~ "HighU+All"
    )
  ) %>%
  filter(!is.na(exploration_category_no_other))  # Exclude NA categories

exploration_counts_no_other <- subject_exploration_no_other %>%
  group_by(exploration_category_no_other) %>%
  summarise(count = n()) %>%
  ungroup()

highu_exploration_plot_no_other <- ggplot(exploration_counts_no_other, aes(x = exploration_category_no_other, y = count, fill = exploration_category_no_other)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
       x = "Exploration Category",
       y = "Number of Subjects") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "bottom")

highu_exploration_plot_no_other


# Step 1: Create exploration flags for each app type without modifying the original dataset
exploration_data <- survey %>%
  mutate(
    explored_HighU = ifelse((review == "Read" | detail == "Read") & purchased_ratings == "HighU", 1, 0),
    explored_HighJ = ifelse((review == "Read" | detail == "Read") & purchased_ratings == "HighJ", 1, 0),
    explored_LowJ = ifelse((review == "Read" | detail == "Read") & purchased_ratings == "LowJ", 1, 0),
    explored_LowU = ifelse((review == "Read" | detail == "Read") & purchased_ratings == "LowU", 1, 0)
  )

# Step 2: Aggregate exploration data at the subject level
subject_exploration <- exploration_data %>%
  group_by(subject, regulatory_focus) %>%
  summarise(
    explored_HighU = max(explored_HighU),
    explored_HighJ = max(explored_HighJ),
    explored_LowJ = max(explored_LowJ),
    explored_LowU = max(explored_LowU)
  ) %>%
  ungroup()

# Step 3: Verify the number of unique subjects and exploration flags
unique_subjects <- subject_exploration %>%
  summarise(
    num_subjects = n_distinct(subject),
    total_explored_HighU = sum(explored_HighU),
    total_explored_HighJ = sum(explored_HighJ),
    total_explored_LowJ = sum(explored_LowJ),
    total_explored_LowU = sum(explored_LowU)
  )

# Print the number of unique subjects and exploration flags
print(unique_subjects)

# Step 4: Summarize counts by regulatory focus and app type
exploration_agg <- subject_exploration %>%
  pivot_longer(cols = starts_with("explored_"), names_to = "App_Type", values_to = "Explored") %>%
  group_by(regulatory_focus, App_Type) %>%
  summarise(Count = sum(Explored)) %>%
  ungroup()

# Print the aggregated data to verify
print(exploration_agg)

# Step 5: Plot the data
ggplot(exploration_agg, aes(x = App_Type, y = Count, fill = App_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ regulatory_focus, nrow = 1) +
  labs(
    x = "",
    y = "# of people Explored",
    fill = "App Type"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )














# Venn of Explorations ----

# Exploration Venn Diagram for DIGIT Submission

# All explorations for each subject
survey_sum <- survey %>%
  group_by(subject) %>%
  summarise(
    highU_explored = any(purchased_ratings == "HighU" & (review == "Read" | detail == "Read")),
    lowJ_explored = any(purchased_ratings == "LowJ" & (review == "Read" | detail == "Read")),
    lowU_explored = any(purchased_ratings == "LowU" & (review == "Read" | detail == "Read")),
    highJ_explored = any(purchased_ratings == "HighJ" & (review == "Read" | detail == "Read"))
  )

# Data for venn
venn_data <- list(
  LowU = survey_sum$subject[survey_sum$lowU_explored == TRUE],
  HighU = survey_sum$subject[survey_sum$highU_explored == TRUE],
  HighJ = survey_sum$subject[survey_sum$highJ_explored == TRUE],
  LowJ = survey_sum$subject[survey_sum$lowJ_explored == TRUE]
)

venn_plot <- ggvenn(
  venn_data,
  fill_color = c("#FFFFFF00", "gray40", "gray40", "#FFFFFF00"), 
  text_size = 4.2
)

venn_plot

# Check vector length
length_highU <- length(venn_data$HighU)
length_highJ <- length(venn_data$HighJ)
length_lowU <- length(venn_data$LowU)
length_lowJ <- length(venn_data$LowJ)

c(HighU = length_highU, HighJ = length_highJ, LowU = length_lowU, LowJ = length_lowJ)

#
total_unique_subjects_venn <- length(unique(c(venn_data$HighU, venn_data$HighJ, venn_data$LowU, venn_data$LowJ)))

total_unique_subjects_venn

# venn_plot<- ggvenn(venn_data, 
#                    fill_color = c("red", "blue", "green", "purple"), 
#                    text_size = 4.2)

ggsave("temporary_files/Explore_Venn.jpg", plot = venn_plot)




# VennDiagram package -----
# All explorations for each subject
survey_sum <- survey %>%
  group_by(subject) %>%
  summarise(
    lowJ_explored = any(purchased_ratings == "LowJ" & (review == "Read" | detail == "Read")),
    highU_explored = any(purchased_ratings == "HighU" & (review == "Read" | detail == "Read")),
    lowU_explored = any(purchased_ratings == "LowU" & (review == "Read" | detail == "Read")),
    highJ_explored = any(purchased_ratings == "HighJ" & (review == "Read" | detail == "Read"))
  )

# Data for venn
venn_data <- list(
  LowU = survey_sum$subject[survey_sum$lowU_explored == TRUE],
  LowJ = survey_sum$subject[survey_sum$lowJ_explored == TRUE],
  HighJ = survey_sum$subject[survey_sum$highJ_explored == TRUE],
  HighU = survey_sum$subject[survey_sum$highU_explored == TRUE]
)


venn_plot <- draw.quad.venn(
  area1 = length(venn_data$LowU),   
  area2 = length(venn_data$LowJ),   
  area3 = length(venn_data$HighU),
  area4 = length(venn_data$HighJ),  
  
  # Adjusted intersections for the swap
  n12 = length(intersect(venn_data$LowU, venn_data$LowJ)),
  n13 = length(intersect(venn_data$LowU, venn_data$HighU)),  
  n14 = length(intersect(venn_data$LowU, venn_data$HighJ)),  
  n23 = length(intersect(venn_data$LowJ, venn_data$HighU)),  
  n24 = length(intersect(venn_data$LowJ, venn_data$HighJ)),  
  n34 = length(intersect(venn_data$HighU, venn_data$HighJ)), 
  
  # Three-way intersections
  n123 = length(Reduce(intersect, list(venn_data$LowU, venn_data$LowJ, venn_data$HighU))),  
  n124 = length(Reduce(intersect, list(venn_data$LowU, venn_data$LowJ, venn_data$HighJ))),  
  n134 = length(Reduce(intersect, list(venn_data$LowU, venn_data$HighU, venn_data$HighJ))), 
  n234 = length(Reduce(intersect, list(venn_data$LowJ, venn_data$HighU, venn_data$HighJ))), 
  
  # Four-way intersection
  n1234 = length(Reduce(intersect, list(venn_data$LowU, venn_data$LowJ, venn_data$HighU, venn_data$HighJ))),
  
  # Labels and colors
  category = c("LowU", "LowJ", "HighU", "HighJ"), 
  fill = c("#FFFFFF00", "#FFFFFF00", "gray40", "gray40"),
  cat.col = "black",
  cat.cex = 1.5,
  cex = 1.7
)

grid.draw(venn_plot)



# ggVennDiagram ----


venn_data <- list(
  LowU = survey_sum$subject[survey_sum$lowU_explored == TRUE],
  HighU = survey_sum$subject[survey_sum$lowJ_explored == TRUE], 
  HighJ = survey_sum$subject[survey_sum$highU_explored == TRUE], 
  LowJ = survey_sum$subject[survey_sum$highJ_explored == TRUE] 
)


# ggVennDiagram(venn_data, label = "both", edge_size = 1.2) +  
#   scale_fill_gradient(low = "white", high = "gray40") +  
#   theme_void() + 
#   theme(legend.position = "none") + 
#   theme(
#     text = element_text(size = 24) 
#   ) +
#   scale_x_continuous(expand = expansion(mult = 0.2)) + 
#   scale_y_continuous(expand = expansion(mult = 0.2))
#   # ggsave("VennDiagram_A4.png", width = 8.27, height = 11.69, dpi = 300) 


ggVennDiagram(venn_data, label = "both", edge_size = 1.2, label_alpha = 0) +  
  scale_fill_gradient(low = "#FFFFFF00", high = "gray40") +  
  theme_void() +  
  theme(legend.position = "none", text = element_text(size = 40))








