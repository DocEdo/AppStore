# Packages -----
source("data_preparation.R")
source("model2.R")
library("tidyverse")
library("tidytext")
library("vcd")
library("readx1")

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

# Install and load necessary packages
install.packages(c("wordcloud", "tm"))
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



































































