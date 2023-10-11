# Packages -----
source("data_preparation.R")
source("model2.R")
library("tidyverse")
library("tidytext")
library("vcd")
library("readx1")

# Visualizations of purchased_ratings vs explore behavior

# Mosaic plot ----
filtered_data <- surveysub %>%
  filter(purchased_ratings %in% c("HighU", "HighJ"))

mosaic(~ read + purchased_ratings, data = filtered_data, shade = TRUE)

# Stacked bar plot ----
age_data <- filtered_data %>%
  group_by(purchased_ratings, read, age) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(age_data, aes(x = read, y = percentage, fill = as.factor(age))) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) + # stack keeps different ages stacked
  facet_grid(purchased_ratings ~ .) +
  labs(y = "Percentage of Ages", x = "Read Behavior", fill = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dodged bar plot ----
ggplot(age_data, aes(x = read, y = percentage, fill = as.factor(age))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + # dodge places bars for different ages side by side
  facet_grid(. ~ purchased_ratings) +
  labs(y = "Percentage of Ages", x = "Read Behavior", fill = "Age") +
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







































































