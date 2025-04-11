# Qualitative Data Analysis



# Why Column first few tries ----

# Tokenize and count words
word_counts <- surveysub %>%
  unnest_tokens(word, why) %>%
  count(word, sort = TRUE)

word_counts

bigram_counts <- surveysub %>%
  unnest_tokens(bigram, why, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

bigram_counts


# Word cloud of all words
wordcloud(words = word_counts$word, freq = word_counts$n, min.freq = 1,
          max.words = 100, colors = brewer.pal(8, "Dark2"))

# Remove unimportant words manually
filtered_words <- word_counts %>% 
  filter(!word %in% c("the", "it", "i", "of", "and", "a", "to", "that", "for", "was", "this", "is", "on", "as", "are", "or"))

# Plot the top 15 words
ggplot(filtered_words %>% top_n(15, n), aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(title = "Most Common Words in App Choice Reasons",
       x = "Word",
       y = "Frequency") +
  theme_minimal() +
  coord_flip()

# Count responses that contain the word "ratings" or "reviews"
num_ratings <- sum(grepl("ratings", surveysub$why, ignore.case = TRUE))
num_reviews <- sum(grepl("reviews", surveysub$why, ignore.case = TRUE))

num_ratings
num_reviews

num_positive_reviews <- sum(grepl("positive reviews|best reviews|good reviews", surveysub$why, ignore.case = TRUE))
num_negative_reviews <- sum(grepl("negative reviews|bad reviews", surveysub$why, ignore.case = TRUE))

num_positive_reviews
num_negative_reviews

# New columns for each category (1 = mentioned, 0 = not mentioned)
surveysub$mentions_reviews <- as.integer(grepl("review|reviews", surveysub$why, ignore.case = TRUE))
surveysub$mentions_ratings <- as.integer(grepl("rating|ratings|star", surveysub$why, ignore.case = TRUE))
surveysub$mentions_features <- as.integer(grepl("feature|features|tracking|pedometer|exercise|fitness|function", surveysub$why, ignore.case = TRUE))
surveysub$mentions_usability <- as.integer(grepl("user friendly|interface|easy|simple", surveysub$why, ignore.case = TRUE))
surveysub$mentions_price <- as.integer(grepl("cheap|free|cost|price|discount", surveysub$why, ignore.case = TRUE))

# Create a summary table
category_counts <- colSums(surveysub[, c("mentions_reviews", "mentions_ratings", "mentions_features", "mentions_usability", "mentions_price")])
total_responses <- nrow(surveysub)

# Convert counts to proportions
category_proportions <- category_counts / total_responses


category_df <- data.frame(Category = names(category_proportions), Proportion = category_proportions)
category_df

ggplot(category_df, aes(x = reorder(Category, -Proportion), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(title = "Proportion of Reasons for Choosing an App",
       x = "Reason",
       y = "Proportion of Responses") +
  theme_minimal()


# Compute proportions separately for HighU and HighJ
category_HighU <- colSums(surveysub[surveysub$purchased_ratings == "HighU", c("mentions_reviews", "mentions_ratings", "mentions_features", "mentions_usability", "mentions_price")]) / nrow(surveysub[surveysub$purchased_ratings == "HighU", ])
category_HighJ <- colSums(surveysub[surveysub$purchased_ratings == "HighJ", c("mentions_reviews", "mentions_ratings", "mentions_features", "mentions_usability", "mentions_price")]) / nrow(surveysub[surveysub$purchased_ratings == "HighJ", ])

# Create a comparison dataframe
category_comparison <- data.frame(
  Category = names(category_HighU),
  HighU = category_HighU,
  HighJ = category_HighJ
)

category_comparison

# Long format
category_long <- pivot_longer(category_comparison, cols = c("HighU", "HighJ"), names_to = "App_Type", values_to = "Proportion")


ggplot(category_long, aes(x = reorder(Category, -Proportion), y = Proportion, fill = App_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Reasons for Choosing HighU vs. HighJ Apps",
       x = "Reason for Choosing App",
       y = "Proportion of Users") +
  scale_fill_manual(values = c("HighU" = "darkorchid", "HighJ" = "orange")) +
  theme_minimal() +
  coord_flip()


# CONFIRM if reviews and ratings are mixed up

# SAVE reviews + other things into variables

# Responses mentioning "reviews" alone
reviews_context2 <- surveysub$why[grepl("review", surveysub$why, ignore.case = TRUE)]
reviews_context2

reviews_context <- surveysub$why[grepl("reviews", surveysub$why, ignore.case = TRUE)]
reviews_context

# Responses mentioning "reviews" along with "ratings" or "stars"
reviews_with_ratings_context <- surveysub$why[grepl("reviews", surveysub$why, ignore.case = TRUE) & 
                                                grepl("rating|ratings|star|stars", surveysub$why, ignore.case = TRUE)]
reviews_with_ratings_context

# Responses mentioning "choice" alone
choice_context <- surveysub$why[grepl("choice", surveysub$why, ignore.case = TRUE)]
choice_context


# Responses mentioning "choice" along with "reviews" or "ratings"
choice_with_reviews_or_ratings_context <- surveysub$why[grepl("choice", surveysub$why, ignore.case = TRUE) & 
                                                          grepl("review|reviews|rating|ratings|star|stars", surveysub$why, ignore.case = TRUE)]
choice_with_reviews_or_ratings_context


# Export them to excel files:
write.xlsx(data.frame(Responses = reviews_context), 
           "temporary_files/reviews_context.xlsx", rowNames = FALSE)
write.xlsx(data.frame(Responses = reviews_with_ratings_context), 
           "temporary_files/reviews_with_ratings_context.xlsx", rowNames = FALSE)
write.xlsx(data.frame(Responses = choice_context), 
           "temporary_files/choice_context.xlsx", rowNames = FALSE)
write.xlsx(data.frame(Responses = choice_with_reviews_or_ratings_context), 
           "temporary_files/choice_with_reviews_or_ratings_context.xlsx", rowNames = FALSE)

# Responses with both "reviews" and "ratings" or even "star"
reviews_as_ratings <- sum(grepl("reviews", surveysub$why, ignore.case = TRUE) & 
                            grepl("rating|ratings|star|stars", surveysub$why, ignore.case = TRUE))

# Total occurrences of "reviews"
total_reviews <- sum(grepl("reviews", surveysub$why, ignore.case = TRUE))
total_reviews

reviews_as_ratings

# Proportion
reviews_confused_with_ratings <- reviews_as_ratings / total_reviews * 100
reviews_confused_with_ratings

# Count how often "choice" appears with "ratings" or "reviews"
choice_as_ratings <- sum(grepl("choice", surveysub$why, ignore.case = TRUE) & 
                           grepl("rating|ratings|review|reviews|star|stars", surveysub$why, ignore.case = TRUE))
choice_as_ratings

# Total occurrences of "choice"
total_choice <- sum(grepl("choice", surveysub$why, ignore.case = TRUE))
total_choice

# Proportion of choice as ratings
choice_confused_with_ratings <- choice_as_ratings / total_choice * 100
choice_confused_with_ratings


# Responses from users who purchased HighJ and HighU ----
why_HighJ <- surveysub$why[surveysub$purchased_ratings == "HighJ"]

why_HighU <- surveysub$why[surveysub$purchased_ratings == "HighU"]

head(why_HighJ, 10)
head(why_HighU, 10)


# Responses mentioning "reviews" for each group 
why_HighJ_reviews <- why_HighJ[grepl("reviews", why_HighJ, ignore.case = TRUE)]
why_HighU_reviews <- why_HighU[grepl("reviews", why_HighU, ignore.case = TRUE)]

# Responses mentioning "reviews" WITH "ratings" or "stars" for each group
why_HighJ_reviews_as_ratings <- why_HighJ[grepl("reviews", why_HighJ, ignore.case = TRUE) & 
                                            grepl("rating|ratings|star|stars", why_HighJ, ignore.case = TRUE)]
why_HighU_reviews_as_ratings <- why_HighU[grepl("reviews", why_HighU, ignore.case = TRUE) & 
                                            grepl("rating|ratings|star|stars", why_HighU, ignore.case = TRUE)]
# Terrible....
length(why_HighU)
length(why_HighJ)

# "ratings" in HighJ and HighU groups
why_HighJ_ratings <- why_HighJ[grepl("rating|ratings|star|stars", why_HighJ, ignore.case = TRUE)]
why_HighU_ratings <- why_HighU[grepl("rating|ratings|star|stars", why_HighU, ignore.case = TRUE)]

# "reviews" AND "ratings" 
why_HighJ_reviews_ratings <- why_HighJ[grepl("reviews", why_HighJ, ignore.case = TRUE) & 
                                         grepl("rating|ratings|star|stars", why_HighJ, ignore.case = TRUE)]
why_HighU_reviews_ratings <- why_HighU[grepl("reviews", why_HighU, ignore.case = TRUE) & 
                                         grepl("rating|ratings|star|stars", why_HighU, ignore.case = TRUE)]

cat("HighJ - 'reviews' mentions:", length(why_HighJ_reviews), "\n")
cat("HighU - 'reviews' mentions:", length(why_HighU_reviews), "\n")

cat("\nHighJ - 'ratings' mentions:", length(why_HighJ_ratings), "\n")
cat("HighU - 'ratings' mentions:", length(why_HighU_ratings), "\n")

cat("\nHighJ - 'reviews' + 'ratings' together:", length(why_HighJ_reviews_ratings), "\n")
cat("HighU - 'reviews' + 'ratings' together:", length(why_HighU_reviews_ratings), "\n")

write.xlsx(data.frame(Responses = why_HighJ), "why_HighJ.xlsx", rowNames = FALSE)
write.xlsx(data.frame(Responses = why_HighU), "why_HighU.xlsx", rowNames = FALSE)

write.xlsx(data.frame(Responses = why_HighJ_reviews), "why_HighJ_reviews.xlsx", rowNames = FALSE)
write.xlsx(data.frame(Responses = why_HighU_reviews), "why_HighU_reviews.xlsx", rowNames = FALSE)

write.xlsx(data.frame(Responses = why_HighJ_ratings), "why_HighJ_ratings.xlsx", rowNames = FALSE)
write.xlsx(data.frame(Responses = why_HighU_ratings), "why_HighU_ratings.xlsx", rowNames = FALSE)

write.xlsx(data.frame(Responses = why_HighJ_reviews_ratings), "why_HighJ_reviews_ratings.xlsx", rowNames = FALSE)
write.xlsx(data.frame(Responses = why_HighU_reviews_ratings), "why_HighU_reviews_ratings.xlsx", rowNames = FALSE)



















