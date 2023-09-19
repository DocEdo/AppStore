# Data Preparation

# Packages ----
library("tidyverse")
library("nnet")
library("readxl")
library("broom.mixed")
library("stats")
library("psych")

# Data ----

# File
survey <- read.csv("regression_dummy_rf.csv", header=T)

# New data without 4 entries per person
purchases <- survey[survey$purchase == 1, ]
purchases

# Select only required columns
surveysub <- subset(purchases, select = c(
  id, subject, purchase, appname, combine, 
  regulatory_focus, platform_preference, 
  visit_frequency, app_expense, previous_experience, 
  gender, income, involvement, age, visit_mean, 
  directlyPurchase, review, detail, apporder, appname, numRating)
)

# Create a new column: purchased_ratings
colnames(surveysub)[
  which(names(surveysub) == "combine")] <- "purchased_ratings"

surveysub$purchased_ratings

# Create the new column: shape
surveysub$shape <- 
  ifelse(surveysub$purchased_ratings == "HighJ" | 
           surveysub$purchased_ratings == "LowJ", "J", "U")

surveysub$shape

# Adding data from raw data files ----

# data_join file
joinxlsx <- "data_join_(432_valid).xlsx"

join_data <- map(excel_sheets(joinxlsx), ~ read_excel(joinxlsx, .x))

# data_join.xlsx important sheets
join_survey <- read_excel(joinxlsx, sheet = "subjectinfo+survey")
join_appev <- read_excel(joinxlsx, sheet = "app+event")

# 20160108_raw data.xlsx file
eight_xlsx <- "20160108_raw data.xlsx"

eight_data <- map(excel_sheets(eight_xlsx), ~ read_excel(eight_xlsx, .x))

# 20160108_raw data.xlsx important sheets
eight_subjectinfo <- read_excel(eight_xlsx, sheet = "subjectinfo")
eight_survey <- read_excel(eight_xlsx, sheet = "survey")

# 20160109_raw data.xlsx file
nine_xlsx <- "20160109_raw data.xlsx"

nine_data <- map(excel_sheets(nine_xlsx), ~ read_excel(nine_xlsx, .x))

# 20160109_raw data.xlsx important sheets
nine_subjectinfo <- read_excel(nine_xlsx, sheet = "subjectinfo")
nine_survey <- read_excel(nine_xlsx, sheet = "survey")

# Adding columns from join_survey into surveysub ----

require(tidyverse)

# Converting age column in join_survey to integer
join_survey <- join_survey %>%
  mutate(age = as.integer(age))

class(join_survey$age)

# Filter rows in join_survey based on matching age column
join_survey_filtered <- join_survey %>%
  filter(age %in% surveysub$age) %>%
  select(age, ReviewPurchase, ReviewOther, DetailPurchase, DetailOther)

# Adding the filtered columns to surveysub
surveysub <- surveysub %>%
  mutate(ReviewPurchase = join_survey_filtered$ReviewPurchase,
         ReviewOther = join_survey_filtered$ReviewOther,
         DetailPurchase = join_survey_filtered$DetailPurchase,
         DetailOther = join_survey_filtered$DetailOther)

nrow(surveysub)
surveysub

age_equal <- all(join_survey_filtered$age == surveysub$age)

if (!age_equal) {
  print("WARNING: matching the datasets did not work on AGE")
}

# Recreating directlyPurchase ----

# Filter rows in join_survey based on matching age column
detail_review_filtered <- join_survey %>%
  filter(age %in% surveysub$age) %>%
  select(age, noDetail_purchase, noReview_purchase)

# view(detail_review_filtered)

# Creating column
detail_review_filtered$directly <- 
  ifelse(detail_review_filtered$noDetail_purchase == 1 & 
           detail_review_filtered$noReview_purchase == 1, 1, 0)

# Compare both direct columns
are_equal <- all(detail_review_filtered$directly == surveysub$directlyPurchase)

if (!are_equal) {
  print("WARNING: matching the datasets did not work on directlyPurchase")
}

# Recreating visit_mean ----
surveysub <- surveysub %>%
  mutate(new_visit_mean = factor(ifelse(visit_frequency > 2, 1, 0)))

# Check if visit_mean and new_visit_mean are the same
are_equal2 <- all(surveysub$visit_mean == surveysub$new_visit_mean)

if (!are_equal2) {
  print("WARNING: matching the datasets did not work on visit_mean")
}

# Column name changes and factorize ----

# regulatory_focus
# surveysub$regulatory_focus <- ifelse(
#   surveysub$regulatory_focus == 1, "Prevention", "Promotion")

# Changing integer columns to factors
surveysub <- surveysub %>% 
  mutate(gender = 
           factor(ifelse(gender == 1, "Female", "Male")), 
         regulatory_focus = 
           factor(ifelse(regulatory_focus == 1, "Prevention", "Promotion")),
         platform_preference = 
           factor(ifelse(platform_preference == 1, "GooglePlay", "AppStore")),
         directlyPurchase =
           factor(ifelse(directlyPurchase == 1, "Yes", "No")),
         review = 
           factor(ifelse(review == 1, "Read", "NotRead")),
         detail = 
           factor(ifelse(detail == 1, "Read", "NotRead")),
         numRating = 
           factor(ifelse(numRating == 1, "High", "Low")),
         involvement = factor(involvement),
         visit_mean = factor(visit_mean),
         ReviewPurchase = factor(ReviewPurchase),
         ReviewOther = factor(ReviewOther),
         DetailPurchase = factor(DetailPurchase),
         DetailOther = factor(DetailOther),
  )

# Collapse review and detail ----

# Combined Variables
surveysub$det_rev <- paste(surveysub$review, surveysub$detail, sep = "_")

head(surveysub$det_rev, 10)

surveysub <- surveysub %>% 
  mutate(det_rev = factor(det_rev))

surveysub$det_rev <- relevel(surveysub$det_rev, ref = "Read_Read")

unique(surveysub$det_rev) # Four categories

# Majority vote

# If the count of "Read" with "Read" is greater than or equal to the count of "NotRead" with "NotRead," we consider "Read" as the majority vote for the combined variable combined otherwise we choose "NotRead" as the majority vote

majority <- function(x, y) {
  counts <- table(x, y)
  combined <- ifelse(counts["Read", "Read"] >= counts["NotRead", "NotRead"], "Read", "NotRead")
  return(combined)
}

surveysub$det_rev_maj <- mapply(majority, surveysub$review, surveysub$detail)
surveysub$det_rev_maj


# Releveling reference categories ----

surveysub$purchased_ratings <- 
  relevel(factor(surveysub$purchased_ratings), ref = "HighJ")

surveysub$shape <- 
  relevel(factor(surveysub$shape), ref = "J")

# Aggregating review and detail behavior
surveysub$read <- (surveysub$detail == "Read") | (surveysub$review == "Read")
surveysub$readboth <- (surveysub$detail == "Read") & (surveysub$review == "Read")

surveysub <- surveysub %>% 
  mutate(readboth = factor(readboth),
         read = factor(read)
         )

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






















