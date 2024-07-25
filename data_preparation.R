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

# Changing integer columns to factors in surveysub and survey
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

survey <- survey %>%
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
         involvement = factor(involvement)
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


# Aggregating review and detail behavior ----
surveysub$explore <- (surveysub$detail == "Read") | (surveysub$review == "Read")
surveysub$readboth <- (surveysub$detail == "Read") & (surveysub$review == "Read")

surveysub <- surveysub %>%
  mutate(readboth = factor(readboth),
         explore = factor(explore)
         )

surveysub$regulatory_focus <-
  relevel(factor(surveysub$regulatory_focus), ref = "Promotion")

surveysub$explore <-
  relevel(factor(surveysub$explore), ref = "TRUE")

# Re leveling reference categories ----

surveysub$purchased_ratings <-
  relevel(factor(surveysub$purchased_ratings), ref = "HighJ")

surveysub$shape <-
  relevel(factor(surveysub$shape), ref = "J")

surveysub$explore <-
  relevel(factor(surveysub$explore), ref = "FALSE")

surveysub$gender <-
  relevel(factor(surveysub$gender), ref = "Male")

surveysub$involvement <-
  relevel(factor(surveysub$involvement), ref = "0")

surveysub$platform_preference <-
  relevel(factor(surveysub$platform_preference), ref = "GooglePlay")

surveysub$regulatory_focus <-
  relevel(factor(surveysub$regulatory_focus), ref = "Prevention")

# Survey data set changes for FE ----
survey$explore <- (survey$detail == "Read") | (survey$review == "Read")

survey <- survey %>%
  mutate(explore = factor(explore)
         )

colnames(survey)[
  which(names(survey) == "combine")] <- "purchased_ratings"

survey$shape <-
  ifelse(survey$purchased_ratings == "HighJ" |
           survey$purchased_ratings == "LowJ", "J", "U")

# Releveling for survey ----
survey$purchased_ratings <-
  relevel(factor(survey$purchased_ratings), ref = "HighJ")

survey$shape <-
  relevel(factor(survey$shape), ref = "J")

survey$numRating <-
  relevel(factor(survey$numRating), ref = "Low")

survey$explore <-
  relevel(factor(survey$explore), ref = "FALSE")

survey$gender <-
  relevel(factor(survey$gender), ref = "Male")

survey$involvement <-
  relevel(factor(survey$involvement), ref = "0")

survey$platform_preference <-
  relevel(factor(survey$platform_preference), ref = "GooglePlay")

survey$regulatory_focus <-
  relevel(factor(survey$regulatory_focus), ref = "Prevention")

# Adding column of open ended questions to surveysub ----

# Change Regulatory Focus to use for comparison
join_survey$regulatory_focus <-
  ifelse(tolower(join_survey$regulatory_focus) == "prevention", "Prevention",
  ifelse(tolower(join_survey$regulatory_focus) == "promotion", "Promotion",
         join_survey$regulatory_focus)
  )

surveysub$why <- join_survey$why

# Compare regulatory focus and why columns in both datasets to confirm nothing is displaced
all(surveysub$why == join_survey$why)

all(surveysub$regulatory_focus == join_survey$regulatory_focus)

surveysub$regulatory_focus <-
  relevel(factor(surveysub$regulatory_focus), ref = "Promotion")



# Extract highU, highJ, lowU, and lowJ exploration for each subject ----
allHighU <- survey[survey$purchased_ratings == "HighU", ]
highU_explored <- (allHighU$review == "Read") | (allHighU$detail == "Read")

allHighJ <- survey[survey$purchased_ratings == "HighJ", ]
highJ_explored <- (allHighJ$review == "Read") | (allHighJ$detail == "Read")

surveysub$highU_explored <- factor(highU_explored)
surveysub$highJ_explored <- factor(highJ_explored)

allLowU <- survey[survey$purchased_ratings == "LowU", ]
lowU_explored <- (allLowU$review == "Read") | (allLowU$detail == "Read")

allLowJ <- survey[survey$purchased_ratings == "LowJ", ]
lowJ_explored <- (allLowJ$review == "Read") | (allLowJ$detail == "Read")

surveysub$lowU_explored <- factor(lowU_explored)
surveysub$lowJ_explored <- factor(lowJ_explored)


# HighU variables ----

# Recoding purchased_ratings to a binary variable (HighU vs HighJ)
survey$highju <- ifelse(survey$purchased_ratings == "HighU", 1, 
                        ifelse(survey$purchased_ratings == "HighJ", 0, NA))

# Recoding purchased_ratings to a binary variable (HighU vs HighJ)
surveysub$highju <- ifelse(surveysub$purchased_ratings == "HighU", 1, 
                           ifelse(surveysub$purchased_ratings == "HighJ", 0, NA))

# Recoding purchased_ratings to a binary of HighU vs the rest in surveysub
surveysub$highu_rest <- as.numeric(surveysub$purchased_ratings == "HighU")

# Recoding purchased_ratings to a binary of HighJ vs the rest in surveysub
surveysub$highj_rest <- as.numeric(surveysub$purchased_ratings == "HighJ")

# Recoding purchased_ratings to a binary of LowJ vs the rest in surveysub
surveysub$lowj_rest <- as.numeric(surveysub$purchased_ratings == "LowJ")

# Recoding purchased_ratings to a binary of LowU vs the rest in surveysub
surveysub$lowu_rest <- as.numeric(surveysub$purchased_ratings == "LowU")

# Recoding purchased_ratings to a binary of HighU vs the rest in survey
survey$highu_rest <- as.numeric(survey$purchased_ratings == "HighU")

# Filtering out NA values (LowJ and LowU)
survey_filtered <- survey %>% filter(!is.na(highju))

