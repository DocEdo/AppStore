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

# Releveling reference categories ----

surveysub$purchased_ratings <- 
  relevel(factor(surveysub$purchased_ratings), ref = "HighJ")

surveysub$shape <- 
  relevel(factor(surveysub$shape), ref = "J")

# Re-creating success_induced ----

# RF check
promotion_cols <- c("rf2", "rf4", "rf6")
prevention_cols <- c("rf3", "rf5", "rf7")

# Combining selected columns into separate data frames
promotion_focus_data <- join_survey[, promotion_cols]
prevention_focus_data <- join_survey[, prevention_cols]

# FA for promotion focus
promotion_factor <- principal(promotion_focus_data, nfactors = 1)
# FA for prevention focus
prevention_factor <- principal(prevention_focus_data, nfactors = 1)

promotion_loadings <- promotion_factor$loadings
prevention_loadings <- prevention_factor$loadings

promotion_loadings
prevention_loadings

# Average variance extracted: AVE = (Sum of squared factor loadings) / (Sum of squared factor loadings + Sum of error variances)

# Calculate Composite Reliability (CR) for promotion focus
promotion_squared_loadings <- promotion_loadings^2
promotion_cr <- sum(promotion_squared_loadings) / (sum(promotion_squared_loadings) + promotion_factor$uniquenesses)

# Calculate Average Variance Extracted (AVE) for promotion focus
promotion_ave <- sum(promotion_squared_loadings) / (sum(promotion_squared_loadings) + sum(promotion_factor$uniquenesses))

# Calculate Composite Reliability (CR) for prevention focus
prevention_squared_loadings <- prevention_loadings^2
prevention_cr <- sum(prevention_squared_loadings) / (sum(prevention_squared_loadings) + prevention_factor$uniquenesses)

# Calculate Average Variance Extracted (AVE) for prevention focus
prevention_ave <- sum(prevention_squared_loadings) / (sum(prevention_squared_loadings) + sum(prevention_factor$uniquenesses))

promotion_cr
promotion_ave
prevention_cr
prevention_ave

# Mean of each CR and AVE
mean_prom_cr <- mean(promotion_cr)
mean_prom_ave <- mean(promotion_ave)
mean_preven_cr <- mean(prevention_cr)
mean_preven_ave <- mean(prevention_ave)

mean_prom_cr
mean_prom_ave
mean_preven_cr
mean_preven_ave

# Difference between average of promotion and prevention items

# Columns for promotion focus and prevention focus
promotion_cols <- c("rf2", "rf4", "rf6")
prevention_cols <- c("rf3", "rf5", "rf7")

# Separate data frames
promotion_focus_data <- join_survey[, promotion_cols]
prevention_focus_data <- join_survey[, prevention_cols]

# Calculating the average scores for promotion focus and prevention focus
promotion_average <- rowMeans(promotion_focus_data, na.rm = TRUE)
prevention_average <- rowMeans(prevention_focus_data, na.rm = TRUE)

mean_difference <- mean(regulatory_focus_difference)
mean_difference
# If mean_difference is positive, it indicates an overall tendency towards promotion focus, 
# and if it is negative, it indicates an overall tendency towards prevention focus.

# Regulatory focus difference score
regulatory_focus_difference <- promotion_average - prevention_average
regulatory_focus_difference

hist(regulatory_focus_difference, main = "Regulatory Focus Difference",
     xlab = "Difference (Promotion - Prevention)")

barplot(regulatory_focus_difference, main = "Regulatory Focus Difference",
        xlab = "Respondents", ylab = "Difference (Promotion - Prevention)")

# One-way anova
join_survey <- mutate(join_survey, 
                      regulatory_focus = factor(regulatory_focus))

anova_result <- aov(
  regulatory_focus_difference ~ regulatory_focus, data = join_survey)

summary(anova_result)

tukey_hsd <- TukeyHSD(anova_result)
tukey_hsd

# Success induced

join_survey$regulatory_focus_difference <- regulatory_focus_difference

# Create a new column 'success_induced' and categorize based on regulatory_focus_difference
join_survey$success_induced <- ifelse(join_survey$regulatory_focus_difference > 0, "yes", "no")

# Print the first few rows of the updated data frame to check the new columns
head(join_survey$success_induced)

sum(join_survey$success_induced == "yes") # new calculation using 
sum(purchases$success_induced == "yes") # original 'collapsed' data set






