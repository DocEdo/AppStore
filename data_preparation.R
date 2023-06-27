# Replicating Regressions
library("mlogit")
library("tidyverse")
library("nnet")
library("readxl")
library("broom.mixed")

# Data ----
survey <- read.csv("regression_dummy_rf.csv", header=T)

# New data without 4 entries per person
purchases <- survey[survey$purchase == 1, ]

purchases

# Only required columns
surveysub <- subset(purchases, select = c(
  id, subject, purchase, appname, combine, 
  regulatory_focus, platform_preference, 
  visit_frequency, app_expense, previous_experience, 
  gender, income, involvement, age, visit_mean, 
  directlyPurchase, review, detail, apporder, appname, numRating)
)

# purchased_ratings
colnames(surveysub)[
  which(names(surveysub) == "combine")] <- "purchased_ratings"

surveysub$purchased_ratings

# Create the new column shape
surveysub$shape <- 
  ifelse(surveysub$purchased_ratings == "HighJ" | 
           surveysub$purchased_ratings == "LowJ", "J", "U")

surveysub$shape

# Raw data files ----

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
view(surveysub)

age_equal <- all(join_survey_filtered$age == surveysub$age)

if (age_equal) {
  print("The columns are the same.")
} else {
  print("The columns are different.")
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

if (are_equal) {
  print("The columns are the same.")
} else {
  print("The columns are different.")
}

# Recreating visit_mean ----
surveysub <- surveysub %>%
  mutate(new_visit_mean = factor(ifelse(visit_frequency > 2, 1, 0)))

# Check if visit_mean and new_visit_mean are the same
are_equal2 <- all(surveysub$visit_mean == surveysub$new_visit_mean)

if (are_equal2) {
  print("The columns are the same.")
} else {
  print("The columns are different.")
}

# Multinom model 1 ----

surveysub$purchased_ratings <- 
  relevel(factor(surveysub$purchased_ratings), ref = "HighJ")

newmodel <- multinom(
  purchased_ratings ~ 
    factor(regulatory_focus) + 
    gender + 
    age + 
    income + 
    platform_preference + 
    visit_frequency + 
    app_expense + 
    previous_experience + 
    involvement, 
  data = surveysub)

summary(newmodel)

summary(newmodel)$coefficients/summary(newmodel)$standard.errors

exp(coef(newmodel))

# Model Exploration ----
# IVs: gender, income, platform_preference, visit_frequency, visit_mean, app_expense, previous_experience, involvement, directlyPurchase, review, detail, factor(apporder), factor(appname), numRating

require("nnet")

# Re-level reference category
surveysub$purchased_ratings <- 
  relevel(factor(surveysub$purchased_ratings), ref = "HighJ")

surveysub$shape <- 
  relevel(factor(surveysub$shape), ref = "J")

# DV: purchased_ratings
model_2 <- multinom(
  purchased_ratings ~ 
    age + 
    gender +
    income + 
    visit_frequency + 
    app_expense + 
    previous_experience +
    regulatory_focus + 
    platform_preference + 
    visit_mean +
    directlyPurchase +
    involvement +
    review +
    detail +
    DetailPurchase +
    ReviewPurchase +
    regulatory_focus * previous_experience +
    regulatory_focus * visit_frequency +
    regulatory_focus * visit_mean +
    regulatory_focus * directlyPurchase +
    regulatory_focus * app_expense + 
    regulatory_focus * DetailPurchase +
    regulatory_focus * ReviewPurchase,
  data = surveysub)

summary(model_2)

z <- 
  summary(model_2)$coefficients/summary(model_2)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
round(p, 4)

round(exp(coef(model_2)),4)

# DV: shape

model_3 <- glm(
  shape ~ 
    numRating +
    age + 
    gender +
    income + 
    visit_frequency + 
    app_expense + 
    previous_experience +
    regulatory_focus + 
    platform_preference + 
    visit_mean +
    directlyPurchase +
    involvement +
    review +
    detail +
    regulatory_focus * previous_experience +
    regulatory_focus * visit_frequency +
    regulatory_focus * visit_mean +
    regulatory_focus * directlyPurchase +
    regulatory_focus * app_expense,
  family = "binomial",
  data = surveysub)

summary(model_3)

#summary(model_3)$coefficients/summary(model_3)$standard.errors

round(exp(coef(model_3)), 4)

# DV: numRating
model_4 <- glm(
  numRating ~ 
    shape +
    age + 
    gender +
    income + 
    visit_frequency + 
    app_expense + 
    previous_experience +
    regulatory_focus + 
    platform_preference + 
    visit_mean +
    directlyPurchase +
    involvement +
    review +
    detail +
    regulatory_focus * previous_experience +
    regulatory_focus * visit_frequency +
    regulatory_focus * visit_mean +
    regulatory_focus * directlyPurchase +
    regulatory_focus * app_expense,
  family = "binomial",
  data = surveysub)

summary(model_4)

#summary(model_4)$coefficients/summary(model_4)$standard.errors

round(exp(coef(model_4)), 4)

# DV: review

model_5 <- glm(
  review ~ 
    numRating +
    shape +
    age + 
    gender +
    income + 
    visit_frequency + 
    app_expense + 
    previous_experience +
    regulatory_focus + 
    platform_preference + 
    visit_mean +
    involvement +
    detail + 
    regulatory_focus * previous_experience +
    regulatory_focus * visit_frequency +
    regulatory_focus * visit_mean +
    regulatory_focus * app_expense,
  family = "binomial",
  data = surveysub)

summary(model_5)

#summary(model_5)$coefficients/summary(model_5)$standard.errors

round(exp(coef(model_5)), 4)

# DV: details

model_6 <- glm(
  detail ~ 
    numRating +
    shape +
    age + 
    gender +
    income + 
    visit_frequency + 
    app_expense + 
    previous_experience +
    regulatory_focus + 
    platform_preference + 
    visit_mean +
    involvement +
    review + 
    regulatory_focus * previous_experience +
    regulatory_focus * visit_frequency +
    regulatory_focus * visit_mean +
    regulatory_focus * app_expense,
  family = "binomial",
  data = surveysub)

summary(model_6)

#summary(model_6)$coefficients/summary(model_6)$standard.errors

round(exp(coef(model_6)), 4)

# The resulting intercept coefficients represents the odds of belonging to category 'HighJ' instead of categories High U, LowJ, and LowU. 

# An odds ratio of 0.009 means that, on average, the odds of belonging to category HighJ instead of category HighU are 0.009 times the odds of category HighU.

# If the odds ratio is less than 1 (e.g., 0.5), it indicates lower odds for category HighJ compared to category HighU. In this case, the odds of belonging to category HighJ are 0.5 times the odds of belonging to category HighU

# In our example, since the odds ratio of HighU is 0.009, it suggests that, on average, the odds of belonging to category HighJ are approximately 0.009 times the odds of belonging to category HighU. This implies that category HighU tends to have slightly higher odds compared to category A.


# Consider how to model a logit----
# Consider how to model a logit (purchase ~ .) where each participant is controlled for, and num_ratings and ratings_shape are IVs

lmmodel <- lm(
  purchase ~ 
    numRating +
    combine +
    subject -1,
  # factor(regulatory_focus) +
  # gender +
  # age +
  # income +
  # platform_preference +
  # visit_frequency +
  # app_expense +
  # previous_experience +
  # involvement,
  data = survey)

lmmodel




# Consider mapping a new column to name the choices----

# Mapping for the new column
purchase_choices <- c("a", "b", "c", "d")

# Use mutate to add the new column
survey_new <- survey %>% 
  mutate( purchase_choice = purchase_choices[
    match(combine, c("HighU", "HighJ", "LowU", "LowJ") )
  ] )

survey_new

df <- subset(survey_new, select = c(purchase_choice, combine) )
df

surveysub <- subset(survey_new, select = c(id, subject, purchase, appname, combine, purchase_choice, regulatory_focus, platform_preference, visit_frequency, app_expense, previous_experience, gender, income, involvement, involvement, age))

surveysub

# Use the distinct function to remove duplicate subject IDs
# survey_new <- distinct(survey, subject, .keep_all = TRUE) 


# Original Modeling ----


# Order by appname and subject, to match the order of prediction: appname 1~4
reorder <- survey[order(survey$subject, survey$appname),]

# reorder2 <- survey_new[order(survey$subject, survey$appname),]
# reorder2 <- na.omit(reorder2)

# Dataframe in mlogit long format
s_combine <- mlogit.data(reorder, 
                         choice = "purchase", 
                         shape = "long", 
                         id.var = "subject", 
                         alt.var = "appname")


# Single coefficient for individual-specific variables

# Notes:
# Based on "Discrete-Choice Logit Models with R" (page.17) 
# attach to appname=1, to get single coefficient for individual-specific variables (part1)

s_combine$rf <- with(
  s_combine, (appname == "1") * regulatory_focus)

s_combine$store <- with(
  s_combine, (appname == "1") * platform_preference)

s_combine$visit <- with(
  s_combine, (appname == "1") * visit_frequency)

s_combine$cost <- with(
  s_combine, (appname == "1") * app_expense)

s_combine$exper <- with(
  s_combine, (appname == "1") * previous_experience)

s_combine$sex <- with(
  s_combine, (appname == "1") * gender)

s_combine$inc <- with(
  s_combine, (appname == "1") * income)

s_combine$inv <- with(
  s_combine, (appname == "1") * involvement)

s_combine$age2 <- with(
  s_combine, (appname == "1") * age)


# Models

full_model <- mlogit(purchase ~ 
                       Ushape + 
                       numRating + 
                       rf + 
                       sex + 
                       age2 + 
                       inc + 
                       store + 
                       visit + 
                       cost + 
                       exper + 
                       inv, 
                     data=s_combine)

summary(full_model)

extension_model <- mlogit(purchase ~ 
                            combine_HighJ + 
                            combine_LowJ + 
                            combine_LowU + 
                            rf + 
                            sex + 
                            age2 + 
                            inc + 
                            store + 
                            visit + 
                            cost + 
                            exper + 
                            inv, 
                          data=s_combine)

summary(extension_model)

interaction_model <- mlogit(purchase ~ 
                              Ushape + 
                              numRating + 
                              rf + 
                              sex + 
                              age2 + 
                              inc + 
                              store + 
                              visit + 
                              cost + 
                              exper + 
                              inv + 
                              I(Ushape*visit_frequency), 
                            data=s_combine)

summary(interaction_model)


# Results in Odd Scale? (helps to say something meaningful)
# e.g., The odds of being economically inactive rather than in employment are 
# 73% higher for women than for men. 



# Nnt package ----

# Data
survey <- read.csv("regression_dummy_rf.csv", header=T)

# Use the distinct function to remove duplicate subject IDs
survey_new <- distinct(survey, subject, .keep_all = TRUE)

#  multinom function from nnet package
#survey_new$ <- relevel(survey_new$, ref = "")

test_full <- multinom(purchase ~ 
                        Ushape + 
                        numRating + 
                        regulatory_focus + 
                        gender + 
                        age + 
                        income + 
                        platform_preference + 
                        visit_frequency + 
                        app_expense + 
                        previous_experience + 
                        involvement, 
                      data = survey_new)

summary(test_full)


test_extension <- multinom(purchase ~ 
                             combine_HighJ + 
                             combine_LowJ + 
                             combine_LowU + 
                             regulatory_focus + 
                             gender + 
                             age + 
                             income + 
                             platform_preference + 
                             visit_frequency + 
                             app_expense + 
                             previous_experience + 
                             involvement, 
                           data = survey_new)

summary(test_extension) # NaNs produced

# Interaction model - final model used in Shirley's thesis
interaction_model <- mlogit(purchase ~ 
                              Ushape + 
                              numRating + 
                              regulatory_focus + 
                              gender + 
                              age + 
                              income + 
                              platform_preference + 
                              visit_frequency + 
                              app_expense + 
                              previous_experience + 
                              involvement + 
                              Ushape*visit_frequency, 
                            data=s_combine)

# We get an error

summary(interaction_model)



















































































































































