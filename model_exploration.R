# Model Exploration

# Packages ----
require("nnet")
require("mlogit")
source("data_preparation.R")

# Example of interpretation: ----

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





































