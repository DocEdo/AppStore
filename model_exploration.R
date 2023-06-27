# Model Exploration

# Packages ----
require("nnet")
require("mlogit")

# Releveling reference categories ----

surveysub$purchased_ratings <- 
  relevel(factor(surveysub$purchased_ratings), ref = "HighJ")

surveysub$shape <- 
  relevel(factor(surveysub$shape), ref = "J")

# Model 1 ----

# Original Model with multinom

# DV: purchased_rating 

model_1 <- multinom( # Multinom
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

summary(model_1)

summary(model_1)$coefficients/summary(model_1)$standard.errors

exp(coef(model_1))

# Model 2 ----

# Our Model Exploration starts here

# DV: purchased_ratings

model_2prev <- multinom(
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
    regulatory_focus * previous_experience,
  data = surveysub)

model_2visit <- multinom(
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
    regulatory_focus * visit_frequency,
  data = surveysub)

model_2vismean <- multinom(
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
    regulatory_focus * visit_mean,
  data = surveysub)

model_2direct <- multinom(
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
    regulatory_focus * directlyPurchase,
  data = surveysub)

model_2app <- multinom(
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
    regulatory_focus * app_expense,
  data = surveysub)

model_2detailp <- multinom(
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
    regulatory_focus * DetailPurchase,
  data = surveysub)

model_2reviewp <- multinom(
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
    regulatory_focus * ReviewPurchase,
  data = surveysub)

# model 2 previous experience
summary(model_2prev)

z <- summary(model_2prev)$coefficients/summary(model_2prev)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
round(p, 4)

round(exp(coef(model_2prev)),4)

# model 2 visit frequency
summary(model_2visit)

z <- summary(model_2visit)$coefficients/summary(model_2visit)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
round(p, 4)

round(exp(coef(model_2visit)),4)

# model 2 visit mean
summary(model_2vismean)

z <- summary(model_2vismean)$coefficients/summary(model_2vismean)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
round(p, 4)

round(exp(coef(model_2vismean)),4)

# model 2 direct purchase
summary(model_2direct)

z <- summary(model_2direct)$coefficients/summary(model_2direct)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
round(p, 4)

round(exp(coef(model_2direct)),4)

# model 2 app expense
summary(model_2app)

z <- summary(model_2app)$coefficients/summary(model_2app)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
round(p, 4)

round(exp(coef(model_2app)),4)

# model 2 detail purchase
summary(model_2detailp)

z <- summary(model_2detailp)$coefficients/summary(model_2detailp)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
round(p, 4)

round(exp(coef(model_2detailp)),4)

# model 2 review purchase
summary(model_2reviewp)

z <- summary(model_2reviewp)$coefficients/summary(model_2reviewp)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
round(p, 4)

round(exp(coef(model_2reviewp)),4)

# Model 3 ----

# DV: shape

model_3prev <- glm(
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
    regulatory_focus * previous_experience,
  family = "binomial",
  data = surveysub)

model_3visit <- glm(
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
    regulatory_focus * visit_frequency,
  family = "binomial",
  data = surveysub)

model_3vismean <- glm(
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
    regulatory_focus * visit_mean,
  family = "binomial",
  data = surveysub)

model_3direct <- glm(
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
    regulatory_focus * directlyPurchase,
  family = "binomial",
  data = surveysub)

model_3app <- glm(
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
    regulatory_focus * app_expense,
  family = "binomial",
  data = surveysub)

# model 3 previous experience
summary(model_3prev)
round(exp(coef(model_3prev)), 4)

# model 3 visit frequency
summary(model_3visit)
round(exp(coef(model_3visit)), 4)

# model 3 visit mean
summary(model_3vismean)
round(exp(coef(model_3vismean)), 4)

# model 3 direct purchase
summary(model_3direct)
round(exp(coef(model_3direct)), 4)

# model 3 app expense
summary(model_3app)
round(exp(coef(model_3app)), 4)

# Model 4 ----

# DV: numRating

model_4prev <- glm(
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
    regulatory_focus * previous_experience,
  family = "binomial",
  data = surveysub)

model_4visit <- glm(
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
    regulatory_focus * visit_frequency,
  family = "binomial",
  data = surveysub)

model_4vismean <- glm(
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
    regulatory_focus * visit_mean,
  family = "binomial",
  data = surveysub)

model_4direct <- glm(
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
    regulatory_focus * directlyPurchase,
  family = "binomial",
  data = surveysub)

model_4app <- glm(
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
    regulatory_focus * app_expense,
  family = "binomial",
  data = surveysub)

# model 4 previous experience
summary(model_4prev)
round(exp(coef(model_4prev)), 4)

# model 4 visit frequency
summary(model_4visit)
round(exp(coef(model_4visit)), 4)

# model 4 visit mean
summary(model_4vismean)
round(exp(coef(model_4vismean)), 4)

# model 4 directly purchase
summary(model_4direct)
round(exp(coef(model_4direct)), 4)

# model 4 app expense
summary(model_4app)
round(exp(coef(model_4app)), 4)

# Model 5 ----

# DV: review

model_5prev <- glm(
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
    regulatory_focus * previous_experience,
  family = "binomial",
  data = surveysub)

model_5visit <- glm(
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
    regulatory_focus * visit_frequency,
  family = "binomial",
  data = surveysub)

model_5vismean <- glm(
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
    regulatory_focus * visit_mean,
  family = "binomial",
  data = surveysub)

model_5app <- glm(
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
    regulatory_focus * app_expense,
  family = "binomial",
  data = surveysub)

# model 5 previous experience
summary(model_5prev)
round(exp(coef(model_5prev)), 4)

# model 5 visit frequency
summary(model_5visit)
round(exp(coef(model_5visit)), 4)

# model 5 visit mean
summary(model_5vismean)
round(exp(coef(model_5vismean)), 4)

# model 5 app expense
summary(model_5app)
round(exp(coef(model_5app)), 4)

# Model 6 ----

# DV: details

model_6prev <- glm(
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
    regulatory_focus * previous_experience,
  family = "binomial",
  data = surveysub)

model_6visit <- glm(
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
    regulatory_focus * visit_frequency,
  family = "binomial",
  data = surveysub)

model_6vismean <- glm(
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
    regulatory_focus * visit_mean,
  family = "binomial",
  data = surveysub)

model_6app <- glm(
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
    regulatory_focus * app_expense,
  family = "binomial",
  data = surveysub)

# model 6 previous experience
summary(model_6prev)
round(exp(coef(model_5prev)), 4)

# model 6 visit frequency
summary(model_6visit)
round(exp(coef(model_5visit)), 4)

# model 6 visit mean
summary(model_6vismean)
round(exp(coef(model_5vismean)), 4)

# model 6 app expense
summary(model_6app)
round(exp(coef(model_5app)), 4)


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





































