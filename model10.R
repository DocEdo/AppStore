# Model 10 Fixed Effects

# Packages ----
require("survival")
require("lme4")
source("data_preparation.R")
library("stargazer")

# Levels of all factor variables
lapply(survey[, sapply(survey, is.factor)], levels)

# Fixed Effects Models

# FE for DV: Purchase----

# Conditional logistic regression with fixed effects for each subject
fe_model <- clogit(purchase ~ regulatory_focus + explore + shape + numRating + strata(subject), data = survey)

# Summary of the model with NAs
summary(fe_model)


# Mixed Model with lme4 package
glmer_model <- glmer(purchase ~ regulatory_focus + explore + shape + numRating + (1 | subject),
                     data = survey, family = binomial)

summary(glmer_model)

# Subject intercept excluded
fe_model1 <- glm(purchase ~ regulatory_focus + explore + shape + numRating + factor(subject) - 1,
                    data = survey, family = binomial)

summary(fe_model1)

# Extract coefficients without all subjects
tidy_femodel1 <- tidy(fe_model1)

tidy_femodel1 <- tidy_femodel1[!grepl("factor\\(subject\\)", tidy_femodel1$term), ]

tidy_femodel1

# Glm
glm_femodel <- glm(purchase ~ regulatory_focus + explore + shape + numRating + factor(subject),
                   data = survey, family = binomial)

summary(glm_femodel)

# Extract coefficients without all subjects
tidy_glm_femodel <- tidy(glm_femodel)
tidy_glm_femodel <- tidy_glm_femodel[!grepl("^factor\\(subject\\)", tidy_glm_femodel $term), ]

tidy_glm_femodel


# lm Fixed Effects
fe_lm_model <- lm(purchase ~ regulatory_focus + explore + shape + numRating + subject - 1, 
                  data = survey)

summary(fe_lm_model)

# FE for DV: explore ---- 
survey$explore_num <- as.numeric(survey$explore) - 1

survey$subject <- as.factor(survey$subject)
# lm function FE -> does not work
fe_lmmodel2 <- lm(explore_num ~ 
                    shape + 
                    numRating + 
                    shape*numRating + 
                    regulatory_focus + 
                    subject - 1,
                  data = survey)

summary(fe_lmmodel2)

stargazer(fe_glmer2, type = "text")

tidy(fe_glmer2)

# Glm without fe
lm_model1 <- glm(explore ~ shape + 
                      numRating + 
                      shape * numRating + 
                      regulatory_focus, 
                    family = binomial, 
                    data = survey)

summary(lm_model1)

# Mixed effects
fe_glmer2 <- glmer(explore ~ 
                     shape + 
                     numRating + 
                     shape * numRating + 
                     regulatory_focus + 
                     (1 | subject),
                   family = binomial, 
                   data = survey)

summary(fe_glmer2)


# Lm without FE
lm_model2 <- lm(explore_num ~ 
                 shape + 
                 numRating + 
                 shape*numRating + 
                 regulatory_focus,
               data = survey)

summary(lm_model2)

# Explore model with interactions ----

# Base
explore_base <- glm(explore ~ 
                      shape + 
                      numRating + 
                      age + 
                      gender + 
                      income + 
                      visit_frequency + 
                      app_expense + 
                      previous_experience + 
                      platform_preference + 
                      involvement + 
                      regulatory_focus, 
                 family = binomial, 
                 data = survey)

summary(explore_base)

# Interaction
explore_int <- glm(explore ~ 
                      shape + 
                      numRating + 
                      age + 
                      gender + 
                      income + 
                      visit_frequency + 
                      app_expense + 
                      previous_experience + 
                      platform_preference + 
                      involvement + 
                      regulatory_focus +
                      regulatory_focus*shape, 
                 family = binomial, 
                 data = survey)

summary(explore_int)

explore_shapenum <- glm(explore ~ 
                      shape + 
                      numRating + 
                      age + 
                      gender + 
                      income + 
                      visit_frequency + 
                      app_expense + 
                      previous_experience + 
                      platform_preference + 
                      involvement + 
                      regulatory_focus +
                      shape * numRating, 
                    family = binomial, 
                    data = survey)

summary(explore_shapenum)

# Model with purchased_ratings Highu vs Highj ----

# Recoding purchased_ratings to a binary variable (HighU vs HighJ)
survey$highju <- ifelse(survey$purchased_ratings == "HighU", 1, 
                           ifelse(survey$purchased_ratings == "HighJ", 0, NA))

# Filtering out NA values (LowJ and LowU)
survey_filtered <- survey %>% filter(!is.na(highju))

# Logit HighU vs HighJ

# Base
explore_ju <- glm(explore ~ 
                    age + 
                    gender + 
                    income + 
                    visit_frequency + 
                    app_expense + 
                    previous_experience + 
                    regulatory_focus + 
                    platform_preference + 
                    involvement + 
                    factor(highju),
                  family = binomial,
                  data = survey_filtered)

summary(explore_ju)

# Shape? (numRating cannot be used because Low level is empty 0)
explore_ju2 <- glm(explore ~ 
                      age + 
                      gender + 
                      income + 
                      visit_frequency + 
                      app_expense + 
                      previous_experience + 
                      regulatory_focus + 
                      platform_preference + 
                      involvement + 
                      highju +
                      shape,
                    family = binomial,
                    data = survey_filtered)

summary(explore_ju2)

# Interaction
explore_jurf <- glm(explore ~ 
                    age + 
                    gender + 
                    income + 
                    visit_frequency + 
                    app_expense + 
                    previous_experience + 
                    regulatory_focus + 
                    platform_preference + 
                    involvement + 
                    factor(highju) +
                    factor(highju) * regulatory_focus,
                  family = binomial,
                  data = survey_filtered)

summary(explore_jurf)

explore_prev <- glm(explore ~ 
                      age + 
                      gender + 
                      income + 
                      visit_frequency + 
                      app_expense + 
                      previous_experience + 
                      regulatory_focus + 
                      platform_preference + 
                      involvement + 
                      factor(highju) +
                      factor(highju) * previous_experience,
                    family = binomial,
                    data = survey_filtered)

summary(explore_prev)

explore_freq <- glm(explore ~ 
                      age + 
                      gender + 
                      income + 
                      visit_frequency + 
                      app_expense + 
                      previous_experience + 
                      regulatory_focus + 
                      platform_preference + 
                      involvement + 
                      factor(highju) +
                      factor(highju) * visit_frequency,
                    family = binomial,
                    data = survey_filtered)

summary(explore_freq)

explore_freq <- glm(explore ~ 
                      age + 
                      gender + 
                      income + 
                      visit_frequency + 
                      app_expense + 
                      previous_experience + 
                      regulatory_focus + 
                      platform_preference + 
                      involvement + 
                      factor(highju) +
                      factor(highju) * visit_frequency,
                    family = binomial,
                    data = survey_filtered)

summary(explore_freq)

# VIF ----

# for Explore ~ HighU vs HighJ
round(vif(explore_ju), 3)








