# Model 10 Fixed Effects

# Packages ----
require("survival")
require("lme4")
source("data_preparation.R")

# Levels of all factor variables
lapply(survey[, sapply(survey, is.factor)], levels)

# Fixed Effects Model----

# Conditional logistic regression with fixed effects for each subject
fe_model <- clogit(purchase ~ regulatory_focus + explore + shape + numRating + involvement + strata(subject), data = survey)

# Summary of the model
summary(fe_model)


# lme4 package 
glmer_model <- glmer(purchase ~ regulatory_focus + involvement + explore + shape + numRating + (1 | subject),
                     data = survey, family = binomial)


# glm 

# subject intercept excluded
fe_model1 <- glm(purchase ~ regulatory_focus + involvement + explore + shape + numRating + factor(subject) - 1,
                    data = survey, family = binomial)
summary(fe_model1)

# subject intercept included
fe_model2 <- glm(purchase ~ regulatory_focus + involvement + explore + shape + numRating + factor(subject), 
                 data = survey, family = binomial)
summary(fe_model2)


# lm Fixed Effects
fe_lm_model <- lm(purchase ~ regulatory_focus + involvement + explore + shape + numRating + subject - 1, 
                  data = survey)
summary(fe_lm_model)





