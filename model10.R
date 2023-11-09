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

# Glm factored subject -> Doesn't work
fe_glmmodel2 <- glm(explore ~ shape + 
                      numRating + 
                      shape * numRating + 
                      regulatory_focus + 
                      factor(subject), 
                    family = binomial, 
                    data = survey,
                    control = glm.control(maxit = 50))

summary(fe_glmmodel2)

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






