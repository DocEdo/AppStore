# # Jaewon meetings

# Packages ----
require("nnet")
require("mlogit")
require("car")
source("data_preparation.R")

# Models

# Data prep ----

# Recoding purchased_ratings to a binary variable (HighU vs HighJ)
survey$highju <- ifelse(survey$purchased_ratings == "HighU", 1, 
                        ifelse(survey$purchased_ratings == "HighJ", 0, NA))

# Recoding purchased_ratings to a binary variable (HighU vs HighJ)
surveysub$highju <- ifelse(surveysub$purchased_ratings == "HighU", 1, 
                           ifelse(surveysub$purchased_ratings == "HighJ", 0, NA))

# Recoding purchased_ratings to a binary of HighU vs the rest in surveysub
surveysub$highu_rest <- as.numeric(surveysub$purchased_ratings == "HighU")

# Recoding purchased_ratings to a binary of HighU vs the rest in survey
survey$highu_rest <- as.numeric(survey$purchased_ratings == "HighU")

# Filtering out NA values (LowJ and LowU)
survey_filtered <- survey %>% filter(!is.na(highju))

# LOGIT: Explore ~ . + shape + ratings + shape*ratings ----
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

# LOGIT: Explore ~ . + HighJU + RF:HighJU ----

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

# LOGIT: Explore ~ . + HighU (versus the rest) + RF:HighU ----

explore_urest <- glm(explore ~ 
                      age + 
                      gender + 
                      income + 
                      visit_frequency + 
                      app_expense + 
                      previous_experience + 
                      regulatory_focus + 
                      platform_preference + 
                      involvement + 
                      factor(highu_rest) +
                      factor(highu_rest) * regulatory_focus,
                    family = binomial,
                    data = survey_filtered)

summary(explore_urest)

# LOGIT: RestHighU ~ . + Explore + RF:Explore ----

highu_rfexplore <- glm(highu_rest ~
                         age +
                         gender +
                         income + 
                         visit_frequency + app_expense + 
                         previous_experience + 
                         regulatory_focus + 
                         platform_preference + 
                         involvement + 
                         explore + 
                         explore * regulatory_focus, 
                       data = surveysub, 
                       family = binomial)

summary(highu_rfexplore)

# 2 x Logit: HighJU ~ . + Explore + RF:Explore (relevel RF both ways) ----

surveysub$regulatory_focus <- 
  relevel(factor(surveysub$regulatory_focus), ref = "Prevention")

surveysub$regulatory_focus <- 
  relevel(factor(surveysub$regulatory_focus), ref = "Promotion")

surveysub_filtered <- surveysub %>% filter(!is.na(highju))

highju_promotion <- glm(highju ~ 
                age + 
                gender + 
                income + 
                visit_frequency + 
                app_expense + 
                previous_experience + 
                regulatory_focus + 
                platform_preference + 
                involvement + 
                explore + 
                regulatory_focus * explore, 
              data = surveysub_filtered, 
              family = binomial)

summary(highju_promotion)


highju_prevention <- glm(highju ~ 
                age + 
                gender + 
                income + 
                visit_frequency + 
                app_expense + 
                previous_experience + 
                regulatory_focus + 
                platform_preference + 
                involvement + 
                explore +
                regulatory_focus * explore, 
              data = surveysub_filtered, 
              family = binomial)

summary(highju_prevention)


# Meeting Followup Models

# IVs: Explored HighU ----

# Create column Explored HighU
surveysub <- surveysub %>%
  mutate(explored_highu = ifelse(purchased_ratings == "HighU" & 
                                   ((detail == "Read") | (review == "Read")), 
                                 "Explored", "Not Explored"))

# Get rid of NAs in HighJU
surveysub_filtered2 <- surveysub %>% filter(!is.na(highju))

# 2 x Logit: HighJU ~ . + Explore + RF:Explore (relevel RF both ways)

# Prevention
surveysub_filtered2$regulatory_focus <- 
  relevel(factor(surveysub_filtered2$regulatory_focus), ref = "Prevention")

highju_exploredu_prev <- glm(highju ~ 
                          age + 
                          gender + 
                          income + 
                          visit_frequency + 
                          app_expense + 
                          previous_experience + 
                          regulatory_focus + 
                          platform_preference + 
                          involvement + 
                          factor(explored_highu) + 
                          regulatory_focus * factor(explored_highu), 
                        data = surveysub_filtered2, 
                        family = binomial)

summary(highju_exploredu_prev)

# Promotion
surveysub_filtered2$regulatory_focus <- 
  relevel(factor(surveysub_filtered2$regulatory_focus), ref = "Promotion")

highju_exploredu_prom <- glm(highju ~ 
                age + 
                gender + 
                income + 
                visit_frequency + 
                app_expense + 
                previous_experience + 
                regulatory_focus + 
                platform_preference + 
                involvement + 
                factor(explored_highu) + 
                regulatory_focus * factor(explored_highu), 
              data = surveysub_filtered2, 
              family = binomial)

summary(highju_exploredu_prom)


# LOGIT: RestHighU ~ . + ExploreHighU + RF:ExploreHighU 

highu_rfexplored_highu <- glm(highu_rest ~
                         age + 
                         gender + 
                         income + 
                         visit_frequency + app_expense + 
                         previous_experience + 
                         regulatory_focus + 
                         platform_preference + 
                         involvement + 
                         factor(explored_highu) + 
                         factor(explored_highu) * regulatory_focus, 
                       data = surveysub, 
                       family = binomial)

summary(highu_rfexplored_highu)

# IVs: Explored HighJ ----

# Explored HighJ
surveysub <- surveysub %>%
  mutate(explored_highj = ifelse(purchased_ratings == "HighJ" & 
                                   ((detail == "Read") | (review == "Read")), 
                                 "Explored", "Not Explored"))

# 2 x Logit: HighJU ~ . + Explore + RF:Explore (relevel RF both ways)

surveysub_filtered3 <- surveysub %>% filter(!is.na(highju))

# Prevention
surveysub_filtered3$regulatory_focus <- 
  relevel(factor(surveysub_filtered3$regulatory_focus), ref = "Prevention")

highju_exploredj_prev <- glm(highju ~ 
                              age + 
                              gender + 
                              income + 
                              visit_frequency + 
                              app_expense + 
                              previous_experience + 
                              regulatory_focus + 
                              platform_preference + 
                              involvement + 
                              factor(explored_highj) + 
                              regulatory_focus * factor(explored_highj), 
                            data = surveysub_filtered3, 
                            family = binomial)

summary(highju_exploredj_prev)

# Promotion
surveysub_filtered3$regulatory_focus <- 
  relevel(factor(surveysub_filtered3$regulatory_focus), ref = "Promotion")

highju_exploredj_prom <- glm(highju ~ 
                              age + 
                              gender + 
                              income + 
                              visit_frequency + 
                              app_expense + 
                              previous_experience + 
                              regulatory_focus + 
                              platform_preference + 
                              involvement + 
                              factor(explored_highj) + 
                              regulatory_focus * factor(explored_highj), 
                            data = surveysub_filtered3, 
                            family = binomial)

summary(highju_exploredj_prom)


# LOGIT: RestHighU ~ . + ExploreHighU + RF:ExploreHighU 

highu_rfexplored_highj <- glm(highu_rest ~
                               age + 
                               gender + 
                               income + 
                               visit_frequency + app_expense + 
                               previous_experience + 
                               regulatory_focus + 
                               platform_preference + 
                               involvement + 
                               factor(explored_highj) + 
                               factor(explored_highj) * regulatory_focus, 
                             data = surveysub, 
                             family = binomial)

summary(highu_rfexplored_highj)


# IV: Explored HighU vs HighJ ----
surveysub_filtered4 <- surveysub %>% filter(!is.na(highju))

surveysub_filtered4 <- surveysub %>%
  filter(purchased_ratings %in% c("HighU", "HighJ")) %>%
  mutate(explored_highs = case_when(
    purchased_ratings == "HighU" & (detail == "Read" | review == "Read") ~ "Explored HighU",
    purchased_ratings == "HighJ" & (detail == "Read" | review == "Read") ~ "Explored HighJ"
  )) %>%
  filter(!is.na(explored_highs))

# 2 x Logit: HighJU ~ . + Explore + RF:Explored HighU vs HighJ (relevel RF both ways)

# Prevention
surveysub_filtered4$regulatory_focus <- 
  relevel(factor(surveysub_filtered4$regulatory_focus), ref = "Prevention")

highs_explored_prev <- glm(highju ~ 
                              age + 
                              gender + 
                              income + 
                              visit_frequency + 
                              app_expense + 
                              previous_experience + 
                              regulatory_focus + 
                              platform_preference + 
                              involvement + 
                              factor(explored_highs) + 
                              regulatory_focus * factor(explored_highs), 
                            data = surveysub_filtered4, 
                            family = binomial,
                            control = glm.control(maxit = 50))

summary(highs_explored_prev)

# Promotion
surveysub_filtered4$regulatory_focus <- 
  relevel(factor(surveysub_filtered4$regulatory_focus), ref = "Promotion")

highs_explored_prom <- glm(highju ~ 
                              age + 
                              gender + 
                              income + 
                              visit_frequency + 
                              app_expense + 
                              previous_experience + 
                              regulatory_focus + 
                              platform_preference + 
                              involvement + 
                              factor(explored_highs) + 
                              regulatory_focus * factor(explored_highs), 
                            data = surveysub_filtered4, 
                            family = binomial)

summary(highs_explored_prom)


# LOGIT: RestHighU ~ . + ExploreHighU + RF:ExploreHighU 

surveysub_rest <- surveysub %>%
  filter(purchased_ratings %in% c("HighU", "HighJ")) %>%
  mutate(explored_highs = case_when(
    purchased_ratings == "HighU" & (detail == "Read" | review == "Read") ~ "Explored HighU",
    purchased_ratings == "HighJ" & (detail == "Read" | review == "Read") ~ "Explored HighJ"
  )) %>%
  filter(!is.na(explored_highs))

highu_rfexplore_highs <- glm(highu_rest ~
                               age + 
                               gender + 
                               income + 
                               visit_frequency + app_expense + 
                               previous_experience + 
                               regulatory_focus + 
                               platform_preference + 
                               involvement + 
                               factor(explored_highs) + 
                               factor(explored_highs) * regulatory_focus, 
                             data = surveysub_rest, 
                             family = binomial)

summary(highu_rfexplore_highs)













