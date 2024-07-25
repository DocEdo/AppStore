# # J1 + SR + ED meetings

# Packages ----
require("tidyverse")
require("nnet")
require("mlogit")
require("car")
source("data_preparation.R")
require("rcompanion")
require("dunn.test")
require("FSA")
require("broom")
require("openxlsx")

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

# Get rid of NAs in HighJU
surveysub_filtered2 <- surveysub %>% filter(!is.na(highju))

# 2 x Logit: HighJU ~ . + Explore + RF:HighUExplored (relevel RF both ways)

# Prevention
surveysub_filtered2$regulatory_focus <- 
  relevel(factor(surveysub_filtered2$regulatory_focus), ref = "Prevention")

highju_hiu_prev <- glm(highju ~ 
                          age + 
                          gender + 
                          income + 
                          visit_frequency + 
                          app_expense + 
                          previous_experience + 
                          regulatory_focus + 
                          platform_preference + 
                          involvement + 
                          highU_explored + 
                          regulatory_focus * highU_explored, 
                        data = surveysub_filtered2, 
                        family = binomial)

summary(highju_hiu_prev)

# Promotion
surveysub_filtered2$regulatory_focus <- 
  relevel(factor(surveysub_filtered2$regulatory_focus), ref = "Promotion")

highju_hiu_prom <- glm(highju ~ 
                         age + 
                         gender + 
                         income + 
                         visit_frequency + 
                         app_expense + 
                         previous_experience + 
                         regulatory_focus + 
                         platform_preference + 
                         involvement + 
                         highU_explored + 
                         regulatory_focus * highU_explored, 
                       data = surveysub_filtered2, 
                       family = binomial)

summary(highju_hiu_prom)

# IVs: Explored HighJ ----

# 2 x Logit: HighJU ~ . + Explore + RF:Explore (relevel RF both ways)

surveysub_filtered3 <- surveysub %>% filter(!is.na(highju))

# Prevention
surveysub_filtered3$regulatory_focus <- 
  relevel(factor(surveysub_filtered3$regulatory_focus), ref = "Prevention")

highju_hij_prev <- glm(highju ~ 
                              age + 
                              gender + 
                              income + 
                              visit_frequency + 
                              app_expense + 
                              previous_experience + 
                              regulatory_focus + 
                              platform_preference + 
                              involvement + 
                              highJ_explored + 
                              regulatory_focus * highJ_explored, 
                            data = surveysub_filtered3, 
                            family = binomial)

summary(highju_hij_prev)

# Promotion
surveysub_filtered3$regulatory_focus <- 
  relevel(factor(surveysub_filtered3$regulatory_focus), ref = "Promotion")

highju_hij_prom <- glm(highju ~ 
                              age + 
                              gender + 
                              income + 
                              visit_frequency + 
                              app_expense + 
                              previous_experience + 
                              regulatory_focus + 
                              platform_preference + 
                              involvement + 
                              highJ_explored + 
                              regulatory_focus * highJ_explored, 
                            data = surveysub_filtered3, 
                            family = binomial)

summary(highju_hij_prom)


# LOGIT: RestHighU ~ . + ExploreHighU + RF:ExploreHighU 

hirest_rfhighj <- glm(highu_rest ~
                               age + 
                               gender + 
                               income + 
                               visit_frequency + app_expense + 
                               previous_experience + 
                               regulatory_focus + 
                               platform_preference + 
                               involvement + 
                               highJ_explored + 
                               highJ_explored * regulatory_focus, 
                             data = surveysub, 
                             family = binomial)

summary(hirest_rfhighj)


# LOGIT: HighU_Explored ~ . ----

explored_base <- glm(highU_explored ~ 
                       age + 
                       gender + 
                       income + 
                       visit_frequency + 
                       app_expense + 
                       previous_experience + 
                       regulatory_focus + 
                       platform_preference + 
                       involvement,
                     family = binomial,
                     data = surveysub)

summary(explored_base)

explored_summary <- tidy(explored_base)

# Round the numeric columns to three decimal places
explored_summary$estimate <- round(explored_summary$estimate, 3)
explored_summary$std.error <- round(explored_summary$std.error, 3)
explored_summary$statistic <- round(explored_summary$statistic, 3)
explored_summary$p.value <- round(explored_summary$p.value, 3)

write.xlsx(explored_summary, file = "explored_model.xlsx")

explored_shapexrating <- glm(highU_explored ~ 
                        age + 
                        gender + 
                        income + 
                        visit_frequency + 
                        app_expense + 
                        previous_experience + 
                        regulatory_focus + 
                        platform_preference + 
                        involvement +
                        shape +
                        numRating +
                        shape * numRating,
                      family = binomial,
                      data = surveysub)

summary(explored_shapexrating)

explored_shapexrf <- glm(highU_explored ~ 
                               age + 
                               gender + 
                               income + 
                               visit_frequency + 
                               app_expense + 
                               previous_experience + 
                               regulatory_focus + 
                               platform_preference + 
                               involvement +
                               shape +
                               numRating +
                               shape * regulatory_focus,
                             family = binomial,
                             data = surveysub)

summary(explored_shapexrf)

explored_ratingxrf <- glm(highU_explored ~ 
                               age + 
                               gender + 
                               income + 
                               visit_frequency + 
                               app_expense + 
                               previous_experience + 
                               regulatory_focus + 
                               platform_preference + 
                               involvement +
                               shape +
                               numRating +
                               numRating * regulatory_focus,
                             family = binomial,
                             data = surveysub)

summary(explored_ratingxrf)



# LOGIT: Purchase(Rest-vs-HighU) ~ . + Explored HighU + RF:Explored HighU ----

filter_highu <- surveysub

# Prevention resthighu
filter_highu$regulatory_focus <- 
  relevel(factor(filter_highu$regulatory_focus), ref = "Prevention")

hirest_rfhighu_prev <- glm(highu_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             highU_explored + 
                             highU_explored * regulatory_focus, 
                           data = filter_highu, 
                           family = binomial)

summary(hirest_rfhighu_prev)

# Promotion resthighu
filter_highu$regulatory_focus <- 
  relevel(factor(filter_highu$regulatory_focus), ref = "Promotion")

hirest_rfhighu_prom <- glm(highu_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             highU_explored + 
                             highU_explored * regulatory_focus, 
                           data = filter_highu, 
                           family = binomial)

summary(hirest_rfhighu_prom)

# Summary into a data frame
purchase_summary <- tidy(hirest_rfhighu_prom)

# Round the numeric columns to three decimal places
purchase_summary$estimate <- round(purchase_summary$estimate, 3)
purchase_summary$std.error <- round(purchase_summary$std.error, 3)
purchase_summary$statistic <- round(purchase_summary$statistic, 3)
purchase_summary$p.value <- round(purchase_summary$p.value, 3)

write.xlsx(purchase_summary, file = "purchase_model.xlsx")

# LOGIT BASE: RestHighU ~ . + ExploreHighU 

hirest_highuexplored <- glm(highu_rest ~
                              age + 
                              gender + 
                              income + 
                              visit_frequency + app_expense + 
                              previous_experience + 
                              regulatory_focus + 
                              platform_preference + 
                              involvement + 
                              highU_explored, 
                            data = surveysub, 
                            family = binomial)

summary(hirest_highuexplored)

# Summary into a data frame
purchasebase_summary <- tidy(hirest_highuexplored)

# Round the numeric columns to three decimal places
purchasebase_summary$estimate <- round(purchasebase_summary$estimate, 3)
purchasebase_summary$std.error <- round(purchasebase_summary$std.error, 3)
purchasebase_summary$statistic <- round(purchasebase_summary$statistic, 3)
purchasebase_summary$p.value <- round(purchasebase_summary$p.value, 3)

write.xlsx(purchasebase_summary, file = "purchasebase_model.xlsx")

# Kruskal Wallis + Dunn Test ----

# Combined variable of four groups
surveysub$fourgroups <- with(surveysub, interaction(regulatory_focus, highU_explored, sep = "_"))

kruskal_fourgroups <- kruskal.test(highu_rest ~ fourgroups, data = surveysub)
kruskal_fourgroups

dunn_fourgroups <- dunnTest(highu_rest ~ fourgroups, data = surveysub, method="bonferroni")
dunn_fourgroups

# Data frame for Dunn's post-hoc test results
dunn_results_df <- data.frame(
  Comparison = c("Prevention_FALSE - Prevention_TRUE",
                 "Prevention_FALSE - Promotion_FALSE",
                 "Prevention_TRUE - Promotion_FALSE",
                 "Prevention_FALSE - Promotion_TRUE",
                 "Prevention_TRUE - Promotion_TRUE",
                 "Promotion_FALSE - Promotion_TRUE"),
  Z_Value = c(-3.9709407, -2.0698411, 1.9243637, -1.7730452, 2.2222135, 0.2990586),
  P_Value_Unadjusted = c(7.158941e-05, 3.846723e-02, 5.430901e-02, 7.622121e-02, 2.626888e-02, 7.648954e-01),
  P_Value_Adjusted = c(0.0004295365, 0.2308033774, 0.3258540573, 0.4573272657, 0.1576132967, 1.0000000000)
)

# Add Kruskal-Wallis test result
kruskal_result_df <- data.frame(
  Test = "Kruskal-Wallis",
  Chi_Squared = 16.003,
  DF = 3,
  P_Value = 0.001133
)

# Combine the data frames
combined_posthoc <- list(
  Kruskal_Wallis = kruskal_result_df,
  Dunns_Post_Hoc = dunn_results_df
)

write.xlsx(combined_posthoc, file = "combined_posthoc_results.xlsx")

combined_results <- data.frame(
  Test_Type = c("Kruskal-Wallis", rep("Dunn's Post-hoc", 6)),
  Comparison = c("Overall",
                 "Prevention_FALSE - Prevention_TRUE",
                 "Prevention_FALSE - Promotion_FALSE",
                 "Prevention_TRUE - Promotion_FALSE",
                 "Prevention_FALSE - Promotion_TRUE",
                 "Prevention_TRUE - Promotion_TRUE",
                 "Promotion_FALSE - Promotion_TRUE"),
  Z_Value = c(NA, -3.9709407, -2.0698411, 1.9243637, -1.7730452, 2.2222135, 0.2990586),
  P_Value_Unadjusted = c(0.001133, 7.158941e-05, 3.846723e-02, 5.430901e-02, 7.622121e-02, 2.626888e-02, 7.648954e-01),
  P_Value_Adjusted = c(NA, 0.0004295365, 0.2308033774, 0.3258540573, 0.4573272657, 0.1576132967, 1.0000000000),
  Kruskal_Wallis_Chi_Squared = c(16.003, rep(NA, 6)),
  DF = c(3, rep(NA, 6))
)

combined_results$Z_Value <- round(combined_results$Z_Value, 3)
combined_results$P_Value_Unadjusted <- round(combined_results$P_Value_Unadjusted, 3)
combined_results$P_Value_Adjusted <- round(combined_results$P_Value_Adjusted, 3)
combined_results$Kruskal_Wallis_Chi_Squared <- round(combined_results$Kruskal_Wallis_Chi_Squared, 3)

combined_results[is.na(combined_results)] <- "-"

write.xlsx(combined_results, file = "Kruskal_Dunns_Results.xlsx")
