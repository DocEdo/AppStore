# Meeting Sep. 6 (post-quals)
source("data_preparation.R")
source("meeting_aug23.R")
library("tidyverse")
library("nnet")
library("broom")

# Explore (HighU-vs-HighJ-vs-both-vs-none)
surveysub <- surveysub %>%
  mutate(
    Explored = case_when(
      highU_explored == TRUE & highJ_explored == FALSE ~ "HighU",  # Explored HighU only
      highU_explored == FALSE & highJ_explored == TRUE ~ "HighJ",  # Explored HighJ only
      highU_explored == TRUE & highJ_explored == TRUE ~ "Both",    # Explored both HighU and HighJ
      highU_explored == FALSE & highJ_explored == FALSE ~ "None"   # Explored neither
    )
  )

surveysub$Explored <- factor(surveysub$Explored)

table(surveysub$Explored)

surveysub$Explored <- relevel(surveysub$Explored, ref = "None")

# Model for highu vs highj vs both vs none
exp_highs_none <- multinom(Explored ~
                             age +
                             gender +
                             income +
                             visit_frequency +
                             app_expense +
                             previous_experience +
                             regulatory_focus +
                             platform_preference +
                             involvement +
                             factor(highU_apporder) +
                             factor(highU_appname),
                           data = surveysub)

summary(exp_highs_none)

round(exp(coef(exp_highs_none)),3)

multinom_pvalues <- function(est_model) {
  summary(est_model)

  z <- summary(est_model)$coefficients/summary(est_model)$standard.errors

  # return p-values
  ( (1 - pnorm(abs(z), 0, 1)) * 2) |> round(3)
}

multinom_pvalues(exp_highs_none)

# tidy_multinom <- tidy(exp_highs_none)
# 
# # odds ratio
# tidy_multinom <- tidy_multinom %>%
#   mutate(odds_ratio = exp(estimate))
# 
# # p-vals (based on z-statistics)
# tidy_multinom <- tidy_multinom %>%
#   mutate(p_value = 2 * (1 - pnorm(abs(estimate / std.error))))
# 
# tidy_multinom_table <- tidy_multinom %>%
#   select(term, y.level, estimate, odds_ratio, p_value)
# 
# # Way too long...
# print(n = 48, tidy_multinom_table)

# 



