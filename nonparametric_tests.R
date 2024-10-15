# Packages -----
source("data_preparation.R")
library("tidyverse")

# Read -> numeric
surveysub$explore_num <- ifelse(surveysub$read == "TRUE", 1, 0)

# Mann-Whitney U test for HighJ
highj_d <- surveysub %>% 
  filter(purchased_ratings == "HighJ")

highj_mw <- wilcox.test(explore_num ~ regulatory_focus, data = highj_d)

# Mann-Whitney U test for LowJ
lowj_d <- surveysub %>% 
  filter(purchased_ratings == "LowJ")

lowj_mw <- wilcox.test(explore_num ~ regulatory_focus, data = lowj_d)

# Mann-Whitney U test for HighU
highu_d <- surveysub %>% 
  filter(purchased_ratings == "HighU")

highu_mw <- wilcox.test(explore_num ~ regulatory_focus, data = highu_d)

# Mann-Whitney U test for LowU
lowu_d <- surveysub %>% 
  filter(purchased_ratings == "LowU")

lowu_mw <- wilcox.test(explore_num ~ regulatory_focus, data = lowu_d)

highj_mw
highj_mw$p.value
lowj_mw
lowj_mw$p.value
highu_mw
highu_mw$p.value
lowu_mw
lowu_mw$p.value


# Kruskal-Wallis H test for Prevention
prev_data <- surveysub %>% 
  filter(regulatory_focus == "Prevention")

kw_prev <- kruskal.test(explore_num ~ purchased_ratings, data = prev_data)

# KW H test for Promotion
prom_data <- surveysub %>% 
  filter(regulatory_focus == "Promotion")

kw_prom <- kruskal.test(explore_num ~ purchased_ratings, data = prom_data)

kw_prev
kw_prom

# ANOVA + Tukey tests
reg_x_ratings <- aov(
  explore_num ~ regulatory_focus * purchased_ratings,
  data = surveysub
)
summary(reg_x_ratings)
# TukeyHSD(reg_x_ratings, conf.level = 0.01)

# New tests with large dataset -----

# Mann-Whitney U Test

# Explore into numeric
survey$explore_num <- as.numeric(survey$explore)

HighJ_d <- survey %>%
  filter(combine == "HighJ")

LowJ_d <- survey %>%
  filter(combine == "LowJ")

HighU_d <- survey %>%
  filter(combine == "HighU")

LowU_d <- survey %>%
  filter(combine == "LowU")

# Mann-Whitney U Test based on regulatory_focus and explore_num

highj_mw <- wilcox.test(explore_num ~ regulatory_focus, data = HighJ_d)

lowj_mw <- wilcox.test(explore_num ~ regulatory_focus, data = LowJ_d)

highu_mw <- wilcox.test(explore_num ~ regulatory_focus, data = HighU_d)

lowu_mw <- wilcox.test(explore_num ~ regulatory_focus, data = LowU_d)

# Display results
list(HighJ = highj_mw, LowJ = lowj_mw, HighU = highu_mw, LowU = lowu_mw)















