# Packages -----
source("data_preparation.R")
library("tidyverse")

library(ggplot2)
library(dplyr)

# Read -> numeric
surveysub$read_num <- ifelse(surveysub$read == "TRUE", 1, 0)

# Mann-Whitney U test for HighJ
highj_d <- surveysub %>% 
  filter(purchased_ratings == "HighJ")

highj_mw <- wilcox.test(read_num ~ regulatory_focus, data = highj_d)

# Mann-Whitney U test for LowJ
lowj_d <- surveysub %>% 
  filter(purchased_ratings == "LowJ")

lowj_mw <- wilcox.test(read_num ~ regulatory_focus, data = lowj_d)

# Mann-Whitney U test for HighU
highu_d <- surveysub %>% 
  filter(purchased_ratings == "HighU")

highu_mw <- wilcox.test(read_num ~ regulatory_focus, data = highu_d)

# Mann-Whitney U test for LowU
lowu_d <- surveysub %>% 
  filter(purchased_ratings == "LowU")

lowu_mw <- wilcox.test(read_num ~ regulatory_focus, data = lowu_d)

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

kw_prev <- kruskal.test(read_num ~ purchased_ratings, data = prev_data)

# KW H test for Promotion
prom_data <- surveysub %>% 
  filter(regulatory_focus == "Promotion")

kw_prom <- kruskal.test(read_num ~ purchased_ratings, data = prom_data)

kw_prev
kw_prom










