source("data_preparation.R")
library(tidyverse)

# Choiceset in Survey dataset

length(unique(survey$choiceset))

table(survey$choiceset)

# data_join Dataset

# join_survey
join_names <- colnames(join_survey)

split(join_names, ceiling(seq_along(join_names) / 10))

write.table(join_names, 'join_survey_columns.txt', row.names = FALSE, col.names = FALSE, quote = FALSE)

unique(join_survey$choiceset)

# join_appev
appev_names <- colnames(join_appev)

split(appev_names, ceiling(seq_along(appev_names) / 10))

write.table(appev_names, 'appev_survey_columns.txt', row.names = FALSE, col.names = FALSE, quote = FALSE)

# 
unique_purchases <- join_appev %>%
  filter(purchase == 1) %>%
  distinct(subjectinfo_id, appname, .keep_all = TRUE)

# Count the number of purchases per appname
appname_counts <- unique_purchases %>%
  group_by(appname) %>%
  summarise(purchase_count = n())

appname_counts


