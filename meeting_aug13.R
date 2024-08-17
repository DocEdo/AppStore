source("data_preparation.R")
library(AlgDesign)
library(readxl)
library(tidyverse)
library(openxlsx)


# According to Aizaki and Nishimura - How to create choice sets: ----
# 5 setps using R:

# Step 1: gen.factorial() full factorial design
# Step 2: optFederov() fractional factorial design (orthogonal from full design)
# Step 3: Making M-1 copies of fractional factorial design
# Step 4: In order to create a choice set with M alternatives, randomly select one of the alternatives (rows) from each of the M sets of the fractional factorial design without replacement. Repeat this step until all alternatives in each of the M sets of the fractional factorial design assigned to P choice sets
# Step 5: Translating the design codes in the choice sets into codes with a unique and corresponding level, and then adding the none-of-these option to each choice set optionally

# Example

# In their example they had 2 two-level factors (two milk labels as 'yes' or 'no') and 1 four-level factor (price as '145' 150' '155' '160')
# In a full factorial design two or more attributes (IVs) are manipulated and all combinations of the levels of each attributes are included.
# In this example, the full factorial design comprises of 16 combinations of the levels of each attribute (=2x2x4)

# Step 1 Generate full factorial design
ffd <- gen.factorial(c(2,2,4), varNames=c("HAC","ECO","PRI"), factors="all")
ffd

# Step 2 Generate fractional factorial design
set.seed(54321)

des <- optFederov(~.,ffd,8) 
# ~. indicates using all the data variables linearly and their names
# ffd is the data contained
# 8 is the number of rows (alternatives) contained in the fractional design

des
alt1 <- des$design
alt1

# Step 3 make copies of the fractional design
alt2 <- alt1

# Step 4 create choice sets using random selection without replacement
alt1 <- transform(alt1, r1=runif(8))
alt1

alt2 <- transform(alt2, r2=runif(8))
alt2

# Sort each fractional design on the basis of it's random variable
alt1_sort <- alt1[order(alt1$r1),]
alt1_sort

alt2_sort <- alt2[order(alt2$r2),]
alt2_sort

# Each line of the sorted fractional factorial design corresponds to each question of the choice experiment. (First line in both alts would be Q1 for both Milk A and Milk B)

# Step 5 Translate the design code

# 72 choice sets lookup dataframe -----

choice_df <- read_excel("72 choice sets.xlsx", sheet = 6)
choice_df

long_choice <- choice_df %>%
  pivot_longer(cols = starts_with("position"),
               names_to = c("position", "type"),
               names_pattern = "position(\\d+)-(.*)") %>% # See comments below
  pivot_wider(names_from = type, values_from = value) %>%
  select(`choice set`, position, app, rating) %>%
  arrange(`choice set`, position)

# position(\\d+): This captures the number after "position" (1, 2, 3, 4)
# -(.*): This captures everything after the hyphen (which will be either app or rating)

long_choice

# Adjust choiceset from 144 to 72 ----
surveysub$adjusted_choiceset <- ifelse(surveysub$choiceset > 72, 
                                       surveysub$choiceset - 72, 
                                       surveysub$choiceset)

surveysub_merged <- merge(surveysub, long_choice, 
                     by.x = c("adjusted_choiceset", "apporder"), # by.x first df
                     by.y = c("choice set", "position"), # by.y second df
                     all.x = TRUE) # keep

# Keep the subject order
surveysub_merged <- surveysub_merged[order(surveysub_merged$subject), ]

surveysub_merged

view(surveysub_merged[, c("subject", "apporder", "app", "adjusted_choiceset", "rating", "purchased_ratings")])

view(survey[, c("subject", "apporder", "appname", "choiceset", "purchased_ratings")])

# Previous approach won't work because Choice sets are structured differently from the actual dataset

# Categorical variables for App Name (recode) and App Position ----

# Recode appname to appname_purchased
surveysub$appname_purchased <- recode(surveysub$appname,
                                  `1` = "JogStats",
                                  `2` = "Map My Walk",
                                  `3` = "FITAPP",
                                  `4` = "Running Watch")

# Convert to factor
surveysub$appname_purchased <- as.factor(surveysub$appname_purchased)
unique(surveysub$appname_purchased)

# Rename apporder and covert to factor 
surveysub$apporder_purchased <- surveysub$apporder

surveysub$apporder_purchased <- as.factor(surveysub$apporder_purchased)
unique(surveysub$apporder_purchased)
class(surveysub$apporder_purchased)

# Logits with App name and order ----

# Model: Purchase_HighU_apps ~ RF:Explore_HighU

filter_highu_apps <- surveysub

# Promotion Rest_HighJ
filter_highu_apps$regulatory_focus <- 
  relevel(factor(filter_highu_apps$regulatory_focus), ref = "Promotion")

hirest_rfhighu_apps <- glm(highu_rest ~
                                  age + 
                                  gender + 
                                  income + 
                                  visit_frequency + app_expense + 
                                  previous_experience + 
                                  regulatory_focus + 
                                  platform_preference + 
                                  involvement + 
                                  appname_purchased +
                                  apporder_purchased +
                                  highU_explored + 
                                  highU_explored * regulatory_focus, 
                                data = filter_highu_apps, 
                                family = binomial)

summary(hirest_rfhighu_apps)

# Summary into a data frame
purchase_highu_apps <- tidy(hirest_rfhighu_apps)

# Round the numeric columns to three decimal places
purchase_highu_apps$estimate <- round(purchase_highu_apps$estimate, 3)
purchase_highu_apps$std.error <- round(purchase_highu_apps$std.error, 3)
purchase_highu_apps$statistic <- round(purchase_highu_apps$statistic, 3)
purchase_highu_apps$p.value <- round(purchase_highu_apps$p.value, 3)

write.xlsx(purchase_highu_apps, file = "temporary_files/highu_apps.xlsx")

# Model: Purchase_HighJ ~ RF:Explore_HighJ

filter_highj_apps <- surveysub

# Promotion Rest_HighJ
filter_highj_apps$regulatory_focus <- 
  relevel(factor(filter_highj_apps$regulatory_focus), ref = "Promotion")

hirest_rfhighj_apps <- glm(highj_rest ~
                             age + 
                             gender + 
                             income + 
                             visit_frequency + app_expense + 
                             previous_experience + 
                             regulatory_focus + 
                             platform_preference + 
                             involvement + 
                             appname_purchased +
                             apporder_purchased +
                             highJ_explored + 
                             highJ_explored * regulatory_focus, 
                           data = filter_highj_apps, 
                           family = binomial)

summary(hirest_rfhighj_apps)

# Summary into a data frame
purchase_highj_apps <- tidy(hirest_rfhighj_apps)

# Round the numeric columns to three decimal places
purchase_highj_apps$estimate <- round(purchase_highj_apps$estimate, 3)
purchase_highj_apps$std.error <- round(purchase_highj_apps$std.error, 3)
purchase_highj_apps$statistic <- round(purchase_highj_apps$statistic, 3)
purchase_highj_apps$p.value <- round(purchase_highj_apps$p.value, 3)

write.xlsx(purchase_highj_apps, file = "temporary_files/highj_apps.xlsx")

# Model: Purchase_LowU ~ RF:Explore_LowU

filter_lowu_apps <- surveysub

# Promotion Rest_LowU
filter_lowu_apps$regulatory_focus <- 
  relevel(factor(filter_lowu_apps$regulatory_focus), ref = "Promotion")

hirest_rflowu_apps <- glm(lowu_rest ~
                            age + 
                            gender + 
                            income + 
                            visit_frequency + app_expense + 
                            previous_experience + 
                            regulatory_focus + 
                            platform_preference + 
                            involvement + 
                            appname_purchased +
                            apporder_purchased +
                            lowU_explored + 
                            lowU_explored * regulatory_focus, 
                          data = filter_lowu_apps, 
                          family = binomial)

summary(hirest_rflowu_apps)

# Summary into a data frame
purchase_lowu_apps <- tidy(hirest_rflowu_apps)

# Round the numeric columns to three decimal places
purchase_lowu_apps$estimate <- round(purchase_lowu_apps$estimate, 3)
purchase_lowu_apps$std.error <- round(purchase_lowu_apps$std.error, 3)
purchase_lowu_apps$statistic <- round(purchase_lowu_apps$statistic, 3)
purchase_lowu_apps$p.value <- round(purchase_lowu_apps$p.value, 3)

write.xlsx(purchase_lowu_apps, file = "temporary_files/lowu_apps.xlsx")

# Model: Purchase_LowJ ~ RF:Explore_LowJ

filter_lowj_apps <- surveysub

# Promotion Rest_LowJ
filter_lowj_apps$regulatory_focus <- 
  relevel(factor(filter_lowj_apps$regulatory_focus), ref = "Promotion")

hirest_rflowj_apps <- glm(lowj_rest ~
                            age + 
                            gender + 
                            income + 
                            visit_frequency + app_expense + 
                            previous_experience + 
                            regulatory_focus + 
                            platform_preference + 
                            involvement + 
                            appname_purchased +
                            apporder_purchased +
                            lowJ_explored + 
                            lowJ_explored * regulatory_focus, 
                          data = filter_lowj_apps, 
                          family = binomial)

summary(hirest_rflowj_apps)


# Summary into a data frame
purchase_lowj_apps <- tidy(hirest_rflowj_apps)

# Round the numeric columns to three decimal places
purchase_lowj_apps$estimate <- round(purchase_lowj_apps$estimate, 3)
purchase_lowj_apps$std.error <- round(purchase_lowj_apps$std.error, 3)
purchase_lowj_apps$statistic <- round(purchase_lowj_apps$statistic, 3)
purchase_lowj_apps$p.value <- round(purchase_lowj_apps$p.value, 3)

write.xlsx(purchase_lowj_apps, file = "temporary_files/lowj_apps.xlsx")




