
library("readxl")
library("tidyverse")

file1 <- "20160108_raw data.xlsx"
file2 <- "20160109_raw data.xlsx"
sheet_name <- "choicesetting"

compare_cols <- c("task", 
                        "task_appinfo", 
                        "task_apporder",
                        "appinfo_position", 
                        "totalrating_position",
                        "distribution_position")


sheet1 <- read_excel(file1, sheet = sheet_name) %>% select(all_of(compare_cols))
sheet2 <- read_excel(file2, sheet = sheet_name) %>% select(all_of(compare_cols))


comparison_result <- all.equal(sheet1, sheet2)
comparison_result

