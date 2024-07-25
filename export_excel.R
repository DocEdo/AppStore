# Export to excel

source("meeting_models.R")


# Main model ----

# Summary into a data frame
purchase_summary <- tidy(hirest_rfhighu_prom)

# Round the numeric columns to three decimal places
purchase_summary$estimate <- round(purchase_summary$estimate, 3)
purchase_summary$std.error <- round(purchase_summary$std.error, 3)
purchase_summary$statistic <- round(purchase_summary$statistic, 3)
purchase_summary$p.value <- round(purchase_summary$p.value, 3)

write.xlsx(purchase_summary, file = "purchase_model.xlsx")


# Summary into a data frame
purchasebase_summary <- tidy(hirest_highuexplored)

# Round the numeric columns to three decimal places
purchasebase_summary$estimate <- round(purchasebase_summary$estimate, 3)
purchasebase_summary$std.error <- round(purchasebase_summary$std.error, 3)
purchasebase_summary$statistic <- round(purchasebase_summary$statistic, 3)
purchasebase_summary$p.value <- round(purchasebase_summary$p.value, 3)

write.xlsx(purchasebase_summary, file = "purchasebase_model.xlsx")


