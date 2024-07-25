# Generate and save figures

source("visualizations.R")

# Save the plots as a PDF ----

# Average Purchase Dodged Bar chart - Figure 1
ggsave("Average Purchase Barchart.pdf", plot = avg_purch_highu_plot, width = 8, height = 6)

# 
