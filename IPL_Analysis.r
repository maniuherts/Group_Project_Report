# ============================================================================
# IPL Cricket Analysis - Comparison of Means
# Research Question: Is there a difference in the mean of total runs scored
# among different match phases (powerplay, middle overs, death overs)?
# 
# Analysis Type: Comparison of Means (3 groups)
# Visualizations : Histogram & Boxplots
# Statistical Test : pairwise.wilcox.test()
# ===========================================================================

library(readr)

#STEP 1: DATA LOADING

df <- read_csv("deliveries.csv")

cat("Dataset loaded successfully\n")
cat("Total rows (deliveries):", nrow(df), "\n")
cat("Total columns:", ncol(df), "\n")
