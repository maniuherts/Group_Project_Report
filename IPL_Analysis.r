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

# STEP 2: PHASE CATEGORIZATION 

# Creating phase categories based on overs
df$phase <- factor(cut(df$over, 
                       breaks = c(0, 6, 15, 20),
                       labels = c("Powerplay", "Middle", "Death"),
                       include.lowest = TRUE))

cat("Phases created:\n")
cat("  Powerplay: Overs 1-6 \n")
cat("  Middle: Overs 7-15 \n")
cat("  Death: Overs 16-20\n")

cat("Phase distribution:\n")
print(table(df$phase))

# Create separate vectors for each phase
powerplay_runs <- df$total_runs[df$phase == "Powerplay"]
middle_runs <- df$total_runs[df$phase == "Middle"]
death_runs <- df$total_runs[df$phase == "Death"]

#STEP 3: DESCRIPTIVE STATISTICS

cat("\n POWERPLAY (Overs 1-6):\n")
cat("  Mean:              ", round(mean(powerplay_runs, na.rm=TRUE), 4), "\n")
cat("  Median:            ", median(powerplay_runs, na.rm=TRUE), "\n")
cat("  Standard Dev:      ", round(sd(powerplay_runs, na.rm=TRUE), 4), "\n")
cat("  IQR:               ", IQR(powerplay_runs, na.rm=TRUE), "\n")
cat("  Sample size (n):   ", length(powerplay_runs), "\n")

cat("\n MIDDLE OVERS (Overs 7-15):\n")
cat("  Mean:              ", round(mean(middle_runs, na.rm=TRUE), 4), "\n")
cat("  Median:            ", median(middle_runs, na.rm=TRUE), "\n")
cat("  Standard Dev:      ", round(sd(middle_runs, na.rm=TRUE), 4), "\n")
cat("  IQR:               ", IQR(middle_runs, na.rm=TRUE), "\n")
cat("  Sample size (n):   ", length(middle_runs), "\n")

cat("\n DEATH OVERS (Overs 16-20):\n")
cat("  Mean:              ", round(mean(death_runs, na.rm=TRUE), 4), "\n")
cat("  Median:            ", median(death_runs, na.rm=TRUE), "\n")
cat("  Standard Dev:      ", round(sd(death_runs, na.rm=TRUE), 4), "\n")
cat("  IQR:               ", IQR(death_runs, na.rm=TRUE), "\n")
cat("  Sample size (n):   ", length(death_runs), "\n")

