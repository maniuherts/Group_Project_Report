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

#STEP 4: PRIMARY VISUALIZATION - BOXPLOT

cat("Creating: boxplot_phase_comparison.png\n")
cat("Purpose: Compare run distributions across all 3 phases\n")

png("boxplot_phase_comparison.png", width=1000, height=700)

boxplot(df$total_runs ~ df$phase,
        main="Comparison of Total Runs Across Match Phases",
        ylab="Total Runs per Delivery",
        xlab="Match Phase",
        col=c("steelblue", "coral", "darkgreen"),
        border="black",
        cex.main=1.3,
        cex.lab=1.2,
        names=c("Powerplay\n(Overs 1-6)", 
                "Middle\n(Overs 7-15)", 
                "Death\n(Overs 16-20)"))

# Added grid for easier reading
grid(NA, NULL, lty=2, col="gray70")

dev.off()

cat("File created: boxplot_phase_comparison.png\n")
cat("Shows: Median, quartiles, outliers for each phase\n")

#STEP 5: SECONDARY VISUALIZATION - HISTOGRAM WITH NORMAL CURVE

cat("Creating: histogram_total_runs_normal.png\n")
cat("Purpose: Assess normality of dependent variable (total runs)\n")

all_runs <- df$total_runs[!is.na(df$total_runs)]

png("histogram_total_runs_normal.png", width=900, height=600)

hist(all_runs,
     main="Distribution of Total Runs per Delivery\n(Dependent Variable - All Phases Combined)",
     xlab="Total Runs per Delivery",
     ylab="Frequency (Density)",
     col="steelblue",
     border="white",
     breaks=30,
     cex.main=1.3,
     cex.lab=1.2,
     cex.axis=1.1,
     prob=TRUE)

# Calculating mean and standard deviation
mean_runs <- mean(all_runs, na.rm=TRUE)
sd_runs <- sd(all_runs, na.rm=TRUE)

# Generating normal distribution curve
x <- seq(min(all_runs), max(all_runs), length=100)
y <- dnorm(x, mean=mean_runs, sd=sd_runs)

# Overlay BLUE normal curve
lines(x, y, col="blue", lwd=2.5, lty=1)

# Added RED mean line
abline(v=mean_runs, col="red", lwd=2, lty=2)

# Adding legend
legend("topright", 
       legend=c(paste("Mean =", round(mean_runs, 3)),
                paste("SD =", round(sd_runs, 3)),
                "Normal Curve (Blue)"),
       col=c("red", "black", "blue"),
       lty=c(2, 0, 1),
       lwd=c(2, 0, 2.5),
       cex=1.1,
       box.lwd=1.5)

dev.off()

cat("File created: histogram_total_runs_normal.png\n")
cat("Shows: Distribution pattern + normal curve overlay\n")
cat("Purpose: Visualize deviation from normality\n")

#STEP 6: OPTIONAL - PHASE-SPECIFIC HISTOGRAMS

# Powerplay Histogram
cat("Creating: histogram_powerplay_normal.png\n")
png("histogram_powerplay_normal.png", width=900, height=600)

hist(powerplay_runs,
     main="Powerplay Overs (1-6)\nDistribution of Total Runs per Delivery",
     xlab="Total Runs per Delivery",
     ylab="Frequency (Density)",
     col="steelblue",
     border="white",
     breaks=20,
     cex.main=1.3,
     cex.lab=1.2,
     prob=TRUE)

mean_pp <- mean(powerplay_runs, na.rm=TRUE)
sd_pp <- sd(powerplay_runs, na.rm=TRUE)
x_pp <- seq(min(powerplay_runs), max(powerplay_runs), length=100)
y_pp <- dnorm(x_pp, mean=mean_pp, sd=sd_pp)
lines(x_pp, y_pp, col="blue", lwd=2.5)
abline(v=mean_pp, col="red", lwd=2, lty=2)
legend("topright", 
       legend=c(paste("Mean =", round(mean_pp, 3)),
                paste("SD =", round(sd_pp, 3))),
       col=c("red", "black"),
       lty=c(2, 0),
       cex=1.1,
       box.lwd=1.5)

dev.off()
cat("Created\n")

# Middle Overs Histogram
cat("Creating: histogram_middle_normal.png\n")
png("histogram_middle_normal.png", width=900, height=600)

hist(middle_runs,
     main="Middle Overs (7-15)\nDistribution of Total Runs per Delivery",
     xlab="Total Runs per Delivery",
     ylab="Frequency (Density)",
     col="coral",
     border="white",
     breaks=20,
     cex.main=1.3,
     cex.lab=1.2,
     prob=TRUE)

mean_mid <- mean(middle_runs, na.rm=TRUE)
sd_mid <- sd(middle_runs, na.rm=TRUE)
x_mid <- seq(min(middle_runs), max(middle_runs), length=100)
y_mid <- dnorm(x_mid, mean=mean_mid, sd=sd_mid)
lines(x_mid, y_mid, col="blue", lwd=2.5)
abline(v=mean_mid, col="red", lwd=2, lty=2)
legend("topright", 
       legend=c(paste("Mean =", round(mean_mid, 3)),
                paste("SD =", round(sd_mid, 3))),
       col=c("red", "black"),
       lty=c(2, 0),
       cex=1.1,
       box.lwd=1.5)

dev.off()
cat("Created\n")

# Death Overs Histogram
cat("Creating: histogram_death_normal.png\n")
png("histogram_death_normal.png", width=900, height=600)

hist(death_runs,
     main="Death Overs (16-20)\nDistribution of Total Runs per Delivery",
     xlab="Total Runs per Delivery",
     ylab="Frequency (Density)",
     col="darkgreen",
     border="white",
     breaks=20,
     cex.main=1.3,
     cex.lab=1.2,
     prob=TRUE)

mean_death <- mean(death_runs, na.rm=TRUE)
sd_death <- sd(death_runs, na.rm=TRUE)
x_death <- seq(min(death_runs), max(death_runs), length=100)
y_death <- dnorm(x_death, mean=mean_death, sd=sd_death)
lines(x_death, y_death, col="blue", lwd=2.5)
abline(v=mean_death, col="red", lwd=2, lty=2)
legend("topright", 
       legend=c(paste("Mean =", round(mean_death, 3)),
                paste("SD =", round(sd_death, 3))),
       col=c("red", "black"),
       lty=c(2, 0),
       cex=1.1,
       box.lwd=1.5)

dev.off()
cat("Created\n")

save(df, powerplay_runs, middle_runs, death_runs, 
     file="prepared_data.RData")

# Loading the prepared data
load("prepared_data.RData")

cat(" Data loaded successfully\n")
cat("Total observations:", nrow(df), "\n")
cat(" Phases:", paste(levels(df$phase), collapse=", "), "\n")

#PERFORMING pairwise.wilcox.test()

cat("Executing: pairwise.wilcox.test(df$total_runs, df$phase, p.adjust.method='BH')\n\n")

wilcox_result <- pairwise.wilcox.test(df$total_runs,     # Dependent variable
                                      df$phase,           # Independent variable
                                      p.adjust.method="BH") # Adjust for multiple comparisons

# Displaying results
cat("TEST RESULTS:\n")
print(wilcox_result)

cat("\n\nP-VALUE MATRIX:\n")
pvalue_matrix <- wilcox_result$p.value
print(pvalue_matrix)


