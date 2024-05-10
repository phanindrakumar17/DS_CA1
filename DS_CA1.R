install.packages("stats")
library(stats)

install.packages("graphics")
library(graphics)

#dataframe
quality_data <- data.frame(
  Student = 1:17,
  No_Visual_Aids = c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61),
  With_Visual_Aids = c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)
)
quality_data


#boxplot
windows(20,12)
boxplot(quality_data$No_Visual_Aids, quality_data$With_Visual_Aids, 
        names = c("No Visual Aids", "With Visual Aids"),
        main = "Quality Scores with and without Visual Aids",
        xlab = "Visual Aids",
        ylab = "Quality Score")


shapiro.test (quality_data$No_Visual_Aids)

shapiro.test (quality_data$With_Visual_Aids)

mean_no_visual <- mean(quality_data$No_Visual_Aids)
median_no_visual <- median(quality_data$No_Visual_Aids)
sd_no_visual <- sd(quality_data$No_Visual_Aids)
skew_no_visual <- sum((quality_data$No_Visual_Aids - mean_no_visual)^3) / (length(quality_data$No_Visual_Aids) * sd_no_visual^3)

mean_with_visual <- mean(quality_data$With_Visual_Aids)
median_with_visual <- median(quality_data$With_Visual_Aids)
sd_with_visual <- sd(quality_data$With_Visual_Aids)
skew_with_visual <- sum((quality_data$With_Visual_Aids - mean_with_visual)^3) / (length(quality_data$With_Visual_Aids) * sd_with_visual^3)

cat("Mean (No Visual Aids):", mean_no_visual, "\n")
cat("Median (No Visual Aids):", median_no_visual, "\n")
cat("Standard Deviation:", sd_no_visual,"\n")
cat("Skewness (No Visual Aids):", skew_no_visual, "\n")
cat("\nMean (With Visual Aids):", mean_with_visual, "\n")
cat("Median (With Visual Aids):", median_with_visual, "\n")
cat("Standard Deviation:", sd_with_visual,"\n")
cat("Skewness (With Visual Aids):", skew_with_visual, "\n")

mean_diff <- mean(quality_data$With_Visual_Aids- quality_data$No_Visual_Aids)
sd_diff <- sd(quality_data$With_Visual_Aids - quality_data$No_Visual_Aids)

# confidence interval for the difference
n <- nrow(quality_data)
t_value <- qt(0.975, df = n - 1)
ci_diff <- t_value * (sd_diff / sqrt(n))

cat("Mean Difference:", mean_diff, "\n")
cat("Standard Deviation of Difference:", sd_diff, "\n")
cat("95% Confidence Interval for the Difference:", mean_diff - ci_diff, "-", mean_diff + ci_diff, "\n")

#testing
t_test <- t.test(quality_data$With_Visual_Aids, quality_data$No_Visual_Aids, paired = TRUE)

t_test



