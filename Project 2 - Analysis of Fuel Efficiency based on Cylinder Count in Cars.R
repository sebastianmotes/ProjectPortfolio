# Analysis of Fuel Efficiency based on Cylinder Count in Cars

# Load necessary libraries
library(nortest)
library(ggplot2)
library(car)

# 1. Checking for Normality
# a. Anderson-Darling test
ad_test_results <- tapply(mtcars$mpg, mtcars$cyl, ad.test)
# N/A because sample size <= 7

# b. Shapiro-Wilks test
sw_test_results <- tapply(mtcars$mpg, mtcars$cyl, shapiro.test)

# c. Q-Q plots
ggplot(mtcars, aes(sample = mpg)) +
  facet_wrap(~cyl) +
  stat_qq() +
  stat_qq_line(color = "blue")

# d. Boxplots for mpg against cylinders
boxplot(mpg ~ cyl, data = mtcars)

# 2. Checking for Equal Variances
bartlett_results <- bartlett.test(mpg ~ cyl, data = mtcars)

# 3. Data Transformation: Natural Log
transmpg <- log(mtcars$mpg)

# 4. Checking for Normality of Transformed Data
# a. Anderson-Darling test
ad_trans_test_results <- tapply(transmpg, mtcars$cyl, ad.test)
# N/A because sample size <= 7

# b. Shapiro-Wilks test
sw_trans_test_results <- tapply(transmpg, mtcars$cyl, shapiro.test)

# c. Q-Q plots for transformed data
cyl_levels <- unique(mtcars$cyl)
for (cyl in cyl_levels) {
  qqPlot(transmpg[mtcars$cyl == cyl], main = paste("Q-Q plot for", cyl, "cylinders"))
}

# d. Boxplots for transformed mpg against cylinders
boxplot(transmpg ~ mtcars$cyl)

# e. Checking for Equal Variances with Transformed Data
bartlett_trans_results <- bartlett.test(transmpg ~ mtcars$cyl)

# 5. ANOVA on Transformed Data
out_trans <- aov(transmpg ~ mtcars$cyl)
summary_trans <- summary(out_trans)

# 6. Confidence Intervals for Transformed Data
mtcars$cyl <- as.factor(mtcars$cyl)
bon <- 1 - (1 - .95)/3
ci_trans <- confint(out_trans, level = bon)
mean_transmpg <- tapply(transmpg, mtcars$cyl, mean)

# 7. Kruskal-Wallis Test
kruskal_results <- kruskal.test(mpg ~ cyl, data = mtcars)
