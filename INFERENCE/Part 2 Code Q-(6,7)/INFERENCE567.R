# QUESTION 5


library(fitdistrplus)

data <- read.csv("/Users/prabuddhadurge/Downloads/adult_data.csv")
head(data)

A <- data$occupation.num   # Discrete variable
B <- data$education.num   # Discrete variable
C <- data$age              # Continuous variable
D <- data$hours.per.week   # Continuous variable

# A <- occupation.num (discrete), B <- education.num (discrete), C <- age (continuous), D <- hours.per.week (continuous)

# Step 1: Dataset A - Discrete Variable: occupation.num
A <- data$occupation.num

# (5a) Compute the median of the data.
a <- median(A, na.rm = TRUE)  # Compute the median of occupation.num
cat("Median of occupation.num (A):", a, "\n")

# (5b) Select a simple random sample of size 100 from the dataset.
set.seed(123)  # For reproducibility
sample_A <- sample(A, size = 100, replace = TRUE)

# (5c) Count the number of observations in the sample that are less than 'a'.
Z <- sum(sample_A <= a)  # Count how many values are less than or equal to the median
cat("Number of observations in the sample less than or equal to median:", Z, "\n")

# (5d) Test the hypothesis that p = P[X ≤ a] is larger than 0.5 at the 5% significance level.
p0 <- 0.5  # Null hypothesis: p = 0.5
p_hat <- Z / 100  # Proportion of observations less than or equal to median

# Perform the binomial test
test_result <- binom.test(Z, 100, p = p0, alternative = "greater", conf.level = 0.95)
cat("Hypothesis test result for occupation.num:\n")
print(test_result)

# (5e) Provide an approximate 95% confidence interval for p.
prop_test <- prop.test(Z, 100, conf.level = 0.95)
cat("95% Confidence Interval for p (occupation.num):\n")
print(prop_test$conf.int)



# QUESTION 6

B <- data$education.num
B <- sample(B, size = 3000)

# (a) Fit a normal distribution to dataset B
fit_b <- fitdist(B, "norm")
cat("Fitted Normal Distribution for education.num(B):\n")
print(fit_b)

# (b) Conduct a Shapiro-Wilk test for normality
shapiro_test_b <- shapiro.test(B)
cat("Shapiro-Wilk Test for Normality education.num(B):\n")
print(shapiro_test_b)

set.seed(123)
data <- data.frame(education.num = rnorm(500, mean = 12, sd = 2))  # Simulated data

# Fit a normal distribution to dataset B (education.num)
fit <- fitdist(data$education.num, "norm")

# Print the summary of the fitted distribution
cat("Summary of the fitted normal distribution:\n")
summary(fit)

# Visualize the fitted distribution
plot(fit)
# Conduct a goodness-of-fit test
gof_results <- gofstat(fit)

# Print the goodness-of-fit test results
cat("Goodness-of-Fit Test Results:\n")
print(gof_results)




# QUESTION 7

B <- data$education.num

# (a) Divide dataset B into two subsets B1 and B2 (3:2 ratio)
set.seed(123)
sample_indices_B <- sample(1:length(B), size = 0.6 * length(B))  # 60% for B1
B1 <- B[sample_indices_B]
B2 <- B[-sample_indices_B]  # Remaining 40% for B2

# (b) Levene's test for equality of variances between B1 and B2
library(car)
levene_test_B <- leveneTest(c(B1, B2) ~ factor(c(rep(1, length(B1)), rep(2, length(B2)))))
cat("Levene's Test for Equality of Variances between B1 and B2 (hours.per.week):\n")
print(levene_test_B)

# (c) t-test for equality of means between B1 and B2
t_test_result_B <- t.test(B1, B2)
cat("t-test for Equality of Means between B1 and B2 (hours.per.week):\n")
print(t_test_result_B)

# (d) Confidence Interval for the difference of means (99% CI)
conf_interval_B <- t.test(B1, B2)$conf.int
cat("99% Confidence Interval for the Difference in Means (hours.per.week):\n")
print(conf_interval_B)
