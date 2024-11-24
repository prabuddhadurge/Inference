# QUESTION 2


library(ggplot2)

# Load the dataset from the CSV file
data <- read.csv("/Users/prabuddhadurge/Downloads/adult_data.csv")

X <- data$occupation.num                            # Discrete variable
Y <- data$age                                       # Continuous variable

# (2a) Frequency distribution for discrete variable X (e.g., occupation.num)
frequency_distribution <- table(X)
print(frequency_distribution)

# (2b) Plot the histogram for continuous variable Y (e.g., age)

# Calculate bin width using Freedman-Diaconis rule
iqr <- IQR(Y)                                       # Interquartile range
bin_width <- 2 * iqr / (length(Y)^(1/3))            # Freedman-Diaconis rule
num_bins <- ceiling((max(Y) - min(Y)) / bin_width)  # Number of bins based on bin width

# Plot the histogram
hist(Y, breaks = num_bins, col = "lightblue", 
     main = "Histogram of Age",
     xlab = "Age",
     ylab = "Frequency")

# (2c) Box-and-Whisker Plot for X and Y
par(mfrow = c(1, 1))

# Boxplot for continuous variable Y
boxplot(Y, main = "Boxplot of Age", col = "lightgreen")

#Boxplot for continuous variable X
boxplot(X, main = "Boxplot of Occupation-num", col = "lightpink")

# Boxplot for continuous variable Y by discrete variable X
boxplot(Y~X, data = data, main = "Boxplot of Age by Occupation", 
        col = c("lightblue", "lightpink", "lightyellow", "lightgreen"))




# QUESTION 3

# (3abc) Generate the 7 datasets
set.seed(123)  # For reproducibility

# Simulated datasets
poisson_data <- rpois(500, lambda = 5)                  # Poisson distribution
uniform_data <- runif(500, min = 3, max = 8)            # Continuous uniform distribution
exponential_data <- rexp(500, rate = 7)                 # Exponential distribution
beta_data <- rbeta(500, shape1 = 2, shape2 = 3)         # Beta distribution
log_normal_data <- rlnorm(500, meanlog = 0, sdlog = 1)  # Log-normal distribution

datasets <- list(
  "Discrete X" = X,
  "Continuous Y" = Y,
  "Poisson(λ=5)" = poisson_data,
  "Uniform[3,8]" = uniform_data,
  "Exponential(λ=7)" = exponential_data,
  "Beta(α=2, β=3)" = beta_data,
  "Log-Normal(μ=0, σ=1)" = log_normal_data
)

# Draw 100 samples and compute means for n = 10, 50, 100
compute_sample_means <- function(data, n) {
  replicate(100, mean(sample(data, size = n, replace = TRUE)))
}

# Compute histograms
plot_histograms <- function(datasets, sample_sizes) {
  par(mfrow = c(7, 4), mar = c(2, 2, 2, 1))             # 7 rows, 4 columns
  
  for (i in 1:length(datasets)) {
    data <- datasets[[i]]
    
    # Plot histogram of the original dataset
    hist(data, main = paste("Original:", names(datasets)[i]), col = "lightblue",
         xlab = "Values", ylab = "Frequency", breaks = 20)
    
    # Compute and plot histograms for sample means
    for (n in sample_sizes) {
      sample_means <- compute_sample_means(data, n)
      hist(sample_means, main = paste("n =", n), col = "lightgreen",
           xlab = "Sample Mean", ylab = "Frequency", breaks = 20)
    }
  }
}

# Run the plotting function for sample sizes n = 10, 50, 100
plot_histograms(datasets, sample_sizes = c(10, 50, 100))

