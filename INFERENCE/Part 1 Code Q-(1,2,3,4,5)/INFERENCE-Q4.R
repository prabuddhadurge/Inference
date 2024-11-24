# Load required libraries
library(missMethods)  # For introducing missing data
library(mice)         # For imputation
library(ggplot2)      # For plotting

# Set seed for reproducibility
set.seed(123)

# Generate bivariate dataset
n <- 500
X <- rnorm(n, mean = 0, sd = 1)  # X ~ N(0, 1)
Y <- rnorm(n, mean = 6, sd = 2)  # Y ~ N(6, 4)
data <- data.frame(X, Y)

# Function to introduce missing data mechanisms
delete_data <- function(data, p, mechanism, ctrl_col = NULL) {
  if (mechanism == "MCAR") {
    return(delete_MCAR(data, p = p, cols_mis = "X"))
  } else if (mechanism == "MAR") {
    return(delete_MAR_rank(data, p = p, cols_mis = "X", cols_ctrl = ctrl_col))
  } else if (mechanism == "NMAR") {
    return(delete_MNAR_censoring(data, p = p, cols_mis = "X"))
  }
}

# Function to plot missing data
plot_missing_data <- function(original, missing, title) {
  # Identify missing indices
  missing_indices <- is.na(missing$X)
  
  # Create a combined dataset
  data_combined <- data.frame(
    X = original$X,
    Y = original$Y,
    Status = ifelse(missing_indices, "Missing", "Observed")
  )
  
  # Assign colors
  colors <- c("blue", "red")
  names(colors) <- c("Observed", "Missing")
  
  # Plot using ggplot2
  ggplot(data_combined, aes(x = X, y = Y, color = Status)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = colors) +
    labs(title = title, x = "X", y = "Y") +
    theme_minimal()
}

# Imputation methods
# Mean Imputation
impute_mean <- function(data) {
  data$X[is.na(data$X)] <- mean(data$X, na.rm = TRUE)
  return(data)
}

# PMM using mice
impute_pmm <- function(data) {
  imputed <- mice(data, method = "pmm", m = 1, maxit = 5, print = FALSE)
  return(complete(imputed))
}

# RMSE calculation
rmse <- function(original, imputed, missing_indices) {
  sqrt(mean((original[missing_indices] - imputed[missing_indices])^2, na.rm = TRUE))
}

# Evaluate imputation methods
evaluate_imputation <- function(data_original, data_missing, method) {
  missing_indices <- is.na(data_missing$X)
  imputed_data <- method(data_missing)
  return(rmse(data_original$X, imputed_data$X, missing_indices))
}

# Initialize results dataframe
results <- data.frame(
  Mechanism = character(),
  Missing_Percentage = numeric(),
  RMSE_Mean = numeric(),
  RMSE_PMM = numeric()
)

# Missingness levels and mechanisms
missingness_levels <- c(0.2, 0.3)
mechanisms <- c("MCAR", "MAR", "NMAR")

# Loop over mechanisms and missingness levels
for (mechanism in mechanisms) {
  for (p in missingness_levels) {
    # Create missing data
    if (mechanism == "MAR") {
      data_missing <- delete_data(data, p, mechanism, ctrl_col = "Y")
    } else {
      data_missing <- delete_data(data, p, mechanism)
    }
    
    # Imputation and RMSE
    rmse_mean <- evaluate_imputation(data, data_missing, impute_mean)
    rmse_pmm <- evaluate_imputation(data, data_missing, impute_pmm)
    
    # Append to results
    results <- rbind(
      results,
      data.frame(
        Mechanism = mechanism,
        Missing_Percentage = p * 100,
        RMSE_Mean = rmse_mean,
        RMSE_PMM = rmse_pmm
      )
    )
    
    # Plot missing data
    title <- paste(mechanism, ": ", p * 100, "% Missing X", sep = "")
    print(plot_missing_data(data, data_missing, title))
  }
}

# Print results
print(results)

# Visualize RMSE results for Mean Imputation
ggplot(results, aes(x = factor(Missing_Percentage), y = RMSE_Mean, fill = Mechanism)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "RMSE of Imputation Methods (Mean)", x = "Missing Percentage", y = "RMSE") +
  theme_minimal()

# Visualize RMSE results for PMM
ggplot(results, aes(x = factor(Missing_Percentage), y = RMSE_PMM, fill = Mechanism)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "RMSE of Imputation Methods (PMM)", x = "Missing Percentage", y = "RMSE") +
  theme_minimal()
