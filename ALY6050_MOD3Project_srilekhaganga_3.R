library(readxl)
library(ggplot2)
library(forecast)
library(lmtest) # For Breusch-Pagan test
library(nortest) # For Anderson-Darling normality test

# 1. Data Loading and Cleaning
# ------------------------------------------------------------

# Load the data from the Excel file
# Read the Excel file
stock_data <- read_excel("D:/ALY6050-enterprise analytics/ALY6050_Module3Project_Data.xlsx", sheet = "6050_Module3Project_Data")


# Remove rows with missing values in AAPL, HON, or Date
stock_data <- stock_data[complete.cases(stock_data[, c("AAPL (Apple Inc) / $", "HON (Honeywell Inc)  /  $", "Date")]), ]


# Data Transformation
# Convert 'Date' to Date format (handle potential errors)
stock_data$Date <- as.Date(stock_data$Date)


# 2. Data Visualization (Part 1(i))
# ------------------------------------------------------------

# Time Series Plot
ggplot(stock_data, aes(x = Date)) +
  geom_line(aes(y = `AAPL (Apple Inc) / $`, color = "AAPL")) +
  geom_line(aes(y = `HON (Honeywell Inc)  /  $`, color = "HON")) +
  labs(
    title = "Time Series Plot of AAPL and HON Stock Prices",
    x = "Date",
    y = "Stock Price ($)",
    color = "Stock"
  ) +
  theme_minimal()

# 3. Short-Term Forecasting (Part 1(ii) & (iii))
# ------------------------------------------------------------

# Exponential Smoothing Functions

# (ii) Simple Exponential Smoothing
simple_exp_smoothing <- function(data, alpha) {
  n <- length(data)
  forecasts <- numeric(n)
  forecasts[1] <- data[1] # First forecast is the first actual value
  
  for (t in 2:n) {
    forecasts[t] <- alpha * data[t - 1] + (1 - alpha) * forecasts[t - 1]
  }
  return(forecasts)
}


# Function to calculate MAPD
calculate_mapd <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual)) * 100
}

# (iii) Adjusted Exponential Smoothing (Holt's Linear Trend Method)
adjusted_exp_smoothing <- function(data, alpha, beta) {
  n <- length(data)
  level <- numeric(n)
  trend <- numeric(n)
  forecasts <- numeric(n)
  
  # Initialize level and trend (Naive initialization)
  level[1] <- data[1]
  trend[1] <- 0 # Assume no initial trend
  forecasts[1] <- level[1] + trend[1]
  
  for (t in 2:n) {
    level[t] <- alpha * data[t - 1] + (1 - alpha) * (level[t - 1] + trend[t - 1])
    trend[t] <- beta * (level[t] - level[t - 1]) + (1 - beta) * trend[t - 1]
    forecasts[t] <- level[t] + trend[t]
  }
  return(forecasts)
}

# Function to calculate MAPE
calculate_mape <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual)) * 100
}

# Parameter Tuning and Evaluation (AAPL)
alpha_values <- c(0.15, 0.35, 0.55, 0.75)
beta_values <- c(0.15, 0.25, 0.45, 0.85)

best_alpha_AAPL <- NULL
best_beta_AAPL <- NULL
min_MAPD_AAPL <- Inf
min_MAPE_AAPL <- Inf


# Simple Exponential Smoothing Tuning (AAPL)
for (alpha in alpha_values) {
  forecasts_AAPL <- simple_exp_smoothing(stock_data$`AAPL (Apple Inc) / $`, alpha)
  mapd <- calculate_mapd(stock_data$`AAPL (Apple Inc) / $`[-1], forecasts_AAPL[-length(forecasts_AAPL)])
  if (mapd < min_MAPD_AAPL) {
    min_MAPD_AAPL <- mapd
    best_alpha_AAPL <- alpha
  }
}


# Adjusted Exponential Smoothing Tuning (AAPL)
if (!is.null(best_alpha_AAPL)) {
  for (beta in beta_values) {
    forecasts_AAPL <- adjusted_exp_smoothing(stock_data$`AAPL (Apple Inc) / $`, best_alpha_AAPL, beta)
    mape <- calculate_mape(stock_data$`AAPL (Apple Inc) / $`[-1], forecasts_AAPL[-length(forecasts_AAPL)])
    if (mape < min_MAPE_AAPL) {
      min_MAPE_AAPL <- mape
      best_beta_AAPL <- beta
    }
  }
}


# Parameter Tuning and Evaluation (HON)
best_alpha_HON <- NULL
best_beta_HON <- NULL
min_MAPD_HON <- Inf
min_MAPE_HON <- Inf


# Simple Exponential Smoothing Tuning (HON)
for (alpha in alpha_values) {
  forecasts_HON <- simple_exp_smoothing(stock_data$`HON (Honeywell Inc)  /  $`, alpha)
  mapd <- calculate_mapd(stock_data$`HON (Honeywell Inc)  /  $`[-1], forecasts_HON[-length(forecasts_HON)])
  if (mapd < min_MAPD_HON) {
    min_MAPD_HON <- mapd
    best_alpha_HON <- alpha
  }
}


# Adjusted Exponential Smoothing Tuning (HON)
if (!is.null(best_alpha_HON)) {
  for (beta in beta_values) {
    forecasts_HON <- adjusted_exp_smoothing(stock_data$`HON (Honeywell Inc)  /  $`, best_alpha_HON, beta)
    mape <- calculate_mape(stock_data$`HON (Honeywell Inc)  /  $`[-1], forecasts_HON[-length(forecasts_HON)])
    if (mape < min_MAPE_HON) {
      min_MAPE_HON <- mape
      best_beta_HON <- beta
    }
  }
}


# Results
cat("Best alpha for AAPL:", best_alpha_AAPL, "\n")
cat("Best beta for AAPL:", best_beta_AAPL, "\n")
cat("Best alpha for HON:", best_alpha_HON, "\n")
cat("Best beta for HON:", best_beta_HON, "\n")

# 4. Long-Term Forecasting (Part 2)
# ------------------------------------------------------------

# Weighted Moving Average
weighted_moving_average <- function(data, weights) {
  n <- length(data)
  wma <- numeric(n)
  
  # WMA cannot be calculated for the first two periods
  wma[1] <- NA
  wma[2] <- NA
  
  for (i in 3:n) {
    wma[i] <- sum(weights * data[(i - 2):(i - 1)])
  }
  return(wma)
}

weights <- c(0.2, 0.3, 0.5)

# Calculate WMA
wma_AAPL <- weighted_moving_average(stock_data$`AAPL (Apple Inc) / $`, weights)
wma_HON <- weighted_moving_average(stock_data$`HON (Honeywell Inc)  /  $`, weights)


# Linear Trend Forecasting

# Linear Trend Forecasting
linear_trend_forecast <- function(data, start_period, forecast_length) {
  base_value <- data[start_period]
  time_series <- 1:start_period
  model <- lm(data[1:start_period] ~ time_series)
  trend <- coef(model)["time_series"]
  forecast <- base_value + trend * (1:forecast_length)
  return(forecast)
}


# Forecast beyond the original dataset
forecast_length <- 5 # Number of periods to forecast (253-257)
forecast_AAPL <- linear_trend_forecast(stock_data$`AAPL (Apple Inc) / $`, 252, forecast_length)
forecast_HON <- linear_trend_forecast(stock_data$`HON (Honeywell Inc)  /  $`, 252, forecast_length)

# Combine WMA and Linear Trend forecasts
combined_forecast_AAPL <- c(wma_AAPL[1:252], forecast_AAPL)
combined_forecast_HON <- c(wma_HON[1:252], forecast_HON)

# Print the forecasted values for periods 253-257
cat("Forecasted AAPL values for periods 253-257:", tail(combined_forecast_AAPL, 5), "\n")
cat("Forecasted HON values for periods 253-257:", tail(combined_forecast_HON, 5), "\n")


# Calculate MAPE for the long-term forecast for AAPL and HON
mape_AAPL_longterm <- calculate_mape(stock_data$`AAPL (Apple Inc) / $`[3:252], wma_AAPL[3:252])
mape_HON_longterm <- calculate_mape(stock_data$`HON (Honeywell Inc)  /  $`[3:252], wma_HON[3:252])


cat("MAPE for AAPL (Long-Term):", mape_AAPL_longterm, "\n")
cat("MAPE for HON (Long-Term):", mape_HON_longterm, "\n")


# 5. Regression Analysis (Part 3)
# ------------------------------------------------------------

# Time Series Regression
time_series_regression <- function(data) {
  time <- 1:length(data)
  model <- lm(data ~ time)
  return(model)
}

# Residual Analysis Functions
perform_residual_analysis <- function(model, data) {
  # Extract residuals
  residuals <- residuals(model)
  
  # 1. Independence of Residuals (Durbin-Watson test)
  dw_test_result <- dwtest(model)
  cat("Durbin-Watson test p-value:", dw_test_result$p.value, "\n")
  
  # 2. Homoscedasticity (Breusch-Pagan test)
  bp_test_result <- bptest(model)
  cat("Breusch-Pagan test p-value:", bp_test_result$p.value, "\n")
  
  # 3. Normality of Residuals (Shapiro-Wilk test)
  shapiro_test_result <- shapiro.test(residuals)
  cat("Shapiro-Wilk test p-value:", shapiro_test_result$p.value, "\n")
  
  # 4. Normality of Residuals (Histogram and Q-Q plot)
  par(mfrow = c(1, 2)) # Set up plotting area
  
  hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")
  qqnorm(residuals, main = "Normal Q-Q Plot")
  qqline(residuals)
  
  par(mfrow = c(1, 1)) # Reset plotting area
  
  return(residuals)
}


# Regression and Residual Analysis (AAPL)
model_AAPL <- time_series_regression(stock_data$`AAPL (Apple Inc) / $`)
cat("AAPL Regression Summary:\n")
print(summary(model_AAPL))

cat("\nAAPL Residual Analysis:\n")
residuals_AAPL <- perform_residual_analysis(model_AAPL, stock_data$`AAPL (Apple Inc) / $`)


# Regression and Residual Analysis (HON)
model_HON <- time_series_regression(stock_data$`HON (Honeywell Inc)  /  $`)
cat("HON Regression Summary:\n")
print(summary(model_HON))

cat("\nHON Residual Analysis:\n")
residuals_HON <- perform_residual_analysis(model_HON, stock_data$`HON (Honeywell Inc)  /  $`)

library(tseries)
library(quadprog)  # For quadratic optimization
library(dplyr)

# 1. Calculate Daily Returns Correctly
stock_data <- stock_data %>%
  arrange(Date) %>%  # Ensure data is sorted by Date
  mutate(
    AAPL_Returns = (`AAPL (Apple Inc) / $` - lag(`AAPL (Apple Inc) / $`)) / lag(`AAPL (Apple Inc) / $`),
    HON_Returns = (`HON (Honeywell Inc)  /  $` - lag(`HON (Honeywell Inc)  /  $`)) / lag(`HON (Honeywell Inc)  /  $`)
  ) 

# Remove NA values using na.omit() instead of drop_na()
stock_data <- na.omit(stock_data)

# 2. Compute Expected Returns and Covariance Matrix
mean_returns <- colMeans(stock_data[, c("AAPL_Returns", "HON_Returns")])
cov_matrix <- cov(stock_data[, c("AAPL_Returns", "HON_Returns")])

# 3. Define Portfolio Optimization Function
optimize_portfolio <- function(expected_returns, cov_matrix) {
  n_assets <- length(expected_returns)
  Dmat <- 2 * cov_matrix  # Quadratic part of the objective function
  dvec <- rep(0, n_assets)  # Linear part of the objective function
  Amat <- cbind(rep(1, n_assets), diag(n_assets))  # Constraints (sum to 1, non-negative)
  bvec <- c(1, rep(0, n_assets))  # Constraint values
  
  # Solve quadratic optimization problem
  result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  return(result$solution)
}

# 4. Optimize Portfolio Weights (P and Q)
optimal_weights <- optimize_portfolio(mean_returns, cov_matrix)

# Extract P and Q
P_optimal <- optimal_weights[1] * 100  # Convert to percentage
Q_optimal <- optimal_weights[2] * 100  # Convert to percentage

# Print Results
cat("Optimal Portfolio Allocation:\n")
cat("Percentage of Investment in AAPL (P):", round(P_optimal, 2), "%\n")
cat("Percentage of Investment in HON (Q):", round(Q_optimal, 2), "%\n")
