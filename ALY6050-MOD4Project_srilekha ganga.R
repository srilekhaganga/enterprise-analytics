#part-1
# Given Data
D <- 15000   # Annual demand
C <- 80      # Unit cost
h <- 0.18 * C  # Holding cost per unit per year
S <- 220     # Ordering cost per order

# Define a sequence of Q values
Q <- seq(100, 5000, by=50)

# Compute Annual Holding Cost
H <- (Q / 2) * h

# Compute Annual Ordering Cost
O <- (D / Q) * S

# Compute Total Cost
Total_Cost <- H + O

# Find the order quantity that minimizes total cost
optimal_index <- which.min(Total_Cost)
optimal_Q <- Q[optimal_index]
optimal_cost <- Total_Cost[optimal_index]

# Print results
cat("Optimal Order Quantity:", optimal_Q, "\n")
cat("Minimum Total Cost:", optimal_cost, "\n")

# Plot Total Cost vs. Order Quantity using Base R Plotting
plot(Q, Total_Cost, type="l", col="blue", lwd=2,
     main="Total Cost vs Order Quantity",
     xlab="Order Quantity (Q)", ylab="Total Cost")

# Highlight Optimal Order Quantity
points(optimal_Q, optimal_cost, col="red", pch=19, cex=1.5)
text(optimal_Q, optimal_cost, labels=paste("Q* =", optimal_Q), pos=4, col="red")


#part-2
# Set seed for reproducibility
set.seed(123)

# Number of Monte Carlo simulations
n_sim <- 1000

# Given parameters
C <- 80        # Unit cost
S <- 220       # Ordering cost per order
h_rate <- 0.18 # Holding cost rate
h <- h_rate * C  # Holding cost per unit per year

# Function to generate triangular distribution without using packages
rtriangle_manual <- function(n, a, b, c) {
  U <- runif(n)  # Generate uniform random numbers
  F_c <- (c - a) / (b - a)  # Cumulative probability at mode
  
  # Compute triangular random variables
  X <- ifelse(U < F_c,
              a + sqrt(U * (b - a) * (c - a)),  # Left side of triangle
              b - sqrt((1 - U) * (b - a) * (b - c)))  # Right side of triangle
  return(X)
}

# Generate demand values using manual triangular function
D_values <- rtriangle_manual(n_sim, 13000, 17000, 15000)

# Function to calculate optimal order quantity and total cost
calculate_optimal_Q <- function(D) {
  Q_values <- seq(100, 5000, by=50)  # Range of possible order quantities
  
  # Compute Costs
  holding_costs <- (Q_values / 2) * h
  ordering_costs <- (D / Q_values) * S
  total_costs <- holding_costs + ordering_costs
  
  # Find the optimal Q minimizing total cost
  min_index <- which.min(total_costs)
  optimal_Q <- Q_values[min_index]
  optimal_cost <- total_costs[min_index]
  
  return(c(optimal_Q, optimal_cost, D / optimal_Q))
}

# Apply function to all demand values
results <- t(sapply(D_values, calculate_optimal_Q))
colnames(results) <- c("Optimal_Order_Q", "Minimum_Total_Cost", "Orders_Per_Year")

# Convert to Data Frame
df_results <- as.data.frame(results)

# Function to compute 95% Confidence Intervals manually
CI_95 <- function(x) {
  mean_x <- mean(x)
  std_x <- sd(x)
  error_margin <- 1.96 * (std_x / sqrt(n_sim))  # 1.96 for 95% CI
  return(c(mean_x - error_margin, mean_x + error_margin))
}

# Compute 95% Confidence Intervals
CI_Q <- CI_95(df_results$Optimal_Order_Q)
CI_Cost <- CI_95(df_results$Minimum_Total_Cost)
CI_Orders <- CI_95(df_results$Orders_Per_Year)

# Print results
cat("95% Confidence Interval for Optimal Order Quantity:", CI_Q, "\n")
cat("95% Confidence Interval for Minimum Total Cost:", CI_Cost, "\n")
cat("95% Confidence Interval for Orders Per Year:", CI_Orders, "\n")

# Simple histogram visualization (Without ggplot2)
hist(df_results$Optimal_Order_Q, breaks=30, col="blue", main="Histogram of Optimal Order Quantity", xlab="Order Quantity")
hist(df_results$Minimum_Total_Cost, breaks=30, col="green", main="Histogram of Minimum Total Cost", xlab="Total Cost")
hist(df_results$Orders_Per_Year, breaks=30, col="purple", main="Histogram of Orders Per Year", xlab="Orders Per Year")


