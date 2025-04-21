# Load lpSolve
if (!require("lpSolve")) install.packages("lpSolve", dependencies = TRUE)
library(lpSolve)

# Define profit = Selling Price - Cost
# Pressure Washer, Go-Kart, Generator, Water Pump (per case of 5)
profit <- c(169.99, 359.99, 290.99, 714.95)

# Define constraints
costs <- c(330, 370, 410, 635)
space <- c(25, 40, 25, 6.25)
marketing <- c(0.7, 0.7, -0.3, -0.3)
generator_pump <- c(0, 0, -1, 2)

constraint_matrix <- rbind(costs, space, marketing, generator_pump)
directions <- c("<=", "<=", ">=", "<=")
budget <- 170000
warehouse_sqft <- 82 * 30 * 5
rhs <- c(budget, warehouse_sqft, 0, 0)

# 1. Solve for Optimal Inventory
solution <- lp("max", profit, constraint_matrix, directions, rhs, all.int = TRUE)

cat("\n--- Optimal Solution ---\n")
cat("Profit: $", solution$objval, "\n")
cat("Pressure Washers:", solution$solution[1], "\n")
cat("Go-Karts:", solution$solution[2], "\n")
cat("Generators:", solution$solution[3], "\n")
cat("Water Pumps (cases):", solution$solution[4], "\n")

# 2. Minimum Water Pump Price for Non-Zero Value
cat("\n--- Sensitivity: Water Pump Selling Price ---\n")
for (price in seq(270, 400, by = 1)) {
  test_profit <- profit
  test_profit[4] <- (price * 5) - 635
  test_solution <- lp("max", test_profit, constraint_matrix, directions, rhs, all.int = TRUE)
  if (test_solution$solution[4] > 0) {
    cat("Minimum Water Pump Price: $", price, "\n")
    cat("Water Pumps Included (cases):", test_solution$solution[4], "\n")
    break
  }
}

# 3. Sensitivity: Budget Changes
cat("\n--- Sensitivity: Budget ---\n")
budget_vals <- seq(170000, 200000, 5000)
budget_results <- c()
for (b in budget_vals) {
  rhs[1] <- b
  sol <- lp("max", profit, constraint_matrix, directions, rhs, all.int = TRUE)
  budget_results <- c(budget_results, sol$objval)
  cat("Budget: $", b, " → Profit: $", sol$objval, "\n")
}

# 4. Sensitivity: Warehouse Space Changes
cat("\n--- Sensitivity: Warehouse Space ---\n")
space_vals <- seq(12300, 15000, 300)
space_results <- c()
rhs[1] <- 170000  # Reset budget to original
for (s in space_vals) {
  rhs[2] <- s
  sol <- lp("max", profit, constraint_matrix, directions, rhs, all.int = TRUE)
  space_results <- c(space_results, sol$objval)
  cat("Warehouse Space:", s, "sq ft → Profit: $", sol$objval, "\n")
}
