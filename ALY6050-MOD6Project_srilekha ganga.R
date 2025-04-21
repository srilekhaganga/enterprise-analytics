## Load required packages
library(lpSolve)
library(quadprog)

### PART 1: TRANSSHIPMENT PROBLEM ###

# Define cost matrices
# Direct shipping cost from plants to disposal sites
costs_plants_to_sites <- matrix(c(
  12, 15, 17,
  14, 9, 10,
  13, 20, 11,
  17, 16, 19,
  7, 14, 12,
  22, 16, 18
), nrow = 6, byrow = TRUE)

# Shipping cost between plants
costs_between_plants <- matrix(c(
  0, 3, 4, 9, 5, 4,
  6, 0, 7, 6, 9, 4,
  5, 7, 0, 3, 4, 9,
  5, 4, 3, 0, 3, 11,
  5, 9, 5, 3, 0, 14,
  4, 7, 11, 12, 8, 0
), nrow = 6, byrow = TRUE)

# Shipping cost between disposal sites
costs_between_sites <- matrix(c(
  0, 12, 10,
  12, 0, 15,
  10, 15, 0
), nrow = 3, byrow = TRUE)

# Define supply (waste generated at plants)
supply <- c(45, 26, 42, 53, 29, 38)

# Define demand (capacity at disposal sites)
demand <- c(65, 80, 105)

# Create decision variables for direct shipment, plant-to-plant, and site-to-site shipping
num_plants <- 6
num_sites <- 3
num_variables <- num_plants * num_sites + num_plants * num_plants + num_sites * num_sites

# Objective function: Minimize total transportation cost
objective <- c(as.vector(costs_plants_to_sites), as.vector(costs_between_plants), as.vector(costs_between_sites))

# Constraints matrix
constraints <- matrix(0, nrow = num_plants + num_sites, ncol = num_variables)

# Supply constraints (plants)
for (i in 1:num_plants) {
  constraints[i, ((i - 1) * num_sites + 1):(i * num_sites)] <- 1  # Outbound from plants
}

# Demand constraints (disposal sites)
for (j in 1:num_sites) {
  constraints[num_plants + j, seq(j, num_plants * num_sites, num_sites)] <- 1  # Inbound to disposal sites
}

# Solve the LP problem
result <- lp(
  direction = "min",
  objective.in = objective,
  const.mat = constraints,
  const.dir = c(rep("<=", num_plants), rep("<=", num_sites)),
  const.rhs = c(supply, demand)
)

# Display results
print("Optimal shipping cost:")
print(result$objval)

print("Optimal shipment plan:")
print(matrix(result$solution[1:(num_plants * num_sites)], nrow = num_plants, byrow = TRUE))

###PART 2
# Install and load required packages
if (!require("quadprog")) install.packages("quadprog")
library(quadprog)

# Asset names
assets <- c("Bonds", "HighTech", "Foreign", "CallOpt", "PutOpt", "Gold")

# Expected returns vector (in decimal)
mu <- c(0.07, 0.12, 0.11, 0.14, 0.14, 0.09)

# Covariance matrix (from PDF)
cov_matrix <- matrix(c(
  0.001,   0.0003, -0.0003,  0.00035, -0.00035, 0.0004,
  0.0003,  0.009,   0.0004,  0.0016,  -0.0016,  0.0006,
  -0.0003,  0.0004,  0.008,   0.0015,  -0.0055, -0.0007,
  0.00035, 0.0016,  0.0015,  0.012,   -0.0005,  0.0008,
  -0.00035,-0.0016, -0.0055, -0.0005,  0.012,   -0.0008,
  0.0004,  0.0006, -0.0007,  0.0008,  -0.0008,  0.005
), nrow = 6, byrow = TRUE)

# Function to solve QP for a given target return
solve_qp <- function(target_return) {
  Dmat <- cov_matrix
  dvec <- rep(0, 6)
  
  # Constraints: total = 1, expected return ≥ target, weights ≥ 0
  Amat <- cbind(
    rep(1, 6),       # sum of weights = 1
    mu,              # expected return ≥ target
    diag(6)          # no short-selling (weights ≥ 0)
  )
  
  bvec <- c(1, target_return, rep(0, 6))
  meq <- 1  # First constraint (sum = 1) is equality
  
  result <- solve.QP(Dmat, dvec, Amat, bvec, meq)
  weights <- round(result$solution, 5)
  exp_return <- round(sum(weights * mu), 5)
  risk <- round(result$value, 7)
  
  return(list(weights = weights, return = exp_return, risk = risk))
}

# Run for target return = 11%
res_11 <- solve_qp(0.11)
cat(" Baseline 11% Return Portfolio\n")
cat("Weights:\n")
print(setNames(res_11$weights, assets))
cat("Expected Return:", res_11$return, "\n")
cat("Portfolio Risk (Variance):", res_11$risk, "\n\n")

# Run for multiple targets and store results
targets <- seq(0.10, 0.135, by = 0.005)
risk_return <- data.frame(Target = numeric(), Return = numeric(), Risk = numeric())

for (r in targets) {
  res <- solve_qp(r)
  risk_return <- rbind(risk_return, data.frame(
    Target = r,
    Return = res$return,
    Risk = res$risk
  ))
}

print(" Efficient Frontier (Return vs. Risk):")
print(risk_return)

# Plot Efficient Frontier
plot(risk_return$Risk, risk_return$Return, type = "b", pch = 19,
     xlab = "Risk (Variance)", ylab = "Expected Return",
     main = "Efficient Frontier: Return vs Risk")
grid()
