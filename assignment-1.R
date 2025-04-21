#part-1
# Set probabilities for Red Sox winning games
p_boston <- 0.6
p_ny <- 0.43

# Calculate probability that Red Sox win the series
prob_win_2_games <- p_boston * p_ny  # Win-Win
prob_win_3_games_WLW <- p_boston * (1 - p_ny) * p_boston  # Win-Loss-Win
prob_win_3_games_LWW <- (1 - p_boston) * p_ny * p_boston  # Loss-Win-Win
prob_redsox_win <- prob_win_2_games + prob_win_3_games_WLW + prob_win_3_games_LWW

# Calculate probability that Yankees win the series
prob_loss_2_games <- (1 - p_boston) * (1 - p_ny)  # Loss-Loss
prob_loss_3_games_LWL <- (1 - p_boston) * p_ny * (1 - p_boston)  # Loss-Win-Loss
prob_loss_3_games_WLL <- p_boston * (1 - p_ny) * (1 - p_boston)  # Win-Loss-Loss
prob_yankees_win <- prob_loss_2_games + prob_loss_3_games_LWL + prob_loss_3_games_WLL

# Net winnings for each scenario
net_win_2_games <- 1000
net_win_3_games <- 480
net_loss_2_games <- -1040
net_loss_3_games <- -540

# Probability distribution table
net_winnings <- c(net_win_2_games, net_win_3_games, net_win_3_games, 
                  net_loss_2_games, net_loss_3_games, net_loss_3_games)
probabilities <- c(prob_win_2_games, prob_win_3_games_WLW, prob_win_3_games_LWW,
                   prob_loss_2_games, prob_loss_3_games_LWL, prob_loss_3_games_WLL)

df_prob <- data.frame(Net_Winnings = net_winnings, Probability = probabilities)

# Expected value and standard deviation
E_X <- sum(df_prob$Net_Winnings * df_prob$Probability)
Var_X <- sum((df_prob$Net_Winnings - E_X)^2 * df_prob$Probability)
SD_X <- sqrt(Var_X)

# Simulate 10,000 random values for X
set.seed(42)
simulated_Y <- sample(df_prob$Net_Winnings, size = 10000, replace = TRUE, prob = df_prob$Probability)

# Estimate mean and 95% confidence interval for Y
mean_Y <- mean(simulated_Y)
std_Y <- sd(simulated_Y)
ci_lower <- mean_Y - qt(0.975, df=9999) * (std_Y / sqrt(10000))
ci_upper <- mean_Y + qt(0.975, df=9999) * (std_Y / sqrt(10000))

# Frequency distribution for Y
simulated_freq <- table(simulated_Y) / length(simulated_Y)
df_simulated <- data.frame(Net_Winnings = as.numeric(names(simulated_freq)),
                           Simulated_Probability = as.numeric(simulated_freq))

# Merge with theoretical probabilities
df_comparison <- merge(df_prob, df_simulated, by = "Net_Winnings", all.x = TRUE)
df_comparison[is.na(df_comparison)] <- 0  # Replace NA with 0

# Chi-squared goodness of fit test
chisq_test <- chisq.test(df_comparison$Simulated_Probability, p = df_comparison$Probability)

# Display results
list(
  "Probability Red Sox Win" = prob_redsox_win,
  "Expected Net Win (E(X))" = E_X,
  "Standard Deviation of X" = SD_X,
  "Simulated Mean (E(Y))" = mean_Y,
  "95% Confidence Interval" = c(ci_lower, ci_upper),
  "Chi-Squared Test Statistic" = chisq_test$statistic,
  "Chi-Squared p-value" = chisq_test$p.value
)


#part-2
# Set probabilities for Red Sox winning games
p_ny <- 0.43
p_boston <- 0.6

# Calculate probability that Red Sox win the series
prob_win_2_games <- p_ny * p_boston  # Win-Win
prob_win_3_games_WLW <- p_ny * (1 - p_boston) * p_ny  # Win-Loss-Win
prob_win_3_games_LWW <- (1 - p_ny) * p_boston * p_ny  # Loss-Win-Win
prob_redsox_win <- prob_win_2_games + prob_win_3_games_WLW + prob_win_3_games_LWW

# Calculate probability that Yankees win the series
prob_loss_2_games <- (1 - p_ny) * (1 - p_boston)  # Loss-Loss
prob_loss_3_games_LWL <- (1 - p_ny) * p_boston * (1 - p_ny)  # Loss-Win-Loss
prob_loss_3_games_WLL <- p_ny * (1 - p_boston) * (1 - p_ny)  # Win-Loss-Loss
prob_yankees_win <- prob_loss_2_games + prob_loss_3_games_LWL + prob_loss_3_games_WLL

# Net winnings for each scenario
net_win_2_games <- 1000
net_win_3_games <- 480
net_loss_2_games <- -1040
net_loss_3_games <- -540

# Probability distribution table
net_winnings <- c(net_win_2_games, net_win_3_games, net_win_3_games, 
                  net_loss_2_games, net_loss_3_games, net_loss_3_games)
probabilities <- c(prob_win_2_games, prob_win_3_games_WLW, prob_win_3_games_LWW,
                   prob_loss_2_games, prob_loss_3_games_LWL, prob_loss_3_games_WLL)

df_prob <- data.frame(Net_Winnings = net_winnings, Probability = probabilities)

# Expected value and standard deviation
E_X <- sum(df_prob$Net_Winnings * df_prob$Probability)
Var_X <- sum((df_prob$Net_Winnings - E_X)^2 * df_prob$Probability)
SD_X <- sqrt(Var_X)

# Simulate 10,000 random values for X
set.seed(42)
simulated_Y <- sample(df_prob$Net_Winnings, size = 10000, replace = TRUE, prob = df_prob$Probability)

# Estimate mean and 95% confidence interval for Y
mean_Y <- mean(simulated_Y)
std_Y <- sd(simulated_Y)
ci_lower <- mean_Y - qt(0.975, df=9999) * (std_Y / sqrt(10000))
ci_upper <- mean_Y + qt(0.975, df=9999) * (std_Y / sqrt(10000))

# Frequency distribution for Y
simulated_freq <- table(simulated_Y) / length(simulated_Y)
df_simulated <- data.frame(Net_Winnings = as.numeric(names(simulated_freq)),
                           Simulated_Probability = as.numeric(simulated_freq))

# Merge with theoretical probabilities
df_comparison <- merge(df_prob, df_simulated, by = "Net_Winnings", all.x = TRUE)
df_comparison[is.na(df_comparison)] <- 0  # Replace NA with 0

# Chi-squared goodness of fit test
chisq_test <- chisq.test(df_comparison$Simulated_Probability, p = df_comparison$Probability)

# Display results
list(
  "Probability Red Sox Win" = prob_redsox_win,
  "Expected Net Win (E(X))" = E_X,
  "Standard Deviation of X" = SD_X,
  "Simulated Mean (E(Y))" = mean_Y,
  "95% Confidence Interval" = c(ci_lower, ci_upper),
  "Chi-Squared Test Statistic" = chisq_test$statistic,
  "Chi-Squared p-value" = chisq_test$p.value
)

#part-3
# Set probabilities for Red Sox winning games
p_boston <- 0.6
p_ny <- 0.43

# Probability calculations for a best-of-five series
# Games are played in the following sequence: Boston, New York, Boston, New York, Boston

# Define all possible ways Red Sox can win the series
prob_win_3_games <- (p_boston * p_ny * p_boston) + (p_boston * (1 - p_ny) * p_boston * p_ny) +
  ((1 - p_boston) * p_ny * p_boston * p_ny * p_boston) + (p_boston * p_ny * (1 - p_boston) * p_ny * p_boston) +
  ((1 - p_boston) * p_ny * (1 - p_boston) * p_ny * p_boston)
prob_redsox_win <- prob_win_3_games

# Probability that Yankees win the series
prob_loss_3_games <- (1 - p_boston) * (1 - p_ny) * (1 - p_boston) + (1 - p_boston) * p_ny * (1 - p_boston) * (1 - p_ny) +
  (p_boston * (1 - p_ny) * (1 - p_boston) * (1 - p_ny) * (1 - p_boston)) + ((1 - p_boston) * (1 - p_ny) * p_boston * (1 - p_ny) * (1 - p_boston)) +
  ((1 - p_boston) * (1 - p_ny) * (1 - p_boston) * (1 - p_ny) * p_boston)
prob_yankees_win <- prob_loss_3_games

# Net winnings for each scenario
net_win_3_games <- 1500  # Win in 3 games
net_win_4_games <- 960   # Win in 4 games
net_win_5_games <- 480   # Win in 5 games
net_loss_3_games <- -1560  # Lose in 3 games
net_loss_4_games <- -1040  # Lose in 4 games
net_loss_5_games <- -520   # Lose in 5 games

# Probability distribution table
net_winnings <- c(net_win_3_games, net_win_4_games, net_win_5_games, 
                  net_loss_3_games, net_loss_4_games, net_loss_5_games)
probabilities <- c(prob_win_3_games, prob_win_3_games, prob_win_3_games, 
                   prob_loss_3_games, prob_loss_3_games, prob_loss_3_games)

# Normalize probabilities to sum to 1
probabilities <- probabilities / sum(probabilities)

df_prob <- data.frame(Net_Winnings = net_winnings, Probability = probabilities)

# Expected value and standard deviation
E_X <- sum(df_prob$Net_Winnings * df_prob$Probability)
Var_X <- sum((df_prob$Net_Winnings - E_X)^2 * df_prob$Probability)
SD_X <- sqrt(Var_X)

# Simulate 10,000 random values for X
set.seed(42)
simulated_Y <- sample(df_prob$Net_Winnings, size = 10000, replace = TRUE, prob = df_prob$Probability)

# Estimate mean and 95% confidence interval for Y
mean_Y <- mean(simulated_Y)
std_Y <- sd(simulated_Y)
ci_lower <- mean_Y - qt(0.975, df=9999) * (std_Y / sqrt(10000))
ci_upper <- mean_Y + qt(0.975, df=9999) * (std_Y / sqrt(10000))

# Frequency distribution for Y
simulated_freq <- table(simulated_Y) / length(simulated_Y)
df_simulated <- data.frame(Net_Winnings = as.numeric(names(simulated_freq)),
                           Simulated_Probability = as.numeric(simulated_freq))

# Merge with theoretical probabilities
df_comparison <- merge(df_prob, df_simulated, by = "Net_Winnings", all.x = TRUE)
df_comparison[is.na(df_comparison)] <- 0  # Replace NA with 0

# Normalize probabilities to sum to 1 before Chi-squared test
df_comparison$Simulated_Probability <- df_comparison$Simulated_Probability / sum(df_comparison$Simulated_Probability)
df_comparison$Probability <- df_comparison$Probability / sum(df_comparison$Probability)

# Chi-squared goodness of fit test
chisq_test <- chisq.test(df_comparison$Simulated_Probability, p = df_comparison$Probability)

# Display results
list(
  "Probability Red Sox Win" = prob_redsox_win,
  "Expected Net Win (E(X))" = E_X,
  "Standard Deviation of X" = SD_X,
  "Simulated Mean (E(Y))" = mean_Y,
  "95% Confidence Interval" = c(ci_lower, ci_upper),
  "Chi-Squared Test Statistic" = chisq_test$statistic,
  "Chi-Squared p-value" = chisq_test$p.value
)
