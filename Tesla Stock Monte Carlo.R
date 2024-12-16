# Tesla Stock Investment Monte Carlo Simulation

# Required libraries
library(ggplot2)
library(dplyr)

# Set random seed for reproducibility
set.seed(123)

# Simulation parameters
n_simulations <- 10000  # Number of Monte Carlo simulations
investment_period <- 365  # Days of investment
initial_investment <- 1000  # Initial investment amount in USD

# Historical Tesla volatility and return estimation
# Note: These are hypothetical parameters and should be updated with real data
annual_return <- 0.4  # 40% expected annual return
annual_volatility <- 0.6  # 60% annual volatility

# Monte Carlo Simulation Function
simulate_tesla_investment <- function(initial_investment, 
                                      annual_return, 
                                      annual_volatility, 
                                      days) {
  # Daily return and volatility
  daily_return <- annual_return / 365
  daily_volatility <- annual_volatility / sqrt(365)
  
  # Generate daily returns
  daily_returns <- rnorm(days, 
                         mean = daily_return, 
                         sd = daily_volatility)
  
  # Calculate cumulative investment value
  investment_path <- initial_investment * cumprod(1 + daily_returns)
  
  return(investment_path[days])
}

# Perform Monte Carlo Simulations
simulation_results <- replicate(
  n_simulations, 
  simulate_tesla_investment(
    initial_investment, 
    annual_return, 
    annual_volatility, 
    investment_period
  )
)

# Create results dataframe
results_df <- data.frame(
  final_value = simulation_results
)

# Calculate key statistics
final_value_stats <- results_df %>%
  summarise(
    mean_final_value = mean(final_value),
    median_final_value = median(final_value),
    min_final_value = min(final_value),
    max_final_value = max(final_value),
    value_at_risk_5pct = quantile(final_value, 0.05),
    probability_loss = mean(final_value < initial_investment) * 100
  )

# Visualization of simulation results
ggplot(results_df, aes(x = final_value)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  geom_vline(xintercept = initial_investment, color = "red", linetype = "dashed") +
  labs(
    title = "Tesla Stock Investment Monte Carlo Simulation",
    x = "Final Investment Value (USD)",
    y = "Frequency of Outcomes"
  ) +
  theme_minimal()

# Print statistics
print(final_value_stats)

# Save simulation results
save(results_df, final_value_stats, file = "tesla_investment_simulation.RData")