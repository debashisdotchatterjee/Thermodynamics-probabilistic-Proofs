# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define parameters
k_B <- 1.38e-23  # Boltzmann's constant
time_steps <- seq(0, 10, by = 0.1)

# Define functions for probability density function (pdf) and entropy
# Use a simple Gaussian distribution for pdf with time-dependent mean
pdf <- function(x, t) {
  mean <- sin(t)
  sd <- 1
  dnorm(x, mean = mean, sd = sd)
}

# Define entropy function using differential entropy concept
entropy <- function(t) {
  integrand <- function(x) {
    rho_x_t <- pdf(x, t)
    ifelse(rho_x_t == 0, 0, -k_B * rho_x_t * log(rho_x_t))
  }
  integrate(integrand, lower = -10, upper = 10)$value  # Use a large finite range
}

# Calculate entropy over time
entropy_values <- sapply(time_steps, entropy)

# Create a data frame for the simulation
data <- data.frame(
  time = time_steps,
  entropy = entropy_values
)

# Plot entropy over time
plot1 <- ggplot(data, aes(x = time, y = entropy)) +
  geom_line(color = "blue") +
  labs(title = "Second Law of Thermodynamics: Entropy Increase",
       x = "Time (t)",
       y = "Entropy (S)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot1
ggsave("entropy_increase.png", plot = plot1, width = 8, height = 6)

# Define the probability density function rho(x, t) for different times
times_to_plot <- c(0, pi / 2, pi, 3 * pi / 2, 2 * pi)
x_values <- seq(-4, 4, by = 0.1)

rho_data <- expand.grid(x = x_values, t = times_to_plot)
rho_data$rho <- mapply(pdf, rho_data$x, rho_data$t)

# Plot probability density function over time
plot2 <- ggplot(rho_data, aes(x = x, y = rho, color = factor(t))) +
  geom_line() +
  labs(title = "Probability Density Function ρ(x, t) at Different Times",
       x = "State (x)",
       y = "Probability Density ρ(x, t)",
       color = "Time (t)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot2
ggsave("pdf_over_time.png", plot = plot2, width = 8, height = 6)

# Additional plot: Entropy derivative over time
entropy_derivative <- diff(entropy_values) / diff(time_steps)
data_derivative <- data.frame(
  time = time_steps[-1],
  entropy_derivative = entropy_derivative
)

plot3 <- ggplot(data_derivative, aes(x = time, y = entropy_derivative)) +
  geom_line(color = "green") +
  labs(title = "Derivative of Entropy Over Time",
       x = "Time (t)",
       y = "dS/dt") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot3
ggsave("entropy_derivative.png", plot = plot3, width = 8, height = 6)

# Display plots
print(plot1)
print(plot2)
print(plot3)
