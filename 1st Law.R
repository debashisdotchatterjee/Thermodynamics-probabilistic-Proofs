# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define parameters
time_steps <- seq(0, 10, by = 0.1)
k_B <- 1.38e-23  # Boltzmann's constant

# Define functions for internal energy, work, and heat
internal_energy <- function(t) {
  50 + 10 * sin(t)  # Example internal energy function
}

work_done <- function(t) {
  5 * t  # Example work done function (linear increase)
}

# Calculate heat added using the First Law: dU = dQ - dW
calculate_heat_added <- function(t, internal_energy, work_done) {
  dU <- diff(internal_energy(t))
  dW <- diff(work_done(t))
  dQ <- dU + dW  # Rearranging the First Law: dQ = dU + dW
  return(c(NA, cumsum(dQ)))  # Return cumulative heat added with an initial NA for correct length
}

# Create a data frame for the simulation
data <- data.frame(
  time = time_steps,
  U = internal_energy(time_steps),
  W = work_done(time_steps)
)

# Calculate heat added
data$Q <- calculate_heat_added(data$time, internal_energy, work_done)

# Plot internal energy, work, and heat over time
plot1 <- ggplot(data, aes(x = time)) +
  geom_line(aes(y = U, color = "Internal Energy (U)")) +
  geom_line(aes(y = W, color = "Work Done (W)")) +
  geom_line(aes(y = Q, color = "Heat Added (Q)")) +
  labs(title = "First Law of Thermodynamics: Energy Conservation",
       x = "Time (t)",
       y = "Energy",
       color = "Energy Components") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot cumulative heat added versus time
plot2 <- ggplot(data, aes(x = time, y = Q)) +
  geom_line(color = "red") +
  labs(title = "Cumulative Heat Added Over Time",
       x = "Time (t)",
       y = "Heat Added (Q)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Verify the First Law: dU - dQ + dW = 0
verification <- diff(data$U) - diff(data$Q) + diff(data$W)
data$verification <- c(NA, verification)

# Plot verification of the First Law
plot3 <- ggplot(data, aes(x = time, y = verification)) +
  geom_line(color = "blue") +
  labs(title = "Verification of the First Law (dU - dQ + dW)",
       x = "Time (t)",
       y = "Verification") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Display plots
plot1
plot2
plot3
################################################
##Self Saved Plots
##################################


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define parameters
time_steps <- seq(0, 10, by = 0.1)
k_B <- 1.38e-23  # Boltzmann's constant

# Define functions for internal energy, work, and heat
internal_energy <- function(t) {
  50 + 10 * sin(t)  # Example internal energy function
}

work_done <- function(t) {
  5 * t  # Example work done function (linear increase)
}

# Calculate heat added using the First Law: dU = dQ - dW
calculate_heat_added <- function(t, internal_energy, work_done) {
  dU <- diff(internal_energy(t))
  dW <- diff(work_done(t))
  dQ <- dU + dW  # Rearranging the First Law: dQ = dU + dW
  return(c(NA, cumsum(dQ)))  # Return cumulative heat added with an initial NA for correct length
}

# Create a data frame for the simulation
data <- data.frame(
  time = time_steps,
  U = internal_energy(time_steps),
  W = work_done(time_steps)
)

# Calculate heat added
data$Q <- calculate_heat_added(data$time, internal_energy, work_done)

# Handle missing values in the first row for cumulative calculations
data$Q[1] <- 0

# Plot internal energy, work, and heat over time
plot1 <- ggplot(data, aes(x = time)) +
  geom_line(aes(y = U, color = "Internal Energy (U)")) +
  geom_line(aes(y = W, color = "Work Done (W)")) +
  geom_line(aes(y = Q, color = "Heat Added (Q)")) +
  labs(title = "First Law of Thermodynamics: Energy Conservation",
       x = "Time (t)",
       y = "Energy",
       color = "Energy Components") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot1
ggsave("plot1.png", plot = plot1, width = 8, height = 6)

# Plot cumulative heat added versus time
plot2 <- ggplot(data, aes(x = time, y = Q)) +
  geom_line(color = "red") +
  labs(title = "Cumulative Heat Added Over Time",
       x = "Time (t)",
       y = "Heat Added (Q)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot2
ggsave("plot2.png", plot = plot2, width = 8, height = 6)

# Verify the First Law: dU - dQ + dW = 0
verification <- diff(data$U) - diff(data$Q) + diff(data$W)
data$verification <- c(NA, verification)

# Handle NA values for the verification plot
data$verification[is.na(data$verification)] <- 0

# Plot verification of the First Law
plot3 <- ggplot(data, aes(x = time, y = verification)) +
  geom_line(color = "blue") +
  labs(title = "Verification of the First Law (dU - dQ + dW)",
       x = "Time (t)",
       y = "Verification") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot3
ggsave("plot3.png", plot = plot3, width = 8, height = 6)

# Display plots
print(plot1)
print(plot2)
print(plot3)
