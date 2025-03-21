#Plotting Variance of Gradients for Different Batch Sizes

# Set seed for reproducibility
set.seed(42)

# Define parameters
sigma_x_sq <- 1/4   # Variance of x
sigma_eps_sq <- 1/100 
theta_star <- 1/2    # True theta
n_samples <- 10000 
batch_sizes <- c(100, 1000)  # Batch sizes to evaluate

# Generate x ~ N(0, sqrt(Σx))
x <- rnorm(n_samples, mean = 0, sd = sqrt(sigma_x_sq))

# Generate noise ε ~ N(0, sqrt(σ^2))
epsilon <- rnorm(n_samples, mean = 0, sd = sqrt(sigma_eps_sq))

# Generate y = θ* x + ε
y <- theta_star * x + epsilon

# Define theta values to evaluate
theta_vals <- seq(0, 1, by = 0.05)

# Function to compute gradient variance for a given batch size
sim_gradient_variance <- function(batch_size) {
  gradient_variances <- numeric(length(theta_vals))
  
  for (i in seq_along(theta_vals)) {
    theta <- theta_vals[i]
    gradient_samples <- numeric(200)  # Store 200 gradient samples
    
    for (j in 1:200) {
      # Random batch selection
      batch_indices <- sample(1:n_samples, batch_size, replace = FALSE)
      x_batch <- x[batch_indices]
      y_batch <- y[batch_indices]
      
      # Compute gradient: ∇θ L(θ) = 2 * x * (θ x - y)
      gradients <- 2 * x_batch * (theta * x_batch - y_batch)
      
      # Store mean gradient of batch
      gradient_samples[j] <- mean(gradients)
    }
    
    # Compute variance of the gradient samples
    gradient_variances[i] <- var(gradient_samples)
  }
  
  return(data.frame(theta = theta_vals, variance = gradient_variances, batch_size = batch_size))
}

# Run simulations for both batch sizes
results_100 <- sim_gradient_variance(100)
results_1000 <- sim_gradient_variance(1000)

# Combine results for plotting
library(ggplot2)
results <- rbind(results_100, results_1000)

# Plot gradient variance across theta values
ggplot(results, aes(x = theta, y = variance, color = as.factor(batch_size))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Gradient Variance Across θ Values",
       x = "θ",
       y = "Variance of Gradient",
       color = "Batch Size") +
  theme_minimal()