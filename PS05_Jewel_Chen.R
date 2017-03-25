# Setup -------------------------------------------------------------------
library(tidyverse)
library(broom)

# Let these be:
# -true function f(x) for a one-dimensional predictor x
# -the standard deviation of the true epsilon i.e. the noise
f <- function(x) {
  5*sin(5*x)
}
sigma <- 2

# This function generates a random sample. Random in terms of both
# -the predictor x
# -the amount of noise eps
generate_sample <- function(f, sample_size, sigma) {
  data_frame(
    x = runif(n = sample_size, min = 0, max = 2*pi),
    f_x = f(x),
    eps = rnorm(n = sample_size, mean = 0, sd = sigma),
    y = f_x + eps
  )
}

# Generate Training Data via Sampling -------------------------------------
# We observe points (x,y). Here we are pretending to know the exact break down 
# of y into f(x) and eps:
train_data <- generate_sample(f, sample_size = 100, sigma = sigma) %>%
  dplyr::select(x, y)

# The true model f(x) is in red:
ggplot(train_data, aes(x=x)) +
  geom_point(aes(y=y)) +
  stat_function(data=data_frame(x=c(0,2*pi)), aes(x=x), fun=f, col="red", n=1000, size=1)

# Compute MSE, Bias^2, Variance for Range of df --------------------------

# Number of simuations to average the behavior of f_hat = y_hat over
n_sim <- 1000
# Sample size of each training data
sample_size <- 200
# Value of x for test data
x0 <- 0.5
# Vector of different degrees of freedom to try:
df_vector <- c(3:99)

# Save values here:
MSE <- rep(0, length(df_vector)) 
bias_squared <- rep(0, length(df_vector))
variance <- rep(0, length(df_vector))

# Do your work here. You'll need a "pull back the curtain" moment at some point:
for(i in 1:length(df_vector)) {
  
  # Save predictions here
  predictions <- rep(0, n_sim)
  
  for(j in 1:n_sim){
    
    # Generate pseudo training data
    train_data <- generate_sample(f, sample_size = sample_size, sigma = sigma)
    
    # Generate the pseudo test point x0
    test_data <- data_frame(x = x0)
    
    # Fit model using smooth.spline()
    model <- smooth.spline(x = train_data$x, y = train_data$y, df = df_vector[i])
    
    # Use the model to predict on test_data
    prediction <- predict(model, test_data$x) 
    
    # Store the prediction in predictions 
    predictions[j] <- prediction$y
  }
  
  # Generate the truth
  set.seed(25)
  truth <- data_frame(
    x = rep(x0, n_sim),
    f_x = f(x),
    eps = rnorm(n = n_sim, mean = 0, sd = sigma),
    y = f_x + eps,
    y_hat = predictions
  ) %>%
    # Calculate MSE, bias, and variance
    summarise(
      MSE = mean((y-y_hat)^2),
      bias_squared = mean((f_x-y_hat))^2,
      variance = var(y_hat)
    )
  
  # Store calculations 
  MSE[i] <- truth$MSE
  bias_squared[i] <- truth$bias_squared
  variance[i] <- truth$variance
  print(paste("Degree of Freedom =", df_vector[i]))
}

# Create Data Frames -------------------------------------------------------
# Create results data frame based on previous results
results <- data_frame(
  df = df_vector,
  MSE = MSE,
  bias_squared = bias_squared,
  variance = variance
) 
# Convert to tidy data format (rows=observations, cols=variables) using
# tidyr::gather
results_tidy <- results %>% 
  tidyr::gather(type, value, -df)

# Plots -------------------------------------------------------

# Plot bias_squared vs variance (on log vs log scale)
bias_sq_vs_var_plot <- 
  ggplot(results, aes(x=bias_squared, y=variance)) +
  geom_text(aes(label=df)) +
  scale_y_log10() +
  scale_x_log10()
bias_sq_vs_var_plot
ggsave(file="bias_sq_vs_var.pdf", plot=bias_sq_vs_var_plot, width = 6, height=4)

# Plot MSE, bias_squared, variance
MSE_plot <- 
  ggplot(results_tidy, aes(x=df, y=value, col=type)) +
  geom_line()
MSE_plot
ggsave(file="MSE.pdf", plot=MSE_plot, width = 6, height=4)