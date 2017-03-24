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
n_sim <- 10000
# Sample size of each training data
sample_size <- 200
# Value of x for test data
x0 <- 0.5
test_data <- data_frame(x=x0)
# Vector of different degrees of freedom to try:
df_vector <- c(3:99)

# Save values here:
MSE <- rep(0, length(df_vector)) 
bias_squared <- rep(0, length(df_vector))
variance <- rep(0, length(df_vector))

# Do your work here. You'll need a "pull back the curtain" moment at some point:

for(df in df_vector){
  predictions <- rep(0, n_sim)
  for(i in 1:n_sim){
    # Sample new train data:
    train_data <- generate_sample(f, sample_size = sample_size, sigma = sigma)
    
    # Fit the model:
    model_spline <- smooth.spline(train_data$x, train_data$y, df=df)
    
    # Get predictions:
    predictions_tbl <- predict(model_spline, test_data$x) %>% 
      tbl_df() %>% 
      rename(y_hat = y)
    # Save predictions
    predictions[i] <- predictions_tbl$y_hat
    # yhat = fhat(x) at point x0

    if(i %% 1000 == 0){
      print(i)
      
    }
  }
  truth <- data_frame(
    x = rep(x0, n_sim),
    f_x = f(x),
    eps = rnorm(n = n_sim, mean = 0, sd = sigma),
    y = f_x + eps
  )
  
  # Add predictions
  truth <- truth %>%
    mutate(y_hat=predictions)
  
  # Avg MSE over n_sim values
  MSE_tbl <- summarise(truth, mse = mean((y-y_hat)^2))
  MSE[df-2] <- MSE_tbl$mse
  # (Avg bias of f_hat)^2
  bias_tbl <- summarise(truth, avg_bias_squared = (mean(y-y_hat))^2)
  bias_squared[df-2] <- bias_tbl$avg_bias_squared
  # Variance of f_hat
  variance_tbl <- summarise(truth, variance = var(y_hat))
  variance[df-2] <- variance_tbl$variance
  
  if(df %% 10 == 0){
    print(df)
    
  }
}


# Create Data Frames -------------------------------------------------------
# Create results data frame based on previous results
results <- data_frame(
  df = df_vector,
  MSE = MSE,
  bias_squared = bias_squared,
  variance = variance
) 
# NOTE: When I ran the simulation, in lines 92-100 I assigned the output of the
# summarise functions directly into the vector like so:
# MSE[df-2] <- summarise(truth, mse = mean((y-y_hat)^2))
# This caused `MSE`, `bias_squared` and `variance` to be lists rather than vectors
# In the present code above, I save numerical values so that these will be 
# vectors and the `results` variable above works.
# For the sake of not waiting for the whole simulation to run again, I used `unlist`
# and the `results2` and `results2_tidy` data frames to get the plots working.
# With the present code, plots should be changed to use `data=results` on
# `bias_sq_vs_var_plot` and `data=results_tidy` on `MSE_plot`


results2 <- data_frame(
  df = df_vector,
  MSE = unlist(MSE),
  bias_squared = unlist(bias_squared),
  variance = unlist(variance)
)
# Convert to tidy data format (rows=observations, cols=variables) using
# tidyr::gather
results_tidy <- results %>% 
  tidyr::gather(type, value, -df)

# See NOTE above.
results2_tidy <- results2 %>%
  tidyr::gather(type, value, -df)



# Plots -------------------------------------------------------

# Plot bias_squared vs variance (on log vs log scale)
bias_sq_vs_var_plot <- 
  ggplot(results2, aes(x=bias_squared, y=variance)) +
  geom_text(aes(label=df)) +
  scale_y_log10() +
  scale_x_log10()
bias_sq_vs_var_plot
ggsave(file="bias_sq_vs_var.pdf", plot=bias_sq_vs_var_plot, width = 6, height=4)

# Plot MSE, bias_squared, variance
MSE_plot <- 
  ggplot(results2_tidy, aes(x=df, y=value, col=type)) +
  geom_line()
MSE_plot
ggsave(file="MSE.pdf", plot=MSE_plot, width = 6, height=4)
