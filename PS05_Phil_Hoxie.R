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
# Vector of different degrees of freedom to try:
df_vector <- c(3:99)

# Save values here:
MSE <- rep(0, length(df_vector)) 
bias_squared <- rep(0, length(df_vector))
variance <- rep(0, length(df_vector))

# Do your work here. You'll need a "pull back the curtain" moment at some point:

test_data <- data_frame(x=x0)

for(k in 3:99) {
  # Save predictions here
  predictions <- rep(0, n_sim)
  
  for(i in 1:n_sim) {
    # FILL THIS IN:
    
    pseudo_train <- generate_sample(f, sample_size = sample_size, sigma = sigma) 
    
    pseudo_train = pseudo_train %>%
      filter(x != x0)
    
    mod_spline <- smooth.spline(pseudo_train$x, pseudo_train$y, df=k)
    
    pseudo_predict <- predict(mod_spline, x=test_data$x) %>% 
      tbl_df() 
    
    pseudo_predict <- pseudo_predict %>% 
      rename(y_hat = y)
    
    predictions[i] <- pseudo_predict$y_hat
    
    if(i %% 100 == 0){
      print(i)
    }
  }
  
  # Pull the Curtain! Reveal truth: i.e. show all components we really know, but
  # pretended not to:
  set.seed(76)
  truth <- data_frame(
    x = rep(x0, n_sim),
    f_x = f(x),
    eps = rnorm(n = n_sim, mean = 0, sd = 0.3),
    y = f_x + eps
  )
  
  # Add predictions
  truth <- truth %>%
    mutate(y_hat=predictions)
  
  # Compute MSE, (Avg Bias of f_hat)^2, Variance of f_hat, sigma^2:
  # FILL THIS IN:
  
  # MSE
  
  Avg_MSE <- truth %>% 
    summarise(MSE = mean((y-y_hat)^2))
  
  MSE[k-2] <- Avg_MSE
  # Avg Bias
  
  avg_bias <- truth %>% 
    summarise(bias = mean((f_x - y_hat)^2))
  
  bias_squared[k-2] <- avg_bias
  
  # variance of f_hat
  
  var_f_hat <- var(truth$y_hat)
  
  variance[k-2] <- var_f_hat
  
}







# Create Data Frames -------------------------------------------------------
# Create results data frame based on previous results
results_test <- data_frame(
  df = as.integer(df_vector),
  MSE = as.numeric(MSE),
  bias_squared = as.numeric(bias_squared),
  variance = as.numeric(variance)
) 
# Convert to tidy data format (rows=observations, cols=variables) using
# tidyr::gather
#results_tidy <- results %>% 
 # tidyr::gather(type, value, -df)

results_tidy <- results_test %>% 
  tidyr::gather(type, value, -df)



# Plots -------------------------------------------------------

# Plot bias_squared vs variance (on log vs log scale)
bias_sq_vs_var_plot <- 
  ggplot(results_test, aes(x=bias_squared, y=variance)) +
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


