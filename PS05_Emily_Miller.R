# Setup -------------------------------------------------------------------
library(tidyverse)
library(broom)
setwd("C:/Users/Emily Miller/Documents/Stat_learning")

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
n_sim <- 1000 #Reduce to 500 or 1000 to reduce running time
# Sample size of each training data
sample_size <- 200
# Value of x for test data
x0 <- 0.5
test_data <- data_frame(x=x0)
# Vector of different degrees of freedom to try:
df_vector <- c(2:60)

# Save values here:
MSE <- rep(0, length(df_vector)) 
bias_squared <- rep(0, length(df_vector))
variance <- rep(0, length(df_vector))

# Do your work here. You'll need a "pull back the curtain" moment at some point:

# Iterate through degrees of freedom between 3 and 99
for (j in 1:length(df_vector)) {
  df <- df_vector[j]
  # Save predictions here
  predictions <- rep(0, n_sim)
  # Create training data set and spline model
  for(i in 1:n_sim) {
    # 1. Sample training data
    train_data <- generate_sample(f, sample_size = 100, sigma = sigma) %>%
      dplyr::select(x, y)
    # 2. Fit Model with specific df
    train_model <- smooth.spline(train_data$x, train_data$y, df = df)
    # 3. Get predictions
    predictions[i] <-as.numeric(unlist(predict(train_model, test_data)))[2]
    if(i %% 100 == 0){
      print(i)
    }
    
  }
  
  # Generate the true model
  set.seed(76)
  truth <- data_frame(
    x = rep(x0, n_sim),
    f_x = f(x),
    eps = rnorm(n = n_sim, mean = 0, sd = 0.3),
    y = f_x + eps,
    y_hat = predictions
  )
  

  # Find MSE, bias_squared, and variance
    MSE_df = mean((truth$y-truth$y_hat)^2)
    bias_squared_df = mean((truth$f_x-truth$y_hat))^2
    var_df = var(truth$y_hat)
  
  # Save MSE bias squared and variance for this degree of freedom
  MSE[j] <- MSE_df
  bias_squared[j] <- bias_squared_df
  variance[j] <- var_df
  print(j)
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
Palette1 <- c('blue','red','yellow')
MSE_plot <- 
  ggplot(results_tidy, aes(x=df, y=value, col=type)) +
  geom_line(size = 1.5) +
  scale_colour_manual(values=Palette1)
MSE_plot
ggsave(file="MSE.pdf", plot=MSE_plot, width = 6, height=4)



