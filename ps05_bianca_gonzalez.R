# Setup -------------------------------------------------------------------
#bianca gonzalez 
#spring 2017
#math 216 
library(tidyverse)
library(broom)

# Let these be:
# -true function f(x) for a one-dimensional predictor x
# -the standard deviation of the true epsilon i.e. the noise
f <- function(x) {
  5*sin(5*x)
}
f
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

glimpse(train_data)

# The true model f(x) is in red:
ggplot(train_data, aes(x=x)) +
  geom_point(aes(y=y)) +
  stat_function(data=data_frame(x=c(0,2*pi)), aes(x=x), fun=f, col="red", n=1000, size=1)


# Compute MSE, Bias^2, Variance for Range of df --------------------------

# Number of simuations to average the behavior of f_hat = y_hat over
n_sim <- 4
# Sample size of each training data
sample_size <- 200
# Value of x for test data
x0 <- 0.5
# Vector of different degrees of freedom to try:
df_vector <- c(3:9)

# Save values here:
MSE <- rep(0, length(df_vector)) 
bias_squared <- rep(0, length(df_vector))
variance <- rep(0, length(df_vector))

# for loop isn't quite working yet - but general concepts are down:
 for (i in 1:length(df_vector)) {
   
   #save predictions here 
   predictions <- rep(0,length(df_vector))
   
   #Create dataset and fit the model with sample size defined above
   train_data <- generate_sample(f, sample_size, sigma = sigma)
   
   # Pseudo sample test data with given point
   test_data <- data_frame(x=x0)
   
   # Fit model to training data
   model_spline <- smooth.spline(train_data$x, train_data$y, df = df_vector[i])
   
   # Make predictions based off sample point of .5 
   predictions <- predict(model_spline, test_data$x)
   
   # Pull the Curtain - Generate truth: for the function created above
   set.seed(76)
   truth <- data_frame(
     x = rep(x0, n_sim),
     f_x = f(x),
     eps = rnorm(n = n_sim, mean = 0, sd = 0.3),
     y = f_x + eps
   )
   
   #now for each time we have done this for different degrees freedom, 
   #let's generate and store our MSE etc.! 
   
   #to avergae number of simulations over
   for (j in 1:n_sim) {
     # Computing average MSE over n_sims. 
      mtemp <- (mean(truth$y - truth$f_x)^2)
      MSE[n_sim] <- mtemp
     
     # (Avg Bias of f_hat)^2  - truth - prediction. 
      btemp <- (mean(truth$y - truth$f_x)^2)
      bias_squared[n_sim] <- btemp
     
     #Variance of f_hat
      vtemp<- var(truth$f_x)
      variance[n_sim] <- vtemp
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
results
# Convert to tidy data format (rows=observations, cols=variables) using
# tidyr::gather
results_tidy <- results %>% 
  tidyr::gather(type, value, -df)
results_tidy

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


