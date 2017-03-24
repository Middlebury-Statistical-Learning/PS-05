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

true_vals <- matrix(ncol = 6, nrow = length(df_vector))

# Do your work here. You'll need a "pull back the curtain" moment at some point:
for(j in df_vector[1]:df_vector[length(df_vector)]){
  
  predictions <- rep(0, n_sim)
  
  for(i in 1:n_sim){
    train_data <- generate_sample(f, 500, sigma=sigma) %>%
      dplyr::select(x,y)
    test_data <- data_frame(x=x0)
    model <- smooth.spline(train_data$x, train_data$y, df = j)
    predictions[i] <- predict(model, x = test_data$x)$y
  }
  
  truth <- data_frame(
    x = rep(x0, n_sim),
    f_x = f(x),
    eps = rnorm(n = n_sim, mean = 0, sd = sigma),
    y = f_x + eps
  ) %>%
    mutate(y_hat=predictions) %>%
    summarise(
      MSE = mean((y-y_hat)^2),
      bias_squared = mean((f_x-y_hat))^2,
      var = var(y_hat)
    ) %>%
    mutate(
      irreducible = sigma^2,
      sum = bias_squared + var + irreducible,
      df = j
    )
  
  true_vals[j,] <- as.matrix(truth)
  
  if(j %% 1 == 0){
    print(j)
  }
}

results <- true_vals %>%
  tbl_df() %>%
  rename(MSE = V1,
         bias_squared = V2,
         variance = V3,
         irreducible = V4,
         sum = V5, 
         df = V6)
  

# Convert to tidy data format (rows=observations, cols=variables) using
# tidyr::gather
results_tidy <- results %>% 
  tidyr::gather(type, value, -df)

# I don't have df = 98 or 99

# Plots -------------------------------------------------------

# Plot bias_squared vs variance (on log vs log scale)
bias_sq_vs_var_plot <- 
  ggplot(results, aes(x=bias_squared, y=variance)) +
  geom_text(aes(label=df)) +
  scale_y_log10() +
  scale_x_log10()
bias_sq_vs_var_plot
ggsave(file="PS05_Ben_Czekanski_bias_sq_vs_var.pdf", plot=bias_sq_vs_var_plot, width = 6, height=4)

# Plot MSE, bias_squared, variance
MSE_plot <- 
  ggplot(results_tidy, aes(x=df, y=value, col=type)) +
  geom_line()
MSE_plot
ggsave(file="PS05_Ben_Czekanski_MSE.pdf", plot=MSE_plot, width = 6, height=4)


