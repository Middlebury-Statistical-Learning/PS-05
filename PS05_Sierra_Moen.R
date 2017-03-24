#Hello! My coding skills are not what you would call ideal. I tried my best and Brenda Li was
#a total superstar at helping me. I marked everything she helped me with and my graph isn't 
#EXACTLY how I would want it to look but tbh I'm glad its there!
# I knew I had to use multiple for loops but I didn't know they were meant to be nested.

# Setup -------------------------------------------------------------------
library(tidyverse)
library(broom)
set.seed(76)

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
View(train_data)
# The true model f(x) is in red:
ggplot(train_data, aes(x=x)) +
  geom_point(aes(y=y)) +
  stat_function(data=data_frame(x=c(0,2*pi)), aes(x=x), fun=f, col="red", n=1000, size=1)


# Compute MSE, Bias^2, Variance for Range of df --------------------------

# Number of simuations to average the behavior of f_hat = y_hat over
n_sim <- 100
#I kept n_sim small for you!
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

for(i in 3:99) {
  predictions<-rep(0,length(n_sim))
  for(j in 1:n_sim) #Brenda told me to distinguish between i and j
    {
    train_data <- generate_sample(f, n_sim, sigma=sigma) %>%
      dplyr::select(x,y)
    
    # 2. "Sample" test data. In this case single point x0
    test_data <- data_frame(x=x0)
    
    # 3. Fit Model
    model <- smooth.spline(train_data$x, df=i)
    
    #I know why I turned this into as.numeric but didn't know how.
    p<-(predict(model,x0)[2])
    predictions[j]<-as.numeric(p)
    
    if(j %% 10 == 0){
      print(j)
    }
  }
    truth <- data_frame(
      x = rep(x0, n_sim),
      f_x = f(x),
      eps = rnorm(n = n_sim, mean = 0, sd = sigma),
      y = f_x + eps
    ) 
    # Add predictions:
    truth<-truth %>% mutate(y_hat=predictions)
    
    j2<-truth %>%
      summarise(
        MSE = mean((y-y_hat)^2),
        bias_squared = mean((f_x-y_hat))^2,
        var = var(y_hat)
      )
    MSE[i] <- as.numeric(j2[1])
    bias_squared[i] <- as.numeric(j2[2])
    variance[i] <- as.numeric(j2[3])
    
  }


# Create Data Frames -------------------------------------------------------
# Create results data frame based on previous results
results <- data_frame(
  df = df_vector,
  MSE = tail(MSE, -2),
  bias_squared = tail(bias_squared, -2),
  variance = tail(variance, -2)
) 
# Convert to tidy data format (rows=observations, cols=variables) using
# tidyr::gather
results_tidy <- results %>% 
  tidyr::gather(type, value, -df)

View(results_tidy)


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


