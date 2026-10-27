# Continental leakage analysis 

# Purpose:        Reproduce Bayesian GLMM continent-level analysis (Table 6A)

# Inputs:         clean_continental.csv: Values are in kha, reduction_t1 is lagged by 1 year

# Output:         Fixed effect posterior means and 95% credible intervals stored in results
#                 and printed to console

# Instructions:
#                 Save the script and input file in the same folder
#                 Set that folder as R's working directory using the setwd() line below or your IDE
#                 Run the entire script
#                 Model fitting may take some time (for us, <10 minutes) 

# Install note:   If any R packages needed to run this script are missing, they will be 
#                 installed automatically by the package-loading block below.
#                 Package versions are not enforced. See README for specific versions. 

# Contacts:       Logan Bingham (logan@tum.de), Thomas Knoke (knoke@tum.de)

# Script begins here:

# Set working directory: add your file path to the following line, then uncomment and run it
#setwd("your_file_path_here/glmm_replication")

# Clear environment
rm(list = ls())

# Install packages
if (!requireNamespace("pacman", quietly = TRUE)) {install.packages("pacman")}
suppressPackageStartupMessages({pacman::p_load(rstanarm, ggplot2)})

# Read and wrangle data
data <- read.csv("clean_continental.csv")

data$continent <- factor(data$continent)
data$year <- factor(data$year)


# Model and priors


model_formula <- increase ~ reduction_t1 + counterfactual + (1 + counterfactual | continent) + 
  (1 + counterfactual | year)

sd_y <- sd(data$increase) #SD of increased deforestation

# Prior SD for fixed effect slopes
slope_prior_sd <- 2.5 * sd_y / c(sd(data$reduction_t1), sd(data$counterfactual))



# Fit model



fit <- stan_glmer(
  formula = model_formula,
  data = data,
  family = gaussian(link = "identity"), 

  #Priors for fixed effect slopes 
  prior = normal(location = 0, scale = slope_prior_sd, autoscale = FALSE), 
  
  #Prior for intercept
  prior_intercept = normal(
    location = mean(data$increase), 
    scale = 2.5 * sd_y, 
    autoscale = FALSE
  ),  
  
  # Residual SD prior
  prior_aux = exponential(rate = 1 / sd_y, autoscale = FALSE), 
  
  # Prior for random effect covariance
  prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1), 
  
  # Sampling
  algorithm = "sampling", 
  chains = 3, 
  iter = 10000, 
  warmup = 2000, 
  thin = 1, 
  seed = 20260914,
  cores = 3, 
  adapt_delta = 0.995, 
  control = list(max_treedepth = 14), 
  QR = FALSE, 
  sparse = FALSE, 
  na.action = na.fail, 
  refresh = 2000 
)



# Fixed effects summaries 


terms <- c("(Intercept)", "reduction_t1", "counterfactual")
draws <- as.matrix(fit)[, terms, drop=FALSE] 

results <- data.frame(
  term = terms, 
  mean = colMeans(draws),
  lower_95 = apply(draws, 2, quantile, probs=0.025), 
  upper_95 = apply(draws, 2, quantile, probs=0.975)
)

# Optional diagnostic
#rstan::check_hmc_diagnostics(fit$stanfit) 

# Results to console
print(results, digits = 5)
prior_summary(fit) # Priors 
print(summary(fit, pars=terms, probs=c(0.025, 0.975)), digits = 5) # Posterior summary & parameter diagnostics



