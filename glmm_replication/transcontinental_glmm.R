# Transcontinental leakage analysis 

# Purpose:        Reproduce Bayesian GLMM transcontinental leakage analysis (Table 6B, Fig S5 bottom)

# Inputs:         clean_transcontinental.csv: Values are in kha, reduction_t1 is lagged by 1 year

# Output:         Fixed effect posterior means and 95% credible intervals and a descriptive plot

# Instructions:   Save the script and input file in the same folder
#                 Set that folder as R's working directory or provide the full path in read.csv()
#                 Run the entire script
#                 Model fitting may take some time (for us, <10 minutes) 

# Install note:   If any R packages needed to run this script are missing, they will be 
#                 installed automatically by the package-loading block below.
#                 Package versions are not enforced, but for documentation, the
#                 versions we used in parentheses:

#                 rstanarm (2.32.2), rstan (2.32.7), StanHeaders (2.39.1), 
#                 ggplot2 (4.0.3), Matrix (1.7-4), lme4 (2.0-6)

# Analysis originally performed in JASP 0.18.2 using rstan version 2.32.3
# Helpful rstanarm articles:  - Functions   https://mc-stan.org/rstanarm/reference/stan_glmer.html
#                             - Priors      https://mc-stan.org/rstanarm/reference/priors.html                                           
#                             - Or use ?stan_glmer          


# Environment used for testing this script: R 4.5.3 (2026-03-11), macOS Tahoe 26.6.2 on Apple Silicon.
              
# Contacts:       Logan Bingham (logan@tum.de), Thomas Knoke (knoke@tum.de)

###############################################################################################################


#Install any missing packages
if (!requireNamespace("pacman", quietly = TRUE)) {install.packages("pacman")}
suppressPackageStartupMessages({pacman::p_load(rstanarm, ggplot2)})

# Read and wrangle data
data <- read.csv("clean_transcontinental.csv")  # Note: Unlike continental script, increase refers to additional deforestation in the other two continents
                                                #       reduction_t1 is still the lagged reduction in the continent identified by "continent" in data

data$continent <- factor(data$continent)
data$year <- factor(data$year)


########################################################################################

# Model and priors

########################################################################################

# Transcontinental
model_formula <- increase ~ reduction_t1 + counterfactual + (1 + counterfactual | continent) + 
  (1 + reduction_t1+ counterfactual | year) # Here year also gets a reduction_t1 slope (relationship can vary annually)

sd_y <- sd(data$increase) #SD of increased deforestation

# Prior SD for fixed effect slopes
slope_prior_sd <- 2.5 * sd_y / c(sd(data$reduction_t1), sd(data$counterfactual))


########################################################################################

# Fit model

########################################################################################

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
  iter = 16000, 
  warmup = 2000, 
  thin = 1, 
  seed = 20260916,
  cores = 3, 
  adapt_delta = 0.999, 
  control = list(max_treedepth = 14), 
  QR = FALSE, 
  sparse = FALSE, 
  na.action = na.fail, 
  refresh = 2000 
)

          
########################################################################################

# Fixed effects summaries 

########################################################################################

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

#Results to console
prior_summary(fit) # Priors 
print(summary(fit, pars=terms, probs=c(0.025, 0.975)), digits = 5) # Posterior summary & parameter diagnostics

########################################################################################

# Plotting (Supplementary Figure 5, bottom)

########################################################################################

fig <- ggplot(data, aes(x=reduction_t1, y = increase, color = continent)) +
  geom_point(size=2.5) +
  geom_smooth(method="lm", formula = y ~ x, se = FALSE, linewidth = 0.8) + # Note: lines are linear regressions by continent for illustration
  scale_color_manual(
    values = c("1" = "#11AFE0", "2" = "#ED7D31", "3" = "#237632"), breaks = c("1", "2", "3"), 
    labels = c("Latin America", "Africa", "Asia"), name = NULL
  ) +
  scale_x_continuous(breaks=seq(-2000, 0, 500))+
  scale_y_continuous(breaks = seq(0, 1400, 200)) +
  coord_cartesian(xlim = c(-2000,0), ylim = c(0, 1400))+
  labs(
    title = "Transcontinental relationships",
    x = "Deforestation reduction (year t-1) (1,000 ha)",
    y = "Additional deforestation in other countries \n on other continents (1,000 ha)"
  )+
  theme_classic(base_size=12) +
  theme(legend.position = "top", plot.title = element_text(hjust=0.5))

print(fig)

# Optional figure export as pdf
#ggsave("transcontinental.pdf", plot = fig, width = 7.5, height = 5.5, units = "in", bg = "white")

# Optional diagnostic
#rstan::check_hmc_diagnostics(fit$stanfit) 

#Results to console
print(results, digits = 5)
prior_summary(fit) # Priors 
print(summary(fit, pars=terms, probs=c(0.025, 0.975)), digits = 5) # Posterior summary & parameter diagnostics
