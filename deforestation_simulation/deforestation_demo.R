
# Replication demo: profit-oriented counterfactual deforestation

# Purpose:        This script reproduces the modeling workflow for a user-selected country
#                 using open-source tools.

# Inputs:         Data, parameters, and calibrated inputs can be found in the accompanying file:
#                 replication_inputs.RDS

# Output:         A graph of simulated annual deforestation. Fixed default seeds
#                 for the Monte Carlo analysis are provided in the input tables to
#                 support reproducible runs of this demo.

# Documentation:  See README for the author list, citation, input description,
#                 and tested R/Package versions.

# Install note:   If any R packages needed to run this script are missing,
#                 they will be installed automatically by the package-loading block below. 
#                 Note that package versions are not enforced. For conflicts between versions, 
#                 see README and attached documentation.

# Contacts:       Logan Bingham (logan@tum.de), Thomas Knoke (knoke @tum.de), 
#                 Jorge Cueva (jorge.cueva@tum.de) Jonathan Fibich (jonathan.fibich@tum.de),
#                 Caroline von Webenau (c.webenau@gmx.de)

# How to use this script:
#
#               Save this script and replication_inputs.rds in the same folder 
#               Set that folder as R's working directory, or else provide the full path in readRDS()
#               Select COUNTRY below
#               Run the entire script

###############################################################################################################

# Script begins here

###############################################################################################################


# Select country from the following list. 

COUNTRY <- "Brazil" # Paste the selected country here with quotes. Name must match the list below exactly

#"Angola", "Argentina", "Bolivia", "Brazil", "Cambodia", "Cameroon", "Colombia", "Côte d'Ivoire", 
# "Ecuador", "Ethiopia", "DR Congo", "Indonesia" , "Lao People's DR", "Madagascar", "Malaysia", 
# "Mexico", "Mozambique", "Myanmar", "Nicaragua", "Nigeria", "Paraguay", "Peru",  
# "R Congo", "Tanzania", "Thailand", "Venezuela", "Vietnam", "Zambia"

# No further user input required. You can now select and run the entire script.

# Install packages and read in data
if (!requireNamespace("pacman", quietly = TRUE)) {install.packages("pacman")}
suppressPackageStartupMessages({pacman::p_load(dplyr, tibble, ggplot2, lpSolve)})

#Read input data and calibrated parameters
  inputs <- readRDS("replication_inputs.rds")

  periods_df <- inputs$periods %>% filter(country == COUNTRY) %>% arrange(period)
  profits <- inputs$profits %>% filter(country == COUNTRY)
  areas <- inputs$initial_areas %>% filter(country == COUNTRY)
  comparison_df <- inputs$annual_series %>%filter(country == COUNTRY) %>% arrange(year)
  settings <- inputs$country_settings %>% filter(country == COUNTRY)
  correction <- inputs$baseline_corrections %>%filter(country == COUNTRY)

  stopifnot(nrow(correction) > 0)


# Parameters (do not modify)
mc_runs          <- 50L
n_scenarios      <- 1000L

planning_period  <- 30
years_per_period <- 5
upside_mult   <- settings$upside_mult
downside_mult <- settings$downside_mult 
forced_start_year <- 1990
initial_areas_1000ha <- setNames(areas$area_1000ha, areas$land_use)
obs_to_ha_mult <- 1000 # units 

#Seed management (default replication seeds provided in input tables)
stopifnot(nrow(settings) == 1L)
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
master_seed_used <- as.integer(settings$default_seed)
set.seed(master_seed_used)
seed_subtitle <- paste("seed:", master_seed_used)


# Land uses
LU <- c(
  "other_land",
  "planted_forest",
  "arable",
  "permanent_cropland",
  "permanent_pasture",
  "new_deforestation",
  "naturally_regenerated"
)

# Normalize land use shares
normalize_lu_shares <- function(x, lu = LU, tol = 1e-10) { 
  
  stopifnot(is.numeric(x), !is.null(names(x))) 
  
  missing <- setdiff(lu, names(x)) 
  extra   <- setdiff(names(x), lu) 
  stopifnot(length(missing) == 0, length(extra) == 0) 
  
  x <- x[lu] 
  x[x < 0] <- 0 
  s <- sum(x) 
  stopifnot(is.finite(s), s > 0) 
  
  if (abs(s - 1) > tol) x <- x / s 
  x 
}

######################################################################################################################
######################################################################################################################

# Functions

######################################################################################################################
######################################################################################################################

# Generate uncertainty scenarios
generate_uncertainty_scenarios <- function(
    n_scenarios,
    profit_coeffs,
    uncertainty_sd,
    upside_mult, 
    downside_mult, 
    seed
) {
  #if (length(seed) != 1 || is.na(seed)) stop("seed must be a single value and not NA") 
  set.seed(as.integer(seed)) 
  
  
  land_uses <- c(
    "planted_forest", 
    "arable", 
    "permanent_cropland",
    "permanent_pasture", 
    "new_deforestation", 
    "naturally_regenerated"
  )
  
  scenarios_matrix <- matrix(NA, nrow = n_scenarios, ncol = length(land_uses)) 
  colnames(scenarios_matrix) <- land_uses 
  
  # Randomly draw profit expectations
  for (lu in land_uses) {
    expected <- profit_coeffs[lu] 
    sd <- uncertainty_sd[lu]
    u <- runif(n_scenarios, min = 0, max = 1) 
    
    scenarios_matrix[, lu] <- expected + (upside_mult * sd) - u * (upside_mult + downside_mult) * sd 
  }
  
  #Store
  tibble(
    scenario_id = 1:n_scenarios,
    planted_forest = scenarios_matrix[, "planted_forest"],
    arable = scenarios_matrix[, "arable"],
    permanent_cropland = scenarios_matrix[, "permanent_cropland"],
    permanent_pasture = scenarios_matrix[, "permanent_pasture"],
    new_deforestation = scenarios_matrix[, "new_deforestation"],
    naturally_regenerated = scenarios_matrix[, "naturally_regenerated"]
  )
}

# Rescale 30 year allocation to timestep
scale_to_5_years <- function(f0, f30, planning_period) {  f0 - 5 * (f0 - f30) / planning_period }

###### ###### ###### ############ ######

# Build optimization problem and set up solver

###### ############ ############ ######

#Profit components 
build_profit_components <- function(scenarios_data, arable_cap) { 
  
  # Profit matrix 
  pmat <- cbind(   
    other_land            = rep(0, nrow(scenarios_data)), 
    planted_forest        = scenarios_data$planted_forest,
    arable                = scenarios_data$arable,
    permanent_cropland    = scenarios_data$permanent_cropland,
    permanent_pasture     = scenarios_data$permanent_pasture,
    new_deforestation     = scenarios_data$new_deforestation,
    naturally_regenerated = scenarios_data$naturally_regenerated
  )
  
  lu_cols <- colnames(pmat)
  ar_col <- "arable" 
  non_ar_cols <- setdiff(lu_cols, ar_col) 
  
  # Minimum profits
  p_min <- apply(pmat, 1, min) 
  
  # Maximum profits s.t. arable cap
  p_star <- apply(pmat, 1, function(row) { 
    
    arable_value <- as.numeric(row[ar_col]) 
    max_non_arable <- max(as.numeric(row[non_ar_cols])) 
    
    if (max_non_arable > arable_value) {
      max_non_arable
    } else {
      arable_cap * arable_value + (1 - arable_cap) * max_non_arable  
    }
  })

  ranges <- p_star - p_min #
  
  list( pmat = pmat, p_star = p_star, ranges = ranges) 
} 


#Build problem
build_lp_problem <- function(
    f0, #                       vector of initial LULC shares
    arable_cap, 
    other_land_min, 
    planted_forest_target, 
    pmat, 
    p_star, 
    ranges
) { 
  
  # Dimensions
  M <- nrow(pmat) # Scenarios
  L <- ncol(pmat) # LULC 
  
  # Objective funtion
  obj <- c(rep(0, L), 1) 
  
  # Constraints
  ncon <- M + 7 
  A <- matrix(0, nrow = ncon, ncol = L + 1) 
  b <- numeric(ncon) # RHS vector
  dir <- character(ncon) # Vector of constraint directions
  
  row <- 0
  
  # Regret 
  for (u in seq_len(M)) { 
    row <- row + 1
    pu  <- pmat[u, ] 
    rng <- ranges[u] 
    
    # Definition
    if (is.finite(rng) && rng > 0) {
      A[row, 1:L]   <- (p_star[u] - pu) / rng * 100 
      A[row, L + 1] <- -1 
    } else {
      A[row, L + 1] <- -1
    }
    
    dir[row] <- "<=" 
    b[row]   <- 0 # RHS
  }
  
  # Constraint: Sum shares == 1 
  row <- row + 1
  A[row, 1:L] <- 1
  dir[row] <- "=="
  b[row] <- 1
  
  fn_idx <- which(colnames(pmat) == "naturally_regenerated")
  fd_idx <- which(colnames(pmat) == "new_deforestation")
  fp_idx <- which(colnames(pmat) == "permanent_pasture")
  ol_idx <- which(colnames(pmat) == "other_land")
  pf_idx <- which(colnames(pmat) == "planted_forest")
  ar_idx <- which(colnames(pmat) == "arable")
  
  # Constraint: Naturally regenerated forest area is reduced by area deforested
  row <- row + 1
  A[row, fn_idx] <- 1
  A[row, fd_idx] <- 1
  dir[row] <- "=="
  b[row] <- f0["naturally_regenerated"]
  
  # Constraint: Pasture cap
  row <- row + 1
  A[row, fp_idx] <- 1
  dir[row] <- "<="
  b[row] <- f0["permanent_pasture"]
  
  # Constraint: Available non-forestland cap
  row <- row + 1
  for (j in seq_len(L)) if (j != fn_idx && j != fd_idx) A[row, j] <- 1 
  dir[row] <- "<="
  b[row] <- 1 - f0["naturally_regenerated"] # RHS = 1 - initial forest share
  
  # Constraint: Other land minimum
  row <- row + 1 
  A[row, ol_idx] <- 1
  dir[row] <- ">="
  b[row] <- other_land_min
  
  # Constraint: Planted forest target
  row <- row + 1
  A[row, pf_idx] <- 1
  dir[row] <- "=="
  b[row] <- planted_forest_target
  
  # Constraint: Arable + permanent cap
 
    row <- row + 1
    crop_idx <- which(colnames(pmat) %in% c("arable", "permanent_cropland"))
    A[row, crop_idx] <- 1
    dir[row] <- "<="
    b[row] <- arable_cap
  
  # Return list for lpSolve()
  list(obj = obj, A = A, dir = dir, b = b, L = L)
}

# Solve
solve_lp_problem <- function(obj, A, dir, b) {
  out <- lpSolve::lp(
    direction = "min",
    objective.in = obj,
    const.mat = A,
    const.dir = dir,
    const.rhs = b
  )
  
  if (!is.list(out) || is.null(out$status) || out$status != 0) return(NULL)
  out
}

# Solve for 1 period
solve_robust_optimization <- function( 
    scenarios_data,
    f0,
    arable_cap,
    other_land_min,
    planted_forest_target,
    total_area_1000ha,
    planning_period #,
) {
  # Build profits from scenarios
  comps <- build_profit_components(
    scenarios_data = scenarios_data,
    arable_cap = arable_cap
  ) # e.g. comps$mat, comps$p_star, or comps$ranges
  
  # Build LP 
  lpdat <- build_lp_problem( 
    f0 = f0,
    arable_cap = arable_cap,
    other_land_min = other_land_min,
    planted_forest_target = planted_forest_target,
    pmat = comps$pmat,
    p_star = comps$p_star,
    ranges = comps$ranges
  )
  
  # Solve
  out <- solve_lp_problem(lpdat$obj, lpdat$A, lpdat$dir, lpdat$b) 
  if (is.null(out)) return(NULL) 
  
  # Store
  sol <- out$solution 
  L <- lpdat$L 
  
  # 30-year shares
  f30 <- sol[1:L] 
  names(f30) <- colnames(comps$pmat) 
  f30 <- normalize_lu_shares(f30) 
  
  # Five-year shares
  f5 <- scale_to_5_years(f0, f30, planning_period)
  f5 <- normalize_lu_shares(f5)
  
  # Deforestation 
  annual_defor_rate <- (f0["naturally_regenerated"] - f5["naturally_regenerated"]) /
    5 / f0["naturally_regenerated"] 
  
  forest_loss_5y_fraction <- f0["naturally_regenerated"] - f5["naturally_regenerated"] # Share of total
  simulated_forest_loss_1000ha <- forest_loss_5y_fraction * total_area_1000ha # Area
  
  #Results
  result <- list(
    f5 = f5,
    annual_defor_rate = annual_defor_rate,
    annual_loss_1000ha = simulated_forest_loss_1000ha / 5
  )
  
  result
}

# Loop over periods

##  Single period loop (A)
run_year_batch <- function(
  period_id,
  year_in_period,
  f0,  #Current state
  # Period parameters, profit + uncertainty vectors
  arable_cap,
  other_land_min,
  planted_forest_target,
  profit_coeffs,
  uncertainty_sd,
  total_area_1000ha,
  planning_period,
  # Monte Carlo 
  n_scenarios, 
  mc_runs,
  upside_mult,
  downside_mult,
  # RNG 
  base_seed
) {
  
  #Store
  annual_rates <- numeric(0) 
  annual_loss_1000ha <- numeric(0)
  f5_list <- list() 
  
  # Loop over Monte Carlo 
  for (mc_run in seq_len(mc_runs)) {
    
    seed <- base_seed + 100000L * as.integer(period_id) + 1000L * 
      as.integer(year_in_period) + as.integer(mc_run)
    
    # Draws for this MC run
    scenarios <- generate_uncertainty_scenarios(
      n_scenarios = n_scenarios,
      profit_coeffs = profit_coeffs,
      uncertainty_sd = uncertainty_sd,
      upside_mult = upside_mult,
      downside_mult = downside_mult,
      seed = seed
    ) # Tibble
    
    # Solve for this set of scenarios
    opt <- solve_robust_optimization(
      scenarios_data = scenarios,
      f0 = f0,
      arable_cap = arable_cap,
      other_land_min = other_land_min,
      planted_forest_target = planted_forest_target,
      total_area_1000ha = total_area_1000ha,
      planning_period = planning_period #, 
    ) 
    
    # Successful runs
    if (!is.null(opt)) { 
      annual_rates <- c(annual_rates, opt$annual_defor_rate)
      annual_loss_1000ha <- c(annual_loss_1000ha, opt$annual_loss_1000ha)
      f5_list[[length(f5_list) + 1]] <- opt$f5 
    }
  }
  
  if (length(annual_rates) == 0) return(list(year_summary = NULL, avg_f5 = NULL)) 
  
  # Average f5 vectors
  f5_mat <- do.call(rbind, f5_list) 
  avg_f5 <- normalize_lu_shares(colMeans(f5_mat))
  
  # Summary
  year_summary <- tibble(
    mc_runs_attempted = mc_runs,
    mc_runs_success = length(annual_rates), 
    annual_defor_rate_mean = mean(annual_rates),
    annual_defor_rate_se = sd(annual_rates) / sqrt(length(annual_rates)), 
    annual_defor_1000ha_mean = mean(annual_loss_1000ha),
    annual_defor_1000ha_se = sd(annual_loss_1000ha) / sqrt(length(annual_loss_1000ha))
  )
  
  list(year_summary = year_summary, avg_f5 = avg_f5) 
}


# Loop B: Repeat Loop A for all periods 
run_all_periods_endogenous <- function(periods_df, 
                                       f0_period1)  {  
  
  total_area_1000ha <- sum(initial_areas_1000ha) # Sum initial areas
  
  f0_current <- f0_period1  
  all_year_results <- list() 
  
  # Loop over periods
  for (p in seq_len(nrow(periods_df))) {
    period_id <- periods_df$period[p] 
    start_year <- periods_df$start_year[p] 
    
    # Period constraints
    arable_cap <- periods_df$arable_cap[p]
    other_land_min <- periods_df$other_land_min[p]
    planted_forest_target <- periods_df$planted_forest_target[p]
    
    # Period vectors
    profit_rows <- profits %>% filter(period == period_id)
    profit_coeffs <- setNames(profit_rows$profit, profit_rows$land_use)
    uncertainty_sd <- setNames(profit_rows$uncertainty_sd, profit_rows$land_use)
    
    year_summaries <- list() 
    avg_f5_years <- list() 
    
    for (yip in 1:years_per_period) { 
      
      # MC for current year
      batch <- run_year_batch( 
        period_id = period_id,
        year_in_period = yip,
        f0 = f0_current,
        arable_cap = arable_cap,
        other_land_min = other_land_min,
        planted_forest_target = planted_forest_target,
        profit_coeffs = profit_coeffs,
        uncertainty_sd = uncertainty_sd,
        total_area_1000ha = total_area_1000ha,
        planning_period = planning_period,
        n_scenarios = n_scenarios,
        mc_runs = mc_runs,
        upside_mult = upside_mult,
        downside_mult = downside_mult,
        base_seed = master_seed_used
      )
      
      if (!is.null(batch$year_summary)) {
        yr <- start_year + (yip - 1) 
        year_summaries[[length(year_summaries) + 1]] <- batch$year_summary %>% 
          mutate(period = period_id, year = yr) 
        avg_f5_years[[length(avg_f5_years) + 1]] <- batch$avg_f5 
      }
    } 
    
    #Annual summaries for this period
    year_df_p <- bind_rows(year_summaries) %>% filter(year <= periods_df$end_year[p])
    all_year_results[[p]] <- year_df_p 
    

    if (length(avg_f5_years) == 0) stop(paste("All runs failed in period", period_id))
    
    # Update f0 for next period
    next_state <- colMeans(do.call(rbind, avg_f5_years))
    
    # Cleared land must become existing pasture
    next_state["permanent_pasture"] <- next_state["permanent_pasture"] + next_state["new_deforestation"]
    next_state["new_deforestation"] <- 0
    f0_current <- normalize_lu_shares(next_state)
  }    
  
  # Combine
  year_df <- bind_rows(all_year_results) %>% 
    mutate(
      annual_defor_ha_mean = annual_defor_1000ha_mean * 1000, 
      annual_defor_ha_se   = annual_defor_1000ha_se * 1000
    )
  
  # All annual results (tibble)
  year_df
}

# Baseline correction
apply_baseline_correction <- function(year_df, obs_mean_ha, years, apply_years, 
                                      correction_col_name = "baseline_correction_ha") {
  
  model_mean <- year_df %>% 
    filter(year %in% years) %>% 
    summarise(mu = mean(annual_defor_ha_mean, na.rm = TRUE)) %>% 
    pull(mu)  
  
  if (!is.finite(model_mean)) stop("Model baseline mean is NA/Inf (check coverage of years).")
  
  #Align the simulated reference mean with the observed reference rate
  correction_ha <- obs_mean_ha - model_mean  # Note: Only after selecting a calibration (external to this replication script)
  
  #Baseline correction only applied within the correction window
  correction_by_year <- ifelse(year_df$year %in% apply_years, correction_ha, 0)
  
  out <- year_df %>%
    mutate(
      annual_defor_ha_mean = annual_defor_ha_mean + correction_by_year,
      annual_defor_1000ha_mean = annual_defor_1000ha_mean + correction_by_year / 1000,
      !!correction_col_name := correction_by_year 
    )
  
  attr(out, "baseline_correction_ha") <- correction_ha 
  out 
}


######################################################################################
######################################################################################

# Run model and plot results

######################################################################################
######################################################################################

# Initial composition
total_area_1000ha <- sum(initial_areas_1000ha)
f0_period1 <- normalize_lu_shares(initial_areas_1000ha / total_area_1000ha) 

# Run 
model_raw <- run_all_periods_endogenous(periods_df, f0_period1)
model_df <- model_raw
obs_years <- comparison_df$year 

# Units and reference lines
observed_ha <- comparison_df$observed * obs_to_ha_mult
published_baseline_ha <- comparison_df$published_baseline * obs_to_ha_mult
published_baseline_sem_ha <- comparison_df$published_baseline_sem * obs_to_ha_mult

#Calculate baseline corrections using the (re)calibration periods for this country
model_uncorrected_df <- model_df
corrected_parts <- list()
correction_summary <- correction
correction_summary$baseline_correction_ha <- NA  

for (i in seq_len(nrow(correction))) {
  
  #Get years for row i
  baseline_years <- correction$observed_start_year[i]:correction$observed_end_year[i]
  baseline_years_model <- correction$model_start_year[i]:correction$model_end_year[i]
  apply_years <- correction$apply_start_year[i]:correction$apply_end_year[i]
  
  #Skip correction windows beyond this run if...
  if (!any(model_uncorrected_df$year %in% apply_years)) next
  
  if (!all(baseline_years_model %in% model_uncorrected_df$year)) {
    stop("The full reference period(s) needed for baseline correction")
  }
  
  # Calculate observed baseline mean
  obs_baseline_mean <- tibble(year = obs_years, v = observed_ha) %>%
    filter(year %in% baseline_years) %>% 
    summarise(mu = mean(v, na.rm = TRUE)) %>% 
    pull(mu) 
  
  #Perform correction
  corrected <- apply_baseline_correction(model_uncorrected_df, obs_mean_ha = obs_baseline_mean,
    years = baseline_years_model, apply_years = apply_years)
  
  correction_summary$baseline_correction_ha[i] <- attr(corrected, "baseline_correction_ha")
  
  corrected_parts[[i]] <- corrected %>%
    filter(year %in% apply_years) %>%
    mutate(correction_id = correction$correction_id[i])
}

model_df <- bind_rows(corrected_parts) %>% arrange(year) 

if (!identical(model_df$year, sort(model_uncorrected_df$year))) {
  stop("Baseline correction window has to cover each reported year once and only once")
}

# Only keep corrections applied during this run
correction_summary <- correction_summary %>% filter(!is.na(baseline_correction_ha))

# Plotting window
model_year_max <- max(model_df$year, na.rm = TRUE) 
obs_year_max <- max(obs_years, na.rm = TRUE) 

year_min <- forced_start_year 
year_max <- min(model_year_max, obs_year_max) 

if (year_min > year_max) stop("No overlapping years (check forced_start_year).")

# Data
model_plot_df <- model_df %>% 
  filter(year >= year_min, year <= year_max) %>% # 
  transmute( 
    year,
    series = "Demo simulation",
    value_ha = annual_defor_ha_mean,
    ymin = annual_defor_ha_mean - 3 * annual_defor_ha_se,
    ymax = annual_defor_ha_mean + 3 * annual_defor_ha_se
  )

obs_df <- tibble( 
  year = obs_years,
  series = "Observed",
  value_ha = observed_ha
) %>% 
  filter(year >= year_min, year <= year_max) 


pub_df <- tibble( 
  year = obs_years,
  series = "Published baseline", 
  value_ha = published_baseline_ha,
  sem_ha = published_baseline_sem_ha 
) %>%
  filter(year >= year_min, year <= year_max) %>%
  mutate( 
    ymin = value_ha - 3 * sem_ha,
    ymax = value_ha + 3 * sem_ha
  )

plot_lines_df <- bind_rows(
  model_plot_df %>% select(year, series, value_ha),
  pub_df %>% select(year, series, value_ha),
  obs_df %>% select(year, series, value_ha) # 
) %>% filter(is.finite(value_ha), is.finite(year)) 

# Small summary 
summary_tbl <- bind_rows( 
  model_plot_df %>% mutate(se_ha = (ymax - ymin) / 6) %>%  
    select(year, series, value_ha, se_ha), 
  pub_df %>% transmute(year, series, value_ha, se_ha = sem_ha), 
  obs_df %>% mutate(se_ha = NA_real_) %>% select(year, series, value_ha, se_ha) 
) %>%
  group_by(series) %>% 
  summarise( 
    n_years = sum(!is.na(value_ha)),
    mean_ha = mean(value_ha, na.rm = TRUE),
    sd_ha   = sd(value_ha, na.rm = TRUE),
    mean_se_ha = mean(se_ha, na.rm = TRUE),
    .groups = "drop" 
  ) 


# Plot
comparison_lines <- plot_lines_df %>% filter(series != "Demo simulation")

p <- ggplot() +
  geom_line(
    data = model_plot_df, aes(x = year, y = ymin),
    color = "#C56A3A", linewidth = 0.75, linetype = "dotted"
  ) +
  geom_line(
    data = model_plot_df, aes(x = year, y = ymax),
    color = "#C56A3A", linewidth = 0.75, linetype = "dotted"
  ) +
  geom_line(
    data = model_plot_df, aes(x = year, y = value_ha, color = series),
    linewidth = 1.25
  ) +
  geom_point(
    data = model_plot_df, aes(x = year, y = value_ha), color = "#D66F3D", size = 1.7
  ) +
  scale_color_manual(values = c( "Demo simulation" = "#D66F3D")) +
  
  scale_y_continuous(
    labels = function(x) x / 1000,
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  labs(
    title = paste("Simulated deforestation baseline:", COUNTRY),
    subtitle = seed_subtitle,
    x = "Year", y = "Deforestation (Kha / year)",color = NULL,
    caption = "Dotted bounds: ± 3 SEM from successful Monte Carlo runs within each year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8)
  )

print(p)
