# ###########################################################
# 
# 
# README
# 
# This script simulates a counterfactual trajectory of hypothetical deforestation rates 
# by assuming that land managers would consider profit expectations for seven 
# land-use/land-cover (LULC) types to make land allocation (and deforestation) decisions. 
# The simulated counterfactuals can serve as a baseline to assess observed deforestation rates. 
# This script enables users to reproduce a multi-period counterfactual simulation for Brazil. 
# It repeatedly solves a robust linear program to choose a future LULC portfolio under 
# uncertain profit expectations for a set of regret-minimising landowner agents. It 
# converts the implied 5-year change in forest share into an annual deforestation series. 
# Agents make 30-year plans and pursue them in 5-year timesteps. After each timestep, 
# they have an opportunity to re-evaluate their plan (see Methods). Each period is 
# represented by five simulated years. for each year, the model runs 50 Monte Carlo repetitions
# in which profit expectations are randomly drawn across 1000 uncertainty scenarios, and
# a robust linear program is solved. Each period simulated five times to capture different 
# random draws from the uncertainty space. Each repetition is assigned to one year of 
# the period in the order of occurrence (i.e. randomly). At the end of each period, 
#the average composition is used to initalise the following period. 
# 
# The script is designed to illustrate the counterfactual deforestation modelling 
# workflow introduced by
# 
#     Knoke, T., Hanley, N., Roman-Cuesta, R. M., Groom, B., Venmans, F., & Paul, C. (2023).
#     "Trends in tropical forest loss and the social value of emission reductions" 
#     (doi 10.1038/s41893-023-01175-9)
# 
# The model structure and coefficients have been drawn from the supplementary materials 
# published at:
# 
# https://zenodo.org/records/8016364 
# 
# There, several workbooks for 5-year periods are stored, which facilitate the 
# simulation of an annual deforestation rate using Frontline Solver. 
# 
# Runtime is less than a minute on a standard machine.
# 
# The initial LULC composition is derived from the "Brazil spreadsheet 1990_1994.xlsx" 
# file (via the provided RDS). Subsequent periods are initialised based on the average 
# LULC composition of  the previous period. Coefficients, profits, and constraints 
# for each period and country are also available in the supplementary materials.
# 
# Outputs: A figure of the profit-oriented counterfactual deforestation trajectory 
# given a specific random seed.
# 
# IMPORTANT: The parameters necessary to run this script are in a separate RDS file, 
# which this script stores in the variable "inputs." For the script to function, 
# save the RDS file in the same folder as the script or provide a full filepath.
# 
# Contact the authors: Logan Bingham (logan@tum.de), Thomas Knoke (knoke@tum.de), 
# Jonathan Fibich (jonathan.fibich@tum.de), Caroline von Webenau (c.webenau@gmx.de)
# 
# Version 12 January 2026
# 
# ###########################################################


# Install packages and read in data
if (!requireNamespace("pacman", quietly = TRUE)) {install.packages("pacman")}

suppressPackageStartupMessages({pacman::p_load(dplyr, tibble, ggplot2, lpSolve)})

INPUT_RDS <- "brazil_multiperiod_data.rds"

inputs <- readRDS(INPUT_RDS)

# Settings - modify
DETERMINISTIC <- FALSE   # TRUE = reproduces simulation for a given DEMO_SEED, set to FALSE for Monte Carlo with random seed 
DEMO_SEED     <- 1L     # only used if DETERMINISTIC = TRUE


# Standard parameters (do not modify)
planning_period  <- 30
years_per_period <- 5
mc_runs          <- 50
n_scenarios      <- 1000
upside_mult   <- 1.0
downside_mult <- 2.0  
forced_start_year <- 1990 
baseline_years <- 2001:2004 # Calibration period in observed series
baseline_years_model <- 2000:2004  # Smoothed average of simulated series

# Seed management
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

if (isTRUE(DETERMINISTIC)) {
  set.seed(as.integer(DEMO_SEED))
  master_seed_used <- as.integer(DEMO_SEED) 
} else {
  master_seed_used <- as.integer(as.numeric(Sys.time())) %% .Machine$integer.max
  if (master_seed_used == 0L) master_seed_used <- 1L
  set.seed(master_seed_used)
}

seed_subtitle <- paste("seed:", master_seed_used) # To replicate an interesting run, set DEMO_SEED to this value and DETERMINISTIC to TRUE

periods_df  <- inputs$periods_df
initial_areas_1000ha <- inputs$initial_areas_1000ha
comparison_df <- inputs$comparison_df
obs_to_ha_mult <- 1000 # units 

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
    upside_mult, # 
    downside_mult, # 
    seed
) {
  if (length(seed) != 1 || is.na(seed)) stop("seed must be a single value and not NA") 
  set.seed(as.integer(seed)) 
  
  
  land_uses <- c(
    "planted_forest", "arable", "permanent_cropland",
    "permanent_pasture", "new_deforestation", "naturally_regenerated"
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


# SUBSECTION: Build problem and solver

    # Part 1: Profit components 
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
        
        # Maximum profits (subject to arable cap)
        p_star <- apply(pmat, 1, function(row) { 
                                                  
          arable_value <- as.numeric(row[ar_col]) 
          max_non_arable <- max(as.numeric(row[non_ar_cols])) 
          
          if (max_non_arable > arable_value) {
            max_non_arable
          } else {
            arable_cap * arable_value + (1 - arable_cap) * max_non_arable  
          }
        })
        
        # Ranges 
        ranges <- p_star - p_min #
        
        #Store
        list( 
          pmat = pmat,
          p_star = p_star,
          ranges = ranges
        ) 
    } 
    
    
    # Part 2: Build LP problem
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
        
        # Constraint: Arable cap
        row <- row + 1
        A[row, ar_idx] <- 1
        dir[row] <- "<="
        b[row] <- arable_cap
        
        # Return list for lpSolve()
        list(obj = obj, A = A, dir = dir, b = b, L = L)
    }
    
    # Part 3: Solve
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
    
    # Part 4: Obtain single period solution
    solve_robust_optimization <- function( 
        scenarios_data,
        f0,
        arable_cap,
        other_land_min,
        planted_forest_target,
        total_area_1000ha,
        planning_period,
        return_debug = FALSE
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
        
        # Solve LP
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
        
        # Annual deforestation rate
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
        
        # Optional debug
        if (isTRUE(return_debug)) {
          result$debug <- list(
            pmat = comps$pmat,
            p_star = comps$p_star,
            ranges = comps$ranges,
            lp = list(obj = lpdat$obj, A = lpdat$A, dir = lpdat$dir, b = lpdat$b),
            lp_solution = out
          )
        }
        
        result
    }

# Subsection: Loop across periods

# Loop A: Single period simulation
run_year_batch <- function(
      # IDs
      period_id,
      year_in_period,
      #Current state
      f0,
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
    
    #Storage
    annual_rates <- numeric(0) 
    annual_loss_1000ha <- numeric(0)
    f5_list <- list() 
    
    # Loop over Monte Carlo 
    for (mc_run in seq_len(mc_runs)) {
     
      seed <- base_seed + 100000L * as.integer(period_id) + 1000L * as.integer(year_in_period) + as.integer(mc_run)
                         
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
        planning_period = planning_period, 
        return_debug = FALSE
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
    
    # Return list
    list(year_summary = year_summary, avg_f5 = avg_f5) 
}


# Loop B: Repeat Loop A for all periods
run_all_periods_endogenous <- function(periods_df, 
                                       f0_period1)  {  
 
  total_area_1000ha <- sum(initial_areas_1000ha) # Sum initial areas  (check units)
  
  # Period-period chaining and storage
  f0_current <- f0_period1  
  all_year_results <- list() 
  
  # Loop over periods
  for (p in seq_len(nrow(periods_df))) {
    period_id <- periods_df$period[p] 
    start_year <- periods_df$start_year[p] 
    
    # Period-specific constraints
    arable_cap <- periods_df$arable_cap[p]
    other_land_min <- periods_df$other_land_min[p]
    planted_forest_target <- periods_df$planted_forest_target[p]
    
    # Period-specific vectors
    profit_coeffs <- periods_df$profit_coeffs[[p]] 
    uncertainty_sd <- periods_df$uncertainty_sd[[p]]
    
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
    year_df_p <- bind_rows(year_summaries) 
    all_year_results[[p]] <- year_df_p 
    
    # Update f0 for next period
    if (length(avg_f5_years) == 0) stop(paste("All runs failed in period", period_id))
    f0_current <- normalize_lu_shares(colMeans(do.call(rbind, avg_f5_years))) 
  }                                                                       
  
  # Combine
  year_df <- bind_rows(all_year_results) %>% 
    mutate(
      annual_defor_ha_mean = annual_defor_1000ha_mean * 1000, # Units
      annual_defor_ha_se   = annual_defor_1000ha_se * 1000
    )
  
  # Tibble of all annual results
  year_df
}


# Baseline correction
apply_baseline_correction <- function(year_df, obs_mean_ha, years, correction_col_name = "baseline_correction_ha") {
  
  model_mean <- year_df %>% 
    filter(year %in% years) %>% 
    summarise(mu = mean(annual_defor_ha_mean, na.rm = TRUE)) %>% 
    pull(mu)  
  
  # Check
  if (!is.finite(model_mean)) stop("Model baseline mean is NA/Inf (check coverage of years).")
  
  # Correction delta - Only happens after calibration (external to this script!)
  correction_ha <- obs_mean_ha - model_mean 
  
  # Apply correction
  out <- year_df %>%
    mutate(
      annual_defor_ha_mean     = annual_defor_ha_mean + correction_ha, 
      annual_defor_1000ha_mean = annual_defor_1000ha_mean + correction_ha / 1000, 
      !!correction_col_name := correction_ha 
    )
  
  # Store
  attr(out, "baseline_correction_ha") <- correction_ha 
  out 
}


######################################################################################
######################################################################################

# Run model and plot

######################################################################################
######################################################################################

    # Initial composition
    total_area_1000ha <- sum(initial_areas_1000ha)
    f0_period1 <- normalize_lu_shares(initial_areas_1000ha / total_area_1000ha) 
    
    # Run model for all periods
    model_df <- run_all_periods_endogenous(periods_df, f0_period1) 
    obs_years <- comparison_df$year 
    # Units
    observed_ha <- comparison_df$observed * obs_to_ha_mult
    published_baseline_ha <- comparison_df$published_baseline * obs_to_ha_mult
    published_baseline_sem_ha <- comparison_df$published_baseline_sem * obs_to_ha_mult
    
    # Baseline correction
    obs_baseline_mean <- tibble(year = obs_years, v = observed_ha) %>% 
      filter(year %in% baseline_years) %>% 
      summarise(mu = mean(v, na.rm = TRUE)) %>% 
      pull(mu) 
  
    model_df <- apply_baseline_correction(model_df, obs_mean_ha = obs_baseline_mean, years = baseline_years_model)
    baseline_correction_ha <- attr(model_df, "baseline_correction_ha") 
    
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
    ) %>%
      filter(is.finite(value_ha), is.finite(year)) 

    # Small summary if desired
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
      ) %>% 
      mutate(
        baseline_correction_ha = ifelse(series == "Demo simulation", baseline_correction_ha, NA_real_) # add baseline correction column: if series name matches, put corrected val, otherwise numeric NA
      )

# Plot
    plot_lines_df <- model_plot_df %>% select(year, series, value_ha)
    
    p <- ggplot() +
      geom_ribbon(
        data = model_plot_df,
        aes(x = year, ymin = ymin, ymax = ymax),
        alpha = 0.15
      ) +
      geom_line(
        data = plot_lines_df,
        aes(x = year, y = value_ha, color = series),
        linewidth = 1
      ) +
      geom_point(
        data = plot_lines_df,
        aes(x = year, y = value_ha, color = series),
        size = 1.6
      ) +
      scale_y_continuous(
        limits = c(0, NA),
        labels = function(x) x / 1000
      ) +
      labs(
        title = "Counterfactual deforestation: demo simulation",
        subtitle = paste("Ribbons: ± 3×SEM", "|", seed_subtitle),
        x = "Year",
        y = "Deforestation (kha / year)",
        color = "Series"
      ) +
      theme_minimal()
    
    print(p)
    


    
    