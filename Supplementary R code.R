###########################################################
 

# README
#   
# This R script runs 50 consecutive linear optimisations of a future land-use 
# portfolio and calculates the annual deforestation to achieve it within the 
# planning period (T). The result is an average of 50 deforestation rates.
#
# It illustrates the counterfactual deforestation modeling approach introduced by
#
#       Knoke, T., Hanley, N., Roman-Cuesta, R. M., Groom, B., Venmans, F., & Paul, C. (2023). 
#       "Trends in tropical forest loss and the social value of emission reductions" 
#       (doi 10.1038/s41893-023-01175-9)
#
# Model structure and coefficients drawn from supplementary materials accompanying 
# the global deforestation analysis by Knoke, Cueva, Bingham, et al. (in prep)
#
# Specifically, this script is a mockup of the supplement "Brazil spreadsheet 1990_1994.xlsx"
# and directly adopts the initial LULC compositions and coefficients from that supplement,
# which is available with other optimization workbooks at
#
#       https://zenodo.org/records/8016364 
#
#
# It can simulate counterfactual, profit-oriented deforestation rates for Brazil 
# from 1990 to 1994. To do so, this script must be run in 5 consecutive iterations. The rates 
# can be assigned to the years 1990-1994 in the order of occurrence (i.e. randomly). 
#
# For the next 5-year period the LULC composition must be updated, as well as the profits, and constraints. 
#
# The start LULC composition of the next 5-year period is the average "LULC composition 
# after five years" of the five preceding repetitions.
#
# Contact the authors: Logan Bingham (logan@tum.de), Thomas Knoke (knoke@tum.de), Jonathan Fibich (jonathan.fibich@tum.de)
#
# Version 23 July 2025
###########################################################


library(tidyverse)
library(lpSolve)

# ========= 1. SCENARIO GENERATION FUNCTION =========

generate_uncertainty_scenarios <- function(n_scenarios = 1000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # profit coefficients from Excel p3 A3:I4
  profit_coeffs <- c(
    other_land = 0,
    planted_forest = 98,
    arable = 331,
    permanent_cropland = 46,
    permanent_pasture = 96,
    new_deforestation = 96,
    naturally_regenerated = 30
  )
  
  # Uncertainty from p3 L3:L5
  uncertainty_sd <- c(
    other_land = 0.0,
    planted_forest = 39.2,
    arable = 53.0,
    permanent_cropland = 4.6,
    permanent_pasture = 3.8,
    new_deforestation = 48.0,
    naturally_regenerated = 9.0
  )
  
  # from p2 AF45:AG46
  upside_mult <- 1.0
  downside_mult <- 2.0
  
  land_uses <- c("planted_forest", "arable", "permanent_cropland", 
                 "permanent_pasture", "new_deforestation", "naturally_regenerated")
  
  scenarios_matrix <- matrix(NA, nrow = n_scenarios, ncol = length(land_uses))
  colnames(scenarios_matrix) <- land_uses
  
  for (lu in land_uses) {
    expected <- profit_coeffs[lu]
    sd <- uncertainty_sd[lu]
    
    rand_values <- runif(n_scenarios, min = 0, max = 1)
    scenarios_matrix[, lu] <- expected + (upside_mult * sd) - rand_values * (upside_mult + downside_mult) * sd
  }
  
  scenarios_wide <- data.frame(
    scenario_id = 1:n_scenarios,
    scenarios_matrix
  )
  
  scenarios_long <- scenarios_wide %>%
    mutate(run_id = 1)
  
  return(list(
    scenarios_wide = scenarios_wide,
    scenarios_long = scenarios_long,
    parameters = list(
      profit_coeffs = profit_coeffs,
      uncertainty_sd = uncertainty_sd,
      upside_mult = upside_mult,
      downside_mult = downside_mult
    )
  ))
}

# ================== 2 INITIALIZE  ===================

# from p2, B16:I17
initial_areas_1000ha <- c(
  `Other land` = 12016,
  `Planted forest` = 3558,
  `Arable` = 47340,
  `Permanent cropland` = 9116,
  `Permanent pasture and grassland` = 178444,
  `New deforestation` = 0,
  `Naturally regenerated forest` = 585340
)

total_area_1000ha <- sum(initial_areas_1000ha)
f0 <- initial_areas_1000ha / total_area_1000ha

arable_cap <- 0.0776225332
other_land_min <- 0.12100384
planted_forest_target <- 0.013
planning_period <- 30

# ================== 3 FUNCTIONS =============================================

recreate_uncertainty_space <- function(scenarios_data) {
  uncertainty_recreated <- data.frame(
    `Uncertainty scenario` = scenarios_data$scenario_id,
    `Other land Uncertainty adjusted Indicator (+/- m × sd)` = 0,
    `Planted forest Uncertainty adjusted Indicator (+/- m × sd)` = scenarios_data$planted_forest,
    `Arable Uncertainty adjusted Indicator (+/- m × sd)` = scenarios_data$arable,
    `Permanent cropland Uncertainty adjusted Indicator (+/- m × sd)` = scenarios_data$permanent_cropland,
    `Permanent pasture and grassland Uncertainty adjusted Indicator (+/- m × sd)` = scenarios_data$permanent_pasture,
    `New deforestation Uncertainty adjusted Indicator (+/- m × sd)` = scenarios_data$new_deforestation,
    `Naturally regenerated forest Uncertainty adjusted Indicator (+/- m × sd)` = scenarios_data$naturally_regenerated,
    check.names = FALSE
  )
  
  land_use_cols <- grep("Uncertainty adjusted Indicator", colnames(uncertainty_recreated), value = TRUE)
  arable_col <- "Arable Uncertainty adjusted Indicator (+/- m × sd)"
  non_arable_cols <- setdiff(land_use_cols, arable_col)
  
  uncertainty_recreated$Minimum <- apply(uncertainty_recreated[, land_use_cols], 1, min)
  
  uncertainty_recreated$Maximum <- apply(uncertainty_recreated, 1, function(row) {
    arable_value <- as.numeric(row[arable_col])
    non_arable_values <- as.numeric(row[non_arable_cols])
    max_non_arable <- max(non_arable_values)
    
    if (max_non_arable > arable_value) {
      return(max_non_arable)
    } else {
      return(arable_cap * arable_value + (1 - arable_cap) * max_non_arable)
    }
  })
  
  uncertainty_recreated$`Range (maximum-minimum)` <- uncertainty_recreated$Maximum - uncertainty_recreated$Minimum
  
  return(uncertainty_recreated)
}

scale_to_5_years <- function(f0, f30, planning_period = 30) {
  f5 <- f0 - 5 * (f0 - f30) / planning_period
  return(f5)
}

solve_robust_optimization <- function(scenarios_data, run_id = 1) {
  
  uncertainty_data <- recreate_uncertainty_space(scenarios_data)
  
  pmat <- cbind(
    `Other land` = rep(0, nrow(uncertainty_data)),
    `Planted forest` = uncertainty_data$`Planted forest Uncertainty adjusted Indicator (+/- m × sd)`,
    `Arable` = uncertainty_data$`Arable Uncertainty adjusted Indicator (+/- m × sd)`,
    `Permanent cropland` = uncertainty_data$`Permanent cropland Uncertainty adjusted Indicator (+/- m × sd)`,
    `Permanent pasture and grassland` = uncertainty_data$`Permanent pasture and grassland Uncertainty adjusted Indicator (+/- m × sd)`,
    `New deforestation` = uncertainty_data$`New deforestation Uncertainty adjusted Indicator (+/- m × sd)`,
    `Naturally regenerated forest` = uncertainty_data$`Naturally regenerated forest Uncertainty adjusted Indicator (+/- m × sd)`
  )
  
  M <- nrow(pmat)
  L <- ncol(pmat)
  
  p_star <- uncertainty_data$Maximum
  p_min <- uncertainty_data$Minimum
  ranges <- uncertainty_data$`Range (maximum-minimum)`
  
  obj <- c(rep(0, L), 1)
  
  ncon <- M + 7
  
  A <- matrix(0, nrow = ncon, ncol = L + 1)
  b <- numeric(ncon)
  dir <- character(ncon)
  
  row <- 0
  
  for (u in 1:M) {
    row <- row + 1
    pu <- pmat[u, ]
    rng <- ranges[u]
    
    if (rng > 0) {
      A[row, 1:L] <- (p_star[u] - pu) / rng * 100
      A[row, L+1] <- -1
    } else {
      A[row, L+1] <- -1
    }
    dir[row] <- "<="
    b[row] <- 0
  }
  
  row <- row + 1
  A[row, 1:L] <- 1
  dir[row] <- "=="
  b[row] <- 1
  
  fn_idx <- which(colnames(pmat) == "Naturally regenerated forest")
  fd_idx <- which(colnames(pmat) == "New deforestation")
  row <- row + 1
  A[row, fn_idx] <- 1
  A[row, fd_idx] <- 1
  dir[row] <- "=="
  b[row] <- f0["Naturally regenerated forest"]
  
  fp_idx <- which(colnames(pmat) == "Permanent pasture and grassland")
  row <- row + 1
  A[row, fp_idx] <- 1
  dir[row] <- "<="
  b[row] <- f0["Permanent pasture and grassland"]
  
  row <- row + 1
  for (j in 1:L) {
    if (j != fn_idx && j != fd_idx) {
      A[row, j] <- 1
    }
  }
  dir[row] <- "<="
  b[row] <- 1 - f0["Naturally regenerated forest"]
  
  ol_idx <- which(colnames(pmat) == "Other land")
  row <- row + 1
  A[row, ol_idx] <- 1
  dir[row] <- ">="
  b[row] <- other_land_min
  
  pf_idx <- which(colnames(pmat) == "Planted forest")
  row <- row + 1
  A[row, pf_idx] <- 1
  dir[row] <- "=="
  b[row] <- planted_forest_target
  
  ar_idx <- which(colnames(pmat) == "Arable")
  row <- row + 1
  A[row, ar_idx] <- 1
  dir[row] <- "<="
  b[row] <- arable_cap
  
  out <- lp(
    direction = "min",
    objective.in = obj,
    const.mat = A,
    const.dir = dir,
    const.rhs = b
  )
  
  if (out$status != 0) {
    return(NULL)
  }
  
  sol <- out$solution
  f30_solved <- sol[1:L]
  names(f30_solved) <- colnames(pmat)
  beta_solved <- sol[L+1]
  
  f5_solved <- scale_to_5_years(f0, f30_solved, planning_period)
  
  natural_forest_5y_1000ha <- f5_solved["Naturally regenerated forest"] * total_area_1000ha
  
  annual_defor_rate <- (f0["Naturally regenerated forest"] - f5_solved["Naturally regenerated forest"]) / 5 / f0["Naturally regenerated forest"]
  
  forest_loss_5y_fraction <- f0["Naturally regenerated forest"] - f5_solved["Naturally regenerated forest"]
  simulated_forest_loss_1000ha <- forest_loss_5y_fraction * total_area_1000ha
  
  profits_achieved <- as.vector(pmat %*% f30_solved)
  
  regrets <- numeric(M)
  for (u in 1:M) {
    rng <- ranges[u]
    if (rng > 0) {
      regrets[u] <- (p_star[u] - profits_achieved[u]) / rng * 100
    }
  }
  
  binding_scenarios <- which(abs(regrets - beta_solved) < 1e-4)
  
  return(list(
    run_id = run_id,
    beta_solved = beta_solved,
    f30_solved = f30_solved,
    f5_solved = f5_solved,
    natural_forest_5y_1000ha = natural_forest_5y_1000ha,
    annual_defor_rate = annual_defor_rate,
    simulated_forest_loss_1000ha = simulated_forest_loss_1000ha,
    binding_scenarios = binding_scenarios,
    n_scenarios = M,
    regrets = regrets,
    p_star = p_star,
    ranges = ranges
  ))
}

# ========= 4. RUN OPTIMIZATIONS =========

n_runs <- 50

optimization_results <- list()
scenario_sets <- list()

all_scenarios_list <- list()

for (run in 1:n_runs) {
  scenario_result <- generate_uncertainty_scenarios(n_scenarios = 1000, seed = NULL)
  scenario_result$scenarios_long$run_id <- run
  scenario_sets[[run]] <- scenario_result
  all_scenarios_list[[run]] <- scenario_result$scenarios_long
}

all_scenarios_combined <- bind_rows(all_scenarios_list)

for (run in 1:n_runs) {
  run_scenarios <- all_scenarios_list[[run]]
  opt_result <- solve_robust_optimization(run_scenarios, run_id = run)
  
  if (!is.null(opt_result)) {
    optimization_results[[run]] <- opt_result
  }
}

# ========= 5. SUMMARY STATISTICS =========

successful_runs <- optimization_results[!sapply(optimization_results, is.null)]
n_successful <- length(successful_runs)

if (n_successful > 0) {
  allocations_30y <- do.call(rbind, lapply(successful_runs, function(x) x$f30_solved))
  allocations_5y <- t(apply(allocations_30y, 1, scale_to_5_years, f0 = f0, planning_period = planning_period))
  colnames(allocations_5y) <- colnames(allocations_30y)
  
  avg_allocations_30y <- colMeans(allocations_30y)
  avg_allocations_5y <- colMeans(allocations_5y)
  
  betas <- sapply(successful_runs, function(x) x$beta_solved)
  all_regrets <- unlist(lapply(successful_runs, function(x) x$regrets))
  
  defor_fraction_30y <- allocations_30y[, "New deforestation"] / f0["Naturally regenerated forest"]
  annual_defor_rate_30y <- defor_fraction_30y / 30
  remaining_forest_30y <- allocations_30y[, "Naturally regenerated forest"]
  forest_loss_30y <- f0["Naturally regenerated forest"] - remaining_forest_30y
  
  forest_change_5y <- allocations_5y[, "Naturally regenerated forest"] - f0["Naturally regenerated forest"]
  annual_defor_rate_5y <- -forest_change_5y / 5 / f0["Naturally regenerated forest"]
  new_defor_5y <- allocations_5y[, "New deforestation"]
  
  change_30y <- sweep(allocations_30y, 2, f0[colnames(allocations_30y)], "-")
  change_5y <- sweep(allocations_5y, 2, f0[colnames(allocations_5y)], "-")
  
  summary_metrics <- list(
    avg_allocations_30y = avg_allocations_30y,
    avg_allocations_5y = avg_allocations_5y,
    change_30y = colMeans(change_30y),
    change_5y = colMeans(change_5y),
    defor_metrics_30y = list(
      mean_fraction = mean(defor_fraction_30y),
      sd_fraction = sd(defor_fraction_30y),
      mean_annual_rate = mean(annual_defor_rate_30y),
      sd_annual_rate = sd(annual_defor_rate_30y)
    ),
    defor_metrics_5y = list(
      mean_forest_change = mean(forest_change_5y),
      sd_forest_change = sd(forest_change_5y),
      mean_annual_rate = mean(annual_defor_rate_5y),
      sd_annual_rate = sd(annual_defor_rate_5y)
    ),
    beta_stats = list(
      mean = mean(betas),
      sd = sd(betas),
      min = min(betas),
      max = max(betas)
    ),
    n_runs = n_successful
  )
  
  # === DEFORESTATION AREA METRICS ===
  forest_losses_1000ha <- sapply(successful_runs, function(x) x$simulated_forest_loss_1000ha)
  
  mean_forest_loss_1000ha <- mean(forest_losses_1000ha)
  se_forest_loss_1000ha <- sd(forest_losses_1000ha) / sqrt(length(forest_losses_1000ha))
  
  # Convert 5-year loss to 1-year loss
  annual_forest_loss_1000ha <- mean_forest_loss_1000ha / 5
  annual_se_forest_loss_1000ha <- se_forest_loss_1000ha / 5
  
  cat("\nSimulated annual deforestation for 50 iterations in 1000ha \n")
  print(round(unname(forest_losses_1000ha) / 5, 2))
  cat(sprintf("Average annual deforestation rate: %.6f\n", mean(annual_defor_rate_5y)))
  cat(sprintf("1-year deforestation (1000 ha): %.2f\n", annual_forest_loss_1000ha))
  cat(sprintf("Standard error (1000 ha): %.2f\n", annual_se_forest_loss_1000ha))

  
  summary_metrics$forest_loss_1000ha <- list(
    mean = mean_forest_loss_1000ha,
    se = se_forest_loss_1000ha,
    sd = sd(forest_losses_1000ha),
    min = min(forest_losses_1000ha),
    max = max(forest_losses_1000ha)
  )
}




