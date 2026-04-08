# README: Counterfactual deforestation simulation demo – Brazil (multi-period)

## Description

This R script implements a counterfactual deforestation modelling approach based on historical profit estimates for alternative land-use/land-cover (LULC) types. It simulates a counterfactual trajectory of hypothetical deforestation rates across multiple periods by assuming that land managers allocate land across seven LULC categories based on uncertain profit expectations.

The script reproduces a multi-period counterfactual simulation for Brazil. It repeatedly solves a robust linear program to choose future land-use portfolios for regret-minimising agents, converts the implied 5-year change in forest share into an annual deforestation series, and updates land-use composition endogenously after the initial period.

The methodology is based on the approach introduced by:

Knoke, T., Hanley, N., Roman-Cuesta, R. M., Groom, B., Venmans, F., & Paul, C. (2023).  
*"Trends in tropical forest loss and the social value of emission reductions"* (doi:10.1038/s41893-023-01175-9).


Model structure and coefficients used in this demo are drawn from the supplementary materials published at:  

https://zenodo.org/records/8016364

---

## Organization

The script is organized around an iterative simulation workflow with endogenous updating.

1. **Scenarios**: For each Monte Carlo repetition, generates 1000 uncertainty scenarios by sampling profit coefficients for each LULC type from an uncertainty range around expected values.

2. **Robust optimization**: For each set of scenarios, solves a linear programming problem to identify the land-use portfolio that minimizes maximum (normalized) regret.

3. **Deforestation conversion** : Converts optimal allocations to annual deforestation rates over 5-year planning periods. The implied 5-year change in natural forest share is converted into an annual deforestation rate and an annual forest-loss area.

4. **Monte Carlo aggregation** : Each simulated year consists of 50 Monte Carlo repetitions. Results are aggregated to produce a mean annual deforestation estimate and associated standard error. The mean 5-year land-use portfolio is  used to initialize the following period.

5. **Multi-period updating ** : Each period is represented by five simulated years. At the end of each period, the average composition  is used to initialize the following period.

The optimisation balances profit-oriented regret minimisation against constraints including:

- Total area constraint (shares sum to one)  
- Forest accounting constraint (natural forest + new deforestation = initial forest area)  
- Land-use capacity limits (e.g. arable land caps, pasture limits)  
- Minimum area requirements for other land uses   
- Non-negativity  

See the Methods section of the manuscript for further detail

---

## System requirements

### Installation time
Please plan in about 10 minutes for a full, first-time installation of R, the required packages and the execution of the script.

### Software dependencies

Required software:
- R (version ≥ 4.5.1)

Required R packages:

- tidyverse  
- lpSolve    

### Operating systems

Tested on:
- macOS Sonoma 14.7.6 (Intel)  
- Windows 11 Pro (64-bit)

### Hardware requirements

- RAM: ~1 GB  
- Storage: ~20 MB for script and packages  
- Processor: any modern dual-core CPU  
- Special hardware: none  

---

## Inputs

- The script requires a single external input file in .rds format, loaded into the object "inputs".  
- This file contains all parameters necessary to run the simulation, including:

  - periods_df: period-specific constraints and profit/uncertainty vectors  
  - initial_areas_1000ha: baseline LULC areas used to initialise the first period  

- The initial LULC composition is derived from the "Brazil spreadsheet 1990_1994.xlsx" workbook via this RDS.  
- Place the RDS file in the same directory as the script or provide a full file path.

---

## Output

- **Format**: Console output, R objects in memory, and a plotted figure  
- **Location**: Results stored in the R workspace  
- **Expected runtime**: Less than one minute on standard hardware  

Main objects created:

- model_df: annual simulation results for the full multi-period run, including  
  - mean annual deforestation rates  
  - standard errors across Monte Carlo repetitions  
  - annual forest loss in 1000 ha and ha  
  - associated year and period identifiers  

- A ggplot figure showing the simulated counterfactual deforestation trajectory with uncertainty ribbons and the random seed used.

---

## Quick start demo

1. Install R from https://www.r-project.org/  
2. Download the script file and the required .rds input file into the same directory.  
3. Open R or RStudio and run:

   source("brazil_multiperiod_demo.R")

The script will execute the full simulation and display the counterfactual deforestation figure.

---

## Reproducibility

The script supports both deterministic and random simulations. When deterministic mode is enabled, a fixed seed reproduces the same uncertainty draws, optimization results, and deforestation trajectory. When disabled, a new random seed is generated and recorded in the figure subtitle, allowing any run to be replicated by re-using that seed.

---

## Citation

Knoke, T, Cueva, J, Bingham, L, Pintado, K, Kindu, M, Döllerer, M, Fibich, J, Köthke, M, Menzel, A, Rammig, A, Senf, C, Biber, P, Wuepper, D, Hänsel, M, Venmans, F, Hanley, N, Paul, P. (2025).  
*Measuring the deforestation that didn't happen - a global perspective.* (preprint)

Underlying methodology:  
Knoke, T., Hanley, N., Roman-Cuesta, R. M., Groom, B., Venmans, F., & Paul, C. (2023).  
*Trends in tropical forest loss and the social value of emission reductions.*  
Nature Sustainability, doi:10.1038/s41893-023-01175-9  

---

## Contact information

Authors:

- Logan Bingham — logan@tum.de  
- Thomas Knoke — knoke@tum.de  
- Jonathan Fibich — jonathan.fibich@tum.de  
- Caroline von Webenau — c.webenau@gmx.de  

---

## License

This project is licensed under the MIT License (2026).
