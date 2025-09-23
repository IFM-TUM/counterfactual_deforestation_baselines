# README: Counterfactual deforestation demo Brazil

## Description

This R script implements a counterfactual, policy-free deforestation modeling approach based on historical profit estimates for various landcover types. It runs 50 consecutive linear optimizations of counterfactual land-use portfolios, from which it derives annual, counterfactual deforestation rates.

The methodology is based on the approach introduced by:

Knoke, T., Hanley, N., Roman-Cuesta, R. M., Groom, B., Venmans, F., & Paul, C. (2023).  
*"Trends in tropical forest loss and the social value of emission reductions"* (doi:10.1038/s41893-023-01175-9).

---

## Organization

The script is organized into four sections:

1. **Scenarios**: For each iteration, generates 1000 uncertainty scenarios by sampling profit coefficients using Monte Carlo.  
2. **Robust optimization**: Solves an LP problem for each set of scenarios to find optimal land-use allocations that minimize maximum regret.  
3. **Deforestation**: Converts optimal allocations to annual deforestation rates over 5-year planning periods.  
4. **Statistics**: Aggregates results across iterations to produce mean deforestation rates with confidence measures.  

The optimization balances profit-oriented regret minimization against constraints including:

- Total area constraint (sum = 1)  
- Legacy regarding forest cover (natural forest + new deforestation = initial forest cover)  
- Land-use capacity limits (e.g., arable land ≤ 7.76% of total area)  
- Minimum area requirements for other land uses
- Non-negativity 

See methods section in manuscript for a detailed description.

---

## System requirements

### Software dependencies

**Required software**:
- R (version ≥ 4.5.1)

**Required R packages**:
- `tidyverse` (version ≥ 2.0.0)  
- `lpSolve` (version ≥ 5.6.23)  

### Operating systems
- Tested on:
  - macOS Sonoma 14.7.6 (Intel)  
  - Windows 11 Pro (64-bit), tested on AMD Ryzen 7 PRO 5850U laptop 

### Hardware requirements
- RAM: 1 GB  
- Storage: 20 MB for script and required R packages  
- Processor: Any dual-core processor (Intel Core i3 or equivalent)  
- Special hardware: None  

---

## Inputs

- No external inputs are required.  
- The script already contains parameters and coefficients needed to generate the uncertainty space, set up the optimization problem, and solve it.  
- Parameters are hardcoded from **Brazil spreadsheet 1990_1994.xlsx** in [Zenodo](https://zenodo.org/records/8016364). 
- Instructions for running script on your own data below.

---

## Output

- **Format**: Console output and R objects/data frames stored in memory  
- **Location**: Results stored in R workspace variables  
- **Expected runtime**: Less than 1 minute on standard hardware (typically a few seconds)  

**Main objects created**:
- `optimization_results`: List containing detailed results from all 50 optimization runs  
- `scenario_sets`: Uncertainty scenario data for each run  
- `all_scenarios_combined`: Combined scenario data across all iterations  
- `summary_metrics`: Statistics including:  
  - Land allocation averages for 30-year and 5-year planning periods  
  - Land-use change metrics by category  
  - Deforestation rate statistics (means, SDs)  
  - Optimization parameters (beta statistics)  
  - Forest loss area metrics with confidence measures  

---

## Quick Start Demo

**Note:** Typical install time: ~5–10 minutes on a standard laptop with stable internet.

1. Install R from [https://www.r-project.org/](https://www.r-project.org/)  
2. Install required packages in R:  
   ```R
   install.packages(c("tidyverse", "lpSolve"))
   ```
3. Download the script file `Supplementary R code.R`.  
4. In R or RStudio, run:  
   ```R
   source("<path_to_the_script_directory>/Supplementary R code.R") # Replace <path_to_the_script_directory> with the filepath to the script on your system
   ```

The script automatically iterates through the optimizations and prints the results to the console. For more detail, user can view created R objects (see above) 

### Example output
```
Simulated annual deforestation for 50 iterations in 1000ha 
 [1]   529.21  4751.73  5817.55  4740.73  7865.91  4609.02   860.90   615.92  8826.48 12055.27  2429.33   358.23
[13]  2252.18   955.11   925.62  3750.37  2243.23  2384.35  1594.90   455.93  3551.63  9344.05  1757.05  7871.07
[25]  5890.40  4030.15  2496.48   733.06  5307.50  5608.97  2258.37  7775.05   607.58  2262.02   499.94  6752.21
[37] 10505.10   650.44  7495.69   942.43   986.71  6900.76   332.81  7543.44  3458.20  5170.95   960.02  4899.47
[49]  2984.74  1719.04
Average annual deforestation rate: 0.006469
1-year deforestation (1000 ha): 3786.35
Standard error (1000 ha): 432.54
```

---

## How to run this script on your own data

1. Edit the vector `initial_areas_1000ha` to contain baseline areas in **1000 ha** units for the same land-use categories:

   ```R
   initial_areas_1000ha <- c(
     `Other land` = ...,
     `Planted forest` = ...,
     `Arable` = ...,
     `Permanent cropland` = ...,
     `Permanent pasture and grassland` = ...,
     `New deforestation` = 0,
     `Naturally regenerated forest` = ...
   )
   ```

   *Note: `New deforestation` should be 0 for the baseline.*

2. Inside the function `generate_uncertainty_scenarios()`, input the profit coefficients, associated uncertainties, and calibration parameters in the vectors:  
   - `profit_coeffs`  
   - `uncertainty_sd`  
   - `upside_mult`  
   - `downside_mult`  

   *Note: `upside_mult` and `downside_mult` require tuning to calibrate the model to the reference period.*

   *Note: Coefficients for updating the code to other countries and periods are contained in ‘Supplementary Coefficients.xls’*

4. Near the top of the script, adjust scalars:  
   - `arable_cap` (upper bound on arable land share)  
   - `other_land_min` (minimum share for Other land)  
   - `planted_forest_target` (fixed share for planted forest)  

   The `planning_period` scalar can be changed to another timeline, but altering this may require more involved edits, so we recommend sticking to 30 years.  

---

## Reproducibility

Running this script reproduces the summary statistics reported in the preprint and Zenodo examples. Minor variations occur due to random sampling unless a fixed seed is specified when generating uncertainty scenarios.  

---

## Citation

Knoke, T, Cueva, J, Bingham, L, Pintado, K, Kindu, M, Döllerer, M, Fibich, J, Köthke, M, Menzel, A, Rammig, A, Senf, C, Biber, P, Wuepper, D, Hänsel, M, Venmans, F, Hanley, N, Paul, P. (2025).  
*Global deforestation rates deviate from a profit-oriented baseline.* Preprint at https://www.researchsquare.com/article/rs-7461473/v1  

Underlying methodology:  
Knoke, T., Hanley, N., Roman-Cuesta, R. M., Groom, B., Venmans, F., & Paul, C. (2023).  
*Trends in tropical forest loss and the social value of emission reductions.*  
Nature Sustainability, doi:10.1038/s41893-023-01175-9  

---

## Contact Information

**Authors:**  

- Logan Bingham: logan@tum.de

- Jorge Cueva Ortiz: jorge.cueva@tum.de

- Thomas Knoke: knoke@tum.de  

- Jonathan Fibich: jonathan.fibich@tum.de  

## License
This project is licensed under the MIT License 2025


