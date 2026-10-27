# README: Leakage analysis using Bayesian GLMMs

## Purpose

This folder contains supplementary code and data for replicating our analysis of potential leakage effects at the continental and transcontinental levels using Bayesian generalized linear mixed models (GLMMS). It accompanies the manuscript:

> Knoke, T, Cueva, J, Bingham, L, Kindu, M, Döllerer, M, Fibich, J, Köthke, M, Menzel, A, Ramig, A, Senf, C, Biber, P. Wüpper, D, Hänsel, M, Venmans, F, Hanley, N, & Paul. C. *Measuring the deforestation that did not happen - a global perspective* (preprint)

The continental and transcontinental analyses correspond to Tables 6a and 6b, respectively.

## Contents of this folder

| Script | Input |
| --- | --- |
| `continental_glmm.R` | `clean_continental.csv` | 
| `transcontinental_glmm.R` | `clean_transcontinental.csv` | 


## Software requirements

These analyses use tools that are available to the public at no cost. Running them requires R, which can be downloaded here: https://www.r-project.org/ . 

An IDE is also recommended. We use RStudio: https://posit.co/downloads

## Instructions for use

1. If needed, install [R](https://www.r-project.org/) (required) and an IDE like [RStudio](https://posit.co/download/rstudio-desktop/) (recommended).

2. Download this subfolder (`glmm_replication`)  or alternatively clone the entire repository and navigate to this folder
3. Open the script corresponding to the analysis you want to run in a new R session 
4. Replace the example path in `setwd()` with the path to this folder, then uncomment and run that line (or set the working directory using your IDE)
5. Select the entire script, and run it

Each script runs separately. If desired, store the results before running the other analysis, as variable assignments are re-used and R objects from the previous run may be overwritten.

Note that the package-loading code block installs any missing packages automatically, which requires an internet connection. Package versions are not enforced.

Model fitting may take some time (for us, <10 minutes). Progress updates are printed to console every 2000 iterations. To change this behavior, modify the `refresh` parameter in the `# Sampling` code block of the `# Fit model` section.

## Results 

Each script reports fixed-effect posterior means and 95% credible intervals in the `results` dataframe
and prints them to the console, along with prior and posterior summaries. The fitted model is stored in the
`fit` object. Results are not saved automatically.

## Testing environment and required packages

These scripts use the following packages. The specific versions we used are in parentheses. 

- `rstanarm (2.32.2)`
- `rstan (2.32.7)`
- `StanHeaders (2.39.1)`
- `ggplot2 (4.0.3)`
- `Matrix (1.7-4)`
- `lme4 (2.0-6)`

The analysis originally performed in JASP 0.18.2 using rstan version 2.32.3. We developed and tested these replication scripts using R 4.5.3 (2026-03-11), macOS Tahoe 26.6.2 on Apple Silicon. Bayesian sampling results can vary across runs and software environments.

## Contacts

- Logan Bingham — logan@tum.de
- Thomas Knoke — knoke@tum.de

## License

See the main repository's MIT license

## Additional notes

We found the following rstanarm documentation helpful:

- Functions   https://mc-stan.org/rstanarm/reference/stan_glmer.html

- Priors      https://mc-stan.org/rstanarm/reference/priors.html  
