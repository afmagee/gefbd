## Data Provenence
All data comes from Wilberg et al. (2019).

## Running a single analysis
To set up an analysis, use the [`analysis.Rev`](src/analysis.Rev) file. This is designed to be called (from within `empirical_analyses/`, all file paths here are relative to this) with 5 arguments,
`<path/to/rb> analysis.Rev --args <treefile> <BDP_prior> <hyperprior_file> <ME_hyperprior> <seed>`
The arguments are,
- `<treefile>` the name of the treefile, must be nested within `data/`
- `<BDP_prior>` the prior on the BDP, allowed are "CRBDP" for constant-rate analysis (used to set the hyperpriors) and "HSMRFBDP" for a horseshoe Markov random field prior
- `<hyperprior_basename>` for HSMRF model, specifies name of file containing hyperpriors on the BDP, must be nested in `data/`, can be NA if using CRBDP
- `<ME_hyperprior>` the hyperprior on the mass extinctions, prior expected number of mass extinction events, if 0 no mass extinctions are estimated
- `<seed>`the random seed, for reproducibility

Other important information is hard-coded into the file and various files called within, including:
- The taxon data file (name of file in `data/`)
- Species sampling fraction of 14/24 (this is a known value in [0,1] for macroevolutionary applications, the proportion of extant species sampled at the present, and 0 for most phylodynamic applications)
- Treatment probability of 0.0 (0 for macroevolution, assumed to be 1 for most phylodynamic applications)
- Details of the HSMRF priors on BDP parameters:
  - Number of intervals is 100
  - Global scale for each vector-valued parameter is 0.0021 in accordance with number of intervals

## Analysis with treatment


## File structure

### [data](data)
Most importantly, contains 6 trees from Wilberg (2019), a taxon data file.
Also contains hyperparameters for HSMRFBDP analyses in text files.
These are gamma distributions fit to the posterior distributions from constant-rate analyses of all 6 trees.

### output
Where all runs will place log files.

### [src](src)
Contains scripts for running and post-processing analyses.

Rev scripts (for analyses):
- [Analysis.Rev](src/Analysis.Rev): Analysis stub file. See above for details.
- [CRBDP.Rev](src/CRBDP.Rev): Called by analysis stub file for constant-rate analyses.
- [HSMRFBDP.Rev](src/HSMRFBDP.Rev): Called by analysis stub file for analyses with time-varying rates.
- [ME.Rev](src/ME.Rev): Called by analysis stub file for analyses with mass extinctions.


R scripts (for post-processing):
- [find_converged_cliques.R](src/find_converged_cliques.R): Finds the set of MCMC chains which have all converged to the same distribution. Uses PSRF to assess inter-chain convergence. Run with `Rscript` (details of arguments in file). Puts runs that did not converge into output/unconverged.
- [summarize_empirical_results.R](src/summarize_empirical_results.R): Log files are large. This summarizes (jointly) all log files for a given analysis run. To be run with `Rscript` *after* filtering for convergence.
- [posteriors2gammaPriors.R](src/posteriors2gammaPriors.R): Fits gamma distributions to posterior samples. Allows inflation of variance.
- [find_converged_cliques_utils.R](src/find_converged_cliques_utils.R): Implements the functions used in `find_converged_cliques.R`
- [rank_based_convergence_diagnostics.R](src/rank_based_convergence_diagnostics.R): Implementations of Vehtari et al. (2020) rank-based convergence diagnostics.
- [CRBDP_convergence_diagnostics.R](src/CRBDP_convergence_diagnostics.R): Basic checking of convergence for constant-rate analyses.
