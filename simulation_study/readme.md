## Simulating trees
To simulate reconstructed phylogenies for power and false positive analyses and posterior predictive checks, use [`simulate_trees.R`](src/simulate_trees.R) to generate a file of all the simulation commands.
Requires posterior distributions in [output](output) for desired posterior.
Then use...

To simulate complete trees with treatment for posterior predictive tests, use...

To simulate complete trees for diversity through time analyses...

## Model adequacy
Once trees are simulated, use [`compute_summary_stats.R`](src/compute_summary_stats.R) to compute summary statistics to test model adequacy.

## Diversity through time
Once trees are simulated, use [`extract_diversity_through_time.R`](src/extract_diversity_through_time.R) to extract diversity through time curves.

## Analyses of simulated data
Analyzing a simulated dataset is accomplished with [`simulated_data_analysis.Rev`](src/simulated_data_analysis.Rev) much the same as running empirical analyses.
This script must be called from within a subdirectory of [simulation_study](simulation_study).

## Convergence
Convergence filtering can be assessed per-dataset using the same functions as for the empirical analyses.

## File structure

### 1_posterior_false_positives

For posterior predictive simulations and analyses of simulated trees *without* mass extinctions.

#### data
Where the posterior predictive trees are written, and where their LTTs and summary statistics are written.

#### output
Where all runs will place log files.

#### summaries
Where summary files of MCMC runs are placed to avoid repeatedly reading in all Rev logfiles.

### 2_posterior_power

For posterior predictive simulations and analyses of simulated trees *with* mass extinctions.

#### data
Where the posterior predictive trees are written, and where their LTTs and summary statistics are written.

#### output
Where all runs will place log files.

#### summaries
Where summary files of MCMC runs are placed to avoid repeatedly reading in all Rev logfiles.


### 3_treatment

For posterior predictive simulations of trees with treatment and with mass extinctions.

#### data
Where the posterior predictive trees are written, and where their LTTs and summary statistics are written.

#### output
Where all runs will place log files.

#### summaries
Where summary files of MCMC runs are placed to avoid repeatedly reading in all Rev logfiles.


### 4_diversity_through_time

For posterior predictive simulations and analyses of simulated trees *without* mass extinctions.

#### data
Where the posterior predictive trees are written, and where their LTTs and diversity through time curves are stored.


### [src](src)
Contains scripts for simulation, and for running and post-processing analyses.

Rev scripts:
- [simulate_trees.Rev](src/simulate_trees.Rev): Rev script that performs tree simulation. Designed to be called from within R. Contains hard-coded parameters like tree age and sampling fraction at present.
- [simulated_data_analysis.Rev](src/Analysis.Rev): Analysis stub file for simulated data analyses. Basically same as analysis stub for empirical analyses, but designed to be called in subdirectories of [simulation_study](simulation_study).
- [simulate_trees_treatment.Rev](src/simulate_trees_treatment.Rev): As simulate_trees.Rev, but with treatment.


R scripts:
- [simulate_trees.R](src/simulate_trees.R): Makes input files for simulation.
- [simulation_helper.R](src/simulation_helper.R): Contains functions called be `simulate_trees.R`
- [compute_summary_stats.R](src/compute_summary_stats.R): Computes summary stats on all tree files simulated.
- [summary_stat_helper.R](src/summary_stat_helper.R): Contains functions called by several other files.
- [summarize_estimates_and_ess.R](src/summarize_estimates_and_ess.R): Loops over analyses of simulated data, generates summary CSV containing parameter summaries for 200 analyses of simulated data.
- [extract_diversity_through_time.R](src/extract_diversity_through_time.R): Measures average number of lineages alive in intervals of trees, records to text files.
