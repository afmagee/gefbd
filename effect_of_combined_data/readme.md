## Running a single analysis
Analyses are run essentially the same as the empirical analyses.
The relevant Rev files are [`analysis_extinct.Rev`](effect_of_combined_data/src/analysis_extinct.Rev) and [`analysis_extant.Rev`](effect_of_combined_data/src/analysis_extant.Rev).
These work like the main empirical analyses.


## File structure

### [data](empirical_analysis/data)
Contains tree T1, and subtrees of only the extinct and extant taxa.
Also contains a taxon file and hyperparameters for HSMRFBDP analyses in text files (a copy of the file in the empirical analysis folder).

### output
Where all runs will place log files.

### [src](empirical_analysis/src)
Contains scripts for running and post-processing analyses.

Rev scripts (for analyses):
- [analysis_extant.Rev](empirical_analysis/src/analysis_extant.Rev): Analysis stub file. See above for details.
- [analysis_extinct.Rev](empirical_analysis/src/analysis_extinct.Rev): Analysis stub file. See above for details.
- [HSMRFBDP_no_fossils.Rev](empirical_analysis/src/HSMRFBDP_no_fossils.Rev): Called by analysis stub file for analysis of extant-only tree.


R scripts (for post-processing):
- [summarize_subset_results.R](empirical_analysis/src/summarize_subset_results.R): Log files are large. This summarizes (jointly) all log files for a given analysis run. To be run with `Rscript` *after* filtering for convergence.
- [chop_tree.R](empirical_analysis/src/chop_tree.R): Extracts extant and extinct subtrees.
