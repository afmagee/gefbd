#!/bin/bash
#SBATCH --job-name=BDSTP_DTT_summary
#SBATCH --output=BDSTP_DTT_summary.log
#SBATCH --error=BDSTP_DTT_summary.err
#SBATCH --ntasks=48
#SBATCH --nodes=1
#SBATCH --mem=256G
#SBATCH --qos=high
#
#SBATCH --mail-user sebastian.hoehna@gmail.com
#SBATCH --mail-type=ALL

Rscript simulation_study/src/extract_diversity_through_time.R data_no_ME 48

tar -cvjf simulation_study/4_diversity_through_time/DTT_no_ME.tar.bz2 simulation_study/4_diversity_through_time/data_no_ME/*.diversity_through_time.txt

Rscript simulation_study/src/extract_diversity_through_time.R data 48

tar -cvjf simulation_study/4_diversity_through_time/DTT.tar.bz2 simulation_study/4_diversity_through_time/data/*.diversity_through_time.txt

Rscript simulation_study/src/extract_diversity_through_time.R data_ME 48

tar -cvjf simulation_study/4_diversity_through_time/DTT_ME.tar.bz2 simulation_study/4_diversity_through_time/data_ME/*.diversity_through_time.txt
