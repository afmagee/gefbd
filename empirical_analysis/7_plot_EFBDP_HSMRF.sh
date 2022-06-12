#!/bin/bash
#SBATCH --job-name=BDSTP_emp_plot
#SBATCH --output=BDSTP_emp_plot.log
#SBATCH --error=BDSTP_emp_plot.err
#SBATCH --ntasks=2
#SBATCH --nodes=1
#SBATCH --mem=32G
#SBATCH --qos=low_prio_res
#
#SBATCH --mail-user sebastian.hoehna@gmail.com
#SBATCH --mail-type=ALL


#Rscript src/plot_HSMRFBDP.R empirical_analysis/output empirical_analysis/figures
#Rscript src/plot_HSMRFBDP.R empirical_analysis/output_treatment empirical_analysis/figures_treatment

module load R

for uncertainty in "none" "tip" "node" "both";
do

    for ds in "Wilberg" "Stubbs";
    do

	    Rscript src/plot_Wilberg.R empirical_analysis/output_${ds}_${uncertainty} empirical_analysis/figures_${ds}_${uncertainty} HSMRFBDP ${ds}

	done

done
