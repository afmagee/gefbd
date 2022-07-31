#!/bin/bash
#SBATCH --job-name=BDSTP_emp_conv_assessment
#SBATCH --output=BDSTP_emp_conv_assessment.log
#SBATCH --error=BDSTP_emp_conv_assessment.err
#SBATCH --ntasks=2
#SBATCH --nodes=1
#SBATCH --mem=32G
#SBATCH --qos=high
#
#SBATCH --mail-user sebastian.hoehna@gmail.com
#SBATCH --mail-type=ALL


for ds in "calsoy_as_gonio" "gavia_molecular" "gavia_mol_minus_thoracosaurs" "stolokro_as_basal_neo" "thalatto_as_basal_crocodyliformes" "thalatto_in_longirostrine_clade";
do
	for me_prior in "0" "0.1" "0.5" "1" "2" "5";
	do
        Rscript src/find_converged_cliques.R empirical_analysis/output ${ds} ${me_prior} 1.01
	done
done
