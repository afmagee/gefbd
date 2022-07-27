#!/bin/bash
#SBATCH --job-name=BDSTP_crocs_CRBDP_hyperpriors
#SBATCH --output=BDSTP_crocs_CRBDP_hyperpriors.log
#SBATCH --error=BDSTP_crocs_CRBDP_hyperpriors.err
#SBATCH --ntasks=2
#SBATCH --nodes=1
#SBATCH --mem=32G
#SBATCH --qos=low_prio_res
#SBATCH --time=12:00:00

module load R

#for ds in "calsoy_as_gonio" "gavia_molecular" "gavia_mol_minus_thoracosaurs" "stolokro_as_basal_neo" "thalatto_as_basal_crocodyliformes" "thalatto_in_longirostrine_clade";
for ds in "Wilberg" "Stubbs";
do

    Rscript src/posteriors2gammaPriors.R empirical_analysis/output_CRBDP/CRBDP_ME_prior_0_${ds}.tre data/${ds}.priors.txt

done

echo "done ..."
