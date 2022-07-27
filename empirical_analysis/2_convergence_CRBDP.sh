#!/bin/bash
#SBATCH --job-name=BDSTP_crocs_CRBDP_convergence
#SBATCH --output=BDSTP_crocs_CRBDP_convergence.log
#SBATCH --error=BDSTP_crocs_CRBDP_convergence.err
#SBATCH --ntasks=2
#SBATCH --nodes=1
#SBATCH --mem=32G
#SBATCH --qos=low
#
#SBATCH --mail-user sebastian.hoehna@gmail.com
#SBATCH --mail-type=ALL

Rscript src/CRBDP_convergence_diagnostics.R

echo "done ..."
