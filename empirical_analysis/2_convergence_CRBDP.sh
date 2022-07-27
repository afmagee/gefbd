#!/bin/bash
#SBATCH --job-name=BDSTP_crocs_CRBDP_convergence
#SBATCH --output=BDSTP_crocs_CRBDP_convergence.log
#SBATCH --error=BDSTP_crocs_CRBDP_convergence.err
#SBATCH --ntasks=2
#SBATCH --nodes=1
#SBATCH --mem=32G
<<<<<<< HEAD
#SBATCH --qos=low
#
#SBATCH --mail-user sebastian.hoehna@gmail.com
#SBATCH --mail-type=ALL
=======
#SBATCH --qos=low_prio_res
#SBATCH --time=12:00:00

module load R
>>>>>>> eee875d8e17c1ce7db7fba2390771c71f15074ae

Rscript src/CRBDP_convergence_diagnostics.R

echo "done ..."
