#!/bin/bash
#SBATCH -J CRBDP
#SBATCH -o ./%x.%j.%N.out
#SBATCH -D /dss/dsshome1/lxc02/di76fup/birth_death_serial_sampling/empirical_analysis/
#SBATCH --get-user-env
#SBATCH --clusters=mpp3
#SBATCH --nodes=1
#SBATCH --mail-type=all
#SBATCH --mail-user=afmagee@uw.edu
#SBATCH --export=ALL
#SBATCH --time=12:00:00
module load slurm_setup

# Start as many background serial jobs as there are cores available on the node
../../revbayes/projects/cmake/rb src/analysis.Rev --args calsoy_as_gonio.equal.tre CRBDP NA 0 1234 > cag.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_molecular.equal.tre CRBDP NA 0 1234 > gm.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_mol_minus_thoracosaurs.equal.tre CRBDP NA 0 1234 > gmmt.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args stolokro_as_basal_neo.equal.tre CRBDP NA 0 1234 > sabn.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_as_basal_crocodyliformes.equal.tre CRBDP NA 0 1234 > tabc.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_in_longirostrine_clade.equal.tre CRBDP NA 0 1234 tilc.out.txt &
wait # for completion of background tasks
