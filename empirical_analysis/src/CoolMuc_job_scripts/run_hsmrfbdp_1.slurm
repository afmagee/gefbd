#!/bin/bash
#SBATCH -J empirical-1
#SBATCH -o ./%x.%j.%N.out
#SBATCH -D /dss/dsshome1/lxc02/di76fup/birth_death_serial_sampling/empirical_analysis/
#SBATCH --get-user-env
#SBATCH --clusters=mpp3
#SBATCH --nodes=1
#SBATCH --ntasks=24
#SBATCH --mail-type=all
#SBATCH --mail-user=afmagee@uw.edu
#SBATCH --export=ALL
#SBATCH --time=48:00:00
module load slurm_setup

# No mass extinctions
mpirun -np4 ../../revbayes/projects/cmake/rb-mpi src/analysis.Rev --args calsoy_as_gonio.equal.tre HSMRFBDP calsoy_as_gonio.equal.tre.priors.txt 0.1 4 1234 > cag.0.1.out.txt &
mpirun -np4 ../../revbayes/projects/cmake/rb-mpi src/analysis.Rev --args gavia_molecular.equal.tre HSMRFBDP gavia_molecular.equal.tre.priors.txt 0.1 4 1234 > gm.0.1.out.txt &
mpirun -np4 ../../revbayes/projects/cmake/rb-mpi src/analysis.Rev --args gavia_mol_minus_thoracosaurs.equal.tre HSMRFBDP gavia_mol_minus_thoracosaurs.equal.tre.priors.txt 0.1 4 1234 > gmmt.0.1.out.txt &
mpirun -np4 ../../revbayes/projects/cmake/rb-mpi src/analysis.Rev --args stolokro_as_basal_neo.equal.tre HSMRFBDP stolokro_as_basal_neo.equal.tre.priors.txt 0.1 4 1234 > sabn.0.1.out.txt &
mpirun -np4 ../../revbayes/projects/cmake/rb-mpi src/analysis.Rev --args thalatto_as_basal_crocodyliformes.equal.tre HSMRFBDP thalatto_as_basal_crocodyliformes.equal.tre.priors.txt 0.1 4 1234 > tabc.0.1.out.txt &
mpirun -np4 ../../revbayes/projects/cmake/rb-mpi src/analysis.Rev --args thalatto_in_longirostrine_clade.equal.tre HSMRFBDP thalatto_in_longirostrine_clade.equal.tre.priors.txt 0.1 4 1234 > tilc.0.1.out.txt &
