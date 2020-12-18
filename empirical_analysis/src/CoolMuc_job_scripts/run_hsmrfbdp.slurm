#!/bin/bash
#SBATCH -J empirical
#SBATCH -o ./%x.%j.%N.out
#SBATCH -D /dss/dsshome1/lxc02/di76fup/birth_death_serial_sampling/empirical_analysis/
#SBATCH --get-user-env
#SBATCH --clusters=mpp3
#SBATCH --nodes=1
#SBATCH --mail-type=all
#SBATCH --mail-user=afmagee@uw.edu
#SBATCH --export=ALL
#SBATCH --time=48:00:00
module load slurm_setup

# Start as many background serial jobs as there are cores available on the node

# No mass extinctions
../../revbayes/projects/cmake/rb src/analysis.Rev --args calsoy_as_gonio.equal.tre HSMRFBDP calsoy_as_gonio.equal.tre.priors.txt 0.0 1234 > cag.0.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_molecular.equal.tre HSMRFBDP gavia_molecular.equal.tre.priors.txt 0.0 1234 > gm.0.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_mol_minus_thoracosaurs.equal.tre HSMRFBDP gavia_mol_minus_thoracosaurs.equal.tre.priors.txt 0.0 1234 > gmmt.0.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args stolokro_as_basal_neo.equal.tre HSMRFBDP stolokro_as_basal_neo.equal.tre.priors.txt 0.0 1234 > sabn.0.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_as_basal_crocodyliformes.equal.tre HSMRFBDP thalatto_as_basal_crocodyliformes.equal.tre.priors.txt 0.0 1234 > tabc.0.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_in_longirostrine_clade.equal.tre HSMRFBDP thalatto_in_longirostrine_clade.equal.tre.priors.txt 0.0 1234 > tilc.0.0.out.txt &

# Prior expected number of mass extinctions 0.1
../../revbayes/projects/cmake/rb src/analysis.Rev --args calsoy_as_gonio.equal.tre HSMRFBDP calsoy_as_gonio.equal.tre.priors.txt 0.1 1234 > cag.0.1.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_molecular.equal.tre HSMRFBDP gavia_molecular.equal.tre.priors.txt 0.1 1234 > gm.0.1.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_mol_minus_thoracosaurs.equal.tre HSMRFBDP gavia_mol_minus_thoracosaurs.equal.tre.priors.txt 0.1 1234 > gmmt.0.1.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args stolokro_as_basal_neo.equal.tre HSMRFBDP stolokro_as_basal_neo.equal.tre.priors.txt 0.1 1234 > sabn.0.1.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_as_basal_crocodyliformes.equal.tre HSMRFBDP thalatto_as_basal_crocodyliformes.equal.tre.priors.txt 0.1 1234 > tabc.0.1.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_in_longirostrine_clade.equal.tre HSMRFBDP thalatto_in_longirostrine_clade.equal.tre.priors.txt 0.1 1234 > tilc.0.1.out.txt &

# Prior expected number of mass extinctions 0.5
../../revbayes/projects/cmake/rb src/analysis.Rev --args calsoy_as_gonio.equal.tre HSMRFBDP calsoy_as_gonio.equal.tre.priors.txt 0.5 1234 > cag.0.5.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_molecular.equal.tre HSMRFBDP gavia_molecular.equal.tre.priors.txt 0.5 1234 > gm.0.5.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_mol_minus_thoracosaurs.equal.tre HSMRFBDP gavia_mol_minus_thoracosaurs.equal.tre.priors.txt 0.5 1234 > gmmt.0.5.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args stolokro_as_basal_neo.equal.tre HSMRFBDP stolokro_as_basal_neo.equal.tre.priors.txt 0.5 1234 > sabn.0.5.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_as_basal_crocodyliformes.equal.tre HSMRFBDP thalatto_as_basal_crocodyliformes.equal.tre.priors.txt 0.5 1234 > tabc.0.5.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_in_longirostrine_clade.equal.tre HSMRFBDP thalatto_in_longirostrine_clade.equal.tre.priors.txt 0.5 1234 > tilc.0.5.out.txt &

# Prior expected number of mass extinctions 1.0
../../revbayes/projects/cmake/rb src/analysis.Rev --args calsoy_as_gonio.equal.tre HSMRFBDP calsoy_as_gonio.equal.tre.priors.txt 1.0 1234 > cag.1.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_molecular.equal.tre HSMRFBDP gavia_molecular.equal.tre.priors.txt 1.0 1234 > gm.1.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_mol_minus_thoracosaurs.equal.tre HSMRFBDP gavia_mol_minus_thoracosaurs.equal.tre.priors.txt 1.0 1234 > gmmt.1.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args stolokro_as_basal_neo.equal.tre HSMRFBDP stolokro_as_basal_neo.equal.tre.priors.txt 1.0 1234 > sabn.1.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_as_basal_crocodyliformes.equal.tre HSMRFBDP thalatto_as_basal_crocodyliformes.equal.tre.priors.txt 1.0 1234 > tabc.1.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_in_longirostrine_clade.equal.tre HSMRFBDP thalatto_in_longirostrine_clade.equal.tre.priors.txt 1.0 1234 > tilc.1.0.out.txt &

# Prior expected number of mass extinctions 2.0
../../revbayes/projects/cmake/rb src/analysis.Rev --args calsoy_as_gonio.equal.tre HSMRFBDP calsoy_as_gonio.equal.tre.priors.txt 2.0 1234 > cag.2.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_molecular.equal.tre HSMRFBDP gavia_molecular.equal.tre.priors.txt 2.0 1234 > gm.2.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_mol_minus_thoracosaurs.equal.tre HSMRFBDP gavia_mol_minus_thoracosaurs.equal.tre.priors.txt 2.0 1234 > gmmt.2.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args stolokro_as_basal_neo.equal.tre HSMRFBDP stolokro_as_basal_neo.equal.tre.priors.txt 2.0 1234 > sabn.2.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_as_basal_crocodyliformes.equal.tre HSMRFBDP thalatto_as_basal_crocodyliformes.equal.tre.priors.txt 2.0 1234 > tabc.2.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_in_longirostrine_clade.equal.tre HSMRFBDP thalatto_in_longirostrine_clade.equal.tre.priors.txt 2.0 1234 > tilc.2.0.out.txt &

# Prior expected number of mass extinctions 5.0
../../revbayes/projects/cmake/rb src/analysis.Rev --args calsoy_as_gonio.equal.tre HSMRFBDP calsoy_as_gonio.equal.tre.priors.txt 5.0 1234 > cag.5.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_molecular.equal.tre HSMRFBDP gavia_molecular.equal.tre.priors.txt 5.0 1234 > gm.5.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args gavia_mol_minus_thoracosaurs.equal.tre HSMRFBDP gavia_mol_minus_thoracosaurs.equal.tre.priors.txt 5.0 1234 > gmmt.5.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args stolokro_as_basal_neo.equal.tre HSMRFBDP stolokro_as_basal_neo.equal.tre.priors.txt 5.0 1234 > sabn.5.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_as_basal_crocodyliformes.equal.tre HSMRFBDP thalatto_as_basal_crocodyliformes.equal.tre.priors.txt 5.0 1234 > tabc.5.0.out.txt &
../../revbayes/projects/cmake/rb src/analysis.Rev --args thalatto_in_longirostrine_clade.equal.tre HSMRFBDP thalatto_in_longirostrine_clade.equal.tre.priors.txt 5.0 1234 > tilc.5.0.out.txt &

wait # for completion of background tasks
