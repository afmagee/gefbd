#!/bin/bash
#SBATCH --job-name=BDSTP_sim_trees
#SBATCH --output=BDSTP_sim_trees.log
#SBATCH --error=BDSTP_sim_trees.err
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --mem=64G
#SBATCH --qos=high
#
#SBATCH --mail-user sebastian.hoehna@gmail.com
#SBATCH --mail-type=ALL

# execute the correct job
parallel -j 4 -a sim_trees_complete_no_ME.txt

tar -cvjf 4_complete_no_ME.tar.bz2 4_diversity_through_time/data_no_ME/*.tre


parallel -j 4 -a sim_trees_complete_ME.txt

tar -cvjf 4_complete_no_ME.tar.bz2 4_diversity_through_time/data_ME/*.tre


echo "done ..."
