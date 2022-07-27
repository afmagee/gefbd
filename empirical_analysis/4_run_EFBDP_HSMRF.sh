#!/bin/bash

N_CORES=16
JOB_DIR="empirical_analysis/jobs_HSMRF"
LOG_DIR="empirical_analysis/logs_HSMRF"
exec=rb-mpi-coal

if [ ${JOB_DIR} != "" ]; then
  if [ ! -d ${JOB_DIR} ]; then
    mkdir ${JOB_DIR}
  else
    rm -f ${JOB_DIR}/*
  fi
fi

if [ ${LOG_DIR} != "" ]; then
  if [ ! -d ${LOG_DIR} ]; then
    mkdir ${LOG_DIR}
  else
    rm -f ${LOG_DIR}/*
  fi
fi


for me_prior in "0.0" "0.5" "2.0";
do

    for uncertainty in "none" "both";
    do

        for ds in "Wilberg" "Stubbs";
        do

            echo "#!/bin/bash
#SBATCH --job-name=BDSTP_crocs_${ds}_${me_prior}
#SBATCH --output=BDSTP_crocs_${ds}_${me_prior}.log
#SBATCH --error=BDSTP_crocs_${ds}_${me_prior}.err
#SBATCH --ntasks=${N_CORES}
#SBATCH --nodes=1
#SBATCH --mem=${N_CORES}G
#SBATCH --qos=low_prio_res
#
#SBATCH --mail-user sebastian.hoehna@gmail.com
#SBATCH --mail-type=NONE

module load gnu
module load boost
module load openmpi

# <path/to/rb> analysis_age_uncertainty.Rev --args <treefile> <TAXON_FILE> <BDP_prior> <hyperprior_file> <ME_hyperprior> <treatement_probability> <age_uncertainty> <NUM_REPS> <seed> <OUTPUT_DIR>
mpirun -np ${N_CORES} ${exec} src/analysis_age_uncertainty.Rev --args ${ds}.tre crocs_taxa_range_${ds}.tsv HSMRFBDP ${ds}.priors.txt ${me_prior} 0.5 ${uncertainty} ${N_CORES} 1234 empirical_analysis/output_${ds}_${uncertainty} > ${LOG_DIR}/${ds}_${uncertainty}_${me_prior}.out
" > ${JOB_DIR}/${ds}_${uncertainty}_${me_prior}.sh
        sbatch ${JOB_DIR}/${ds}_${uncertainty}_${me_prior}.sh

        done

    done

done



echo "done ..."
