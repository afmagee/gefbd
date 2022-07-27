#!/bin/bash

N_CORES=4
JOB_DIR="jobs_CRBDP"
LOG_DIR="logs_CRBDP"
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


#for ds in "calsoy_as_gonio" "gavia_molecular" "gavia_mol_minus_thoracosaurs" "stolokro_as_basal_neo" "thalatto_as_basal_crocodyliformes" "thalatto_in_longirostrine_clade";
for ds in "Wilberg" "Stubbs";
do

    echo "#!/bin/bash
#SBATCH --job-name=BDSTP_crocs_${ds}
#SBATCH --output=BDSTP_crocs_${ds}.log
#SBATCH --error=BDSTP_crocs_${ds}.err
#SBATCH --ntasks=${N_CORES}
#SBATCH --nodes=1
#SBATCH --mem=${N_CORES}G
#SBATCH --qos=low_prio_res
#SBATCH --time=7-00:00:00

module load gnu/7
module load boost
module load openmpi

# <path/to/rb> analysis.Rev --args <treefile> <taxonfile> <BDP_prior> <hyperprior_file> <ME_hyperprior> <treatement_probability> <age_uncertainty> <NUM_REPS> <seed>
mpirun -np ${N_CORES} ${exec} src/analysis_age_uncertainty.Rev --args ${ds}.tre crocs_taxa_range_${ds}.tsv CRBDP none 0 0.5 none ${N_CORES} 1234 empirical_analysis/output_CRBDP > ${LOG_DIR}/${ds}.out
" > ${JOB_DIR}/${ds}.sh
    sbatch ${JOB_DIR}/${ds}.sh

done

echo "done ..."
