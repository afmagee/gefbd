#!/bin/bash
#Rscript src/plot_HSMRFBDP.R empirical_analysis/output empirical_analysis/figures
#Rscript src/plot_HSMRFBDP.R empirical_analysis/output_treatment empirical_analysis/figures_treatment

N_CORES=2
JOB_DIR="empirical_analysis/jobs_GMRF_plot"
LOG_DIR="empirical_analysis/logs_GMRF_plot"

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


#for me_prior in "0.0" "0.5" "2.0";
for me_prior in "0.5";
do

#    for uncertainty in "none" "both";
    for uncertainty in "both";
    do

#        for ds in "Wilberg" "Stubbs";
        for ds in "Stubbs";
        do

#            echo "#!/bin/bash
##SBATCH --job-name=BDSTP_emp_plot_${ds}_${uncertainty}_${me_prior}
##SBATCH --output=BDSTP_emp_plot_${ds}_${uncertainty}_${me_prior}.log
##SBATCH --error=BDSTP_emp_plot_${ds}_${uncertainty}_${me_prior}.err
##SBATCH --ntasks=2
##SBATCH --nodes=1
##SBATCH --mem=32G
##SBATCH --qos=low_prio_res
##
##SBATCH --mail-user sebastian.hoehna@gmail.com
##SBATCH --mail-type=ALL
#
#module load R
#
#Rscript src/plot_HSMRF_ME.R empirical_analysis/output_${ds}_${uncertainty} empirical_analysis/figures_${ds}_${uncertainty} GMRFBDP ${ds} ${me_prior}" > ${JOB_DIR}/${ds}_${uncertainty}_${me_prior}.sh
#            sbatch ${JOB_DIR}/${ds}_${uncertainty}_${me_prior}.sh

            Rscript src/plot_HSMRF_ME.R empirical_analysis/output_${ds}_${uncertainty} empirical_analysis/figures_${ds}_${uncertainty} GMRFBDP ${ds} ${me_prior}


        done

    done

done
