#!/bin/bash
#Rscript src/plot_HSMRFBDP.R empirical_analysis/output empirical_analysis/figures
#Rscript src/plot_HSMRFBDP.R empirical_analysis/output_treatment empirical_analysis/figures_treatment

N_CORES=4
JOB_DIR="empirical_analysis/jobs_HSMRF_plot"
LOG_DIR="empirical_analysis/logs_HSMRF_plot"

for me_prior in "0.0" "0.1" "0.5" "1.0" "2.0" "5.0";
do

    for uncertainty in "none" "tip" "node" "both";
    do

        for ds in "Wilberg" "Stubbs";
        do

            echo "#!/bin/bash
#SBATCH --job-name=BDSTP_emp_plot_${ds}_${uncertainty}_${me_prior}
#SBATCH --output=BDSTP_emp_plot_${ds}_${uncertainty}_${me_prior}.log
#SBATCH --error=BDSTP_emp_plot_${ds}_${uncertainty}_${me_prior}.err
#SBATCH --ntasks=2
#SBATCH --nodes=1
#SBATCH --mem=32G
#SBATCH --qos=low_prio_res
#
#SBATCH --mail-user sebastian.hoehna@gmail.com
#SBATCH --mail-type=ALL

module load R

Rscript src/plot_Wilberg.R empirical_analysis/output_${ds}_${uncertainty} empirical_analysis/figures_${ds}_${uncertainty} HSMRFBDP ${ds} ${me_prior}" > ${JOB_DIR}/${ds}_${uncertainty}_${me_prior}.sh
            sbatch ${JOB_DIR}/${ds}_${uncertainty}_${me_prior}.sh

        done

	done

done
