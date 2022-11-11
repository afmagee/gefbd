source("src/plot_helper.R")

# Get arguments
args = commandArgs(trailingOnly=TRUE)

# Path
OUTPUT_DIR  = args[1]
FIGURES_DIR = args[2]
MODEL       = args[3]
ds          = args[4]
this.ME     = args[5]

#OUTPUT_DIR  = "empirical_analysis/output_Stubbs_both"
#FIGURES_DIR = "figures_Stubbs_both"
#MODEL = "HSMRFBDP"
#ds = "Stubbs"
#this.ME = "0"

grid.times <- computeGridEndTimes(100,243.5)
#grid.times <- computeGridEndTimes(10,248.9)
#grid.times <- computeGridEndTimes(50,243.5)

## Create figures directory
dir.create(FIGURES_DIR, showWarnings=FALSE)

cat(ds,"\n")
cat(this.ME,"\n")
cat(rep("=",40),"\n",sep="")

# single run plots
for ( rep in 1:16 ) {

    cat("Rep", rep, "\n")

    posterior <- read.table(paste0(OUTPUT_DIR,"/", MODEL,"_ME_prior_",this.ME,"_",ds,".tre_run_",rep,".log"),
                            stringsAsFactors=FALSE,header=TRUE )

    pdf(paste0(FIGURES_DIR, "/continuous_parameters_ME_prior_",this.ME,"_",ds,"_",MODEL,"_run_",rep,".pdf"),width=8,height=2)
      plotContinuousParameters(rb.posterior = posterior,
                               interval.times = grid.times,
                               parameter.names = c("speciation","extinction","fossilization"),
                               ci.width = 0.9)
    dev.off()

    if (this.ME != "0") {
      pdf(paste0(FIGURES_DIR, "/ME_prior_",this.ME,"_",ds,"_",MODEL,"_run_",rep,".pdf"),width=4,height=3)
        plotMEBF(rb.posterior = posterior,
                 interval.times = grid.times,
                 parameter.name = "mass_extinction_probabilities",
                 prior.probability = as.numeric(this.ME)/99)
      dev.off()
    }
    rm(posterior)

}

PLOT.COMBINED = FALSE

if ( PLOT.COMBINED ) {

  cat("Combined ...\n")

  posterior <- getConcatenatedLogFile(paste0(OUTPUT_DIR,"/", MODEL,"_ME_prior_",this.ME,"_",ds,""))

  pdf(paste0(FIGURES_DIR, "/continuous_parameters_ME_prior_",this.ME,"_",ds,"_",MODEL,".pdf"),width=8,height=2)
    plotContinuousParameters(rb.posterior = posterior,
                             interval.times = grid.times,
                             parameter.names = c("speciation","extinction","fossilization"),
                             ci.width = 0.9)
  dev.off()

  if (this.ME != "0") {
    pdf(paste0(FIGURES_DIR, "/ME_prior_",this.ME,"_",ds,"_",MODEL,".pdf"),width=4,height=3)
      plotMEBF(rb.posterior = posterior,
               interval.times = grid.times,
               parameter.name = "mass_extinction_probabilities",
               prior.probability = as.numeric(this.ME)/99)
    dev.off()
  }
  rm(posterior)
}

cat("\n")
