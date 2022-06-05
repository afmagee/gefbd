source("src/plot_helper.R")

# Get arguments
args = commandArgs(trailingOnly=TRUE)

# Path
OUTPUT_DIR  = args[1]
FIGURES_DIR = args[2]
MODEL       = args[3]
ds.names    = args[4]
#OUTPUT_DIR  = "empirical_analysis/output_tip_age"
#FIGURES_DIR = "figures_tip_age"

cat(OUTPUT_DIR,"\n")
cat(FIGURES_DIR,"\n")

grid.times <- computeGridEndTimes(100,243.5)
#grid.times <- computeGridEndTimes(10,248.9)

# Get names of datasets
#ds.names <- list.files("data/")
#ds.names <- ds.names[!grepl("priors",ds.names,fixed=TRUE)]
#ds.names <- "Stubs2021_cal3_1"
#ds.names <- "gavia_mol_minus_thoracosaurs.equal.tre"

ME.priors <- c("0","0.1","0.5","1","2","5")
#ME.priors <- c("0","0.1","0.5")

## Create figures directory
dir.create(FIGURES_DIR, showWarnings=FALSE)

for (i in 1:length(ME.priors)) {
  this.ME <- ME.priors[i]

  cat(this.ME,"\n")

  for (ds in ds.names) {

    cat("\t",ds,"\n")

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
}
