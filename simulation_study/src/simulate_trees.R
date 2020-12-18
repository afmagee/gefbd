source("simulation_study/src/simulation_helper.R")

DATASET <- "calsoy_as_gonio.equal.tre"
ME_PRIOR <- 0.5
LOGS_PATH <- "empirical_analysis/output/"
SIMULATOR_INPUT_PATH <- "simulation_study/1_posterior_false_positives/data/"

RB_SIM_SCRIPT <- "simulation_study/src/simulate_trees.Rev"
RB <- "rb"

N_SIMULATIONS <- 10000

#########
# simulate without mass extinction
#########

dir.create(SIMULATOR_INPUT_PATH, showWarnings = FALSE, recursive=TRUE)

prefix <- paste0(LOGS_PATH,"HSMRFBDP_ME_prior_0_",DATASET)

rb <- getConcatenatedLogFile(prefix)

params <- getParameters(rb,"speciation_rate","extinction_rate","fossilization_rate",NA)


samples <- round(seq(1,dim(rb)[1],length.out=N_SIMULATIONS))
seed <- 1234

for (i in 1:length(samples)) {
  sim.par <- getSimulatorInput(params,samples[i])
  outpath <- paste0(SIMULATOR_INPUT_PATH,i,".txt")
  cat(sim.par,file=outpath,sep="\n")
  cmd <- paste(RB,RB_SIM_SCRIPT,"--args",SIMULATOR_INPUT_PATH,SIMULATOR_INPUT_PATH,i,seed+i,sep=" ")
  cat(cmd,"\n",sep="",file="sim_trees_FDR.txt",append=(i>1))
#  system(cmd)
}

cat("Done with no-ME sims\n")


#########
# simulate with mass extinction
#########
SIMULATOR_INPUT_PATH <- "simulation_study/2_posterior_power/data/"

dir.create(SIMULATOR_INPUT_PATH, showWarnings = FALSE, recursive=TRUE)

prefix <- paste0(LOGS_PATH,"HSMRFBDP_ME_prior_",ME_PRIOR,"_",DATASET)

rb <- getConcatenatedLogFile(prefix)

params <- getParameters(rb,"speciation_rate","extinction_rate","fossilization_rate","mass_extinction_probabilities")

samples <- round(seq(1,dim(rb)[1],length.out=N_SIMULATIONS))
seed <- 1234

for (i in 1:length(samples)) {
  sim.par <- getSimulatorInput(params,samples[i],28,0.9,0.5)
  outpath <- paste0(SIMULATOR_INPUT_PATH,i,".txt")
  cat(sim.par,file=outpath,sep="\n")
  cmd <- paste(RB,RB_SIM_SCRIPT,"--args",SIMULATOR_INPUT_PATH,SIMULATOR_INPUT_PATH,i,seed+i,sep=" ")
  cat(cmd,"\n",sep="",file="sim_trees_power.txt",append=(i>1))
#  system(cmd)
}

cat("Done with ME sims\n")
