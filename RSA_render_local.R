###############################################################################################################
## Script for RSA locally - 04/07/23
###############################################################################################################

# add group for young male offtake & raise max age of males in flock

filepath <- "/Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/3. Population_Dynamics_Model/pop-dynamics_April2020/demographic_model_1/"

## Sims script: set up 23 June to run base vaccination simulations
source(paste0(filepath,"scripts/setup.R"))
list.files(paste0(filepath,"functions"), full.names = TRUE) %>% map(source)
source(paste0(filepath, "scripts/load_data.R"))


## SET FILENAMES ##
filesave <- F
tdate <- Sys.Date()
filename1 <- paste0("RSA_output_", tdate, ".csv")
filename2 <- paste0("RSA_pars-set_", tdate, ".csv")

# -----------------------
## MODEL PARAMETERS ##

TimeStop_dynamics <- 25*52 # 10 years
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
output <- "summary_all" # define output type "summary" (age proporiotns), "summary_all" (age-sex proportions) or "dynamics"
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0
lhs_n <- 1e4
#lhs_n <- 1e5

# ---------------------------------
## SENSITIVITY ANALYSIS PARAMETERS:
pairs_plot <- F
SA <- TRUE
rates <- "yrly" # wkly
set.seed(1)

# select parameter min-max pair (see RSA_var_input.csv)
pars_min <- "min.4"
pars_max <- "max.4"
if(rates == "wkly"){
  pars_min <- "wkly.min.1"
  pars_max <- "wkly.max.1"
}
fixdata <- "sim.1" # fix_input_2 : sim.2 for pR=1... 
# fixdata <- "sim.3" # sim.3 for N = 100 (14-07-2023)
vardata <- "sim.1" # select dataset for test data, # test_1 dataset, all pars set to 0 and all animals in age group 1 (susceptible)

# SA <- F
if(SA == TRUE){
  # latin hypercube sampling of parameter space:
  # output is var_input_set dataframe with sampled parameter sets for each variable input (demographic) parameter
  source("scripts/RSA/RSA_lhs2.R")
}

# If testing age-sex dynamics for 
# var_input_set <- tenyr_pars
# var_input_set <- valid_as_pars2_df ## from RSA_analysis-2.2.R
#################################
################################
## MODEL ##
#################################
################################


## RSA FOR-LOOP
RSAoutput <- c(); pR_noIm_df <- c(); pop_dynamics <- c()
summary_df <- output_func(TimeStop_dynamics, output) # create summary data frame to store summary stats for each timestep
turnover <- F; dynamics <- T; transmission <- F; 
clean_environment <- T

# timepoints for the population growth
t2 <- 15*52
t1 <- TimeStop_dynamics

# insert model from RSA_test2.Rmd
# see sensobol script - GSA_pars-sobol-set
var_input_backup <- var_input_set %>% as.data.frame()
if(filesave == T){
  write_csv(var_input_backup, paste0(filepath, "output/", filename2)) # save paramter set
}
   
rm(var_input_set)


## PARALLELISATION ##

cores=detectCores()
cl <- makeCluster(cores[1]-1) # for running locally
registerDoParallel(cl)

RSAout <- foreach (i = 1:nrow(var_input_backup), 
                   .packages = c("dplyr"),
                   .combine = "rbind") %dopar% {
                     
                     print(i)
                     var_input_full <- unlist(var_input_backup[i,])
                     
                     RSA_func(
                       imm_decay_corrected,
                       var_input_full,
                       # var_input_backup[2,],
                       fix_age_data_full,
                       f_list, # initial state of female population
                       m_list, # initial state of male population
                       TimeStop_dynamics, # 1 year, weekly timestep for demographic component
                       TimeStop_transmission, # 1 day, hourly timestep for transission component
                       output, # model output: tracker or summary stats
                       summary_df, #
                       clean_environment
                     )
                   }
stopCluster(cl)

# filename <- paste0("RSAoutput_", tdate, ".RData")
# # save RData
# write.csv2(RSAout, paste0(filepath, "output/", filename))
if(filesave == T){
  write_csv(RSAout, paste0(filepath, "output/", filename1)) # save output
  
}


