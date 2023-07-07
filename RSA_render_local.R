###############################################################################################################
## Script for RSA on RVC cluster - 04/07/23
###############################################################################################################
# this code needs modifying for cluster
# - makeCluster with 30cores
# - lhs_n = 1e4
# - parallel computing with foreach and doPar packages

filepath <- "/Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/Population_Dynamics_Model/pop-dynamics_April2020/demographic_model_1/"
# filepath <- "/storage/users/bsavagar/"
## Sims script: set up 23 June to run base vaccination simulations
source(paste0(filepath,"scripts/setup.R"))
list.files(paste0(filepath,"functions"), full.names = TRUE) %>% map(source)
source(paste0(filepath, "scripts/load_data.R"))
# #
# ###############################################################################################################
# ## PARALLEL COMPUTATION ##
# # 
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) # for running locally
# registerDoParallel(cl)
# stopCluster(cl)
# -----------------------
## SET FILENAMES ##

tdate <- Sys.Date()
filename1 <- paste0("RSA_output_", tdate, ".csv")


# -----------------------
## MODEL PARAMETERS ##

TimeStop_dynamics <- 25*52 # 10 years
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
output <- "summary_all" # define output type "summary" (age proporiotns), "summary_all" (age-sex proportions) or "count"
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0
pars_filename <- "set_pars_RSA2.R"
# lhs_n <- 1e4
lhs_n <- 100

# ---------------------------------
## SENSITIVITY ANALYSIS PARAMETERS:
pairs_plot <- F
SA <- TRUE
# set.seed(1)

# select parameter min-max pair (see RSA_var_input.csv)
pars_min <- "min.3"
pars_max <- "max.3"
fixdata <- "sim.1" # fix_input_2 : sim.2 for pR=1
vardata <- "sim.1" # select dataset for test data, # test_1 dataset, all pars set to 0 and all animals in age group 1 (susceptible)

if(SA == TRUE){
  # latin hypercube sampling of parameter space:
  # output is var_input_set dataframe with sampled parameter sets for each variable input (demographic) parameter
  source("scripts/RSA/RSA_lhs2.R")
}

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

var_input_backup <- var_input_set %>% as.data.frame()
rm(var_input_set)


## PARALLELISATION ##

cores=detectCores()
cl <- makeCluster(cores[1]-1) # for running locally
registerDoParallel(cl)

st <- Sys.time()
RSAout <- foreach (i = 1:nrow(var_input_backup), 
                   .packages = c("dplyr"),
                   .combine = "rbind") %dopar% {
                     
                     # tried defining all objects so accessible in global environment and environment of each foreach loop
                     # .GlobalEnv$imm_decay_corrected <- imm_decay_corrected
                     # .GlobalEnv$var_input_backup <- var_input_backup
                     # .GlobalEnv$fix_age_data_full <- fix_age_data_full
                     # .GlobalEnv$f_list <- f_list # initial state of female population
                     # .GlobalEnv$m_list <- m_list # initial state of male population
                     # .GlobalEnv$TimeStop_dynamics <- TimeStop_dynamics # 1 year, weekly timestep for demographic component
                     # .GlobalEnv$TimeStop_transmission <- TimeStop_transmission # 1 day, hourly timestep for transission component
                     # .GlobalEnv$output <- output # model output: tracker or summary stats
                     # .GlobalEnv$summary_df <- summary_df #
                     # .GlobalEnv$clean_environment <- clean_environment
                     # .GlobalEnv$fixdata <- fixdata
                     # .GlobalEnv$SA <- SA
                     
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
Sys.time()-st
stopCluster(cl)

## Practice For-Loop

# 
# outout <- c()
# st <- Sys.time()
# for(i in 1:nrow(var_input_backup)){
#   var_input_full <- unlist(var_input_backup[i,])
#   
#   #var_input_full <- var_input_backup[3,]
#   
#   RSAout <- RSA_func(
#     
#     imm_decay_corrected,
#     var_input_full,
#     # var_input_full = var_input_backup[i,]
#     fix_age_data_full,
#     f_list, # initial state of female population
#     m_list, # initial state of male population
#     TimeStop_dynamics, # 1 year, weekly timestep for demographic component
#     TimeStop_transmission, # 1 day, hourly timestep for transission component
#     output, # model output: tracker or summary stats
#     summary_df, #
#     clean_environment
#   )
#   
#   outout <- outout %>%
#     rbind(RSAout)
# }
# Sys.time()-st