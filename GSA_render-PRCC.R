###############################################################################################################
## Script for GSA on RVC cluster - 29/08/23
# With RSA output parameter sets as input for PRCC 
###############################################################################################################

# set cluster filepath
filepath <- "/storage/users/bsavagar/"
# filepath <- paste0(getwd(), "/")

# SETUP

source(paste0(filepath,"scripts/setup.R")) # load libraries

# Functions:
source(paste0(filepath, "functions/demos_summary.R"))
source(paste0(filepath, "functions/dynmod-vac.R"))
source(paste0(filepath, "functions/GSA_outputs.R"))
source(paste0(filepath, "functions/GSAfunc.R"))
source(paste0(filepath, "functions/output.R"))

source(paste0(filepath, "scripts/gsa_load_data.R")) # load data: fixdata, vardata & immunity data

# ###############################################################################################################

## SET FILENAMES ##

tdate <- Sys.Date()
filename1 <- paste0("GSA_output_PRCC_", tdate, ".csv")
filename2 <- paste0("GSA_pars-set_PRCC_", tdate, ".csv")
# -----------------------
## MODEL PARAMETERS ##

TimeStop_dynamics <- 25*52 # 10 years
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
Vstart <- 20*52 # 20 years
output <- "summary_all" # define output type "summary" (age proporiotns), "summary_all" (age-sex proportions) or "count"
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0
#lhs_n <- 1e4
lhs_n <- 1e5

# ---------------------------------
## SENSITIVITY ANALYSIS PARAMETERS:
pairs_plot <- F
SA <- TRUE
rates <- "yrly"
set.seed(1)

# select parameter min-max pair (see RSA_var_input.csv)
pars_min <- "min.1" # min.1, max.1 for conditions used for RSA 240723
pars_max <- "max.1" # min.4, max.4 include young male offtake and increase age of males
if(rates == "wkly"){
  pars_min <- "wkly.min.1"
  pars_max <- "wkly.max.1"
}
fixdata <- "sim.1" # fix_input_2 : sim.2 for pR=1... 
# fixdata <- "sim.3" # sim.3 for N = 100 (14-07-2023)
vardata <- "sim.1" # select dataset for test data, # test_1 dataset, all pars set to 0 and all animals in age group 1 (susceptible)

if(SA == TRUE){
  # latin hypercube sampling of parameter space:
  # output is var_input_set dataframe with sampled parameter sets for each variable input (demographic) parameter
  source("scripts/GSA/GSA_lhs.R")
}
varnames <- colnames(var_input)
## Use RSA output as GSA input pars:

# original local data source: read_csv("output/RSA_output/RSA_pars-set_2023-08-24.csv")
var_input_set <- read_csv("data/GSA_parameters/GSA_var_input-PRCC.csv", col_names=T)
var_input_set <- var_input_set %>% rename(
  "NET_offtake_m"= off_mA,
  "NET_offtake_f"= off_f,
  "mortality_y"= mort_Y,
  "mortality_a"= mort_A,
  "birth_rate"= birth_rate,
  "adu_f_max_yrs"= max_yrs_F,
  "adu_m_max_yrs"= max_yrs_M,
  "min_age_offtake"= min_off,
  "min_age_repro"= min_repro,
  "NET_offtake_m2"=off_mY 
)

#################################
################################
## MODEL ##
#################################
################################


## RSA FOR-LOOP
GSAoutput <- c(); pR_noIm_df <- c(); pop_dynamics <- c()
summary_df <- output_func(TimeStop_dynamics, output) # create summary data frame to store summary stats for each timestep
turnover <- F; dynamics <- T; transmission <- F; 
clean_environment <- T

# timepoints for the population growth
t2 <- 15*52
t1 <- TimeStop_dynamics

var_input_backup <- var_input_set %>% as.data.frame()
write_csv(var_input_backup, paste0(filepath, "output/", filename2)) # save paramter set
rm(var_input_set)


## PARALLELISATION ##

cores=detectCores()
cl <- makeCluster(50) # for running locally
registerDoParallel(cl)

GSAout <- foreach (i = 1:nrow(var_input_backup), 
                   .packages = c("dplyr"),
                   .combine = "rbind") %dopar% {
                   
                     print(i)
# i <- sample(nrow(var_input_backup),1)
                     var_input_full <- unlist(var_input_backup[i,])
                     
                     GSA_func(
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
                       clean_environment,
                       Vstart
                     )
                   }
stopCluster(cl)

# filename <- paste0("RSAoutput_", tdate, ".RData")
# # save RData
# write.csv2(RSAout, paste0(filepath, "output/", filename))
write_csv(GSAout, paste0(filepath, "output/", filename1)) # save output


