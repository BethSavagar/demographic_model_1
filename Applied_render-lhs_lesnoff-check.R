# Script rendering applied-profiles with lhs on uncertain parameters
# Author: Beth Savagar
# Date: 10.10.23

# 9/11/23: Run model with LHS of lesnoff parameter space. Analyse results against original valid population conditions and against lesnoff population growth reports. 

applied_example <- F
local <- T
############
## SETUP ##
############

# local filepath: 
if(local){
  filepath <- ""
}else{
  # cluster filepath
  filepath <- "/storage/users/bsavagar/"
}

## Libraries
source(paste0(filepath,"scripts/setup.R")) 

## Functions:
source(paste0(filepath, "functions/Appliedfunc.R")) # wrapper function
source(paste0(filepath, "functions/demos_summary.R")) # summary stats for output
source(paste0(filepath, "functions/dynmod-vac.R")) # flock dynamics model function
source(paste0(filepath, "functions/output.R")) # what to include in output df
source(paste0(filepath, "functions/GSA_outputs.R")) # summary stats accounting for vac

## Load parameters
datafile <- "-lesnoff_check"
source(paste0(filepath, "scripts/applied/applied_load_data2.R")) 

## Datasets
datasets <- fix_age_data_full %>% select(-c(parameter)) %>% colnames()

######################
## MODEL SETUP ##
######################

## Simulation:
TimeStop_dynamics <- 20*52 # 2 years only interested in rates for 1 y
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0

## Model Output
output <- "dynamics" # Output options: "summary" (stats with age-group prop), "summary_all" (stats with age-sex group prop), "dynamics" (pop metrics over time)

#######################
## Vaccination Setup ##
#######################
# see applied_vaccination
pV <- 1
source("scripts/applied/applied_vaccination.R")
Vstart <- Vprog %>% filter(Vround==1) %>% pull(Vweek)

##################
## RUN MODEL ##
##################

# create empty dataframe to store output
summary_df <- output_func(TimeStop_dynamics, output) 

turnover <- F; 
dynamics <- T; 
transmission <- F; 
clean_environment <- T

# timepoints for population growth
t2 <- TimeStop_dynamics/2 # 5 year pop growth
t1 <- TimeStop_dynamics


#####################
## PARALLELISATION ##
#####################

print("start-parallel")

## Local parallelisation
if(local){
  cores=detectCores()
  cl <- makeCluster(cores[1]-1)
  registerDoParallel(cl)
}else{
  cores=detectCores()
  cl <- makeCluster(10)
  registerDoParallel(cl)
}
print("parallel-verified")
print("start-model")

##################################
## LHS DEMOGRAPHIC PARS ##
##################################

dataset <- "lesnoff.yr"
data_filename <- gsub("\\.","",dataset)

if(dataset == "lesnoff.ft"){
  rates <- "wkly"
}else{
  rates <- "yrly"
}

lhs <- T
lhs_n <- 1e3 # number of sets (parameter combinations)
pairs_plot <- F
SA <- TRUE

# set seed
set.seed(1) # so that results are consistent

pars_min <- paste0(dataset, ".min") # minimum val
pars_max <-  paste0(dataset, ".max") # maximum val
fixdata <- dataset # set dataset for state_vars
vardata <- dataset

source("scripts/applied/applied_lhs.R") # output var_input_df contains all parameter combinations for uncertain demographics.

# Format demographic parameter sets:
var_input_backup <- var_input_df %>% as.data.frame()

##############
## FOR LOOP ##
##############

# validOut = list to store outputs
GSAoutput <- c(); 
Out_list <- list()
validOut <- list()
validPars <- list()

################
## SIMULATION ##
################
Out <- foreach (i = 1:nrow(var_input_backup), 
                .packages = c("dplyr", "tibble")
) %dopar% {
  
  print(i)
  var_input_full <- unlist(var_input_backup[i,])
  
  App_func(
    imm_decay_corrected,
    var_input_full,
    fix_age_data_full,
    f_list, # initial state of female population
    m_list, # initial state of male population
    TimeStop_dynamics, # 1 year, weekly timestep for demographic component
    TimeStop_transmission, # 1 day, hourly timestep for transission component
    output, # model output: tracker or summary stats
    summary_df, #
    clean_environment,
    Vstart,
    Vprog,
    fixdata,
    vardata
  )
}

############
############

output <- "summary_all"
# create empty dataframe to store output
summary_df <- output_func(TimeStop_dynamics, output) 

################
## SIMULATION ##
################
Out2 <- foreach (i = 1:nrow(var_input_backup), 
                .packages = c("dplyr", "tibble"),
                .combine = "rbind"
) %dopar% {
  
  print(i)
  var_input_full <- unlist(var_input_backup[i,])
  
  App_func(
    imm_decay_corrected,
    var_input_full,
    fix_age_data_full,
    f_list, # initial state of female population
    m_list, # initial state of male population
    TimeStop_dynamics, # 1 year, weekly timestep for demographic component
    TimeStop_transmission, # 1 day, hourly timestep for transission component
    output, # model output: tracker or summary stats
    summary_df, #
    clean_environment,
    Vstart,
    Vprog,
    fixdata,
    vardata
  )
}

Out2 <- Out2 %>% rename("midyr_growth"="tenyr_growth")
