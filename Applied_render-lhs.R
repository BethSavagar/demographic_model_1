# Script rendering applied-profiles with lhs on uncertain parameters
# Author: Beth Savagar
# Date: 10.10.23

# 21/10/23 - update script for applied-vaccination (see applied_render_local and applied_vaccination). 
# for each parameter set add loop through each different vaccination coverage level
# set up master loop to run through each different profile and save the output (possible this is the foreach statement?)
applied_example <- F
local <- F
# local <- T
############
## SETUP ##
############

# cluster filepath
filepath <- "/storage/users/bsavagar/"

# local filepath: 
if(local){
  filepath <- "/Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/3. Population_Dynamics_Model/pop-dynamics_April2020/demographic_model_1/"
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
source(paste0(filepath, "scripts/applied/applied_load_data.R")) 

## Dataset
datasets <- fix_age_data_full %>% select(-parameter) %>% colnames()
dataset <- datasets[1]
data_filename <- gsub("\\.","",dataset)

## Set filenames:

tdate <- Sys.Date()
filename1 <- paste0("applied_output_lhs_", data_filename, "-", tdate, ".csv")
filename1.rds <- paste0("applied_output_lhs_", data_filename, "-", tdate, ".RData")
filename2 <- paste0("applied_pars_lhs_", data_filename, "-",tdate, ".csv")
filename3 <- paste0("applied_outputSummary_lhs_", data_filename, "-",tdate, ".csv")

######################
## MODEL SETUP ##
######################

## Simulation:
TimeStop_dynamics <- 10*52 # 10 years (based on earlier simulations)
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
Vstart <- 5*52 # 20 years
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0

## Model Output
output <- "dynamics" # Output options: "summary" (stats with age-group prop), "summary_all" (stats with age-sex group prop), "dynamics" (pop metrics over time)


######################
## LHS parameters ##
######################

## Setup
lhs <- T
lhs_n <- 1e3 # number of sets (parameter combinations)
pairs_plot <- F
SA <- TRUE

if(dataset == "lesnoff.T"){
  rates <- "wkly"
}else{
  rates <- "yrly"
}

# set seed
set.seed(1) # so that results are consistent

pars_min <- paste0(dataset, ".min") # minimum val
pars_max <-  paste0(dataset, ".max") # maximum val
fixdata <- dataset # set dataset for state_vars
vardata <- dataset
if(lhs){
  vardata <- "value"
}
source("scripts/applied/applied_lhs.R") # output var_input_df contains all parameter combinations for uncertain demographics.

varnames <- colnames(var_input)


#######################
## Vaccination Setup ##
#######################
# see applied_vaccination
# 
source("scripts/applied/applied_vaccination.R")
Vstart <- Vprog %>% filter(Vround==1) %>% pull(Vweek)



##################
## RUN MODEL ##
##################
GSAoutput <- c(); 
# pR_noIm_df <- c(); 
# pop_dynamics <- c()

summary_df <- output_func(TimeStop_dynamics, output) # create empty dataframe to store output

turnover <- F; 
dynamics <- T; 
transmission <- F; 
clean_environment <- T

# timepoints for population growth
t2 <- 5*52 # 5 year pop growth
t1 <- TimeStop_dynamics

# Format demographic parameter sets:
var_input_backup <- var_input_df %>% as.data.frame() %>% slice_sample(n=5)
#var_input_backup <- var_demo_data_full %>% column_to_rownames("parameter") %>% t() %>% as.data.frame()
write_csv(var_input_backup, paste0(filepath, "output/", filename2)) # save parameter set
rm(var_input_df)

#####################
## PARALLELISATION ##
#####################

print("start-parallel")

cores=detectCores()
cl <- makeCluster(10)
registerDoParallel(cl)

## Local parallelisation
# if(local <- T){
#   cores=detectCores()
#   cl <- makeCluster(cores[1]-1)
#   registerDoParallel(cl)
# }
print("parallel-verified")
print("start-model")
# 
# library(doSNOW)
# cl <- makeCluster(2)
# registerDoSNOW(cl)
# 
# pb <- txtProgressBar(max = nrow(var_input_backup), style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)
# result <- foreach(i = 1:nrow(var_input_backup), 
#                   .packages = c("dplyr", "tibble"), 
#                   .options.snow = opts) %dopar% {
#                     ...
#                   }
# close(pb)
# stopCluster(cl) 

Out <- foreach (i = 1:nrow(var_input_backup), 
                   .packages = c("dplyr", "tibble"), #,
                   .combine = "rbind"
                ) %dopar% {
                     
                     print(i)
                     var_input_full <- unlist(var_input_backup[i,])
                     
                     App_func(
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
                       Vstart,
                       fixdata,
                       vardata
                     )
              }
stopCluster(cl)

# Out %>% mutate(set = rep(1:nrow(var_input_backup), each = TimeStop_dynamics))

# Summary statistics: pop grwoth & immunity metrics
# Out_summary <- sapply(Out, function(x)
#   GSA_output(x,Vstart)
#   ) %>%
#   t() %>%
#   as.data.frame() %>%
#   mutate(set = 1:nrow(var_input_backup))

# dynplot <- do.call(rbind, Out) %>% mutate(set = rep(1:length(Out), each = TimeStop_dynamics))

# Out = list of datafranes (same as RData)
write_csv(Out, paste0(filepath, "output/", filename1)) # save output
saveRDS(Out, paste0(filepath, "output/", filename1.rds)) # save output
write_csv(Out_summary, paste0(filepath, "output/", filename3)) 

