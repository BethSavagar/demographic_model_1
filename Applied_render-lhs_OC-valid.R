# Script rendering applied-profiles with lhs on uncertain parameters
# Author: Beth Savagar
# Date: 10.10.23

# 21/10/23 - update script for applied-vaccination (see applied_render_local and applied_vaccination). 
# for each parameter set add loop through each different vaccination coverage level
# set up master loop to run through each different profile and save the output (possible this is the foreach statement?)

# 26/10/23 - update script with lhs for each parameter set, see if any output is valid

applied_example <- F
local <- T
############
## SETUP ##
############

# local filepath: 
if(local){
  filepath <- "/Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/3. Population_Dynamics_Model/pop-dynamics_April2020/demographic_model_1/"
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
datafile <- "-OC_lhs"
source(paste0(filepath, "scripts/applied/applied_load_data2.R")) 

## Datasets
datasets <- fix_age_data_full %>% select(-c(parameter)) %>% colnames()


######################
## MODEL SETUP ##
######################

## Simulation:
TimeStop_dynamics <- 10*52 # 10 years (based on earlier simulations)
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0

## Model Output
output <- "summary_all" # Output options: "summary" (stats with age-group prop), "summary_all" (stats with age-sex group prop), "dynamics" (pop metrics over time)


#######################
## Vaccination Setup ##
#######################
# see applied_vaccination
pV <- 1
source("scripts/applied/applied_vaccination.R")
Vstart <- Vprog %>% filter(Vround==1) %>% pull(Vweek)

##################################
## Demographic parameters - LHS ##
##################################

## Setup
lhs <- T
lhs_n <- 1e3 # number of sets (parameter combinations)
#lhs_n <- 5000
pairs_plot <- F
SA <- TRUE


##################################
## Valid Age-Sex Conditions ##
##################################

## Min and max proportions for each sex-age group (see age-sex SS)
pfKid.min <- 0.05; pfKid.max <- 0.19
pfSub.min <- 0.06; pfSub.max <- 0.19
pfAdu.min <- 0.21; pfAdu.max <- 0.62

pmKid.min <- 0.05; pmKid.max <- 0.16
pmSub.min <- 0.04; pmSub.max <- 0.15
pmAdu.min <- 0.01; pmAdu.max <- 0.15

##################
## RUN MODEL ##
##################


summary_df <- output_func(TimeStop_dynamics, output) # create empty dataframe to store output

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
# 

##############
## FOR LOOP ##
##############

# validOut = list to store outputs
GSAoutput <- c(); 
Out_list <- list()
validOut <- list()
validPars <- list()
# pR_noIm_df <- c(); 
# pop_dynamics <- c()

for(d in 1:length(datasets)){
  dataset <- datasets[d]
  data_filename <- gsub("\\.","",dataset)
  
  ## Set filenames:
  # tdate <- Sys.Date()
  # filename1 <- paste0("applied_output_lhs_", data_filename, "-", tdate, ".csv")
  # filename1.rds <- paste0("applied_output_lhs_", data_filename, "-", tdate, ".RData")
  # filename2 <- paste0("applied_pars_lhs_", data_filename, "-",tdate, ".csv")
  # filename3 <- paste0("applied_outputSummary_lhs_", data_filename, "-",tdate, ".csv")
  
  if(dataset == "lesnoff.T"){
    rates <- "wkly"
  }else{
    rates <- "yrly"
  }
  
  #########
  ## LHS ##
  #########
  
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
  
  # Format demographic parameter sets:
  var_input_backup <- var_input_df %>% as.data.frame()

  ################
  ## SIMULATION ##
  ################
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
        Vprog,
        fixdata,
        vardata
      )
    }
    
  Out_list[[d]] <- Out
  
  #########################################################
  # Retain behavioral parameter sets
  #########################################################
  
  # add set id to output df:
  Out <- Out %>%
    mutate(set = 1:nrow(Out))
  
  # tidy parameters dataframe: 
  var_input <- var_input_backup %>%
    select(NET_offtake_m, NET_offtake_m2, NET_offtake_f, everything()) %>%
    rename(off_mY = NET_offtake_m2,
           off_mA = NET_offtake_m,
           off_f = NET_offtake_f,
           mort_Y = mortality_y,
           mort_A = mortality_a,
           max_yrs_F = adu_f_max_yrs,
           max_yrs_M = adu_m_max_yrs,
           min_off = min_age_offtake,
           min_repro = min_age_repro
    ) %>%
    mutate(set = 1:nrow(var_input_backup))
  
  # Edit Out to contain identifier variables for pop growth (5%,15%) and pop growth with age-sex structure 
  Out_ext <- Out %>%
    mutate(tenyr_growth = replace_na(tenyr_growth, 0),
           # add variables to identify parameter sets with growth between 5% and 15%
           tenyr_15 = ifelse(tenyr_growth>=0.85 & tenyr_growth <=1.15, 1, 0),
           tenyr_05 = ifelse(tenyr_growth>=0.95 & tenyr_growth <=1.05, 1, 0)) %>%
    
    # add variables to identify parameter sets with growth between 5% and 15% AND age-sex conditions
    mutate(
      tenyr_15age = ifelse(
        tenyr_15 == 1 &
          pfKid >= pfKid.min & pfKid <= pfKid.max &
          pfSub >= pfSub.min & pfSub <= pfSub.max &
          pfAdu >= pfAdu.min & pfAdu <= pfAdu.max &
          
          pmKid >= pmKid.min & pmKid <= pmKid.max &
          pmSub >= pmSub.min & pmSub <= pmSub.max &
          pmAdu >= pmAdu.min & pmAdu <= pmAdu.max,1,0),
      
      tenyr_05age = ifelse(
        tenyr_05 == 1 &
          pfKid >= pfKid.min & pfKid <= pfKid.max &
          pfSub >= pfSub.min & pfSub <= pfSub.max &
          pfAdu >= pfAdu.min & pfAdu <= pfAdu.max &
          
          pmKid >= pmKid.min & pmKid <= pmKid.max &
          pmSub >= pmSub.min & pmSub <= pmSub.max &
          pmAdu >= pmAdu.min & pmAdu <= pmAdu.max,1,0)
    )
  

  ##################
  ## Valid Output ##
  ##################
  
  Out_valid <- Out_ext %>%
    filter(tenyr_15age == 1 |
             tenyr_05age == 1)
  
  ## Refine parameter dataframe:
  Out_parameters <- var_input %>%
    left_join(Out_ext %>%
                select(tenyr_growth,
                       tenyr_15,
                       tenyr_05,
                       tenyr_15age,
                       tenyr_05age,
                       set), by = c("set"))
  
  pars_valid <- Out_parameters %>% 
    filter(tenyr_15age == 1 |
             tenyr_05age == 1)  %>%
    select(-c(tenyr_15,
              tenyr_05,
              set))
  
  validOut[[d]] <- Out_valid
  validPars[[d]] <- pars_valid
  
}

tdate <- Sys.Date()
filename  <- paste0("applied_output_lhs2_ALL-", tdate, ".RData")
saveRDS(Out_list, file = paste0("output/Applied/", filename))
filename2  <- paste0("applied_output_lhs2_VALID-", tdate, ".RData")
saveRDS(validOut, file = paste0("output/Applied/", filename2))
filename3  <- paste0("applied_output_pars_VALID-", tdate, ".RData")
saveRDS(validPars, file = paste0("output/Applied/", filename3))
