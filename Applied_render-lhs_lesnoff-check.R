# Script rendering applied-profiles with lhs on uncertain parameters
# Author: Beth Savagar
# Date: 10.10.23

# 9/11/23

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
TimeStop_dynamics <- 10*52 # 2 years only interested in rates for 1 y
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

dataset <- "lesnoff.yr"
data_filename <- gsub("\\.","",dataset)
  
if(dataset == "lesnoff.ft"){
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

source("scripts/applied/applied_lhs.R") # output var_input_df contains all parameter combinations for uncertain demographics.

# Format demographic parameter sets:
var_input_backup <- var_input_df %>% as.data.frame()

################
## SIMULATION ##
################
Out <- foreach (i = 1:nrow(var_input_backup), 
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

# female mortality <12m target
F1targ_minW <- 0.006
F1targ_maxW <- 0.007

F2targ_minW <- 0.0025
F2targ_maxW <- 0.0035


# calc F1 and F2 weekly mortality
mort_Fs <- sapply(Out, function(x) {
  testdf <- x %>% mutate(age = rep(1:156, TimeStop_dynamics))
  F0init <- testdf %>% filter(w == 1, age == 1) %>% pull("fpop")
  F12end <- testdf %>% filter(w == 52, age == 52) %>% pull("fpop")
  
  mort_A <- 1 - F12end / F0init # annual mortality
  mort_F <- 1 - ((1 - mort_A) ^ (1 / 26)) # fortnightly mortality
  
  return(mort_F)
})

summary(mort_Fs)

valid_mort <- which(mort_Fs>=F1targ_minW & mort_Fs<=F1targ_maxW)
valid_mort_df <- var_input_backup[valid_mort, ] %>% select(mortality_y, mortality_a)

ggplot(valid_mort_df, aes(x=mortality_y, y = mortality_a))+geom_point()


# AGe-Sex plots

agesex <- Out %>% 
  select(starts_with("pf"), starts_with("pm")) %>% 
  gather(key = "par", value = "prop")

ggplot(agesex, aes(x=par, y = prop))+geom_boxplot()
