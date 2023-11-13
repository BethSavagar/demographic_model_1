###############################################################################################################
## Script for RSA on RVC cluster - 04/07/23
###############################################################################################################
# Test function locally - 9Nov23
# Verified function works.
# LHS of parameter space to identify valid parameter sets
# Using RSAfunc to produce population growth and age-sex structure of population (output %in% summary, summary_all)
# Or produce full pop dynamics (output == dynamics)

## Filepath ##
local <- T
if(local){
  filepath <- ""
}else(
  filepath <- "/storage/users/bsavagar/"
)
filesave <- F

## Setup ##

source(paste0(filepath,"scripts/setup.R"))

## Functions ##

source(paste0(filepath, "functions/demos_summary.R")) # compute output statistics (dynamics, summary, summary_all)
source(paste0(filepath, "functions/RSAfunc.R")) # wrapper function for fynamics model
source(paste0(filepath, "functions/dynmod.R")) # dynamocs model simulation
source(paste0(filepath, "functions/output.R")) # generate output dataframe

## Data ##
source(paste0(filepath, "scripts/load_data.R"))
# #
# ###############################################################################################################

# -----------------------
## SET FILENAMES ##

if(filesave){
  tdate <- Sys.Date()
  filename1 <- paste0("RSA_output_", tdate, ".csv")
  filename1rds <- paste0("RSA_output_", tdate, ".rds")
  filename2 <- paste0("RSA_pars_", tdate, ".csv")
}


# -----------------------
## MODEL PARAMETERS ##

TimeStop_dynamics <- 25*52 # 10 years
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
output <- "summary_all" # define output type "summary" (age proporiotns), "summary_all" (age-sex proportions) or "dynamics"
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0
lhs_n <- 2e5

# ---------------------------------
## SENSITIVITY ANALYSIS PARAMETERS:
pairs_plot <- F
SA <- TRUE
rates <- "yrly"

# var_demo_data min-max pair: yearly rate
pars_min <- "min.final" # final min - max pair
pars_max <- "max.final" # 
fixdata <- "sim.final" # final state-vars 
# vardata <- "sim.4" ?

set.seed(1)
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
RSAoutput <- c(); 
summary_df <- output_func(TimeStop_dynamics, output) # create summary data frame to store summary stats for each timestep
turnover <- F; 
transmission <- F; 
clean_environment <- T

# timepoints for the population growth
t2 <- TimeStop_dynamics - (10*52) # 10 yrs before end of simulation
t1 <- TimeStop_dynamics

## DEFINE PARAMETER SET: 

var_input_backup <- var_input_set %>% as.data.frame()
if(local){
  var_input_backup <- var_input_set %>% as.data.frame() %>% slice_sample(.,n=5)
}

if(filesave){
  write_csv(var_input_backup, paste0(filepath, "output/", filename2)) # save paramter set
  rm(var_input_set)
}


## PARALLELISATION ##

if(local){
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) # for running locally
  registerDoParallel(cl)
}else{
  cores=detectCores()
  cl <- makeCluster(50) # for running locally
  registerDoParallel(cl)
}

RSAout <- foreach (i = 1:nrow(var_input_backup), 
                   .packages = c("dplyr")) %dopar% {
                   
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

if(output %in% c("summary", "summary_all")){
  RSAout <- do.call(rbind, RSAout)
  if(filesave == T){
    write_csv(RSAout, paste0(filepath, "output/", filename1)) # save output .csv if output is summary (as df)
  }
}else if(output == "dynamics" & filesave){
  saveRDS(RSAout, paste0(filepath, "output/", filename1rds)) # save output RDS if output is dynamics (as list)
}


# IF VALID-ONLY

#########################################################
# Analysis 1 - Retain behavioral parameter sets
#########################################################

# if output is dynamics quickly convert to summary dataframe:
if(output == "dynamics"){
  
  RSAoutput <- sapply(RSAout, function(x)
                      x %>%
                        filter(w==TimeStop_dynamics) %>%
                        mutate(pop_growth = x[nrow(x),"sum_pop"] / x[1,"sum_pop"],
                               tenyr_growth = (x[t1, "sum_pop"]) / x[t2, "sum_pop"],
                               tenyr_anngrowth = (x[t1, "sum_pop"] - (x[t2, "sum_pop"]))/((t1-t2)/52)
                        )) %>%
    t() %>%
    as.data.frame() %>%
    # add set id to df
    mutate(set = 1:nrow(.))
}else{
  # add set id to output df:
  RSAoutput <- RSAout %>%
    mutate(set = 1:nrow(.))
}




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

# Define age-sex structure conditions: 

## Min and max proportions for each sex-age group (see age-sex SS)
pfKid.min <- 0.05; pfKid.max <- 0.19
pfSub.min <- 0.06; pfSub.max <- 0.19
pfAdu.min <- 0.21; pfAdu.max <- 0.62

pmKid.min <- 0.05; pmKid.max <- 0.16
pmSub.min <- 0.04; pmSub.max <- 0.15
pmAdu.min <- 0.01; pmAdu.max <- 0.15

#####################################

RSAoutput_ext <- RSAoutput %>%
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

RSAout_valid <- RSAoutput_ext %>%
  filter(tenyr_15age == 1 |
           tenyr_05age == 1)
## Refine parameter dataframe:

RSAparameters <- var_input %>%
  left_join(RSAoutput_ext %>%
              select(tenyr_growth,
                     tenyr_15,
                     tenyr_05,
                     tenyr_15age,
                     tenyr_05age,
                     set), by = c("set"))

# RSA pars 15 age contains parameter sets which produce valid results

RSApars_valid <- RSAparameters %>% 
  filter(tenyr_15age == 1 |
           tenyr_05age == 1)  %>%
  select(-c(tenyr_15,
            tenyr_05,
            set))

valid_filename1

if(filesave){
  valid_filename1 <- paste0("RSA_validOut_", tdate, ".csv")
  valid_filename2 <- paste0("RSA_validPars_", tdate, ".csv")
  
  write_csv(RSAout_valid, paste0(filepath, "output/", valid_filename1)) # save output
  write_csv(RSApars_valid, paste0(filepath, "output/", valid_filename2)) # save output
  
}
