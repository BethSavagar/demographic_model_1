###############################################################################################################
## Script for RSA on RVC cluster - 04/07/23
###############################################################################################################
# this code needs modifying for cluster
# - makeCluster with 30cores
# - lhs_n = 1e4
# - parallel computing with foreach and doPar packages

filepath <- "/storage/users/bsavagar/"
## Sims script: set up 23 June to run base vaccination simulations
source(paste0(filepath,"scripts/setup.R"))
list.files(paste0(filepath,"functions"), full.names = TRUE) %>% map(source)
source(paste0(filepath, "scripts/load_data.R"))
# #
# ###############################################################################################################

## SET FILENAMES ##

tdate <- Sys.Date()
filename1 <- paste0("RSA_output_valid_", tdate, ".csv")
filename2 <- paste0("RSA_pars_valid_", tdate, ".csv")

# -----------------------
## MODEL PARAMETERS ##

TimeStop_dynamics <- 25*52 # 10 years
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
output <- "summary_all" # define output type "summary" (age proporiotns), "summary_all" (age-sex proportions) or "count"
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0
pars_filename <- "set_pars_RSA2.R"
lhs_n <- 5e5

# ---------------------------------
## SENSITIVITY ANALYSIS PARAMETERS:
pairs_plot <- F
SA <- TRUE
rates <- "yrly"
set.seed(1)

# select parameter min-max pair (see RSA_var_input.csv)
pars_min <- "min.5" # min.3 & max.3 for all sims pre 24/07/23)
pars_max <- "max.5" # min.4, max.4 include young male offtake and increase age of males
if(rates == "wkly"){
  pars_min <- "wkly.min.1"
  pars_max <- "wkly.max.1"
}
fixdata <- "sim.4" # fix_input_2 : sim.2 for pR=1... 
# fixdata <- "sim.3" # sim.3 for N = 100 (14-07-2023)
vardata <- "sim.4" # select dataset for test data, # test_1 dataset, all pars set to 0 and all animals in age group 1 (susceptible)

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
write_csv(var_input_backup, paste0(filepath, "output/", filename2)) # save paramter set
rm(var_input_set)


## PARALLELISATION ##

cores=detectCores()
cl <- makeCluster(50) # for running locally
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

#########################################################
# Analysis 1 - Retain behavioral parameter sets
#########################################################

# add set id to output df:
RSAout <- RSAout %>%
  mutate(set = 1:nrow(RSAout))

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

RSAoutput_ext <- RSAout %>%
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

write_csv(RSAout_valid, paste0(filepath, "output/", filename1)) # save output
write_csv(RSApars_valid, paste0(filepath, "output/", filename2)) # save output

