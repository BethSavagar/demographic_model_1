
filepath <- "/storage/users/bsavagar/dynmod/"

###############################################################################################################
# Set Up for Parallel Computing
# 
# cores=detectCores()
# cl <- makeCluster(30) #not to overload your computer
# registerDoParallel(cl)
# ########
# # File saving:
# #
# tdate <- Sys.Date()
# filename <- paste0("RSA_output_", tdate, ".RData")
###########################

# -----------------------
## SETUP ##
# Load libraries, functions etc
library(tidyverse)
library(readr)
library(here)
library(ggpubr)
library(knitr)
here()
source("functions/demos_summary.R")
source("functions/output.R")
source("functions/dynmod.R")

fix_age_data_full <- read_csv("data/RSA_parameters/RSA_fix_input.csv",col_names=T)
var_demo_data_full <- read_csv("data/RSA_parameters/RSA_var_input.csv", col_names=T)
imm_decay_corrected <- read_csv("data/imm_decay_bodjo_v2.csv") # "scripts/demographic-data/mat-imm-decay.R" for workings

# -----------------------
## MODEL PARAMETERS ##

TimeStop_dynamics <- 10*52 # 10 years
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
output <- "summary_all" # define output type "summary" (age proporiotns), "summary_all" (age-sex proportions) or "count"
min_pop <- 1 # set minimum size of population
pars_filename <- "set_pars_RSA.R"
lhs_n <- 1e4

# ---------------------------------
## SENSITIVITY ANALYSIS PARAMETERS:

SA <- TRUE


if(SA == TRUE){
  # select parameter min-max pair (see RSA_var_input.csv)
  pars_min <- "min.3"
  pars_max <- "max.3"
  # latin hypercube sampling of parameter space:
  # output is var_input_set dataframe with sampled parameter sets for each variable input (demographic) parameter
  source("scripts/RSA/RSA_lhs.R")
}else{
  # test_1 dataset, all aprameters set to 0 and all animals in age group 1 (susceptible)
  dataset1 <- "sim.1" # select dataset for test data
  dataset2 <- "sim.1" # select dataset for age data
}

## RSA FOR-LOOP
RSAoutput <- c()

for(i in 1:lhs_n){
  
  source("scripts/parameters/set-pars-RSA.R")
  # output is demographic pars dataframe (for sex-age-groupings) and starting population m_list, f_list
  
  
  # ---------------------------------
  ## TRANSMISSION PARAMETERS:
  transmission <- F # include transmission? T = Yes, F = No
  
  # transmission_pars <- "fixed"
  # transmission_filepath <- paste0("scripts/parameters/set-pars-transmission-",transmission_pars,".R")
  # source(transmission_filepath)
  source(here("scripts", "parameters", "set-pars-transmission-fixed.R"))
  print(c(i,off_F ,off_M ,mort_1,mort_2,birth_r))
  ## Clean Parameters Environment? ##
  clean_environment <- T
  if (clean_environment == T) {
    rm(var_demo_data,fix_age_data,kid_f_prop,sub_f_prop,adu_f_prop,kid_m_prop,sub_m_prop,adu_m_prop,kid_max,sub_max, max_age_F,max_age_M,fIm_init,fS_init,fE_init,fI_init,fR_init,mIm_init,mS_init,mE_init,mI_init,mR_init,off_1,off_F,off_M,mort_1,mort_2,birth_r,ppr_mort_1,ppr_mort_2)
  }
  
  # -----------------------
  ## SUMMARY STATS ##
  
  # create summary data frame to store summary stats for each timestep
  summary_df <- output_func(TimeStop_dynamics, output)
  
  # nb need to add if statement to summary (if output == summary, if output == counts)
  summary_df <- summary_demos(w = 1, f_list, m_list, output, summary_df)
  
  output_df <- dynmod_func(
    f_list,# initial state of female population
    m_list,# initial state of male population
    TimeStop_dynamics,# 1 year, weekly timestep for demographic component
    TimeStop_transmission,# 1 day, hourly timestep for transission component
    output,# model output: tracker or summary stats
    demographic_pars,
    summary_df
  )
  
  output_df <- output_df %>% replace(is.na(.), 0)
  
  res <- output_df %>%
    filter(w==TimeStop_dynamics) %>%
    mutate(pop_growth = output_df[nrow(output_df),"sum_pop"] / output_df[1,"sum_pop"], 
           set = i)
  
  RSAoutput <- RSAoutput %>%
    rbind(res)
}


######################################
## SAVE ##

tdate <- Sys.Date()
filename <- paste0("RSAoutput_", tdate, ".csv")
# save RData
write.csv(RSAoutput, paste0(filepath, "outputs/", filename))
# write.csv(Runs_tracker, "~/folder/runs_tracker.csv")
