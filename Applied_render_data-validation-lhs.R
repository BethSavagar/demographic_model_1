###############################################################################################################
## Flock Dynamics - Applied Profile ~ Baseline/Basic Example
###############################################################################################################
# Script for validating data and model output of Applied-Examples
# Author: Beth Savagar
# Date: 03/02/2023

############
## SETUP ##
############

filepath <- "/Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/3. Population_Dynamics_Model/pop-dynamics_April2020/demographic_model_1/"

## Libraries
source(paste0(filepath,"scripts/setup.R")) 

## Functions:
source(paste0(filepath, "functions/Appliedfunc.R")) # wrapper function
source(paste0(filepath, "functions/demos_summary.R")) # summary stats for output
source(paste0(filepath, "functions/dynmod-vac.R")) # flock dynamics model function
source(paste0(filepath, "functions/output.R")) # what to include in output df
source(paste0(filepath, "functions/GSA_outputs.R")) # summary stats accounting for vac

## Load parameters
applied_example <- T
# Select dataset:
data <- "-lesnoff-lhs" 
demfile <- paste0("demographics", data, ".csv")
statefile <- paste0("state_vars", data, ".csv")

source(paste0(filepath, "scripts/applied/applied_load_data.R")) 

## Dataset
datasets <- fix_age_data_full %>% select(-parameter) %>% colnames()

######################
## MODEL SETUP ##
######################

## Simulation:
TimeStop_dynamics <- 10*52 # 10 years (based on earlier simulations)
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0

## Model Output
output <- "dynamics" # Output options: "summary" (stats with age-group prop), "summary_all" (stats with age-sex group prop), "dynamics" (pop metrics over time)


#######################
## Vaccination Setup ##
#######################
# Not interested in vaccination here
pV <- 1 
source("scripts/applied/applied_vaccination.R")
Vstart <- Vprog %>% filter(Vround==1) %>% pull(Vweek)


############################
## Demographic parameters ##
############################

## Setup
lhs <- T
lhs_n <- 1e3 # number of sets (parameter combinations)
#lhs_n <- 5000
pairs_plot <- F
SA <- TRUE

##################
## RUN MODEL ##
##################
GSAoutput <- c(); 
Out <- list()
vOut <- list()

summary_df <- output_func(TimeStop_dynamics, output) # create empty dataframe to store output

#turnover <- F; 
# transmission <- F; 
clean_environment <- T

# timepoints for population growth
t2 <- TimeStop_dynamics/2 # 5 year pop growth
t1 <- TimeStop_dynamics

# # Format demographic parameter sets:
# var_input_backup <- var_demo_data_full %>% column_to_rownames("parameter") %>% t() %>% as.data.frame()
# Format demographic parameter sets:
var_input_backup <- var_input_df %>% as.data.frame()
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



####################
## OUTPUT SUMMARY ##
####################

Out <- Out %>% mutate(set = rep(1:nrow(var_input_backup), each = TimeStop_dynamics))
summary_all <- c()
for(i in 1:nrow(var_input_backup)){
  Out_i <- Out %>% filter(set == i)
  
  sevenyr_growth <- Out_i[t1, "sum_pop"] / Out_i[t1-(52*7), "sum_pop"]
  twoyr_growth <- Out_i[t1, "sum_pop"] / Out_i[t1-(52*2), "sum_pop"]
  
  summary_i <- GSA_output(Out_i,Vstart) %>% mutate(set = i,
                                                   sevenyr_growth = sevenyr_growth,
                                                   twoyr_growth = twoyr_growth)
summary_all <- rbind(summary_all, summary_i)
}
  
dynplot_i <- Out

## AGE STRUCTURE
age_struc <- dynplot_i %>%
  select(w, set, starts_with("pf"), starts_with("pm")) %>%
  gather(key = "age_group", value="prop", -c(w,set))

age_boxplot <- age_struc %>% filter(w == max(w))

ggplot(age_struc, aes(x=age_group, y = prop))+
         geom_boxplot()

plot_age <- ggplot(age_struc, aes(x = w, y = prop,col = age_group,group = age_group)) + 
  geom_line() + 
  labs(x = "week", y = "population-propotion", title = "Age Structure") +
  theme_bw()+
  theme(legend.position = "right")
  # theme(legend.position = "bottom") +
  # guides(color = guide_legend(nrow = 1))

minpop <- dynplot_i %>% filter(w==1) %>% slice(1) %>% pull(sum_pop)

plot_growth <- ggplot(dynplot_i, aes(x=w, y=sum_pop/minpop))+
  geom_line()+
  labs(x="week", y="population-growth", title = "Population growth")+
  theme_bw()

ggplot(dynplot_i %>% filter(w==max(w)), aes(x=w, y=sum_pop/minpop))+
  geom_boxplot()+
  theme_bw()

ggarrange(plot_age, plot_growth, ncol=1)



