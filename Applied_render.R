###############################################################################################################
## Script for CGIAR dynamics
###############################################################################################################

# add group for young male offtake & raise max age of males in flock

filepath <- "/Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/3. Population_Dynamics_Model/pop-dynamics_April2020/demographic_model_1/"

## Sims script: set up 23 June to run base vaccination simulations
source(paste0(filepath,"scripts/setup.R"))
# list.files(paste0(filepath,"functions"), full.names = TRUE) %>% map(source)

# Functions:
source(paste0(filepath, "functions/demos_summary.R"))
source(paste0(filepath, "functions/dynmod-vac.R"))
source(paste0(filepath, "functions/GSA_outputs.R"))
source(paste0(filepath, "functions/Appliedfunc.R"))
source(paste0(filepath, "functions/output.R"))

source(paste0(filepath, "scripts/applied/load_cgiar.R"))

# -----------------------
## MODEL PARAMETERS ##

TimeStop_dynamics <- 25*52 # 10 years
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
Vstart <- 20*52 # 20 years
output <- "dynamics" # for parameter testing
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0
#
rates <- "yrly" # wkly

## RSA FOR-LOOP
RSAoutput <- c(); pR_noIm_df <- c(); pop_dynamics <- c()
summary_df <- output_func(TimeStop_dynamics, output) # create summary data frame to store summary stats for each timestep
turnover <- F; dynamics <- T; transmission <- F; 
clean_environment <- T

# timepoints for the population growth
t2 <- 15*52
t1 <- TimeStop_dynamics

var_input_full <- var_demo_data_full

# cgiar 1 is sheep, cgiar 2 is goats, lesnoff.T for lesnoff total 
fixdata <- "oc.goat.semiaridP"

if(fixdata == "lesnoff.T"){
  rates <- "2wkly"
}

output_dynamics <- App_func(
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


colnames(output_dynamics)
#ggplot(output_dynamics, aes(x=w, y=sum_pop))+geom_point()
#ggplot(output_dynamics, aes(x=w, y=prop_immune))+geom_point()

age_struc <- output_dynamics %>%
  select(w, starts_with("pf"), starts_with("pm")) %>%
  gather(key = "age_group", value="prop", -w)

#ggplot(age_struc, aes(x=w, y=prop, col=age_group, group = age_group))+geom_line()

## Just first 10 years

colnames(output_dynamics)

A <- ggplot(output_dynamics %>% filter(w<500), aes(x=w, y=sum_pop/75))+
  geom_line()+
  facet_wrap(profile)+
  labs(x="week", y="population-growth", title = "Population growth, 10 years")

B<- ggplot(output_dynamics, aes(x=w, y=prop_immune))+
  geom_point()+
  labs(x="week", y="proportion-immune", title = "Immune decay, 10 years")+
  coord_cartesian(xlim=c(0,500))

age_struc <- output_dynamics %>%
  select(w, starts_with("pf"), starts_with("pm")) %>%
  gather(key = "age_group", value="prop", -w)

C<- ggplot(age_struc, aes(x=w, y=prop, col=age_group, group = age_group))+
  geom_line()+
  labs(x="week", y="population-proportion", title = "age-sex structure, 10 years")+
  coord_cartesian(xlim=c(0,500))

ggarrange(A,B,C, ncol = 1)

############################################################
############################################################

datasource <- fix_age_data_full %>%
  select(starts_with("oc.goat")) %>%
  colnames

oc_df <- c()
for(i in 1:length(datasource)){
  
  fixdata <- datasource[i]
  
  output_dynamics <- App_func(
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
  output_df <- output_dynamics %>%
    mutate("profile" = fixdata)
  oc_df <- rbind(oc_df, output_df)
  
}

colnames(oc_df)
#ggplot(output_dynamics, aes(x=w, y=sum_pop))+geom_point()
#ggplot(output_dynamics, aes(x=w, y=prop_immune))+geom_point()


## Just first 10 years

# oc_df <- oc_df %>% 
#   mutate(profile = factor(profile,
#          levels = c("oc.shp.aridP",
#            "oc.shp.semiaridP", 
#            "oc.shp.semiaridM",
#            "oc.shp.subhumidM",
#            "oc.shp.humidM")))

oc_df <- oc_df %>% 
  mutate(profile = factor(profile,
                          levels = c("oc.goat.aridP",
                                     "oc.goat.semiaridP", 
                                     "oc.goat.semiaridM",
                                     "oc.goat.subhumidM",
                                     "oc.goat.humidM")))

A <- ggplot(oc_df %>% filter(w<500), aes(x=w, y=sum_pop/75))+
  geom_line()+
  facet_wrap(~profile,ncol=5)+
  labs(x="week", y="population-growth", title = "Population growth, 10 years")

B<- ggplot(oc_df, aes(x=w, y=prop_immune))+
  geom_point()+
  facet_wrap(~profile,ncol=5)+
  labs(x="week", y="proportion-immune", title = "Immune decay, 10 years")+
  coord_cartesian(xlim=c(0,500))

age_struc <- oc_df %>%
  select(w, starts_with("pf"), starts_with("pm"), profile) %>%
  gather(key = "age_group", value="prop", -c(w,profile))

C<- ggplot(age_struc, aes(x=w, y=prop, col=age_group, group = age_group))+
  geom_line()+
  facet_wrap(~profile,ncol=5)+
  labs(x="week", y="population-proportion", title = "age-sex structure, 10 years")+
  coord_cartesian(xlim=c(0,500))

ggarrange(A,B,C, ncol = 1)
