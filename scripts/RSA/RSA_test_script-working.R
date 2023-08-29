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

# -----------------------
## PARAMETERS ##

TimeStop_dynamics <- 10*52 # 10 years
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
output <- "summary" # define output type "summary" (age proporiotns), "summary_all" (age-sex proportions) or "count"
min_pop <- 1 # set minimum size of population

# test_1 dataset, all aprameters set to 0 and all animals in age group 1 (susceptible)
dataset1 <- "sim.1" # select dataset for test data
dataset2 <- "sim.1" # select dataset for age data

i <- sample(1:1000,1)
SA <- TRUE
source(here("scripts","parameters","set-pars-RSA.R"))

## Transmission Parameters ##
transmission <- F # include transmission? T = Yes, F = No

# transmission_pars <- "fixed"
# transmission_filepath <- paste0("scripts/parameters/set-pars-transmission-",transmission_pars,".R")
# source(transmission_filepath)
source(here("scripts","parameters","set-pars-transmission-fixed.R"))

## Clean Parameters Environment? ##
clean_environment <- T
if(clean_environment == T){
  rm(var_demo_data,fix_age_data, kid_f_prop,sub_f_prop,adu_f_prop,
     kid_m_prop,sub_m_prop,adu_m_prop,kid_max,sub_max,max_age_F,max_age_M,
     fIm_init, fS_init, fE_init, fI_init, fR_init,mIm_init, mS_init, mE_init, mI_init, mR_init, off_1,off_F, off_M,mort_1,mort_2,mort_end,birth_r,ppr_mort_1, ppr_mort_2)
}

# -----------------------
## SUMMARY STATS ## 

# create summary data frame to store summary stats for each timestep
summary_df <- output_func(TimeStop_dynamics,output)

# nb need to add if statement to summary (if output == summary, if output == counts)
summary_df <- summary_demos(w=1, f_list, m_list, output, summary_df)



output_df <- dynmod_func(
  f_list, # initial state of female population
  m_list, # initial state of male population
  TimeStop_dynamics, # 1 year, weekly timestep for demographic component
  TimeStop_transmission, # 1 day, hourly timestep for transission component
  output, # model output: tracker or summary stats
  demographic_pars,
  summary_df
)




summary_df <- output_df %>%
  mutate(w = as.numeric(w))

p1 <- ggplot(summary_df,aes(x=w,y=sum_pop))+
  geom_line(size = 1.5)+
  labs(title = "Total Population",
       x="time (weeks)", 
       y = "total population")+
  theme_bw()


## melt summary_df to long format
summary_long <- summary_df %>%
  gather(key="stat", value="prop", -w)


## create df for different stats:

colors_agesex <- RColorBrewer::brewer.pal(4,"Set1")
colors_agesex[4] <- "black"
  
# age-sex
summary_agesex <- summary_long %>%
  filter(!stat %in% c("sum_pop",
                      "prop_inf",
                      "prop_immune")) %>%
  mutate(stat = ordered(stat, 
                        levels = c("pKid", "pSub", "pAdu", "pF")))


## Plots:
# age-sex proportions
p2 <- ggplot(summary_agesex, aes(x=w, y=prop, group=stat, col=stat))+
  geom_line(size=1)+
  # scale_color_brewer(palette = "Set1")+
  scale_color_manual(values = colors_agesex)+
  labs(title = "Population Proportion",
       x = "weeks", 
       y = "Proportion of total population", 
       col = "age-sex cat")+
  facet_wrap(~stat)+
  theme_bw()

# immunity
p3 <- ggplot(summary_df, aes(x=w, y=prop_immune))+
  geom_line()+
  coord_cartesian(xlim=c(0,20))+
  scale_x_continuous(breaks = seq(0,20,1))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(title = "Immune Dynamics",
       x = "Weeks",
       y = " Proportion Immune")+
  theme_bw()
# geom_point(data = data.frame("w"=1:length(immunity),"imm"=immunity), aes(x=w, y=imm), col ="blue")+

p1
