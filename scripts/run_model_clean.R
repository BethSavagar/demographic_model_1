###############################
## RUN MODEL SCRIPT ##
###############################
# 13/03/23

# -----------------------
## SETUP ##
# Load libraries, functions etc
library(tidyverse)
library(readr)
source("functions/demos_summary.R")
source("functions/output.R")
source("functions/dynmod.R")


# -----------------------
## PARAMETERS ##

TimeStop_dynamics <- 25*52 # 1 year, weekly timestep for demographic component
TimeStop_transmission <- 24 # 1 day, hourly timestep for transission component
output <- "summary" # define output type "summary" or "count"
# if output is summary then we get a dataframe with: 
# week (w), sum_pop,"prop_immune","prop_inf","pKid","pYou","pJuv","pSub","pAdu","pF" 
# if output is summary_all we get a data frame with proportion in each sex-age-hroup

## Demographic Parameters ##

# define parameter set to use: "fixed", "baobab"...
flock_profile <- "script" # "baobab" # imm_test ()
pars_filepath <- paste0("scripts/parameters/set-pars-",flock_profile,".R")
source(pars_filepath) # within demographics script load in csvs for each set of parameters?

## Transmission Parameters ##
transmission <- F # include transmission? T = Yes, F = No

transmission_pars <- "fixed"
transmission_filepath <- paste0("scripts/parameters/set-pars-transmission-",transmission_pars,".R")
source(transmission_filepath)

## Clean Parameters Environment? ##
clean_environment <- T
if(clean_environment == T){
  rm(test_data,test_age_data, kid_f_prop,you_f_prop,juv_f_prop,sub_f_prop,adu_f_prop,
     kid_m_prop,you_m_prop,juv_m_prop,sub_m_prop,adu_m_prop,kid_max, you_max,juv_max,sub_max,max_age_F,max_age_M,
     fIm_init, fS_init, fE_init, fI_init, fR_init,mIm_init, mS_init, mE_init, mI_init, mR_init, off_1,off_F, off_M,mort_1,mort_2,mort_end,birth_r,ppr_mort_1, ppr_mort_2)
}

# -----------------------
## SUMMARY STATS ## 

# create summary data frame to store summary stats for each timestep
summary_df <- output_func(TimeStop_dynamics,output)

# nb need to add if statement to summary (if output == summary, if output == counts)
summary_df <- summary_demos(w=1, f_list, m_list, output, summary_df)


# -----------------------
## MODEL

output_df <- dynmod_func(
  f_list, # initial state of female population
  m_list, # initial state of male population
  TimeStop_dynamics, # 1 year, weekly timestep for demographic component
  TimeStop_transmission, # 1 day, hourly timestep for transission component
  output, # model output: tracker or summary stats
  demographic_pars,
  summary_df
  )


## plotting and analysis

summary_df <- output_df %>%
  mutate(w = as.numeric(w))

ggplot(summary_df,aes(x=w,y=sum_pop))+
  geom_line(size = 1.5)+
  labs(title = "Total Population",
       x="time (weeks)", 
       y = "total population")+
  theme_bw()


## melt summary_df to long format
summary_long <- summary_df %>%
  gather(key="stat", value="prop", -w)


## create df for different stats:

colors_agesex <- RColorBrewer::brewer.pal(6,"Set1")
colors_agesex[6] <- "black"
  
# age-sex
summary_agesex <- summary_long %>%
  filter(!stat %in% c("sum_pop",
                      "prop_inf",
                      "prop_immune")) %>%
  mutate(stat = ordered(stat, 
                        levels = c("pKid", "pYou", "pJuv", "pSub", "pAdu", "pF")))

## Plots:
# age-sex proportions
ggplot(summary_agesex, aes(x=w, y=prop, group=stat, col=stat))+
  geom_line(size=1)+
  # scale_color_brewer(palette = "Set1")+
  scale_color_manual(values = colors_agesex)+
  labs(x = "weeks", y = "Proportion of total population", col = "age-sex cat")+
  theme_bw()

# age_sex numbers:

numbers_long <- summary_df %>%
  mutate(nKid = pKid*sum_pop,
         nYou = pYou*sum_pop,
         nJuv = pJuv*sum_pop,
         nSub = pSub*sum_pop,
         nAdu = pAdu*sum_pop,
         nF = pF*sum_pop) %>%
  select(w,sum_pop,nKid, nYou, nJuv, nSub, nAdu, nF) %>%
  gather(key="stat", value="prop", -w)

agesex_total <- numbers_long %>%
  mutate(stat = ordered(stat, 
                        levels = c("nKid", "nYou", "nJuv", "nSub", "nAdu", "nF", "sum_pop")))

colors_Nagesex <- RColorBrewer::brewer.pal(7,"Set1")
colors_Nagesex[7] <- "black"

ggplot(agesex_total, aes(x=w, y=prop, group=stat, col=stat))+
  geom_line(size=1)+
  # scale_color_brewer(palette = "Set1")+
  scale_color_manual(values = colors_Nagesex)+
  labs(x = "weeks", y = "total population", col = "age-sex cat")+
  theme_bw()


# immunity
ggplot(summary_df, aes(x=w, y=prop_immune))+
  geom_line()

