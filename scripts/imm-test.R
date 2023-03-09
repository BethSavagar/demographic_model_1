
## Tests of Flock Dynamics Model 08/03/23

# 1) Offspring Immunity Dynamics (08/03/23)

# Run model with conditions to replicate Bodjo et al paper where offspring immunity dynamics are drawn from.
# •	Tested duration of maternal immunity in 112 lambs up to 150 days after birth, born to ewes vaccinated with the homologous PPR vaccine “Nigeria 75/1" at day 90 and day 120 of pregnancy.

# Parameters: 
# - set 112 lambs to immune compartment 
# - set rest of population to 0
# - set mortality, offtake, to 0

#####################################################################################################

## SETUP ##
# Load libraries etc
library(tidyverse)
source("functions/demos_summary.R")

## MODEL SETUP ##

TimeStop_dynamics <- 0.5*52 # 6 months enough for immunity test
TimeStop_transmission <- 24 # 1 day, hourly timestep for transission component
output <- "summary" # define output type "summary" or "count"

## TRANSMISSION PARAMETERS ##
transmission <- F # include transmission? T = Yes, F = No

## DEMOGRAPHIC PARAMETERS ##
# define parameter set to use: "fixed", "baobab"...
flock_profile <- "imm-test" # "baobab"
pars_filepath <- paste0("scripts/set-pars-",flock_profile,".R")
source(pars_filepath)

# -- Put lines 40-51 within set-pars.R? --
# Pull demographic rates as vectors from data-frames (for vectorised equations)
# NB: create if statements for if there's a difference between males and females?
immunity <- demographic_pars %>% pull(imm) # no differene m/f
# Imm_b immune birth rate is already defined (proportion of offspring born with maternal immunity)

net_off_F <- demographic_pars %>% pull(net_off_F)
net_off_M <- demographic_pars %>% pull(net_off_M)
mort <- demographic_pars %>% pull(mort) # no m/f difference
# mort_M <- age_pars_M %>% pull(mort)
ppr_mort <- demographic_pars %>% pull(ppr_mort)
# ppr_mort_M <- age_pars_M %>% pull(ppr_mort)
birth <- demographic_pars %>% pull(birth)
# ------------------------------------------

## INITIAL POPULATION STATES ##

pR <- 0 # proportion initially immune (due to prior infection)
# define SEIR vectors for male and female age groups
fIm_init <- rep(0,max_age_F); mIm_init <- rep(0,max_age_F)
fIm_init[1] <- 0.5*N_tot; mIm_init[1] <- 0.5*N_tot; 
fS_init <- rep(0,max_age_F); mE_init <- rep(0,max_age_F) # all S except already recovered
mS_init <- rep(0,max_age_F); mE_init <- rep(0,max_age_F) # all S except already recovered
fE_init <- rep(0,max_age_F); mE_init <- rep(0,max_age_F)
fI_init <- rep(0,max_age_F); mI_init <- rep(0,max_age_F)
fR_init <- rep(0,max_age_F); mR_init <- rep(0,max_age_F)

if(pR>0){
  fR_init <- demographic_pars %>% pull(pop_init_F) *pR
  mR_init <- demographic_pars %>% pull(pop_init_M) *pR
}


f_list <- list("fIm"=fIm_init,
               "fS"=fS_init,
               "fE"=fE_init,
               "fI"=fI_init,
               "fR"=fR_init)
m_list <- list("mIm"=mIm_init,
               "mS"=mS_init,
               "mE"=mE_init,
               "mI"=mI_init,
               "mR"=mR_init)

## SUMMARY STATS ## This goes in main text...
# create summary data frame to store summary stats for each timestep
if(output == "summary"){
  sum_stats <-  c("w", "sum_pop", "prop_immune", "prop_inf", "pKid", "pYou", "pJuv", "pSub", "pAdu", "pF")
  summary_df <- as.data.frame(matrix(0,nrow = TimeStop_dynamics,ncol = length(sum_stats)))
  colnames(summary_df) <- sum_stats
}else if(output == "counts"){
  # make df
}


# nb need to add if statement to summary (if output == summary, if output == counts)
summary_df <- summary_demos(w=1,f_list,m_list, output)


###############################
## DEMOGRAPHICS LOOP ##
###############################

for(w in 2:TimeStop_dynamics){
  
  if(w == 2){
    fIm_prev <- fIm_init ; mIm_prev <- mIm_init
    fS_prev <- fS_init ; mS_prev <- mS_init
    fE_prev <- fE_init ; mE_prev <- mE_init
    fI_prev <- fI_init ; mI_prev <- mI_init
    fR_prev <- fR_init ; mR_prev <- mR_init
  }
  
  ## BIRTHS ## 
  
  # Only females > 18M can give birth (birth rate =0 for all other age groups)
  # No M/F differentiation for births
  Im_births <- sum(birth* Imm_b* fR_prev) # immune births 
  S_births <- sum(birth*(1-Imm_b)* fR_prev) + sum(birth*fS_prev) # Sus births = non-immune births to R mothers, and births to S mothers.
  
  E_births <- 0; I_births <- 0; R_births <- 0 # set EIR births to 0
  
  
  ## SEIR DEMOGRAPHICS ##
  
  # Immune Offspring Demographics (f/m)
  fIm_new <- fIm_prev*(immunity)*(1-net_off_F)*(1-mort) # decline in mat immunity, offtake and mortality
  mIm_new <- mIm_prev*(immunity)*(1-net_off_M)*(1-mort)
  
  fIm_cur <- c(0.5*Im_births, fIm_new[1:max_age_F-1]) # 0.5*births for F/M, # subset demographics remove last age-group
  mIm_cur <- c(0.5*Im_births, mIm_new[1:max_age_F-1]) # 0.5*births for F/M, # subset demographics remove last age-group
  
  ##
  # Susceptible Demographics
  fS_new <- fS_prev*(1-net_off_F)*(1-mort) + # survival (those not offtake, and not died)
    fIm_prev*(1-immunity) # add offspring which have lost immunity
  
  mS_new <- mS_prev*(1-net_off_M)*(1-mort) +
    mIm_prev*(1-immunity) # add offspring which have lost immunity
  
  # update fS and mS matrices
  fS_cur <- c(0.5*S_births, fS_new[1:max_age_F-1]) # 0.5* births are F, lose last age group
  mS_cur <- c(0.5*S_births, mS_new[1:max_age_F-1])# 0.5* births are M, lose last age group
  
  ##
  # Exposed Demographics:
  fE_new <- fE_prev*(1-net_off_F)*(1-mort) # F offtake and mortality
  mE_new <- mE_prev*(1-net_off_M)*(1-mort) # M offtake and mortality
  
  fE_cur <- c(0.5*E_births,fE_new[1:max_age_F-1]) # F add births (E_births = 0)  and remove last age group
  mE_cur <- c(0.5*E_births,mE_new[1:max_age_F-1]) # M add births (E_births = 0) and remove last age group
  
  ##
  # Infectious Demographics:
  fI_new <- fI_prev*(1-net_off_F)*(1-mort) # F offtake and mortality (**ppr_mortality in disease loop?)
  mI_new <- mI_prev*(1-net_off_M)*(1-mort) # M offtake and mortality
  
  fI_cur <- c(0.5*I_births,fI_new[1:max_age_F-1]) # F add births (I_births = 0) and remove last age group
  mI_cur <- c(0.5*I_births,mI_new[1:max_age_F-1]) # M add births (I_births = 0) and remove last age group
  
  # Recovered Demographics:
  fR_new <- fR_prev*(1-net_off_F)*(1-mort) # offtake and mortality (**ppr_mortality in disease loop?)
  mR_new <- mR_prev*(1-net_off_M)*(1-mort)
  
  fR_cur <- c(0.5*R_births,fR_new[1:max_age_F-1]) # F add births (R_births = 0) and remove last age group
  mR_cur <- c(0.5*R_births,mR_new[1:max_age_F-1]) # M add births (R_births = 0) and remove last age group
  
  
  # -----------------------
  ## TRANSMISSION LOOP ##
  #
  #
  #
  # ----------------------
  
  
  
  ## OUTPUT ##
  
  f_list <- list("fIm"=fIm_cur,
                 "fS"=fS_cur,
                 "fE"=fE_cur,
                 "fI"=fI_cur,
                 "fR"=fR_cur)
  m_list <- list("mIm"=mIm_cur,
                 "mS"=mS_cur,
                 "mE"=mE_cur,
                 "mI"=mI_cur,
                 "mR"=mR_cur)
  
  summary_df <- summary_demos(w,f_list,m_list,output)
  
  ## UPDATE STATES ##
  
  fIm_prev <- fIm_cur ; mIm_prev <- mIm_cur
  fS_prev <- fS_cur ; mS_prev <- mS_cur
  fE_prev <- fE_cur ; mE_prev <- mE_cur
  fI_prev <- fI_cur ; mI_prev <- mI_cur
  fR_prev <- fR_cur ; mR_prev <- mR_cur
  
}

## plotting and analysis

summary_df <- summary_df %>%
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

# immunity
ggplot(summary_df, aes(x=w, y=prop_immune))+
  geom_line()+
  geom_line(data = imm_decay_corrected, aes(x=wk, y=imm), col = "red")+coord_cartesian(xlim=c(0,20))+
  scale_x_continuous(breaks = seq(0,20,1))+
  scale_y_continuous(breaks = seq(0,1,0.1))
  # geom_point(data = data.frame("w"=1:length(immunity),"imm"=immunity), aes(x=w, y=imm), col ="blue")+
  
