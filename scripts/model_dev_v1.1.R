## Flock Dynamics Model Code v1.1 (19/01/23)
# 06/02/23 -- rudimentary model works fine.

# 30/01/23 
# -- think about use of apply and functions for creating the M/F vectors, and for updating population (demographic) states within the model
# what is the output, run some tests of this demographic model
# how to maintain population i.e. stability or growth?

## SETUP ##

library(tidyverse)


#########################################################################################################################################
## MODEL SETUP ##
# potentially reconfigure this stuff as lists and use apply?

# Soure parameters script
source("scripts/set-pars.R")

TimeStop_dynamics <- 5*52 # 1 year, weekly timestep for demographic component
TimeStop_transmission <- 24 # 1 day, hourly timestep for transission component

# initial POPULATION-STATE
# define SEIR vectors for male and female age groups
Im_init_F <- rep(0,max_age_F); Im_init_M <- rep(0,max_age_M)
S_init_F <- age_pars_F %>% pull(pop_init_F) # all population begin S
S_init_M <- age_pars_M %>% pull(pop_init_M) # all population begin S
E_init_F <- rep(0,max_age_F); E_init_M <- rep(0,max_age_M)
I_init_F <- rep(0,max_age_F); I_init_M <- rep(0,max_age_M)
R_init_F <- rep(0,max_age_F); R_init_M <- rep(0,max_age_M)

# Sex-Age-Disease-State Tracking matrices
# rows = age in weeks, cols = time in weeks (dynamics model)

week_cols <- paste0(rep("w",TimeStop_dynamics), 1:TimeStop_dynamics)

# set matrices and populate with initial population state
# immune births
fIm_mat <- as.data.frame(matrix(nrow = max_age_F, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = Im_init_F)

mIm_mat <- as.data.frame(matrix(nrow = max_age_M, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = Im_init_M)

# susceptibles
fS_mat <- as.data.frame(matrix(nrow = max_age_F, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = S_init_F)

mS_mat <- as.data.frame(matrix(nrow = max_age_M, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = S_init_M)

# exposed
fE_mat <- as.data.frame(matrix(nrow = max_age_F, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = E_init_F)

mE_mat <- as.data.frame(matrix(nrow = max_age_M, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = E_init_M)

# infectious
fI_mat <- as.data.frame(matrix(nrow = max_age_F, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = I_init_F)

mI_mat <- as.data.frame(matrix(nrow = max_age_M, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = I_init_M)

# recovered
fR_mat <- as.data.frame(matrix(nrow = max_age_F, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = R_init_F)

mR_mat <- as.data.frame(matrix(nrow = max_age_M, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = R_init_M)

## DEMOGRAPHIC PARAMS
# Pull demographic rates as vectors from data-frames (for vectorised equations)
# NB: create if statements for if there's a difference between males and females?
immunity_F <- age_pars_F %>% pull(imm)
immunity_M <- age_pars_M %>% pull(imm)
# Imm_b immune birth rate is already defined (proportion of offspring born with maternal immunity)
net_off_F <- age_pars_F %>% pull(net_off_F)
net_off_M <- age_pars_M %>% pull(net_off_M)
mort_F <- age_pars_F %>% pull(mort)
mort_M <- age_pars_M %>% pull(mort)
ppr_mort_F <- age_pars_F %>% pull(ppr_mort)
ppr_mort_M <- age_pars_M %>% pull(ppr_mort)
birth <- age_pars_F %>% pull(birth)


###############################
## DEMOGRAPHICS LOOP ##
###############################

for(w in 2:TimeStop_dynamics){
  
  # update week of simulation
  w_prev <- paste0("w",w-1)
  w_cur <- paste0("w",w)

  ## BIRTHS ## 
  
  # Only females > 18M can give birth (birth rate =0 for all other age groups)
  # No M/F differentiation for births
  Im_births <- sum(birth* Imm_b* fR_mat[,w_prev]) # immune births 
  S_births <- sum(birth*(1-Imm_b)* fR_mat[,w_prev]) + sum(birth*fS_mat[,w_prev]) # Sus births = non-immune births to R mothers, and births to S mothers.
  
  E_births <- 0; I_births <- 0; R_births <- 0 # set EIR births to 0
  
  
  ## SEIR DEMOGRAPHICS ##
  
  # Immune Offspring Demographics (f/m)
  fIm_new <- fIm_mat[,w_prev]*(immunity_F)*(1-net_off_F)*(1-mort_F) # decline in mat immunity, offtake and mortality
  mIm_new <- mIm_mat[,w_prev]*(immunity_M)*(1-net_off_M)*(1-mort_M)
  
  fIm_mat[,w_cur] <- c(0.5*Im_births, fIm_new[1:max_age_F-1]) # 0.5*births for F/M, # subset demographics remove last age-group
  mIm_mat[,w_cur] <- c(0.5*Im_births, mIm_new[1:max_age_M-1]) # 0.5*births for F/M, # subset demographics remove last age-group
  
  ##
  # Susceptible Demographics
  fS_new <- fS_mat[,w_prev]*(1-net_off_F)*(1-mort_F) + # survival (those not offtake, and not died)
    fIm_mat[,w_prev]*(1-immunity_F) # add offspring which have lost immunity
  
  mS_new <- mS_mat[,w_prev]*(1-net_off_M)*(1-mort_M) +
    mIm_mat[,w_prev]*(1-immunity_M) # add offspring which have lost immunity
  
  # update fS and mS matrices
  fS_mat[,w_cur] <- c(0.5*S_births, fS_new[1:max_age_F-1]) # 0.5* births are F, lose last age group
  mS_mat[,w_cur] <- c(0.5*S_births, mS_new[1:max_age_M-1])# 0.5* births are M, lose last age group
  
  ##
  # Exposed Demographics:
  fE_new <- fE_mat[,w_prev]*(1-net_off_F)*(1-mort_F) # F offtake and mortality
  mE_new <- mE_mat[,w_prev]*(1-net_off_M)*(1-mort_M) # M offtake and mortality
  
  fE_mat[,w_cur] <- c(0.5*E_births,fE_new[1:max_age_F-1]) # F add births (E_births = 0)  and remove last age group
  mE_mat[,w_cur] <- c(0.5*E_births,mE_new[1:max_age_M-1]) # M add births (E_births = 0) and remove last age group
  
  ##
  # Infectious Demographics:
  fI_new <- fI_mat[,w_prev]*(1-net_off_F)*(1-mort_F) # F offtake and mortality (**ppr_mortality in disease loop?)
  mI_new <- mI_mat[,w_prev]*(1-net_off_M)*(1-mort_M) # M offtake and mortality
  
  fI_mat[,w_cur] <- c(0.5*I_births,fI_new[1:max_age_F-1]) # F add births (I_births = 0) and remove last age group
  mI_mat[,w_cur] <- c(0.5*I_births,mI_new[1:max_age_M-1]) # M add births (I_births = 0) and remove last age group
  
  # Recovered Demographics:
  fR_new <- fR_mat[,w_prev]*(1-net_off_F)*(1-mort_F) # offtake and mortality (**ppr_mortality in disease loop?)
  mR_new <- mR_mat[,w_prev]*(1-net_off_M)*(1-mort_M)
  
  fR_mat[,w_cur] <- c(0.5*R_births,fR_new[1:max_age_F-1]) # F add births (R_births = 0) and remove last age group
  mR_mat[,w_cur] <- c(0.5*R_births,mR_new[1:max_age_M-1]) # M add births (R_births = 0) and remove last age group
  
  # trnamsission loop here >>>>>
  
 
  ## OUTPUT ##
  # reduce output so that rather than storing entire matrices we just produce summary statistics: need to decide what stats are of interest (06/02/23)
  # totals = dataframe tracking population in each disease-state over time 
  # sum totals for each disease compartment, and total population size.
  totals <- data.frame(
    "Im" = apply(fIm_mat, 2, sum)+apply(mIm_mat, 2, sum),
    "S" = apply(fS_mat, 2, sum)+apply(mS_mat, 2, sum),
    "E" = apply(fE_mat, 2, sum)+apply(mE_mat, 2, sum),
    "I" = apply(fI_mat, 2, sum)+apply(mI_mat, 2, sum),
    "R" = apply(fR_mat, 2, sum)+apply(mR_mat, 2, sum)
  ) %>%
    mutate("all" = apply(.,1,sum), # total population size
           "imm_frac" = (R+Im)/all, # immune proportion of population
           "time" = 1:TimeStop_dynamics) # timestep
  
  ## CREATE OUTPUTS THAT DON'T DEPEND ON TRACKING MATRIX >>>>
  
  # total population size
  # proportion immune population
  
}

ggplot(totals,aes(x=time,y=all))+
  geom_line(size = 1.5)+
  labs(title = "Total Population",
       x="time (weeks)", 
       y = "total population")+
  theme_bw()

## test age-sex proportions

colnames(fS_mat) <- 1:ncol(fS_mat)
S_pop <- fS_mat %>%
  mutate(age_gp = age_pars_F %>% pull(age_cat)) %>%
  gather(-age_gp, key="week", value = "pop") %>%
  mutate(week=as.numeric(week))%>%
  group_by(age_gp, week) %>%
  summarise(pop=sum(pop))

ggplot(S_pop,aes(x=week,y=pop, group = age_gp, col = age_gp))+
  geom_line(size = 1.5)+
  labs(title = "FEMALE",
       x="time (weeks)", 
       y = "total population")+
  theme_bw()

colnames(mS_mat) <- 1:ncol(mS_mat)
mS_pop <- mS_mat %>%
  mutate(age_gp = age_pars_M %>% pull(age_cat)) %>%
  gather(-age_gp, key="week", value = "pop") %>%
  mutate(week=as.numeric(week))%>%
  group_by(age_gp, week) %>%
  summarise(pop=sum(pop))

ggplot(mS_pop,aes(x=week,y=pop, group = age_gp, col = age_gp))+
  geom_line(size = 1.5)+
  labs(title = "MALE",
       x="time (weeks)", 
       y = "total population")+
  theme_bw()


allS_pop <- S_pop %>%
  left_join(mS_pop, by=c("age_gp","week")) %>%
  mutate(all_pop = pop.x+pop.y)

ggplot(allS_pop,aes(x=week,y=all_pop, group = age_gp, col = age_gp))+
  geom_line(size = 1.5)+
  labs(title = "ALL (male & female)", 
       x="time (weeks)", 
       y = "total population")+
  theme_bw()
