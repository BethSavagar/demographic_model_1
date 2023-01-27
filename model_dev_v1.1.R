## Flock Dynamics Model Code v1.1 (19/01/23)
# 27/01/23 
# -- think about use of apply and functions for creating the M/F vectors, and for updating population (demographic) states within the model
# what to use as output, run some tests of this demographic model

# Setup 

library(tidyverse)

# define parameters (probably a separate script in due course)

beta <- 1 # transmission rate, S-E (per hour)
sigma <- 1 # rate of becoming infectious , E-I (per hour)
gamma <- 1 # rate of becoming recovery, I-R (per hour)

N_tot <- 1700 # number of animals in population (flock)

# AGE GROUPS: max age = 5y, 1 week groups, = 261 (260.714)

max_age_F <- 5*52 # 5y*52w
max_age_M <- 3*52 # 3y*52w
wk2mnth <- 4.345 # no. of weeks per month
age_cuts <- c(0.5,1,1.5)
Imm <- 1:17 # Imm (1-4m) = 1w-17w
You <- 18:26 # Young (5-6m) = 18w-26w
Juv <- 27:52 # Juvenile (6-12m) = 27w-52w
Sub <- 53:78 # Sub-adult (12-18m) = 53w-78w
Adu_F <- 79:max_age_F # Adult (18m-5y) = 79w-261w
Adu_M <- 79:max_age_M # Adult (18m-5y) = 79w-261w


# demographic parameters: 

# waning of maternal immunity for first 4months (17 wk) 
Imm_b <- 0.92 # proportion of young born to immune mothers that gain maternal antibodies
Imm_wane <- data.frame(weeks = c(4,8,12), 
                       immunity = c(0.91,0.38,0.15)) # monthly decline in mat immunity (hammami 2016/18)
off_1 <- 0 # NET offtake rate <12M (per week) # NO trade of animals <12m
off_F <- 0 # NET offtake rate FEMALE >12M (per week)
off_M <- 0 # NET offtake rate MALE >12M (per week)
mort_1 <- 0.007 # natural mortality rate <6M (per week)
mort_2 <- 0.002 # natural mortality rate >6M  (per week)
# see https://math.stackexchange.com/questions/1122085/how-to-calculate-a-monthly-mortality-rate for calc of weekly rate from yearly mortality of 0.3, 0.1 respectively.
mort_end <- 1 # natural mortality rate final age cat  (per week)
ppr_mort_1 <- 0 # natural mortality rate <6M (per week)
ppr_mort_2 <- 0 # natural mortality rate >6M  (per week)
birth_r <- 1.5/52 # only animals >18M
age_p_f <- c(0,0,0.15,0.15,0.5) # proportion of population in each age group (initial)
age_p_m <- c(0,0,0.1,0.05,0.05)

# offtake
demos <- data.frame(age_cat = c("Imm","You","Juv","Sub","Adu"),
                    net_off_F = c(off_1,off_1,off_1,off_F,off_F),
                    net_off_M = c(off_1,off_1,off_1,off_M,off_M),
                    mort = c(mort_1,mort_1,mort_2,mort_2,mort_2),
                    ppr_mort = c(ppr_mort_1,ppr_mort_1,ppr_mort_2,ppr_mort_2,ppr_mort_2),
                    birth = c(0,0,0,0,birth_r),
                    age_p_F = age_p_f, # proportion of population in each age group F
                    age_p_M = age_p_m, # proportion of population in each age group M
                    n_weeks_F = c(length(Imm), length(You), length(Juv), length(Sub), length(Adu_F)), # number of weeks (sub-compartments) in each age group
                    n_weeks_M = c(length(Imm), length(You), length(Juv), length(Sub), length(Adu_M)) # number of weeks (sub-compartments) in each age group
                    )
                
# Female dataframe of demographic rates for each week-long age group in the population.
age_pars_F <- data.frame(
  age_weeks = 1:max_age_F,
  age_cat = c(rep("Imm",length(Imm)),
              rep("You",length(You)),
              rep("Juv",length(Juv)),
              rep("Sub",length(Sub)),
              rep("Adu",length(Adu_F)))) %>%
  # fill in maternal immunity
  left_join(Imm_wane, c("age_weeks" = "weeks")) %>%
  mutate(immunity = ifelse(is.na(immunity) & age_cat=="Imm", 1, 
                           if_else(is.na(immunity),0, immunity))
         ) %>%
  # join dataframe containing demographics
  left_join(demos, by = "age_cat") %>%
  select(-c(net_off_M, age_p_M, n_weeks_M)) %>%
  # divide population in each age group into week-long sub-compartments , equally
  mutate(pop_init_F = (age_p_F*N_tot)/n_weeks_F,
         mort = ifelse(age_weeks == max_age_F, 1, mort)) 


# Male dataframe of demographic rates for each week-long age group in the population.
age_pars_M <- data.frame(
  age_weeks = 1:max_age_M,
  age_cat = c(rep("Imm",length(Imm)),
              rep("You",length(You)),
              rep("Juv",length(Juv)),
              rep("Sub",length(Sub)),
              rep("Adu",length(Adu_M)))) %>%
  # fill in maternal immunity
  left_join(Imm_wane, c("age_weeks" = "weeks")) %>%
  mutate(immunity = ifelse(is.na(immunity) & age_cat=="Imm", 1, 
                           if_else(is.na(immunity),0, immunity))
  ) %>%
  # join dataframe containing demographics
  left_join(demos, by = "age_cat") %>%
  select(-c(net_off_F, age_p_F, n_weeks_F, birth)) %>%
  # divide population in each age group into week-long sub-compartments , equally
  mutate(pop_init_M = (age_p_M*N_tot)/n_weeks_M,
         mort = ifelse(age_weeks == max_age_M, 1, mort)) 

# initial POPULATION-STATE
Im_init_F <- rep(0,max_age_F); Im_init_M <- rep(0,max_age_M)
S_init_F <- age_pars_F %>% pull(pop_init_F)
S_init_M <- age_pars_M %>% pull(pop_init_M)
E_init_F <- rep(0,max_age_F); E_init_M <- rep(0,max_age_M)
I_init_F <- rep(0,max_age_F); I_init_M <- rep(0,max_age_M)
R_init_F <- rep(0,max_age_F); R_init_M <- rep(0,max_age_M)

#################################################################################
#################################################################################
## MODEL SET UP ## 
# potentially reconfigure this stuff as lists and use apply?

TimeStop_dynamics <- 52 # 1 year, weekly timestep
TimeStop_transmission <- 24 # 1 day, hourly timestep

# set up matrices for m/f disease states population trackers...
week_cols <- paste0(rep("w",TimeStop_dynamics), 1:TimeStop_dynamics)

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

# exposure
fE_mat <- as.data.frame(matrix(nrow = max_age_F, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = E_init_F)

mE_mat <- as.data.frame(matrix(nrow = max_age_M, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = E_init_M)

# immune
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


# Pull demographic rates as vectors from data-frames
# create if statements for if there's a difference between males and females?
immunity_F <- age_pars_F %>% pull(immunity)
immunity_M <- age_pars_M %>% pull(immunity)
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
  Im_births <- sum(birth* Imm_b* fR_mat[,w_prev]) # immune births (no M/F differentiation for births)
  S_births <- sum(birth*(1-Imm_b)* fR_mat[,w_prev]) + sum(birth*fS_mat[,w_prev]) # Sus births
  
  E_births <- 0; I_births <- 0; R_births <- 0 # set EIR births to 0
  
  
  ## DEMOGRAPHICS ##
  
  # Immune Offspring Demographics
  fIm_new <- fIm_mat[,w_prev]*(immunity_F - (net_off_F+mort_F)) # decline in mat immunity, offtake and mortality
  # fIm_new <- immunity_F*fIm_mat[,w_prev]- fIm_mat[,w_prev]*(net_off_F+mort_F) 
  mIm_new <- mIm_mat[,w_prev]*(immunity_M - (net_off_M+mort_M))
  
  fIm_mat[,w_cur] <- c(0.5*Im_births, fIm_new[1:max_age_F-1]) # 0.5*births for F/M, # subset demographics remove last age-group
  mIm_mat[,w_cur] <- c(0.5*Im_births, mIm_new[1:max_age_M-1]) # 0.5*births for F/M, # subset demographics remove last age-group
  
  # Susceptible Demographics
  fS_new <- fS_mat[,w_prev] +
    (1-immunity_F)*fIm_mat[,w_prev]- # add offspring which have lost immunity
    fS_mat[,w_prev]*(net_off_F+mort_F) # offtake and mortality
  
  mS_new <- mS_mat[,w_prev] +
    (1-immunity_M)*mIm_mat[,w_prev]- # add offspring which have lost immunity
    mS_mat[,w_prev]*(net_off_M+mort_M) # offtake and mortality
    
  fS_mat[,w_cur] <- c(0.5*S_births, fS_new[1:max_age_F-1])
  mS_mat[,w_cur] <- c(0.5*S_births, mS_new[1:max_age_M-1])
  
  # Exposed Demographics:
  fE_new <- fE_mat[,w_prev]- fE_mat[,w_prev]*(net_off_F+mort_F) # F offtake and mortality
  mE_new <- mE_mat[,w_prev]- mE_mat[,w_prev]*(net_off_M+mort_M) # M offtake and mortality
  
  fE_mat[,w_cur] <- c(0.5*E_births,fE_new[1:max_age_F-1]) # F add births and remove last age group
  mE_mat[,w_cur] <- c(0.5*E_births,mE_new[1:max_age_M-1]) # M add births and remove last age group
  
  # Infectious Demographics:
  fI_new <- fI_mat[,w_prev]- fI_mat[,w_prev]*(net_off_F+mort_F) # offtake and mortality (**ppr_mortality in disease loop?)
  mI_new <- mI_mat[,w_prev]- mI_mat[,w_prev]*(net_off_M+mort_M)
  
  fI_mat[,w_cur] <- c(0.5*I_births,fI_new[1:max_age_F-1]) # F add births (E_births = 0) and remove last age group
  mI_mat[,w_cur] <- c(0.5*I_births,mI_new[1:max_age_M-1]) # M add births (M_births = 0) and remove last age group
  
  # Recovered Demographics:
  fR_new <- fR_mat[,w_prev]- fR_mat[,w_prev]*(net_off_F+mort_F) # offtake and mortality (**ppr_mortality in disease loop?)
  mR_new <- mR_mat[,w_prev]- mR_mat[,w_prev]*(net_off_M+mort_M)
  
  fR_mat[,w_cur] <- c(0.5*R_births,fR_new[1:max_age_F-1]) # F add births (R_births = 0) and remove last age group
  mR_mat[,w_cur] <- c(0.5*R_births,mR_new[1:max_age_M-1]) # M add births (R_births = 0) and remove last age group
  
 
  ## OUTPUT ##
  
  # sum totals for each disease compartment, and total population size.
  totals <- data.frame(
    "time" = 1:TimeStop_dynamics,
    "Im" = apply(fIm_mat, 2, sum)+apply(mIm_mat, 2, sum),
    "S" = apply(fS_mat, 2, sum)+apply(mS_mat, 2, sum),
    "E" = apply(fE_mat, 2, sum)+apply(mE_mat, 2, sum),
    "I" = apply(fI_mat, 2, sum)+apply(mI_mat, 2, sum),
    "R" = apply(fR_mat, 2, sum)+apply(mR_mat, 2, sum)
  ) %>%
    mutate("all" = apply(.,1,sum))
}

