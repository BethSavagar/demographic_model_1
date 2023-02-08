## Flock Dynamics Model Code v1.1 (19/01/23)
# 06/02/23 -- rudimentary model works fine.

# 30/01/23 
# -- think about use of apply and functions for creating the M/F vectors, and for updating population (demographic) states within the model
# what is the output, run some tests of this demographic model
# how to maintain population i.e. stability or growth?

## SETUP ##

library(tidyverse)

#########################################################################################################################################
# define parameters (probably a separate script in due course)

# Transmission Parameters (not coded yet)
beta <- NA # transmission rate, S-E (per hour)
sigma <- NA # rate of becoming infectious , E-I (per hour)
gamma <- NA # rate of becoming recovery, I-R (per hour)


# Flock Structure: Sex-Age groups
N_tot <- 1700 # number of animals in population (flock or village)?

# AGE GROUPS: max age = 5y Females 3y Males
max_age_F <- 5*52 # 5y*52w (52 as approximation for 1y)
max_age_M <- 3*52 # 3y*52w
wk2mnth <- 4.345 # no. of weeks per month
age_cuts_mnth <- c(4,6,12,18)
age_cuts_wk <- round(age_cuts_mnth*wk2mnth) # age-group boundaries in months
Imm <- 1:age_cuts_wk[1] # Immune Offspring (1-4m) = 1w-17w 
You <- (age_cuts_wk[1]+1):age_cuts_wk[2] # Young (5-6m) = 18w-26w  
Juv <- (age_cuts_wk[2]+1):age_cuts_wk[3] # Juvenile (6-12m) = 27w-52w
Sub <- (age_cuts_wk[3]+1):age_cuts_wk[4] # Sub-adult (12-18m) = 53w-78w
Adu_F <- (age_cuts_wk[4]+1):max_age_F # Adult F (18m-5y) = 79w-261w
Adu_M <- (age_cuts_wk[4]+1):max_age_M # Adult M (18m-3y) = 79w-261w

# Demographic Parameters

# # waning of maternal immunity for first 4months (17 wk) 
source("scripts/demographic-data/mat-imm-decay.R")
Imm_b <- imm_decay_corrected %>% filter(week_corrected ==0) %>% pull(imm) # proportion of young born to immune mothers that gain maternal antibodies
# Imm_wane <- data.frame(weeks = c(4,8,12), immunity = c(0.91,0.38,0.15)) # monthly decline in mat immunity (data from hammami 2016/18)
# 
# ## integrate new immunity calculation... 06/02/23

off_1 <- 0 # NET offtake rate <12M (per week) - NO trade of animals <12m
off_F <- 0.001 # NET offtake rate FEMALE >12M (per week) 
off_M <- 0.001 # NET offtake rate MALE >12M (per week)
mort_1 <- 0.007 # natural mortality rate <6M (per week)
mort_2 <- 0.002 # natural mortality rate >6M  (per week)
# see https://math.stackexchange.com/questions/1122085/how-to-calculate-a-monthly-mortality-rate for calc of weekly rate from yearly mortality of 0.3, 0.1 respectively.
mort_end <- 1 # natural mortality rate for final age group (per week)
ppr_mort_1 <- 0 # natural mortality rate <6M (per week)
ppr_mort_2 <- 0 # natural mortality rate >6M  (per week)
birth_r <- 1.5/52 # only animals >18M
age_p_f <- c(0,0,0.15,0.15,0.5) # proportion of population in each age group (initial)
age_p_m <- c(0,0,0.1,0.05,0.05)

# Dataframe of demographic parameters for age-sex group
demos <- data.frame(age_cat = c("Imm","You","Juv","Sub","Adu"), # age group
                    net_off_F = c(off_1,off_1,off_1,off_F,off_F), # female offtake
                    net_off_M = c(off_1,off_1,off_1,off_M,off_M), # male offtake
                    mort = c(mort_1,mort_1,mort_2,mort_2,mort_2), # natural mortality
                    ppr_mort = c(ppr_mort_1,ppr_mort_1,ppr_mort_2,ppr_mort_2,ppr_mort_2), # ppr_mortality
                    birth = c(0,0,0,0,birth_r), # birth rate (only Adu_F)
                    age_p_F = age_p_f, # proportion of population in each age group F
                    age_p_M = age_p_m, # proportion of population in each age group M
                    n_weeks_F = c(length(Imm), length(You), length(Juv), length(Sub), length(Adu_F)), # number of weeks (sub-compartments) in each age group
                    n_weeks_M = c(length(Imm), length(You), length(Juv), length(Sub), length(Adu_M)) # number of weeks (sub-compartments) in each age group
                    )

## CREATE M/F DATAFRAMES ##
                
# Female dataframe of demographic rates for each week-long age group in the population.
# rows = weeklong age gorup, cols = demographic rates 
age_pars_F <- data.frame(
  age_weeks = 1:max_age_F, # nrow = age in weeks
  age_cat = c(rep("Imm",length(Imm)),
              rep("You",length(You)),
              rep("Juv",length(Juv)),
              rep("Sub",length(Sub)),
              rep("Adu",length(Adu_F)))) %>%
  # fill in maternal immunity
  left_join(imm_decay_corrected, c("age_weeks" = "week_corrected")) %>%
  mutate(imm = ifelse(is.na(imm),0,imm)) %>%
  # left_join(Imm_wane, c("age_weeks" = "weeks")) %>%
  # mutate(immunity = ifelse(is.na(immunity) & age_cat=="Imm", 1, # immunity wanes at weeks 4,8,12 (and 17) between which immunity is maintained i.e. =1
  #                          if_else(is.na(immunity),0, immunity))) %>%
  # Join demographics (Female)
  left_join(demos, by = "age_cat") %>%
  # remove male variables
  select(-c(net_off_M, age_p_M, n_weeks_M)) %>%
  # divide initial population between age groups according to proportions in age_p_F
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
  left_join(imm_decay_corrected, c("age_weeks" = "week_corrected")) %>%
  mutate(imm = ifelse(is.na(imm),0,imm)) %>%
  # left_join(Imm_wane, c("age_weeks" = "weeks")) %>%
  # mutate(immunity = ifelse(is.na(immunity) & age_cat=="Imm", 1, # immunity wanes at weeks 4,8,12 (and 17) between which immunity is maintained i.e. =1
  #                          if_else(is.na(immunity),0, immunity))) %>%
  # Join demographics (Female)
  left_join(demos, by = "age_cat") %>%
  # remove female variables
  select(-c(net_off_F, age_p_F, n_weeks_F, birth)) %>%
  # divide initial population between age groups according to proportions in age_p_F
  mutate(pop_init_M = (age_p_M*N_tot)/n_weeks_M,
         mort = ifelse(age_weeks == max_age_M, 1, mort)) 


#########################################################################################################################################
## MODEL SETUP ##
# potentially reconfigure this stuff as lists and use apply?

TimeStop_dynamics <- 52 # 1 year, weekly timestep for demographic component
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
           "imm_frac" = (I+Im)/all, # immune proportion of population
           "time" = 1:TimeStop_dynamics) # timestep
}

ggplot(totals,aes(x=time,y=all))+
  geom_line()+
  labs(x="time (weeks)", y = "total population")+
  theme_bw()
