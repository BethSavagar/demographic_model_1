## Flock Dynamics Model Code v1.1 (19/01/23)
# task for next week: 
# -- think about splitting up males and females and how to do this with matrices/vectors

# Setup 

library(tidyverse)

# define parameters (probably a separate script in due course)

beta <- 1 # transmission rate, S-E (per hour)
sigma <- 1 # rate of becoming infectious , E-I (per hour)
gamma <- 1 # rate of becoming recovery, I-R (per hour)

birth <- 1 # birth rate (per week)
N_tot <- 1700 # number of animals in population (flock)

# AGE GROUPS: max age = 5y, 1 week groups, = 261 (260.714)
Imm <- 1:17 # Imm (1-4m) = 1w-17w
You <- 18:26 # Young (5-6m) = 18w-26w
Juv <- 27:52 # Juvenile (6-12m) = 27w-52w
Sub <- 53:78 # Sub-adult (12-18m) = 53w-78w
Adu <- 79:261 # Adult (18m-5y) = 79w-261w
max_age <- 261

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
birth_r <- 0.5 # only animals >18M
age_p <- c(0,0,0.2,0.3,0.5) # proportion of population in each age group (initial)

# offtake
demos <- data.frame(age_cat = c("Imm","You","Juv","Sub","Adu"),
                    net_off_F = c(off_1,off_1,off_1,off_F,off_F),
                    net_off_M = c(off_1,off_1,off_1,off_M,off_M),
                    mort = c(mort_1,mort_1,mort_2,mort_2,mort_2),
                    ppr_mort = c(ppr_mort_1,ppr_mort_1,ppr_mort_2,ppr_mort_2,ppr_mort_2),
                    birth = c(0,0,0,0,birth_r),
                    age_p = age_p, # proportion of population in each age group
                    age_n = age_p*N_tot, # number of animals in age group
                    n_weeks = c(length(Imm), length(You), length(Juv), length(Sub), length(Adu)) # number of weeks (sub-compartments) in each age group
                    )
                
# dataframe of demogrpahic rates for each week-long age group in the population.
age_params <- data.frame(
  age_weeks = 1:max_age,
  age_cat = c(rep("Imm",length(Imm)),
              rep("You",length(You)),
              rep("Juv",length(Juv)),
              rep("Sub",length(Sub)),
              rep("Adu",length(Adu)))) %>%
  # fill in maternal immunity
  left_join(Imm_wane, c("age_weeks" = "weeks")) %>%
  mutate(immunity = ifelse(is.na(immunity) & age_cat=="Imm", 1, 
                           if_else(is.na(immunity),0, immunity))
         ) %>%
  left_join(demos, by = "age_cat") %>%
  # divide population in each age group into week-long sub-compartments , equally
  mutate(pop_init = age_n/n_weeks,
         mort = ifelse(age_weeks == max_age, 1, mort)) 


# initial age groups (all susceptible)
Im_init <- rep(0,max_age)
S_init <- age_params %>% pull(pop_init)
E_init <- rep(0,max_age)
I_init <- rep(0,max_age)
R_init <- rep(0,max_age)

#################################################################################
#################################################################################
## MODEL ##

TimeStop_dynamics <- 52 # 1 year, weekly timestep
TimeStop_transmission <- 24 # 1 day, hourly timestep

Im_mat <- as.data.frame(matrix(nrow = max_age, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = Im_init)

S_mat <- as.data.frame(matrix(nrow = max_age, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = S_init)

E_mat <- as.data.frame(matrix(nrow = max_age, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = E_init)

I_mat <- as.data.frame(matrix(nrow = max_age, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = I_init)

R_mat <- as.data.frame(matrix(nrow = max_age, ncol = TimeStop_dynamics)) %>%
  rename_all(~week_cols) %>%
  mutate("w1" = R_init)

#Demo rates as vectors
immunity <- age_params %>% pull(immunity)
net_off <- age_params %>% pull(net_off)
mort <- age_params %>% pull(mort)
ppr_mort <- age_params %>% pull(ppr_mort)
birth <- age_params %>% pull(birth)

# Demographics LOOP

for(w in 2:TimeStop_dynamics){
  
  # update week of simulation
  w_prev <- paste0("w",w-1)
  w_cur <- paste0("w",w)

  ## BIRTHS ## 
  
  # immune births
  Im_births <- sum(birth* Imm_b* R_mat[,w_prev])
  # susceptible births
  S_births <- sum(birth*(1-Imm_b)* R_mat[w_prev]) + sum(birth*S_mat[,w_prev])
  
  E_births <- 0; I_births <- 0; R_births <- 0 # set EIR births to 0
  
  ## DEMOGRAPHICS ##
  
  # Born Immune Demographics
  Im_demos <- immunity*Im_mat[,w_prev]- #decline in mat immunity with age
    Im_mat[,w_prev]*(net_off+mort) # offtake and mortality
  
  Im_new <- c(Im_births,Im_demos) %>%
    slice(1:max_age) # add births and remove last age group
    
  # Susceptible Demographics:

  S_demos <- 	S_mat[,w_prev]
    (1-immunity)*Im_mat[,w_prev]- # kids/lambs which have lost immunity
    S_mat[,w_prev]*(net_off+mort) # offtake and mortality
  
  S_new <- c(S_births,S_demos) %>%
    slice(1:max_age) # add births and remove last age group
  
  # Exposed Demographics:
  
  E_demos <- E_mat[,w_prev]-
    E_mat[,w_prev]*(net_off+mort) # offtake and mortality
  
  E_new <- c(E_births,E_demos) %>%
    slice(1:max_age) # add births and remove last age group
  
  # Infectious Demographics:
  
  I_demos <- I_mat[,w_prev]-
    I_mat[,w_prev]*(net_off+mort) # offtake and mortality (**ppr_mortality in disease loop?)
  
  I_new <- c(I_births,I_demos) %>%
    slice(1:max_age) # add births and remove last age group
  
  
  # Recovered Demographics:
  
  R_demos <- R_mat[,w_prev]-
    R_mat[,w_prev]*(net_off+mort) # offtake and mortality (**ppr_mortality in disease loop?)
  
  R_new <- c(R_births,R_demos) %>%
    slice(1:max_age) # add births and remove last age group
  
}




