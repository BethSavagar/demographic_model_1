## Flock Dynamics Model Code v1.1 (19/01/23)

# Setup 

library(tidyverse)

# define parameters (probably a separate script in due course)

beta <- 1 # transmission rate, S-E (per hour)
sigma <- 1 # rate of becoming infectious , E-I (per hour)
gamma <- 1 # rate of becoming recovery, I-R (per hour)

birth <- 1 # birth rate (per week)
N_tot <- 1700 # number of animals in population

# AGE GROUPS: max age = 5y, 1 week groups, = 261 (260.714)
Imm <- 1:17 # Imm (1-4m) = 1w-17w
You <- 18:26 # Young (5-6m) = 18w-26w
Juv <- 27:52 # Juvenile (6-12m) = 27w-52w
Sub <- 53:78 # Sub-adult (12-18m) = 53w-78w
Adu <- 79:261 # Adult (18m-5y) = 79w-261w


# demographic parameters: 

# waning of maternal immunity for first 4months (17 wk) (see below)
Imm_wane <- data.frame(weeks = c(1,5,9,13),
                       immunity = c(0.92,0.91,0.38,0.15))
off_1 <- 0 # NET offtake rate <12M (per week)
off_2 <- 0 # NET offtake rate >12M (per week)
mort_1 <- 0 # natural mortality rate <6M (per week)
mort_2 <- 1 # natural mortality rate >6M  (per week)
ppr_mort_1 <- 0 # natural mortality rate <6M (per week)
ppr_mort_2 <- 1 # natural mortality rate >6M  (per week)
birth <- 1 # only animals >18M
age_p <- c(0,0,0.2,0.3,0.5)

# offtake
demos <- data.frame(age_cat = c("Imm","You","Juv","Sub","Adu"),
                    net_off = c(off_1,off_1,off_1,off_2,off_2),
                    mort = c(mort_1,mort_1,mort_2,mort_2,mort_2),
                    ppr_mort = c(ppr_mort_1,ppr_mort_1,ppr_mort_2,ppr_mort_2,ppr_mort_2),
                    birth = c(0,0,0,0,birth),
                    age_p = age_p, # proportion of population in each age group
                    age_n = age_p*N_tot, # number of animals in age group
                    n_weeks = c(length(Imm), length(You), length(Juv), length(Sub), length(Adu)) # number of weeks (sub-compartments) in each age group
                    )
                

age_params <- data.frame(
  age_weeks = 1:max(Adu),
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
  left_join(demos) %>%
  mutate(pop_init = age_n/n_weeks)


# initial age groups (all susceptible)



#################################################################################
#################################################################################
## MODEL ##

TimeStop_dynamics <- 52 # 1 year, weekly timestep
TimeStop_transmission <- 24 # 1 day, hourly timestep

# Demographics LOOP

for(w in 1:TimeStop_dynamics){
  
  # immune offspring, immune compartment
  
  Imm_1 <- b*imm*sum(R_adult_f)
  
  
  
}




