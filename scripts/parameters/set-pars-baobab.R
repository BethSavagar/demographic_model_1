# set pars baobab - test 

# Flock Structure #

# Flock Size:
N_tot <- 100 # number of animals in population (flock or village)?

# Age Groups: 
# max age = 7y (F), 5y (M)
max_age_F <- 9*52 # 5y*52w (52 as approximation for 1y)
max_age_M <- 5*52 # 3y*52w
wk2mnth <- 4.345 # no. of weeks per month
# boundaries for each age category:
age_cuts_mnth <- c(4,6,12,18) #  0-4m and 4-6m to account for maternal immunity (-4m) & lamb/kid demographics (-6m)
age_cuts_wk <- round(age_cuts_mnth*wk2mnth) # age-group boundaries in wks
Kid <- 1:age_cuts_wk[1] # Immune Offspring (1-4m) = 1w-17w 
You <- (age_cuts_wk[1]+1):age_cuts_wk[2] # Young (5-6m) = 18w-26w  
Juv <- (age_cuts_wk[2]+1):age_cuts_wk[3] # Juvenile (6-12m) = 27w-52w
Sub <- (age_cuts_wk[3]+1):age_cuts_wk[4] # Sub-adult (12-18m) = 53w-78w
Adu_F <- (age_cuts_wk[4]+1):max_age_F # Adult F (18m-5y) = 79w-261w
Adu_M <- (age_cuts_wk[4]+1):max_age_M # Adult M (18m-3y) = 79w-261w


# Maternal Immunity #

# waning of maternal immunity for first 4months (17 wk)
# data from Bodjo et al. (following Hammami, El Arbi)
imm_decay_corrected <- read_csv("data/imm_decay_bodjo_v2.csv") # "scripts/demographic-data/mat-imm-decay.R" for workings
Imm_b <- imm_decay_corrected %>% 
  filter(wk ==0) %>% 
  pull(imm_corrected) # Imm_b = # proportion of young born to immune mothers that gain maternal antibodies


# Demographic Rates ##

source("~/OneDrive - Royal Veterinary College/PPR Collaborations/Data Bank/Baobab_data (Apr2021)/look-at-data.R")

median_rate

off_1 <- 0 # NET offtake rate <12M (per week) - NO trade of animals <12m

int <- median_rate %>% filter(parameter=="hint") %>% pull(rate_wk)
off_1 <- median_rate %>% filter(parameter=="hoff") %>% pull(rate_wk)
off_F <- int-off # NET offtake rate FEMALE >12M (per week) 
off_M <- int-off# NET offtake rate MALE >12M (per week)
mort_1 <- median_rate %>% filter(parameter=="hdea") %>% pull(rate_wk) # natural mortality rate <6M (per week)
mort_2 <- median_rate %>% filter(parameter=="hdea") %>% pull(rate_wk) # natural mortality rate >6M  (per week)
# see https://math.stackexchange.com/questions/1122085/how-to-calculate-a-monthly-mortality-rate for calc of weekly rate from yearly mortality of 0.3, 0.1 respectively.
mort_end <- 1 # natural mortality rate for final age group (per week)
birth_r <- median_rate %>% filter(parameter=="reprod") %>% pull(rate_wk) # only animals >18M
age_p_f <- c(0.025,0.025,0.1,0.15,0.45) # proportion of population in each age group (initial)
age_p_m <- c(0.025,0.025,0.1,0.05,0.05)

# PPR mortality in transmission script?
ppr_mort_1 <- 0 # ppr mortality rate <6M (per week)
ppr_mort_2 <- 0 # ppr mortality rate >6M  (per week)


# Dataframe of demographic parameters for age-sex group
demos <- data.frame(age_cat = c("Kid","You","Juv","Sub","Adu"), # age group
                    net_off_F = c(off_1,off_1,off_1,off_F,off_F), # female offtake
                    net_off_M = c(off_1,off_1,off_1,off_M,off_M), # male offtake
                    mort_F = c(mort_1,mort_1,mort_2,mort_2,mort_2),
                    mort_M = c(mort_1,mort_1,mort_2,mort_2,mort_2), # natural mortality
                    ppr_mort = c(ppr_mort_1,ppr_mort_1,ppr_mort_2,ppr_mort_2,ppr_mort_2), # ppr_mortality
                    birth = c(0,0,0,0,birth_r), # birth rate (only Adu_F)
                    age_p_F = age_p_f, # proportion of population in each age group F
                    age_p_M = age_p_m, # proportion of population in each age group M
                    n_weeks_F = c(length(Kid), length(You), length(Juv), length(Sub), length(Adu_F)), # number of weeks (sub-compartments) in each age group
                    n_weeks_M = c(length(Kid), length(You), length(Juv), length(Sub), length(Adu_M)) # number of weeks (sub-compartments) in each age group
)

## CREATE DEMO DATAFRAMES ##

# Dataframe of demographic rates for each week-long age group in the population.
# rows = weeklong age group, cols = demographic rates 
demographic_pars <- data.frame(
  age_weeks = 1:max_age_F, # nrow = age in weeks
  age_cat = c(rep("Kid",length(Kid)),
              rep("You",length(You)),
              rep("Juv",length(Juv)),
              rep("Sub",length(Sub)),
              rep("Adu",length(Adu_F)))) %>%
  # fill in maternal immunity
  left_join(imm_decay_corrected %>% select(wk, "imm" = imm_corrected), c("age_weeks" = "wk")) %>%
  
  mutate(imm = ifelse(is.na(imm),0,imm)) %>%
  # Join demographics (Female)
  left_join(demos, by = "age_cat") %>%
  # divide initial population between age groups according to proportions in age_p_F
  mutate(pop_init_F = (age_p_F*N_tot)/n_weeks_F,
         mort_F = ifelse(age_weeks == max_age_F, 1, mort_F)) %>%
  mutate(age_p_M = ifelse(age_weeks>max_age_M,0,age_p_M),
         pop_init_M = (age_p_M*N_tot)/n_weeks_M,
         mort_M = ifelse(age_weeks >= max_age_M, 1, mort_M)) %>%
  # select relevant variables   
  select(age_weeks,
         age_cat,
         imm,
         birth,
         mort_F,
         mort_M,
         ppr_mort,
         net_off_F,
         net_off_M,
         pop_init_F,
         pop_init_M
  )

# Delete interim parameters to clean up environment?


## INITIAL POPULATION STATES ##

pR <- 0.5 # proportion initially immune (due to prior infection)
# define SEIR vectors for male and female age groups
fIm_init <- rep(0,max_age_F); mIm_init <- rep(0,max_age_F)
fS_init <- demographic_pars %>% pull(pop_init_F) *(1-pR) # all S except already recovered
mS_init <- demographic_pars %>% pull(pop_init_M) *(1-pR) # all S except already recovered
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