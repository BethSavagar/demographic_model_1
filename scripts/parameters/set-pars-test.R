## Default Parameters Script ##
# editing on model_clean: read in data from csvs
# Parameter set selected in run-model.R script
# think about reading in parameter data from .csv files - how would this work (01/03/23)
# see .csv files for parameters

# ---------------------------
## DATASETS ##

test_age_data_full <- read_csv("data/test_data/test-age-parameters.csv")
test_data_full <- read_csv("data/test_data/test-parameters.csv", col_names=T)

if(!exists("dataset1")){
  dataset1 <- "test_1" # select dataset for test data
  message(paste("Using default demographics dataset -", dataset1, "- for simulation"))
}
if(!exists("dataset2")){
  dataset2 <- "test_1" # select dataset for test data
  message(paste("Using default age dataset -", dataset2, "- for simulation"))
}

test_data <- test_data_full %>%
  select(parameter, "value" = `dataset1`)

test_age_data <- test_age_data_full %>%
  select(parameter, "value" = `dataset2`)


# ---------------------------
## DEMOGRAPHIC PARAMETERS ##

## Flock Size ##
N_tot <- test_data %>% filter(parameter=="N_tot") %>% pull(value) # number of animals in population (flock or village)?

## Flock Structure - proportions ##

kid_f_prop <- test_age_data %>% filter(parameter=="kid_f_prop") %>% pull(value) 
you_f_prop <- test_age_data %>% filter(parameter=="you_f_prop") %>% pull(value) 
juv_f_prop <- test_age_data %>% filter(parameter=="juv_f_prop") %>% pull(value) 
sub_f_prop <- test_age_data %>% filter(parameter=="sub_f_prop") %>% pull(value) 
adu_f_prop <- test_age_data %>% filter(parameter=="adu_f_prop") %>% pull(value) 

kid_m_prop <- test_age_data %>% filter(parameter=="kid_m_prop") %>% pull(value) 
you_m_prop <- test_age_data %>% filter(parameter=="you_m_prop") %>% pull(value) 
juv_m_prop <- test_age_data %>% filter(parameter=="juv_m_prop") %>% pull(value) 
sub_m_prop <- test_age_data %>% filter(parameter=="sub_m_prop") %>% pull(value) 
adu_m_prop <- test_age_data %>% filter(parameter=="adu_m_prop") %>% pull(value) 

age_p_f <- c(kid_f_prop,you_f_prop,juv_f_prop,sub_f_prop,adu_f_prop) # proportion of population in each age group (initial)
age_p_m <- c(kid_m_prop,you_m_prop,juv_m_prop,sub_m_prop,adu_m_prop)

# proportion immune:
pR <- test_age_data %>% filter(parameter=="pR") %>% pull(value) 

## Flock Structure - age groups ##

kid_max <- test_age_data %>% filter(parameter=="kid_max") %>% pull(value) 
you_max <- test_age_data %>% filter(parameter=="you_max") %>% pull(value) 
juv_max <- test_age_data %>% filter(parameter=="juv_max") %>% pull(value) 
sub_max <- test_age_data %>% filter(parameter=="sub_max") %>% pull(value) 
max_age_F <- test_age_data %>% filter(parameter=="adu_f_max_yrs") %>% pull(value)*52 
max_age_M <- test_age_data %>% filter(parameter=="adu_m_max_yrs") %>% pull(value)*52 

# boundaries for each age category:
wk2mnth <- 4.345 # weeks per month
age_cuts_mnth <- c(kid_max,you_max,juv_max,sub_max) #  0-4m and 4-6m to account for maternal immunity (-4m) & lamb/kid demographics (-6m)

age_cuts_wk <- round(age_cuts_mnth*wk2mnth) # age-group boundaries in wks
Kid <- 1:age_cuts_wk[1] # Immune Offspring (1-4m) = 1w-17w 
You <- (age_cuts_wk[1]+1):age_cuts_wk[2] # Young (5-6m) = 18w-26w  
Juv <- (age_cuts_wk[2]+1):age_cuts_wk[3] # Juvenile (6-12m) = 27w-52w
Sub <- (age_cuts_wk[3]+1):age_cuts_wk[4] # Sub-adult (12-18m) = 53w-78w
Adu_F <- (age_cuts_wk[4]+1):max_age_F # Adult F (18m-5y) = 79w-261w
Adu_M <- (age_cuts_wk[4]+1):max_age_M # Adult M (18m-3y) = 79w-261w


## Maternal Immunity ##

# - Bodjo et al (following ElArbi)
# - waning of maternal immunity for first 4months (17 wk)
imm_decay_corrected <- read_csv("data/imm_decay_bodjo_v2.csv") # "scripts/demographic-data/mat-imm-decay.R" for workings
Imm_b <- imm_decay_corrected %>% 
  filter(wk ==0) %>% 
  pull(imm_corrected) # Imm_b = # proportion of young born to immune mothers that gain maternal antibodies

## Dynamics ##

off_1 <- test_data %>% filter(parameter=="NET_offtake_y") %>% pull(value) # NET offtake rate <12M (per week) - NO trade of animals <12m
off_F <- test_data %>% filter(parameter=="NET_offtake_f") %>% pull(value) # NET offtake rate FEMALE >12M (per week) 
off_M <- test_data %>% filter(parameter=="NET_offtake_m") %>% pull(value) # NET offtake rate MALE >12M (per week)
mort_1 <- test_data %>% filter(parameter=="mortality_y") %>% pull(value) # natural mortality rate <6M (per week)
mort_2 <- test_data %>% filter(parameter=="mortality_a") %>% pull(value) # natural mortality rate >6M  (per week)
mort_end <- test_data %>% filter(parameter=="mortality_end") %>% pull(value) # natural mortality rate for final age group (per week)
birth_r <- test_data %>% filter(parameter=="birth_rate") %>% pull(value) # only animals >18M

# PPR mortality in transmission script?
ppr_mort_1 <- test_data %>% filter(parameter=="ppr_mortality_y") %>% pull(value) # ppr mortality rate <6M (per week)
ppr_mort_2 <- test_data %>% filter(parameter=="ppr_mortality_a") %>% pull(value) # ppr mortality rate >6M  (per week)


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

# pR = proportion initially immune (due to prior infection)
# define SEIR vectors for male and female age groups
fIm_init <- rep(0,max_age_F); mIm_init <- rep(0,max_age_F)
fS_init <- demographic_pars %>% pull(pop_init_F) *(1-pR) # all S except already recovered
mS_init <- demographic_pars %>% pull(pop_init_M) *(1-pR) # all S except already recovered
fE_init <- rep(0,max_age_F); mE_init <- rep(0,max_age_F)
fI_init <- rep(0,max_age_F); mI_init <- rep(0,max_age_F)
fR_init <- rep(0,max_age_F); mR_init <- rep(0,max_age_F)

if(dataset2=="test_1"){
  fS_init <- rep(0,max_age_F)
  fS_init[1] <- 0.5*N_tot # all S1
  mS_init <- rep(0,max_age_F)
  mS_init[1] <- 0.5*N_tot # all S1
}

if(dataset2=="test_birth"){
  fS_init <- rep(0,max_age_F)
  fS_init[min(Adu_F)] <- 0.5*N_tot # all S1
  mS_init <- rep(0,max_age_F)
  mS_init[min(Adu_M)] <- 0.5*N_tot # all S1
}

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



# ---------------------------------
## CREATE DEMOGRAPHIC DATAFRAMES ##
# 
# # Dataframe of demographic parameters for age-sex group
# demos <- data.frame(age_cat = c("Kid","You","Juv","Sub","Adu"), # age group
#                     net_off_F = c(off_1,off_1,off_1,off_F,off_F), # female offtake
#                     net_off_M = c(off_1,off_1,off_1,off_M,off_M), # male offtake
#                     mort_F = c(mort_1,mort_1,mort_2,mort_2,mort_2),
#                     mort_M = c(mort_1,mort_1,mort_2,mort_2,mort_2), # natural mortality
#                     ppr_mort = c(ppr_mort_1,ppr_mort_1,ppr_mort_2,ppr_mort_2,ppr_mort_2), # ppr_mortality
#                     birth = c(0,0,0,0,birth_r), # birth rate (only Adu_F)
#                     age_p_F = age_p_f, # proportion of population in each age group F
#                     age_p_M = age_p_m, # proportion of population in each age group M
#                     n_weeks_F = c(length(Kid), length(You), length(Juv), length(Sub), length(Adu_F)), # number of weeks (sub-compartments) in each age group
#                     n_weeks_M = c(length(Kid), length(You), length(Juv), length(Sub), length(Adu_M)) # number of weeks (sub-compartments) in each age group
# )
# 
# ## Demographics by weekly age-category ##
# 
# # Dataframe of demographic rates for each week-long age group in the population.
# # rows = weeklong age group, cols = demographic rates 
# demographic_pars <- data.frame(
#   age_weeks = 1:max_age_F, # nrow = age in weeks
#   age_cat = c(rep("Kid",length(Kid)),
#               rep("You",length(You)),
#               rep("Juv",length(Juv)),
#               rep("Sub",length(Sub)),
#               rep("Adu",length(Adu_F)))) %>%
#   # fill in maternal immunity
#   left_join(imm_decay_corrected %>% select(wk, "imm" = imm_corrected), c("age_weeks" = "wk")) %>%
#   
#   mutate(imm = ifelse(is.na(imm),0,imm)) %>%
#   # Join demographics (Female)
#   left_join(demos, by = "age_cat") %>%
#   # divide initial population between age groups according to proportions in age_p_F
#   mutate(pop_init_F = (age_p_F*N_tot)/n_weeks_F,
#          mort_F = ifelse(age_weeks == max_age_F, 1, mort_F)) %>%
#   mutate(age_p_M = ifelse(age_weeks>max_age_M,0,age_p_M),
#          pop_init_M = (age_p_M*N_tot)/n_weeks_M,
#          mort_M = ifelse(age_weeks >= max_age_M, 1, mort_M)) %>%
#   # select relevant variables   
#   select(age_weeks,
#          age_cat,
#          imm,
#          birth,
#          mort_F,
#          mort_M,
#          ppr_mort,
#          net_off_F,
#          net_off_M,
#          pop_init_F,
#          pop_init_M
#          )
# 
# # ---------------------------------
# ## INITIAL POPULATION STATE ##
# 
# pR <- 0.5 # proportion initially immune (due to prior infection)
# # define SEIR vectors for male and female age groups
# fIm_init <- rep(0,max_age_F); mIm_init <- rep(0,max_age_F)
# fS_init <- demographic_pars %>% pull(pop_init_F) *(1-pR) # all S except already recovered
# mS_init <- demographic_pars %>% pull(pop_init_M) *(1-pR) # all S except already recovered
# fE_init <- rep(0,max_age_F); mE_init <- rep(0,max_age_F)
# fI_init <- rep(0,max_age_F); mI_init <- rep(0,max_age_F)
# fR_init <- rep(0,max_age_F); mR_init <- rep(0,max_age_F)
# 
# if(dataset=="test_1"){
#   fS_init <- rep(0,max_age_F)
#   fS_init[1] <- 0.5*N_tot # all S1 
#   mS_init <- rep(0,max_age_F)
#   mS_init[1] <- 0.5*N_tot # all S1 
# }
# 
# if(pR>0){
#   fR_init <- demographic_pars %>% pull(pop_init_F) *pR
#   mR_init <- demographic_pars %>% pull(pop_init_M) *pR
# }
# 
# 
# f_list <- list("fIm"=fIm_init,
#                "fS"=fS_init,
#                "fE"=fE_init,
#                "fI"=fI_init,
#                "fR"=fR_init)
# m_list <- list("mIm"=mIm_init,
#                "mS"=mS_init,
#                "mE"=mE_init,
#                "mI"=mI_init,
#                "mR"=mR_init)

