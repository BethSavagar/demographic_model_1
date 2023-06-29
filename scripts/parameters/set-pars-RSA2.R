## RSA parameters 
# review 26/04/23 - need to figure out parameterisation of age groups etc - make as simple as possible. 

# ---------------------------
## DATASETS ##

# fix_age_data_full <- read_csv("data/RSA_parameters/RSA_fix_input.csv",col_names=T)
# var_demo_data_full <- read_csv("data/RSA_parameters/RSA_var_input.csv", col_names=T)

if(!exists("dataset1")){
  dataset1 <- "sim.1" # select dataset for test data
  message(paste("Using default demographics dataset -", dataset1, "- for simulation"))
}
if(!exists("dataset2")){
  dataset2 <- "sim.1" # select dataset for test data
  message(paste("Using default age dataset -", dataset2, "- for simulation"))
}

var_demo_data <- var_demo_data_full %>% select(parameter, "value" = `dataset1`)

fix_age_data <- fix_age_data_full %>% select(parameter, "value" = `dataset2`)


# ---------------------------
## DEMOGRAPHIC PARAMETERS ##

## Flock Size ##
N_tot <- fix_age_data %>% filter(parameter=="N_tot") %>% pull(value) # number of animals in population (flock or village)?

## Flock Structure - proportions ##
# set proportion of male/female: kid (<6m), sub (<12m), adu (12m+)

kid_f_prop <- fix_age_data %>% filter(parameter=="kid_f_prop") %>% pull(value) 
sub_f_prop <- fix_age_data %>% filter(parameter=="sub_f_prop") %>% pull(value) 
adu_f_prop <- fix_age_data %>% filter(parameter=="adu_f_prop") %>% pull(value) 

kid_m_prop <- fix_age_data %>% filter(parameter=="kid_m_prop") %>% pull(value) 
sub_m_prop <- fix_age_data %>% filter(parameter=="sub_m_prop") %>% pull(value) 
adu_m_prop <- fix_age_data %>% filter(parameter=="adu_m_prop") %>% pull(value) 

age_p_f <- c("kid_f_p"=kid_f_prop,"sub_f_p"=sub_f_prop,"adu_f_p"=adu_f_prop) # sex-age group proportions (INITIAL)
age_p_m <- c("kid_m_p"=kid_m_prop,"sub_m_p"=sub_m_prop,"adu_m_p"=adu_m_prop)

# proportion immune initially:
pR <- fix_age_data %>% filter(parameter=="pR") %>% pull(value) 

## Flock Structure - age groups ##
wk2mnth <- 4.345 # weeks per month

kid_max <- fix_age_data %>% filter(parameter=="kid_max") %>% pull(value) 
kid_max_wks <- round(kid_max*wk2mnth)
sub_max <- fix_age_data %>% filter(parameter=="sub_max") %>% pull(value) 
sub_max_wks <- round(sub_max*wk2mnth)

if(SA == T){
  max_age_F <- var_input_set[i,"adu_f_max_yrs"]*52
  max_age_M <- var_input_set[i,"adu_m_max_yrs"]*52  
  
}

# 

Kid <- 1:kid_max_wks # Kid: 1-6m (1-26w)
Sub <- (kid_max_wks+1):sub_max_wks # Sub: 6-12m (26-52w)
Adu_F <- (sub_max_wks+1):max_age_F # Adult F: 12m+
Adu_M <- (sub_max_wks+1):max_age_M # Adult M: 12m+


## Maternal Immunity ##

# - Bodjo et al (following ElArbi)
# - waning of maternal immunity for first 4months (17 wk)
# imm_decay_corrected <- read_csv("data/imm_decay_bodjo_v2.csv") # "scripts/demographic-data/mat-imm-decay.R" for workings
Imm_b <- imm_decay_corrected %>% 
  filter(wk ==0) %>% 
  pull(imm_corrected) # Imm_b = # proportion of young born to immune mothers that gain maternal antibodies


############################
# other dataset... >> stremalining this code (27/04/23)
## Dynamics ##

min_age_offtake <- fix_age_data %>% filter(parameter=="min_age_offtake") %>% pull(value)
min_offtake_wks <- round(min_age_offtake*wk2mnth)
min_age_repro <- fix_age_data %>% filter(parameter=="min_age_repro") %>% pull(value)
min_repro_wks <- round(min_age_repro*wk2mnth)
# 
off_1 <- var_demo_data %>% filter(parameter=="NET_offtake_y") %>% pull(value) # NET offtake rate <12M (per week) - NO trade of animals <12m
off_F <- var_demo_data %>% filter(parameter=="NET_offtake_f") %>% pull(value) # NET offtake rate FEMALE >12M (per week)
off_M <- var_demo_data %>% filter(parameter=="NET_offtake_m") %>% pull(value) # NET offtake rate MALE >12M (per week)
mort_1 <- var_demo_data %>% filter(parameter=="mortality_y") %>% pull(value) # natural mortality rate <6M (per week)
mort_2 <- var_demo_data %>% filter(parameter=="mortality_a") %>% pull(value) # natural mortality rate >6M  (per week)
# mort_end <- var_demo_data %>% filter(parameter=="mortality_end") %>% pull(value) # natural mortality rate for final age group (per week)
birth_r <- var_demo_data %>% filter(parameter=="birth_rate") %>% pull(value) # only animals >18M
# 
# # PPR mortality in transmission script?
ppr_mort_1 <- var_demo_data %>% filter(parameter=="ppr_mortality_y") %>% pull(value) # ppr mortality rate <6M (per week)
ppr_mort_2 <- var_demo_data %>% filter(parameter=="ppr_mortality_a") %>% pull(value) # ppr mortality rate >6M  (per week)


##### RSA params
if(SA==T){
  off_F <- var_input_set[i,"NET_offtake_f"] 
  off_M <- var_input_set[i,"NET_offtake_m"] 
  mort_1 <- var_input_set[i,"mortality_y"]
  mort_2 <- var_input_set[i,"mortality_a"]
  # mort_end <- var_demo_data %>% filter(parameter=="mortality_end") %>% pull(value) # natural mortality rate for final age group (per week)
  birth_r <- var_input_set[i,"birth_rate"]
  
  min_age_offtake <- var_input_set[i,"min_age_offtake"]
  min_age_repro <- var_input_set[i,"min_age_repro"]
  
}




# convert dynamics to weekly rates:

off_1 <- 1-((1-off_1)^(1/52))
off_F <- 1-((1-off_F)^(1/52))
off_M <- 1-((1-off_M)^(1/52))
mort_1 <- 1-((1-mort_1)^(1/52))
mort_2 <- 1-((1-mort_2)^(1/52))
birth_r <- birth_r / 52
min_offtake_wks <- round(min_age_offtake*wk2mnth)
min_repro_wks <- round(min_age_repro*wk2mnth)

ppr_mort_1 <- 1-((1-ppr_mort_1)^(1/52))
ppr_mort_2 <- 1-((1-ppr_mort_2)^(1/52))

#####################################

demographic_pars <- data.frame(
  age_weeks = 1:max_age_F) %>% # nrow = age in weeks
  mutate(age_cat = ifelse(age_weeks <= kid_max_wks, "Kid",
                   ifelse(age_weeks <= sub_max_wks, "Sub", "Adu"))) %>%
  # fill in maternal immunity
  left_join(imm_decay_corrected %>% select(wk, "imm" = imm_corrected), c("age_weeks" = "wk")) %>%
  
  mutate(imm = ifelse(is.na(imm),0,imm)) %>%
  ## Join Demographics Data >>>
  mutate(net_off_F = ifelse(age_weeks<min_offtake_wks, off_1, off_F),
         net_off_M = ifelse(age_weeks<min_offtake_wks, off_1,off_M),
         # mort_F = ifelse(age_weeks<=kid_max, mort_1, mort_2),
         # mort_F = ifelse(age_weeks == max_age_F, 1, mort_F),
         mort_F = ifelse(age_weeks<=kid_max_wks, mort_1, 
                         ifelse(age_weeks == max_age_F, 1, mort_2)),
         mort_M = ifelse(age_weeks<=kid_max_wks, mort_1, 
                         ifelse(age_weeks == max_age_M, 1, mort_2)),
         ppr_mort = ifelse(age_weeks<=kid_max_wks, ppr_mort_1,ppr_mort_2), # ppr_mortality
         birth = ifelse(age_weeks<min_repro_wks,0,birth_r), # birth rate (only Adu_F|)
         
         age_p_F = ifelse(age_cat=="Kid", kid_f_prop,
                          ifelse(age_cat=="Sub", sub_f_prop, adu_f_prop)),
         age_p_M = ifelse(age_cat=="Kid", kid_m_prop,
                          ifelse(age_cat=="Sub", sub_m_prop, 
                                 ifelse(age_cat =="Adu" & age_weeks<=max_age_M, adu_m_prop,0))),
         n_weeks_F = ifelse(age_cat=="Kid", kid_max_wks,
                            ifelse(age_cat=="Sub", sub_max_wks - kid_max_wks, 
                                   ifelse(age_cat =="Adu" & age_weeks<=max_age_F, max_age_F - sub_max_wks,0))),
         n_weeks_M = ifelse(age_cat=="Kid", kid_max_wks,
                            ifelse(age_cat=="Sub", sub_max_wks - kid_max_wks, 
                                   ifelse(age_cat =="Adu" & age_weeks<=max_age_M, max_age_M - sub_max_wks,0)))
         ) %>%
  
  # divide initial population between age groups according to proportions in age_p_F
  mutate(pop_init_F = (age_p_F*N_tot)/n_weeks_F,
         pop_init_M = (age_p_M*N_tot)/n_weeks_M, 
         pop_init_M = ifelse(is.na(pop_init_M), 0, pop_init_M)) %>%
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


################################
## INITIAL POPULATION STATES ##
################################

# pR = proportion initially immune (due to prior infection)
# define SEIR vectors for male and female age groups
# add input param for pIm, pS, pE, pI, pR at t=0 - 27/04/23

fIm_init <- rep(0,max_age_F); mIm_init <- rep(0,max_age_F)
# fIm_init <- demographic_pars %>% pull(pop_init_F)*pIm
# mIm_init <- demographic_pars %>% pull(pop_init_M)*pIm
fS_init <- demographic_pars %>% pull(pop_init_F) *(1-pR) # all S except already recovered
mS_init <- demographic_pars %>% pull(pop_init_M) *(1-pR) # all S except already recovered
fE_init <- rep(0,max_age_F); mE_init <- rep(0,max_age_F)
# fE_init <- demographic_pars %>% pull(pop_init_F)*pE
# mE_init <- demographic_pars %>% pull(pop_init_M)*pE
fI_init <- rep(0,max_age_F); mI_init <- rep(0,max_age_F)
# fI_init <- demographic_pars %>% pull(pop_init_F)*pI 
# mI_init <- demographic_pars %>% pull(pop_init_M)*pI
fR_init <- demographic_pars %>% pull(pop_init_F)*pR
mR_init <- demographic_pars %>% pull(pop_init_M)*pR

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

