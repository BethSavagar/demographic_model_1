### RSA ANALYSIS 3 - 25/08/2023 ###

################
# setup
################
library(tidyverse)


################
# Load Datasets
################

# population dynamics: 
GSAoutput <- read.csv("output/GSA_output/GSA_output_sobol_2023-08-30.csv")
# parameter sets:
sobol_input <- read.csv("output/GSA_output/GSA_pars-set_sobol_2023-08-30.csv")
# behavioural parameters with +/- 15% growth saved as GSA_var_input-PRCC in data folder for GSA analysis with PRCC

# take a look at the data: 
str(GSAoutput) # output contains population and immunity dyanmics and age-sex structure from sobol sampling of refined parameter ranges. 
str(sobol_input) # contains parameter sets for each output
summary(sobol_input)


################
# Clean Data 
################

# add set id to output df:
GSAoutput <- GSAoutput %>%
  mutate(GSAset = 1:nrow(GSAoutput))

# tidy parameters dataframe: 
sobol_input <- sobol_input %>%
  select(NET_offtake_m, NET_offtake_m2, NET_offtake_f, everything()) %>%
  rename(off_mY = NET_offtake_m2,
         off_mA = NET_offtake_m,
         off_f = NET_offtake_f,
         mort_Y = mortality_y,
         mort_A = mortality_a,
         max_yrs_F = adu_f_max_yrs,
         max_yrs_M = adu_m_max_yrs,
         min_off = min_age_offtake,
         min_repro = min_age_repro
  ) %>%
  mutate(GSAset = 1:nrow(sobol_input))

## If needed creation of parameter dataframe with outputs of interest:
# 
# GSA_all <- sobol_input %>%
#   left_join(GSAoutput %>% select(c("GSAset","pop_growth","tenyr_growth","imm_6m","imm_12m","imm70_w")), by = c("GSAset")) %>%
#   select(-GSAset)


#########################################################
# Analysis 1 - Identify behavioral parameter GSAsets
#########################################################

# Define age-sex structure conditions: 

## Min and max proportions for each sex-age group (see age-sex SS)

pfKid.min <- 0.05; pfKid.max <- 0.19
pfSub.min <- 0.06; pfSub.max <- 0.19
pfAdu.min <- 0.21; pfAdu.max <- 0.62

pmKid.min <- 0.05; pmKid.max <- 0.16
pmSub.min <- 0.04; pmSub.max <- 0.15
pmAdu.min <- 0.01; pmAdu.max <- 0.15

####################
## GSA ANALYSIS 1 ##
####################

# Sobol sensitivity analysis on complete data.frame

# Join parameters dataframe with outputs of interest:
# (i) overall population growth
# (ii) tenyr population growth
# (iii) immunity at 6m
# (iv) immunity at 12m
# (v) immunity < 70%


# params for sobol matrices
N <- 1e4 # scaling factor, set to at least 1000
params <- colnames(sobol_input %>% select(-GSAset))
order <- "first"

# (i) overall population growth
ind_popgrowth <- sobol_indices(Y = GSAoutput$pop_growth, N = N, params = params) # compute indices
plot(ind_popgrowth) # first and total order indices

# (ii) tenyr population growth
ind_tenyr <- sobol_indices(Y = GSAoutput$tenyr_growth, N = N, params = params) # compute indices
plot(ind_tenyr)

# (iii) immunity at 6m
ind_imm6m <- sobol_indices(Y = GSAoutput$imm_6m, N = N, params = params) # compute indices
plot(ind_imm6m)

# (iv) immunity at 12m
ind_imm12m <- sobol_indices(Y = GSAoutput$imm_12m, N = N, params = params) # compute indices
plot(ind_imm12m)

# (v) immunity < 70%
ind_imm70 <- sobol_indices(Y = GSAoutput$imm70_w, N = N, params = params) # compute indices
plot(ind_imm70)

# Additional Plotting Options:
plot_multiscatter(data = sobol_input %>% select(-GSAset), N = N, Y = GSAoutput$pop_growth, params = params)
plot_scatter(data = sobol_input %>% select(-GSAset), Y = GSAoutput$pop_growth, N = N, params = params)
plot_uncertainty(Y = GSAoutput$pop_growth, N = N)


# PRCC on complete data frame
library(epiR)

# Total pop growth
dat.1 <- sobol_input %>% select(-GSAset) %>% cbind(GSAoutput %>% select(pop_growth))
prcc_popgrowth <- epi.prcc(dat.1, sided.test = 2, conf.level = 0.95)

ggplot(prcc_popgrowth, aes(x=var, y=est))+
  geom_col()+
  labs(x="par", y="prcc")+
  theme_bw()

# Immunity at 6m
dat.2 <- sobol_input %>% select(-GSAset) %>% cbind(GSAoutput %>% select(imm_6m))
prcc_imm6m <- epi.prcc(dat.2, sided.test = 2, conf.level = 0.95)

ggplot(prcc_imm6m, aes(x=var, y=est))+
  geom_col()+
  labs(x="par", y="prcc")+
  theme_bw()

# Immunity below 70%
dat.3 <- sobol_input %>% select(-GSAset) %>% cbind(GSAoutput %>% select(imm70_w))
prcc_imm70 <- epi.prcc(dat.3, sided.test = 2, conf.level = 0.95)

ggplot(prcc_imm70, aes(x=var, y=est))+
  geom_col()+
  labs(x="par", y="prcc")+
  theme_bw()

# Bar plot comparing prcc for each output
prcc.all <- rbind(prcc_popgrowth %>% select(var,est) %>% mutate(output = "popgrowth"),
                 prcc_imm6m %>% select(var,est) %>% mutate(output = "imm6m"),
                 prcc_imm70 %>% select(var,est) %>% mutate(output = "imm70")    )

ggplot(prcc.all, aes(x=var, y=est, group = output, fill = output))+
  geom_col(position = "dodge")+
  labs(x="par", y="prcc")+
  theme_bw()

####################
## GSA ANALYSIS 2 ##
####################

# Analysis on 

GSAoutput_ext <- GSAoutput %>%
  mutate(tenyr_growth = replace_na(tenyr_growth, 0),
         # add variables to identify parameter sets with growth between 5% and 15%
         tenyr_15 = ifelse(tenyr_growth>=0.85 & tenyr_growth <=1.15, 1, 0),
         tenyr_05 = ifelse(tenyr_growth>=0.95 & tenyr_growth <=1.05, 1, 0)) %>%
  
  # add variables to identify parameter sets with growth between 5% and 15% AND age-sex conditions
  mutate(
    tenyr_15age = ifelse(
      tenyr_15 == 1 &
      pfKid >= pfKid.min & pfKid <= pfKid.max &
      pfSub >= pfSub.min & pfSub <= pfSub.max &
      pfAdu >= pfAdu.min & pfAdu <= pfAdu.max &
      
      pmKid >= pmKid.min & pmKid <= pmKid.max &
      pmSub >= pmSub.min & pmSub <= pmSub.max &
      pmAdu >= pmAdu.min & pmAdu <= pmAdu.max,1,0),
    
    tenyr_05age = ifelse(
      tenyr_05 == 1 &
        pfKid >= pfKid.min & pfKid <= pfKid.max &
        pfSub >= pfSub.min & pfSub <= pfSub.max &
        pfAdu >= pfAdu.min & pfAdu <= pfAdu.max &
        
        pmKid >= pmKid.min & pmKid <= pmKid.max &
        pmSub >= pmSub.min & pmSub <= pmSub.max &
        pmAdu >= pmAdu.min & pmAdu <= pmAdu.max,1,0)
  )

# How many parameter sets are behavioural for growth of 5%, 15% and growth of 5%-15% with age-sex structure.

GSAoutput_ext %>% group_by(tenyr_15, tenyr_05) %>% count()
GSAoutput_ext %>% group_by(tenyr_15age) %>% count()
GSAoutput_ext %>% group_by(tenyr_05age) %>% count()

## ID which parameter sets are within 15% growth and age-sex conditions

sobol_par_subset <- sobol_input %>% left_join(GSAoutput_ext %>% select(c(pop_growth,tenyr_growth,imm_6m,imm_12m,imm70_w,tenyr_15age,GSAset)), by = c("GSAset")) %>% filter(tenyr_15age==1) 


