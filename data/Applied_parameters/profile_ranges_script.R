# Identify any behavioural parameter sets within flock profiles ranges:

getwd()

# Load profiles parameter ranges and orginal parameter ranges
profile_ranges <- read.csv("data/Applied_parameters/profile_ranges.csv")
GSA_ranges <- read.csv("data/GSA_parameters/GSA_var_input.csv")

# select minimums & maximums from profiles
profile_mins <- profile_ranges %>%
  select(ends_with(".min")) 

profile_maxs <- profile_ranges %>%
  select(ends_with(".max"))

# profile names
profiles <- profile_ranges %>%
select(ends_with("min"), ends_with("max")) %>%
  colnames() %>%
  gsub(".max","",.) %>%
  gsub(".min","",.) %>% 
  unique()

# store T or F depending on whether profile parameters are within original parameter range
tracker <- profile_ranges

profile_mins>=GSA_ranges$min.1 
profile_maxs<=GSA_ranges$max.1


tracker.min <- apply(profile_mins, 2, function(x)
  x <- ifelse(x>=GSA_ranges$min.1, 1, 0)
)


tracker.max <- apply(profile_maxs, 2, function(x)
  x <- ifelse(x<=GSA_ranges$max.1, 1, 0)
)


tracker <- cbind(tracker.min, tracker.max)










################
# setup
################
library(tidyverse)
library(sensobol)
library(epiR)


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


## SETUP ##

# Analysis on only behavioural parameter sets

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


######################################################
######################################################


colnames(sobol_par_subset)











