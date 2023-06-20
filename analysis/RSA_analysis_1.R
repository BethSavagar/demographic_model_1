# RSA analysis 
# Running RSA_test.Rmd on 01-06-2023 with 10'000 lhs parameter sets and 25 year simulations
# 1) Analysis of population growth for:
##  a) total time
# 2) Sex-age structure on population.
# 3) Population dynamics
library(tidyverse)

# LOAD DATASET
RSAoutput <- read.csv("output/RSA_output/RSAoutput_20220510.csv")
var_input_set <- ###
### Analysis 1 - Population Growth Condition ###

# I: Overall population growth
# - how many populations are maintained (growth or decay) with final population >0 at the end of the simulation?
validRSA <-  RSAoutput %>%
  filter(pop_growth != 0)
# plot the population size, and population growth for valid simulations.
ggplot(validRSA, aes(x=set, y=sum_pop))+geom_point()+coord_cartesian(y=c(0,250))
ggplot(validRSA, aes(x=set, y=pop_growth))+geom_point()+coord_cartesian(y=c(0,2.5))

# - subset populations are stable i.e. with overall growth of +/- 5%
validRSA2 <- validRSA %>%
  filter(pop_growth>=0.95, pop_growth <=1.05)

# Plot the parameter sets corresponding to i. population maintenance, ii. population stability: 

# population maintenance parameters plot
valid_pars <- RSAoutput %>%
  filter(pop_growth != 0) %>%
  pull(set)
valid_pars_df <- var_input_set[valid_pars,]
pairs(valid_pars_df)

# population stability parameters plot
valid_pars2 <- validRSA2 %>%
  pull(set)
valid_pars_df2 <-  var_input_set[valid_pars2,]
pairs(valid_pars_df2)

# II: Sex-Age summary:
# - Of those parameter sets which have stable population growth, do any maintain the required population structure?

# Plot the sex-age structure of stable populations
validpops <- validRSA2 %>% 
  select(starts_with("pf")|starts_with("pm")|starts_with(("pF"))) %>%
  gather(key=stat, value=prop) %>%
  mutate(sex = ifelse(stat %in% c("pmKid","pmSub","pmAdu"), "M", "F"))

ggplot(validpops, aes(x=stat, y=prop, col = sex))+geom_boxplot()


# III: Population Dynamics:

source("scripts/RSA/validpop_dynamics.R")
ggplot(validpop_long, aes(x=w, y=pop, col = as.character(set)))+geom_line()


###########################
## ADDITIONAL CONDITIONS ##

# ## Min and max proportions for each sex-age group
# pF.min <- 0.6; pF.max <- 0.8
# 
# pfKid.min <- 0; pfKid.max <- 1
# pfSub.min <- 0; pfSub.max <- 1
# pfAdu.min <- 0; pfAdu.max <- 1
# 
# pmKid.min <- 0; pmKid.max <- 1
# pmSub.min <- 0; pmSub.max <- 1
# pmAdu.min <- 0; pmAdu.max <- 1

## Min and max proportions for each sex-age group
pF.min <- 0.5; pF.max <- 0.8

pfKid.min <- 0.08; pfKid.max <- 0.18
pfSub.min <- 0.11; pfSub.max <- 0.15
pfAdu.min <- 0.24; pfAdu.max <- 0.42

pmKid.min <- 0.08; pmKid.max <- 0.16
pmSub.min <- 0.09; pmSub.max <- 0.14
pmAdu.min <- 0.08; pmAdu.max <- 0.15

validRSA3 <- validRSA2 %>%
  filter(pop_growth>=0.95, pop_growth <=1.05) %>%
  filter(pfKid >= pfKid.min, pfKid <= pfKid.max, 
         pfSub >= pfSub.min, pfSub <= pfSub.max,
         pfAdu >= pfAdu.min, pfAdu <= pfAdu.max,
         
         pmKid >= pmKid.min, pmKid <= pmKid.max,
         pmSub >= pmSub.min, pmSub <= pmSub.max,
         pmAdu >= pmAdu.min, pmAdu <= pmAdu.max)

valid_pars3 <- validRSA3 %>%
  pull(set)

valid_pars_df3 <-  var_input_set[valid_pars3,]

pairs(valid_pars_df3)



