library(here)
library(ggplot2)
library(reshape2)
library(dplyr)

here()

#Imagine population with 3 female age classes and 2 male age classes
#create survival and fecundity vectors for each season (example with 2 seasons, hot and cold)

#fecundity in form: F1, F2, F3, M1, M2
fecundity_HOT <- c(0, 0.75, 1.1, 0, 0)
fecundity_COLD <- c(0, 0.6, 0.9, 0, 0) #simulate reduced fecundity during cold season

#survival (greater during cold than hot season?)
survival_HOT <- c(0.75, 0.8, 0, 0.75, 0)
survival_COLD <- c(0.8, 0.95, 0, 0.8, 0)

#number of age groups
age_groups <- c('F1', 'F2', 'F3', 'M1', "M2")
#fecundity:
#nb fecundity this should be proportional to the number of offspring in each sex class (e.g. 50% female, 50% male)
p_female <- 0.5

#number of female age classes:
female_ages <- 3
