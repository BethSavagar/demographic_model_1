library(here)
library(ggplot2)
library(reshape2)
library(dplyr)

here() # use instead of setwd() so that you can use relative paths to access scripts

#Imagine population with 3 female age classes and 2 male age classes
#create survival and fecundity vectors for each season (example with 2 seasons, hot and cold)


########################################################################### 
################# CREATE PROJECTION MATRICES #########################

# Source the prjoection matrix function to generate seasonal projection matrices
# Use here() to access proj_mat function in scripts folder

source(here("scripts", "projection_matrix_function.R"))


#create variables required for projection matrix function

#number of age groups
age_groups <- c('F1', 'F2', 'F3', 'M1', "M2")

#fecundity in form: F1, F2, F3, M1, M2
fecundity_HOT <- c(0, 1.5, 2, 0, 0)
fecundity_COLD <- c(0, 1.3, 1.5, 0, 0) #simulate reduced fecundity during cold season

#survival (greater during cold than hot season?)
survival_HOT <- c(0.75, 0.9, 0, 0.85, 0)
survival_COLD <- c(0.8, 0.95, 0, 0.9, 0)

# NB - the survival and fecundity vectors could be stored in a df with each row corresponding to each season and accessed by indexing

#proportion female:
# this ensures that the offspring are assigned proportionally to male and female sex classes
p_female <- 0.5

#number of female age classes:
female_ages <- 3

#create projection matrices for each season
mat_HOT <- proj_matrix(age_groups, survival_HOT, fecundity_HOT, p_female, female_ages)
mat_COLD <- proj_matrix(age_groups, survival_COLD, fecundity_COLD, p_female, female_ages)

# Create a list of matrices of length 12, corresponding to 12 months of year
# Each item in the list should be the correct matrix according to the season e.g. items 1:6 are mat_HOT and 6-12 are mat_COLD

n_month = 12 #number of months simulated
hot_season = 6 #ie first 6 months are hot season
proj_Mat_list <- vector("list", length = n_month)
for(i in 1:n_month){
  if(i <= hot_season){
    proj_Mat_list[[i]] <- mat_HOT #this needs work...
  }else{
    proj_Mat_list[[i]] <- mat_COLD
  }
}


########################################################### 
############### SIMULATE POPULATION ###########################

#source population projection function:

source(here("scripts", "pop_projection_function.R"))

#set variavbles required for population projection function:
age_groups <- c('F1', 'F2', 'F3', 'M1', "M2")
pop_init <- c(50,40,60,20,40)#initial population vector
months <- 12 #number of simulations i.e. years/months
proj_Mat_list <- proj_Mat_list #list of matrices for 12 months of year

pop_projection <- population_projection(pop_init,n_sim = months,proj_Mat_list, age_groups)

#make useable by ggplot
pop_df <- as.data.frame(t(pop_projection))
colnames(pop_df) <- age_groups
#pop_projection matrix as dataframe suitable for ggplot (transposed)

#use dplyr to create a new column with the years in the first column of the df
pop_df <- pop_df %>% mutate(months = c(0:months)) %>% select(months, everything())


#long data format for ggplot
pop_df_long <- melt(pop_df, id = "months")

ggplot(pop_df_long, aes(x = months, y = value, colour = variable)) + geom_line()

