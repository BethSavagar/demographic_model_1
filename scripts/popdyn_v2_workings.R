################################################################################
## FILE INFORMATION ##
# workbook developing demograpahic model
# 19/10/22 have made a start to attempt building a generalisable dynamics model.
################################################################################


################################################################################
## Setup

# load libraries etc: 
library(tidyverse)

################################################################################
## Data load

# load example demographic data, stratified by sex-age groups.

surv <- data.frame("age_cat" = all_age_cats, 
                   "s" = rep(c(0.9,0.8,0.7,0), 2))


################################################################################
## Sex-Age category dataframe

# Define sex-age categories:

F_age_cats <- c("F_new", "F_kid", "F_young", "F_adult")
M_age_cats <- c("M_new", "M_kid", "M_young", "M_adult") 
all_age_cats <- c(F_age_cats, M_age_cats)

# max ages (lifespan in flock) of F & M, in month ts
F_max_age <- 12*7 # 7y
M_max_age <- 4*7 # 4y

# define age sequences (in 1 month timestep):
new <- 0:1 #months
kid <- 2:6
young <- 7:15
adult <- 16:F_max_age

## Construct dataframe of sex, age in months and sex-age category

age_cats_df <- data.frame(
  "sex"=c(rep("F",F_max_age+1), 
          rep("M",M_max_age+1)),
  "month" = c(0:F_max_age, 
              0:M_max_age)) %>%
  mutate(
    "sex" = as.factor(sex),
    "age_cat" = case_when(sex=="F" & month %in% new ~ "F_new",
                          sex=="F" & month %in% kid ~ "F_kid",
                          sex=="F" & month %in% young ~ "F_young",
                          sex=="F" & month %in% adult ~ "F_adult",
                          sex=="M" & month %in% new ~ "M_new", 
                          sex=="M" & month %in% kid ~ "M_kid",
                          sex=="M" & month %in% young ~ "M_young", 
                          sex=="M" & month %in% adult ~ "M_adult"),
    "cat_code" = paste0(sex, month)) 

################################################################################# 
## Sex-Age demographics table

# Construct sex-age stratified dataframe with demographic rates
# demographics: birth rate (F_adult only), death rate, intake, offtake

demog_df <- age_cats_df %>%
  # join demographic parameters on to age-sex structured dataframe.
  left_join(surv, by = "age_cat")
                        
                                   
pop_vec_init <- vector(mode="numeric",
                       length = F_max_age + M_max_age)