

#function to create projection matrix given the following variables: 
#age_groups : a vector containing reference names for each age group e.g. F1, F2, F3, M1, M2
# survival : a vector with the survival into the next age category for each age group, in the same order as the age_groups vector
# fecundity : a vector with the fecundity of each age group (only females), in the same order as the age_groups vector
# p_female : the proportion of offspring which are female
# female_ages : the numbre of female age categories, required to identify which row of the proj mat corresponds to the first male age group

proj_matrix <- function(age_groups, survival, fecundity, p_female, female_ages){
  proj_Mat <- matrix(0, nrow = length(age_groups), ncol = length(age_groups))
  
  for (i in 2:nrow(proj_Mat)){
    proj_Mat[i,i-1] <- survival[i-1]
  }
  
  male_offspring <- (female_ages+1) #which row of matrix corresponds to male offspring
  for (i in 1:ncol(proj_Mat)){
    proj_Mat[1,i] <- p_female*fecundity[i]
    proj_Mat[male_offspring,i] <- (1-p_female)*fecundity[i]
  }
  
  return(proj_Mat)
}

