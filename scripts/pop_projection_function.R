population_projection <- function(pop_init,
                                  n_sim,
                                  proj_Mat_list,
                                  age_groups) {
  pop_projection <-
    matrix(0, nrow = length(pop_init), ncol = n_sim + 1) #nb n_sim+1 to account for init population size
  pop_projection[, 1] <- pop_init
  
  for (i in 1:n_sim) {
    pop_projection[, i + 1] <-
      proj_Mat_list[[i]] %*% pop_projection[, i]
  }
 
  return(pop_projection)
}
