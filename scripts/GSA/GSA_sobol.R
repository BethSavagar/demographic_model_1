###############################################################################################################
## Generate demographic parameters matrix with LHS sampling - 04/07/23
###############################################################################################################

library(sensobol)
# Script to construct matrix containing lhs_n number of parameter sets sampled from the parameter space defined for RSA in var_demo_data_full.
var_input <- read_csv("data/GSA_parameters/GSA_var_input-sobol.csv") %>%
  as.data.frame() %>%
  select(-`...1`)

var_input <-  var_input %>% rename(
  "NET_offtake_m"= off_mA,
  "NET_offtake_f"= off_f,
  "mortality_y"= mort_Y,
  "mortality_a"= mort_A,
  "birth_rate"= birth_rate,
  "adu_f_max_yrs"= max_yrs_F,
  "adu_m_max_yrs"= max_yrs_M,
  "min_age_offtake"= min_off,
  "min_age_repro"= min_repro,
  "NET_offtake_m2"=off_mY 
)


# Generate samples using sobol sampling

N <- 1e4 # scaling factor, set to at least 1000
params <- colnames(var_input)
order <- "first"
# Create sample matrix using Sobol' Quasi Random Numbers.
mat <- sobol_matrices(N = N, 
                      params = params, 
                      # type - "lhs" default is sobol quasi-random numbers 
                      order = order # One of "first", "second", "third" or "fourth" 
)



for(j in 1:ncol(var_input)){
  mat[,j] <- qunif(mat[,j],var_input[1,j],var_input[2,j])
}

var_input_set <- as.data.frame(mat)

colnames(var_input_set) <- colnames(var_input)


