###############################################################################################################
## Generate demographic parameters matrix with LHS sampling - 04/07/23
###############################################################################################################
# Script to construct matrix containing lhs_n number of parameter sets sampled from the parameter space defined for RSA in var_demo_data_full.

var_input <- var_demo_data_full %>%
  filter(parameter %in% c(
    "NET_offtake_m",
    "NET_offtake_m2",
    "NET_offtake_f",
    "mortality_y",
    "mortality_a",
    "birth_rate",
    "adu_f_max_yrs",
    "adu_m_max_yrs",
    "min_age_offtake",
    "min_age_repro"
    )) %>%
  select(c(parameter,`pars_min`,`pars_max`)) %>%
  #select(c(parameter,`min.1`,`max.1`)) %>%
  column_to_rownames(var = "parameter") %>%
t() %>%
  as.data.frame()


# Generate samples using Latin hypercube sampling

var_input_set <- randomLHS(lhs_n, ncol(var_input))
dim(var_input_set)

for(j in 1:ncol(var_input)){
  var_input_set[,j] <- qunif(var_input_set[,j],var_input[1,j],var_input[2,j])
}

colnames(var_input_set) <- colnames(var_input)

