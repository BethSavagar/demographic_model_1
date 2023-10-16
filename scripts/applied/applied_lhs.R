##################################################################################################
## Generate demographic parameters matrix for applied-profiles with lhs on uncertain parameters ##
##################################################################################################


# Script to construct matrix containing lhs_n number of parameter sets sampled from the parameter space defined for RSA in var_demo_data_full.

var_input <- var_demo_data_full %>%
  # select uncertain parameters:
  filter(parameter %in% c(
    "NET_offtake_y",
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
  # select corresponding min and max columns.
  select(c(parameter,`pars_min`,`pars_max`)) %>%
  # change matrix orientation
  column_to_rownames(var = "parameter") %>%
  t() %>%
  as.data.frame()

## Generate samples using LHS:

# LHS frame:
var_input_df <- randomLHS(lhs_n, ncol(var_input))

# Parameter vals on LHS frame:
for(j in 1:ncol(var_input)){
  var_input_df[,j] <- qunif(var_input_df[,j],var_input[1,j],var_input[2,j])
}

# set colnames:
colnames(var_input_df) <- colnames(var_input)

# pairs(var_input_df)
