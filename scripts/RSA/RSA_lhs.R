# install.packages("lhs")
library(sensitivity)
library(lhs)

# var_demo_data_full <- read_csv("data/RSA_parameters/RSA_var_input.csv", col_names=T)

var_input <- var_demo_data_full %>%
  filter(parameter %in% c(
    "NET_offtake_m",
    "NET_offtake_f",
    "mortality_y",
    "mortality_a",
    "birth_rate")) %>%
  select(c(parameter,`pars_min`,`pars_max`)) %>%
  #select(c(parameter,`min.1`,`max.1`)) %>%
  column_to_rownames(var = "parameter") %>%
t() %>%
  as.data.frame()


# Generate samples using Latin hypercube sampling

var_input_set <- randomLHS(lhs_n, ncol(var_input))
dim(var_input_set)

for(i in 1:ncol(var_input)){
  var_input_set[,i] <- qunif(var_input_set[,i],var_input[1,i],var_input[2,i])
}

colnames(var_input_set) <- colnames(var_input)


# just a plot of each variable combination - i think the fact it is all black shows good coverage of the parameter space?
pairs(var_input_set)

