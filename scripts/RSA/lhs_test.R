# install.packages("lhs")
library(sensitivity)
library(lhs)

var_demo_data_full <- read_csv("data/RSA_parameters/RSA_var_input.csv", col_names=T)

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
lhs_n <- 1000
var_input_set <- randomLHS(lhs_n, ncol(var_input))
dim(var_input_set)

for(i in 1:ncol(var_input)){
  var_input_set[,i] <- qunif(var_input_set[,i],var_input[1,i],var_input[2,i])
}

colnames(var_input_set) <- colnames(var_input)


# just a plot of each variable combination - i think the fact it is all black shows good coverage of the parameter space?
pairs(var_input_set)




# 
# ################## TEST >>>> 
# 
# set <- randomLHS(n,ncol(inputs))
# 
# Y <- randomLHS(22, 2) 
# set[,1] <- qunif(set[,1], 0, 0.34) 
# Y[,2] <- qunif(Y[,2], 20, 30) 
# 
# 
# 
# 
# 
# 
# 
# # Define input parameters and their ranges
# inputs <- data.frame(x1 = c(0, 1),
#                      x2 = c(0, 1))
# 
# # Define the model function
# model <- function(x){
#   return(x[1]^2 + x[2]^2)
# }
# 
# # Generate samples using Latin hypercube sampling
# n <- 1000
# set <- randomLHS(ncol(inputs), n)
# 
# # Perform Sobol sensitivity analysis
# sa <- sobol(model, X1 = set[,1], X2 = set[,2], order = 2, nboot = 100)
# 
# # View the results
# print(sa)