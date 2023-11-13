library(sensobol)


filepath <- "/Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/3. Population_Dynamics_Model/pop-dynamics_April2020/demographic_model_1/"

## Sims script: set up 23 June to run base vaccination simulations
source(paste0(filepath,"scripts/setup.R"))
list.files(paste0(filepath,"functions"), full.names = TRUE) %>% map(source)
source(paste0(filepath, "scripts/load_data.R"))

lhs_n <- 100
#lhs_n <- 1e5

# ---------------------------------
## SENSITIVITY ANALYSIS PARAMETERS:
pairs_plot <- F
SA <- TRUE
rates <- "yrly" # wkly
set.seed(1)

# select parameter min-max pair (see RSA_var_input.csv)
pars_min <- "min.4"
pars_max <- "max.4"
if(rates == "wkly"){
  pars_min <- "wkly.min.1"
  pars_max <- "wkly.max.1"
}
fixdata <- "sim.1" # fix_input_2 : sim.2 for pR=1... 
# fixdata <- "sim.3" # sim.3 for N = 100 (14-07-2023)
vardata <- "sim.1" # select dataset for test data, # test_1 dataset, all pars set to 0 and all animals in age group 1 (susceptible)

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




# Define settings
N <- lhs_n; 
params <- colnames(var_input); 
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



# just a plot of each variable combination - i think the fact it is all black shows good coverage of the parameter space?
if(pairs_plot == T){
  pairs(var_input_set)
}







