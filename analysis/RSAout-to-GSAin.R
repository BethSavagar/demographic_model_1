var_input_set <- read_csv("data/GSA_parameters/GSA_var_input-PRCC.csv", col_names=T)
apply(var_input_set,2,min)

mn <- apply(var_input_set,2,min)
mx <- apply(var_input_set,2,max)

rbind(mn, mx)

var_input <- rbind(mn,mx)
write.csv(var_input, file = "data/GSA_parameters/GSA_var_input-sobol.csv", row.names = F)
