# load data for Applied-Examples & Applied-LHS

fix_age_data_full <- read_csv("data/Applied_parameters/state_vars.csv",col_names=T)
var_demo_data_full <- read_csv("data/Applied_parameters/demographics-lhs.csv", col_names=T)
if(applied_example){
  var_demo_data_full <- read_csv("data/Applied_parameters/demographics.csv", col_names=T)
}
imm_decay_corrected <- read_csv("data/imm_decay_bodjo_v2.csv") 
