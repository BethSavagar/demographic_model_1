# load data for GSA

fix_age_data_full <- read_csv("data/GSA_parameters/GSA_fix_input.csv",col_names=T)
var_demo_data_full <- read_csv("data/GSA_parameters/GSA_var_input.csv", col_names=T)
imm_decay_corrected <- read_csv("data/imm_decay_bodjo_v2.csv") 
