# load data for Applied-Examples & Applied-LHS

fix_age_data_full <- read_csv(paste0("data/Applied_parameters/state_vars",datafile,".csv"),col_names=T)
var_demo_data_full <- read_csv(paste0("data/Applied_parameters/demographics",datafile,".csv"), col_names=T)
imm_decay_corrected <- read_csv("data/imm_decay_bodjo_v2.csv") 
