# load data for GSA

fix_age_data_full <- read_csv("data/Applied_parameters/cgiar_1.csv",col_names=T)
var_demo_data_full <- read_csv("data/Applied_parameters/cgiar_2.csv", col_names=T)
imm_decay_corrected <- read_csv("data/imm_decay_bodjo_v2.csv") 
