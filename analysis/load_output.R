# broad input ranges, preliminary results to understand population dynamics, including conditions (i) 10y growth, (ii) age-sex relaxed and (iii) age-sex strict
# see presentation:RSA_3_170723 for details.

# 100,000 parameter sets from SCC with N=50
RSAoutput <- read.csv("output/RSA_output/RSA_output_2023-07-12.csv")
var_input_set <- read.csv("output/RSA_output/RSA_pars-set_2023-07-12.csv")

# 100,000 parameter sets from SCC with N=100
# RSAoutput <- read.csv("output/RSA_output/RSA_output_2023-07-17.csv")
# var_input_set <- read.csv("output/RSA_output/RSA_pars-set_2023-07-17.csv")

# 10,000 parameter sets from SCC with N=150 --> WEEKLY RATES (wkly.min.1 / wkly.max.1 - from bryony's thesis)
# RSAoutput <- read.csv("output/RSA_output/RSA_output_2023-07-18.csv")
# var_input_set <- read.csv("output/RSA_output/RSA_pars-set_2023-07-18.csv")