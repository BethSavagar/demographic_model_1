# test fit distributions to data from original RSA tenyr growth behavioural parameters. 

library(rriskDistributions)

# run RSA_analysis_3.R script up to line 125, producing RSApars_15age df which contains parameter sets which produce behavioural outputs (age-sex strucutre & 15% growth)

# test with max_yrs_F (relatively uniform dist) and birth_rate (which is skewed distribution)

str(RSApars_15age)

data2fit <- RSApars_15age$max_yrs_F

res1 <- fit.cont(data2fit)


data2fitb <- RSApars_15age$max_yrs_F

res2 <- fit.cont(data2fitb)
