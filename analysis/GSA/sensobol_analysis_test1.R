library(sensobol)

## Define the base sample size and the parameters
N <- lhs_n
params <- colnames(var_input)

## Create sample matrix to compute first and total-order indices:
mat <- var_input_backup

## Compute the model output (using the Ishigami test function):
Y <- RSAout$tenyr_growth

## Compute and bootstrap the Sobol' indices:
ind <- sobol_indices(Y = Y, N = N, params = params)

plot(ind)


