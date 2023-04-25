install.packages("lhs")
library(sensitivity)
library(lhs)

# Define input parameters and their ranges
inputs <- data.frame(x1 = c(0, 1),
                     x2 = c(0, 1))

# Define the model function
model <- function(x){
  return(x[1]^2 + x[2]^2)
}

# Generate samples using Latin hypercube sampling
n <- 1000
set <- randomLHS(inputs, n)

# Perform Sobol sensitivity analysis
sa <- sobol(model, X1 = set[,1], X2 = set[,2], order = 2, nboot = 100)

# View the results
print(sa)