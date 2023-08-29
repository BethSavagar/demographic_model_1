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
set <- lhs(inputs, n)

# Perform Sobol sensitivity analysis
sa <- sobol(model, X1 = set[,1], X2 = set[,2], order = 2, nboot = 100)

# View the results
print(sa)




#########################################################################
# Test case : the non-monotonic Sobol g-function
# The method of sobol requires 2 samples
# (there are 8 factors, all following the uniform distribution on [0,1])
library(boot)
n <- 1000
X1 <- data.frame(matrix(runif(8 * n), nrow = n))
X2 <- data.frame(matrix(runif(8 * n), nrow = n))
# sensitivity analysis
x <- sobol(model = sobol.fun, X1 = X1, X2 = X2, order = 2, nboot = 100)
print(x)
#plot(x)
library(ggplot2)
ggplot(x)
