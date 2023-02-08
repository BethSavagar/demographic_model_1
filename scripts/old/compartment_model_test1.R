
#age groups

pop_dyn <- function(t, var, survival, fecundity){
  
  A1 <- var[1]
  A2 <- var[2]
  A3 <- var[3]
  
  f1 <- fecundity[1]
  f2 <- fecundity[2]
  f3 <- fecundity[3]
  
  s1 <- survival[1]
  s2 <- survival[2]
  s3 <- survival[3]

    dA1 <- f2*A2+f3*A3
    dA2 <- s1*A1
    dA3 <- s2*A2

list(c(dA1, dA2, dA3))
}