library(sensitivity)


# Try sobol SA with a proxy model which takes input pars, and returns (pre-calculated) output

X <- tenyr_pars # refined input parameters (with stable growth)
Soutput <- tenyr_growth %>% pull(tenyr_growth) # output (pop growth) corresponding to tenyr_pars input parameters

# proxy model 
# takes X (input pars) as input
# returns Db (output)
modelS <- function(X){
  
  Db <- Soutput 
  return(Db)
  
}

# run with sobol2002 function:
sobol2002(modelS, X, nboot = 0)
# " Error in ncol(X2) : argument "X2" is missing, with no default "

sobol2002(modelS, X1=X, X2=X, nboot = 0)
# Error in d[, 2] : subscript out of bounds


# GF dfefined function: 

modelS <- function(X){
  for(i in 1:nrow(X)){
    Lw <- vector(length=ncol(X),mode="list")
    for(j in 1:ncol(X)){
      Lw[[j]] <- which(Db[,j]==X[i,j]) # Db includes the parameters (same order as in X1 and X2) and the output variable
    }
    w <- Reduce(intersect, Lw) # repeated value across all elements
  }
  Db[w,"output"]
}

Db <- tenyr_pars %>% mutate(output=Soutput)

X1 <- tenyr_pars; X2 <- tenyr_pars

x <- sobol2002(model = modelS, X1, X2, nboot = 1)
# Error in d[, 2] : subscript out of bounds



## Sobol Function Code ##

function (model = NULL, X1, X2, nboot = 0, conf = 0.95, ...) # X1 & X2 are subsets of input matrix?
{
  if ((ncol(X1) != ncol(X2)) | (nrow(X1) != nrow(X2))) 
    stop("The samples X1 and X2 must have the same dimensions")
  p <- ncol(X1)
  X <- rbind(X1, X2)
  for (i in 1:p) { # loop over columns (pars) in X1
    Xb <- X1
    Xb[, i] <- X2[, i] # define Xb as X1 with column (i) switched out with column (i) from X2, i.e. Xb == X1 except in 1 parameter (col i)
    X <- rbind(X, Xb) # X becomes master matrix with all possible combinations of X1 & X2, including rows which vary in only 1 parameter.
  }
  
  # where the sensitivity analysis happens? >>>
  x <- list(model = model, X1 = X1, X2 = X2, nboot = nboot, 
            conf = conf, X = X, call = match.call())
  class(x) <- "sobol2002"
  if (!is.null(x$model)) {
    
    # tried to bodge a fix by running response() line by line and assigning variable y to Soutput but tell() still failed: error "Error in d[, 2] : subscript out of bounds"
    
    response(x, ...)
    tell(x)
    
  }
  return(x)
}

