library(here)
library(ggplot2)
library(reshape2)


here()

# example from the internet http://ecovirtual.ib.usp.br/doku.php?id=en:ecovirt:roteiro:pop_str:pstr_mtr#:~:text=The%20growth%20of%20a%20population,make%20projections%20for%20different%20scenarios.
# imagine a population of 3 age groups, in which the latter two groups reproduce with fecundity 0.5 and 2 respectively
# survival into the next age category from A1 to A2 is 0.75 and from A2 to A3 is 0.9, A3 is the terminal class so there is no survival after this

#vector of fecuntity for age groups 1-3
fecundity <- c(0, 0.5, 2)

#vector of survival for age groups 1-3
survival <- c(0.75, 0.9, 0)

#empty matrix of dimensions 3*3 equal to number of age groups
Mat_A <- matrix(0, nrow = 3, ncol = 3)

#filling the matrix
for (i in 1:ncol(Mat_A)){
  Mat_A[1,i] <- fecundity[i]
}

for (i in 2:nrow(Mat_A)){
    Mat_A[i,i-1] <- survival[i-1]
}

#create a matrix for the initial population with 50 offspring, 40 juveniles and 60 adults:
#nb must be a matrix in order to do matrix multiplication?

N0 <- matrix(c(50,40,60), ncol = 1)

#calculate the population for generation 1: 

N1 <- Mat_A %*% N0

#simulate the population over the next 10 years

#set the number of years/generations
years <- 10

#container for population projection with nrow equal to the number of age groups (Mat_A) and ncol equal to the number of years/generations (+1 for the initial poulation vector)
pop_projection <- matrix(0, nrow = nrow(Mat_A), ncol = years+1)
pop_projection[,1] <- N0

for(i in 1:years){
  pop_projection[, i+1] <- Mat_A %*% pop_projection[ ,i]
}

#pop_projection matrix as dataframe suitable for ggplot (transposed)

pop_df <- as.data.frame(t(pop_projection))
colnames(pop_df) <- c("offspring", "juvenile", "adult")
pop_df$years <- c(1:11)

ggplot(pop_df, aes(x = years, y = offspring)) + geom_line()

#yse reshape2 library to convert data into long format with year as the id
pop_df_long <- melt(pop_df, id = "years")

ggplot(pop_df_long, aes(x = years, y = value, colour = variable)) + geom_line()




