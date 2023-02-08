library(here)
library(ggplot2)
library(reshape2)
library(dplyr)

here()

# example from the internet http://ecovirtual.ib.usp.br/doku.php?id=en:ecovirt:roteiro:pop_str:pstr_mtr#:~:text=The%20growth%20of%20a%20population,make%20projections%20for%20different%20scenarios.
# imagine a sex and age structured population where females live for 3 years (3 age categories) and males live for 2 years (2 age categories)
# vector of fecundity corresponding to females in age group 1-3 and males in age group 1-2 F1, F2, F3, M1, M2
fecundity <- c(0, 0.5, 2, 0, 0)

#vector of survival for age groups 1-3
survival <- c(0.75, 0.9, 0, 0.8, 0)

#number of age groups
age_groups <- c('F1', 'F2', 'F3', 'M1', "M2")

#empty matrix of dimensions 3*3 equal to number of age groups
Mat_A <- matrix(0, nrow = length(age_groups), ncol = length(age_groups))

#filling the matrix, 

#survival:
#When filling the matrix important to put survival in first since otherwise the 0 terms for survival may overwrite some of the fecundities
for (i in 2:nrow(Mat_A)){
  Mat_A[i,i-1] <- survival[i-1]
}

#fecundity:
#nb fecundity this should be proportional to the number of offspring in each sex class (e.g. 50% female, 50% male)
p_female <- 0.5

#this works since we know that row 4 is the start of the male group, may not be possible on larger scale
for (i in 1:ncol(Mat_A)){
  Mat_A[1,i] <- p_female*fecundity[i]
  Mat_A[4,i] <- (1-p_female)*fecundity[i]
}



#create a matrix for the initial population split by sex and age class: F1, F2, F3, M1, M2
#nb must be a matrix in order to do matrix multiplication?

N0 <- matrix(c(30,40,60,30,55), ncol = 1)

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
colnames(pop_df) <- age_groups

#use dplyr to create a new column with the years in the first column of the df
pop_df <- pop_df %>% mutate(years = c(1:11)) %>% select(years, everything())

#add some additional variables for total number of females, males and overall
pop_df <- pop_df %>% mutate(F_total = F1+F2+F3, M_total = M1+M2, All = F1+F2+F3+M1+M2)

#yse reshape2 library to convert data into long format with year as the id
pop_df_long <- melt(pop_df, id = "years")

ggplot(pop_df_long, aes(x = years, y = value, colour = variable)) + geom_line()




