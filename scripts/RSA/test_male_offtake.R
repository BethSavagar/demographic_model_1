wks<-25
off_M <- 0.026
M <- vector(length = wks)
new_M <- 2
M[1] <- new_M
M_df <- matrix(nrow=25,ncol=25)
M_df[1,] <- M

for(i in 2:nrow(M_df)){
  
  M2 <- M*(1-off_M)
  M_update <- c(new_M, M2[1:(wks-1)])
  
  M_df[i,]<- M_update
  M <- M_update
  
}