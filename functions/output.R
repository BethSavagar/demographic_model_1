output_func <- function(
    TimeStop_dynamics,
    output
){
  if(output == "summary"){
    sum_stats <-  c("w", "sum_pop", "prop_immune", "prop_inf", "pKid", "pYou", "pJuv", "pSub", "pAdu", "pF")
    summary_df <- as.data.frame(matrix(0,nrow = TimeStop_dynamics,ncol = length(sum_stats)))
    colnames(summary_df) <- sum_stats
  }else if(output == "counts"){
    # make df
  }
  return(summary_df)
}

