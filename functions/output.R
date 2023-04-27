output_func <- function(
    TimeStop_dynamics,
    output
){
  if(output == "summary"){
    sum_stats <-  c("w", "sum_pop", "prop_immune", "prop_inf", "pF","pKid", "pSub", "pAdu")
    summary_df <- as.data.frame(matrix(0,nrow = TimeStop_dynamics,ncol = length(sum_stats)))
    colnames(summary_df) <- sum_stats
  }else if(output == "summary_all"){
    sum_stats <-  c("w", "sum_pop", "prop_immune", "prop_inf", "pF", "pfKid", "pfSub", "pfAdu","pmKid", "pmSub", "pmAdu")
    summary_df <- as.data.frame(matrix(0,nrow = TimeStop_dynamics,ncol = length(sum_stats)))
    colnames(summary_df) <- sum_stats
  }else if(output == "counts"){
    # make df
  }
  return(summary_df)
}

