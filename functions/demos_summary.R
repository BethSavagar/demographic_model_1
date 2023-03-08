# summary stats function for demographics model

# ## SUMMARY STATS ## This goes in main text...
# # create summary data frame to store summary stats for each timestep
# if(output == "summary"){
#   sum_stats <-  c("w", "sum_pop", "prop_immune", "prop_inf", "pKid", "pYou", "pJuv", "pSub", "pAdu", "pF")
#   summary_df <- as.data.frame(matrix(0,
#                                      nrow = TimeStop_dynamics, 
#                                      ncol = length(sum_stats)))
#   colnames(summary_df) <- sum_stats
# }else if(output == "counts"){
#   # make df
# }
# 
# 
# f_list <- list("fIm"=fIm_init,
#                "fS"=fS_init,
#                "fE"=fE_init,
#                "fI"=fI_init,
#                "fR"=fR_init)
# m_list <- list("mIm"=mIm_init,
#                "mS"=mS_init,
#                "mE"=mE_init,
#                "mI"=mI_init,
#                "mR"=mR_init)
# 
# 
# 

summary_demos <- function(
    w,
    f_list,
    m_list,
    output
){
  
  fIm <- f_list[["fIm"]]
  fS <- f_list[["fS"]]
  fE <- f_list[["fE"]]
  fI <- f_list[["fI"]]
  fR <- f_list[["fR"]]
  
  mIm <- m_list[["mIm"]]
  mS <- m_list[["mS"]]
  mE <- m_list[["mE"]]
  mI <- m_list[["mI"]]
  mR <- m_list[["mR"]]
  
  if(output == "summary"){
    # total population size
    fpop <- fIm+fS+fE+fI+fR
    mpop <- mIm+mS+mE+mI+mR
    pop_tot <- fpop+mpop
    sum_pop <- sum(fpop)+sum(mpop)
    
    # proportion immune
    fimmune <- fIm+fR
    mimmune <- mIm+mR
    sum_immune <- sum(fimmune)+sum(mimmune)
    prop_immune <- sum_immune / sum_pop
    
    # proportion infectious
    sum_inf <- sum(fI)+sum(mI)
    prop_inf <- sum_inf/sum_pop
    
    # proportion in each age-group
    Kid_tot <- pop_tot[Kid] ; sum_Kid <- sum(Kid_tot) ; pKid <- sum_Kid/sum_pop
    You_tot <- pop_tot[You] ; sum_You <- sum(You_tot) ; pYou <- sum_You/sum_pop
    Juv_tot <- pop_tot[Juv] ; sum_Juv <- sum(Juv_tot) ; pJuv <- sum_Juv/sum_pop
    Sub_tot <- pop_tot[Sub] ; sum_Sub <- sum(Sub_tot) ; pSub <- sum_Sub/sum_pop
    Adu_tot <- pop_tot[Adu_F] ; sum_Adu <- sum(Adu_tot) ; pAdu <- sum_Adu/sum_pop
    
    # sex-age group
    fKid_tot <- fpop[Kid] ; sum_fKid <- sum(fKid_tot) ; pfKid <- sum_fKid/sum(fpop)
    mKid_tot <- mpop[Kid] ; sum_mKid <- sum(mKid_tot) ; pmKid <- sum_mKid/sum(mpop)
    fYou_tot <- fpop[You] ; sum_fYou <- sum(fYou_tot) ; pfYou <- sum_fYou/sum(fpop)
    mYou_tot <- mpop[You] ; sum_mYou <- sum(mYou_tot) ; pmYou <- sum_mYou/sum(mpop)
    fJuv_tot <- fpop[Juv] ; sum_fJuv <- sum(fJuv_tot) ; pfJuv <- sum_fJuv/sum(fpop)
    mJuv_tot <- mpop[Juv] ; sum_mJuv <- sum(mJuv_tot) ; pmJuv <- sum_mJuv/sum(mpop)
    fSub_tot <- fpop[Sub] ; sum_fSub <- sum(fSub_tot) ; pfSub <- sum_fSub/sum(fpop)
    mSub_tot <- mpop[Sub] ; sum_mSub <- sum(mSub_tot) ; pmSub <- sum_mSub/sum(mpop)
    fAdu_tot <- fpop[Adu_F] ; sum_fAdu <- sum(fAdu_tot) ; pfAdu <- sum_fAdu/sum(fpop)
    mAdu_tot <- mpop[Adu_F] ; sum_mAdu <- sum(mAdu_tot) ; pmAdu <- sum_mAdu/sum(mpop)
    
    # proportion female
    pF <- sum(fpop)/sum(pop_tot)
    
    # add row to summary stats dataframe
    summary_df[w,] <- c(w, sum_pop, prop_immune, prop_inf, pKid,pYou,pJuv,pSub,pAdu,pF)
    
  }else if(output == "counts"){
    # tracking matrix...
    #...
    #...
    count_df <- c()
  }
  
  
  # Output of Model: 
  if(output == "summary"){
    return(summary_df)
  }else if(output == "counts"){
    return(count_df)
  }
  
  
}


