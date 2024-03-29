# 29 Aug - include vacciantion fro GSA

dynmod_func <- function(
    f_list, # initial state of female population
    m_list, # initial state of male population
    TimeStop_dynamics, # 1 year, weekly timestep for demographic component
    TimeStop_transmission, # 1 day, hourly timestep for transission component
    output, # model output: tracker or summary stats
    demographic_pars,
    summary_df,
    Imm_b,
    Kid,
    Sub,
    Adu_F,
    Vstart,
    Vprog
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
  
  ###############################
  ## SET DEMOGRAPHIC PARS ##
  ###############################
  
  # Pull demographic rates as vectors from data-frames (for vectorised equations)
  # NB: create if statements for if there's a difference between males and females?
  immunity <- demographic_pars %>% pull(imm) # no difference m/f
  # Imm_b immune birth rate is already defined (proportion of offspring born with maternal immunity)
  
  net_off_F <- demographic_pars %>% pull(net_off_F)
  net_off_M <- demographic_pars %>% pull(net_off_M)
  mort_F <- demographic_pars %>% pull(mort_F) # no m/f difference
  mort_M <- demographic_pars %>% pull(mort_M)
  # mort_M <- age_pars_M %>% pull(mort)
  ppr_mort <- demographic_pars %>% pull(ppr_mort)
  # ppr_mort_M <- age_pars_M %>% pull(ppr_mort)
  birth <- demographic_pars %>% pull(birth)
  
  ###############################
  ## DEMOGRAPHICS LOOP ##
  ###############################
  
  
  
  for(w in 2:TimeStop_dynamics){
    
    if(w == 2){
      fIm_prev <- fIm ; mIm_prev <- mIm
      fS_prev <- fS ; mS_prev <- mS
      fE_prev <- fE ; mE_prev <- mE
      fI_prev <- fI ; mI_prev <- mI
      fR_prev <- fR ; mR_prev <- mR
    }
 
    ## BIRTHS ## 
    
    # Only females > 18M can give birth (birth rate =0 for all other age groups)
    # No M/F differentiation for births
    Im_births <- sum(birth* Imm_b* fR_prev) # immune births 
    S_births <- sum(birth*(1-Imm_b)* fR_prev) + sum(birth*fS_prev) # Sus births = non-immune births to R mothers, and births to S mothers.
    
    E_births <- 0; I_births <- 0; R_births <- 0 # set EIR births to 0
    
    
    ## SEIR DEMOGRAPHICS ##
    
    # Immune Offspring Demographics (f/m)
    fIm_new <- fIm_prev*(immunity)*(1-net_off_F)*(1-mort_F) # decline in mat immunity, offtake and mortality
    mIm_new <- mIm_prev*(immunity)*(1-net_off_M)*(1-mort_M)
    
    fIm_cur <- c(0.5*Im_births, fIm_new[-length(fIm_new)]) # 0.5*births for F/M, # subset demographics remove last age-group
    mIm_cur <- c(0.5*Im_births, mIm_new[-length(mIm_new)]) # 0.5*births for F/M, # subset demographics remove last age-group
    
    ##
    # Susceptible Demographics
    fS_new <- fS_prev*(1-net_off_F)*(1-mort_F) + # survival (those not offtake, and not died)
      fIm_prev*(1-immunity) # add offspring which have lost immunity
    
    mS_new <- mS_prev*(1-net_off_M)*(1-mort_M) +
      mIm_prev*(1-immunity) # add offspring which have lost immunity
    
    # update fS and mS matrices
    fS_cur <- c(0.5*S_births, fS_new[-length(fS_new)]) # 0.5* births are F, lose last age group
    mS_cur <- c(0.5*S_births, mS_new[-length(mS_new)])# 0.5* births are M, lose last age group
    
    ##
    # Exposed Demographics:
    fE_new <- fE_prev*(1-net_off_F)*(1-mort_F) # F offtake and mortality
    mE_new <- mE_prev*(1-net_off_M)*(1-mort_M) # M offtake and mortality
    
    fE_cur <- c(0.5*E_births,fE_new[-length(fE_new)]) # F add births (E_births = 0)  and remove last age group
    mE_cur <- c(0.5*E_births,mE_new[-length(mE_new)]) # M add births (E_births = 0) and remove last age group
    
    ##
    # Infectious Demographics:
    fI_new <- fI_prev*(1-net_off_F)*(1-mort_F) # F offtake and mortality (**ppr_mortality in disease loop?)
    mI_new <- mI_prev*(1-net_off_M)*(1-mort_M) # M offtake and mortality
    
    fI_cur <- c(0.5*I_births,fI_new[-length(fI_new)]) # F add births (I_births = 0) and remove last age group
    mI_cur <- c(0.5*I_births,mI_new[-length(mI_new)]) # M add births (I_births = 0) and remove last age group
    
    # Recovered Demographics:
    fR_new <- fR_prev*(1-net_off_F)*(1-mort_F) # offtake and mortality (**ppr_mortality in disease loop?)
    mR_new <- mR_prev*(1-net_off_M)*(1-mort_M)
    
    fR_cur <- c(0.5*R_births,fR_new[-length(fR_new)]) # F add births (R_births = 0) and remove last age group
    mR_cur <- c(0.5*R_births,mR_new[-length(mR_new)]) # M add births (R_births = 0) and remove last age group
    
    # ---------------
    ## VACCINATION ##
    if(w %in% Vprog$Vweek){
      
      round_i <- Vprog %>% filter(Vweek == w) %>% pull(Vround)
      
      # Vcov is a vector of vaccination coverage for each age group on round_i
      # V_coverage is a df containing hte coverage vector for every round
      pV <- Vprog %>% filter(Vweek == w) %>% pull(pV)
      Vtype <- Vprog %>% filter(Vweek == w) %>% pull(Vtype)
      Vmin <- Vprog %>% filter(Vweek == w) %>% pull(Vmin)
      Vmax <- Vprog %>% filter(Vweek == w) %>% pull(Vmax)
      
      
      # defining the age range for vaccination and producing a Vcov vector
      # Vcov is a vector of vaccination coverage for each age group on round_i
      
      if(Vtype=="full"){
        # if full all animals over the minimum age are vaccinated
        Vcov <- rep(pV, length(fS_cur))
        Vcov[0:Vmin] <- 0
      }else if(Vtype=="partial"){
        # if partial only a subset of animals are vaccinated
        Vcov <- rep(pV, length(fS_cur))
        Vcov[0:Vmin] <- 0
        Vcov[Vmax:length(Vcov)] <- 0
      }
      
      fR_cur <- fR_cur + Vcov*(fS_cur+fE_cur+fI_cur) # new recovered units are all those already immune plus newly vaccinated units from non-immune (SEI) compartments
      fS_cur <- fS_cur*(1-Vcov) 
      fE_cur <- fE_cur*(1-Vcov) 
      fI_cur <- fI_cur*(1-Vcov)
      
      mR_cur <- mR_cur+Vcov*(mS_cur+mE_cur+mI_cur) # new recovered units are all those already immune plus newly vaccinated units from non-immune (SEI) compartments
      mS_cur <- mS_cur*(1-Vcov) 
      mE_cur <- mE_cur*(1-Vcov) 
      mI_cur <- mI_cur*(1-Vcov)
    }
    # ---------------
    
    # -----------------------
    ## TRANSMISSION LOOP ##
    # 
    #
    #
    # ----------------------
    
    
    
    ## OUTPUT ##
    
    f_list <- list("fIm"=fIm_cur,
                   "fS"=fS_cur,
                   "fE"=fE_cur,
                   "fI"=fI_cur,
                   "fR"=fR_cur)
    m_list <- list("mIm"=mIm_cur,
                   "mS"=mS_cur,
                   "mE"=mE_cur,
                   "mI"=mI_cur,
                   "mR"=mR_cur)
    
    summary_df <- summary_demos( w,
                                 f_list,
                                 m_list,
                                 output,
                                 summary_df,
                                 Kid,
                                 Sub,
                                 Adu_F)
    
    min_pop <- 1
    if(summary_df[w,"sum_pop"]<min_pop){
      f_list <- list("fIm"=0,
                     "fS"=0,
                     "fE"=0,
                     "fI"=0,
                     "fR"=0)
      m_list <- list("mIm"=0,
                     "mS"=0,
                     "mE"=0,
                     "mI"=0,
                     "mR"=0)
      
      summary_df <- summary_demos( w,
                                   f_list,
                                   m_list,
                                   output,
                                   summary_df,
                                   Kid,
                                   Sub,
                                   Adu_F)
    }
    
    ## UPDATE STATES ##
    
    fIm_prev <- fIm_cur ; mIm_prev <- mIm_cur
    fS_prev <- fS_cur ; mS_prev <- mS_cur
    fE_prev <- fE_cur ; mE_prev <- mE_cur
    fI_prev <- fI_cur ; mI_prev <- mI_cur
    fR_prev <- fR_cur ; mR_prev <- mR_cur
    
  }
  
  
  summary_df <- summary_df %>% replace(is.na(.), 0)
  return(summary_df)
}

  
  



