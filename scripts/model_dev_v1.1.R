## Flock Dynamics Model Code v1.1 (19/01/23)
# 20/02/23
# -- outputs_dev branch: switch from tracker matrix to vectors and summary stats as output (reduce computational lload)

# 06/02/23 -- rudimentary model works fine.

# 30/01/23 
# -- think about use of apply and functions for creating the M/F vectors, and for updating population (demographic) states within the model
# what is the output, run some tests of this demographic model
# how to maintain population i.e. stability or growth?

## SETUP ##

library(tidyverse)


#########################################################################################################################################
## MODEL SETUP ##
# potentially reconfigure this stuff as lists and use apply?

# Source parameters script
source("scripts/set-pars.R")

TimeStop_dynamics <- 5*52 # 1 year, weekly timestep for demographic component
TimeStop_transmission <- 24 # 1 day, hourly timestep for transission component
output <- "summary" # define output type "summary" or "count"

# initial POPULATION-STATE

# define SEIR vectors for male and female age groups
fIm_init <- rep(0,max_age_F); mIm_init <- rep(0,max_age_M)
fS_init <- age_pars_F %>% pull(pop_init_F) *(1-pR) # all S except already recovered
mS_init <- age_pars_M %>% pull(pop_init_M) *(1-pR) # all S except already recovered
fE_init <- rep(0,max_age_F); mE_init <- rep(0,max_age_M)
fI_init <- rep(0,max_age_F); mI_init <- rep(0,max_age_M)
fR_init <- rep(0,max_age_F); mR_init <- rep(0,max_age_M)

if(pR>0){
  fR_init <- age_pars_F %>% pull(pop_init_F) *pR
  mR_init <- age_pars_M %>% pull(pop_init_M) *pR
}

## SUMMARY STATS DF
# create summary data frame to store summary stats for each timestep

if(output == "summary"){
  sum_stats <-  c("w", "sum_pop", "prop_immune", "prop_inf", "pKid", "pYou", "pJuv", "pSub", "pAdu", "pF")
  summary_df <- as.data.frame(matrix(0,
                                     nrow=TimeStop_dynamics, 
                                     ncol = length(sum_stats)))
  colnames(summary_df) <- sum_stats
}else if(output == "counts"){
  # make df
}


# total population size
fpop <- fIm_init+fS_init+fE_init+fI_init+fR_init
mpop <- mIm_init+mS_init+mE_init+mI_init+mR_init
mpop <- c(mpop, rep(0,max_age_F-max_age_M))
pop_tot <- fpop+mpop
sum_pop <- sum(fpop)+sum(mpop)

# population growth (yearly calculation)
# proportion immune
fimmune <- fIm_init+fR_init
mimmune <- mIm_init+mR_init
sum_immune <- sum(fimmune)+sum(mimmune)
prop_immune <- sum_immune / sum_pop

# proportion infectious
sum_inf <- sum(fI_init)+sum(mI_init)
prop_inf <- sum_inf/sum_pop

# proportion in each age-group
Kid_tot <- pop_tot[Kid] ; sum_Kid <- sum(Kid_tot) ; pKid <- sum_Kid/sum_pop
You_tot <- pop_tot[You] ; sum_You <- sum(You_tot) ; pYou <- sum_You/sum_pop
# KidYou_tot <- Kid_tot+You_tot ; sum_KidYou <- sumKid+sumYou ; pKidYou <- sumKidYou/sum_pop
Juv_tot <- pop_tot[Juv] ; sum_Juv <- sum(Juv_tot) ; pJuv <- sum_Juv/sum_pop
Sub_tot <- pop_tot[Sub] ; sum_Sub <- sum(Sub_tot) ; pSub <- sum_Sub/sum_pop
Adu_tot <- pop_tot[Adu_F] ; sum_Adu <- sum(Adu_tot) ; pAdu <- sum_Adu/sum_pop
# also add for female and male stratification (of adults/all) ??

# proportion female
F_tot <- fIm_init+fS_init+fE_init+fI_init+fR_init
F_sum <- sum(F_tot)
pF <- F_sum/sum_pop

summary_df[1,] <- c(1, sum_pop, prop_immune, prop_inf, pKid,pYou,pJuv,pSub,pAdu,pF)




## DEMOGRAPHIC PARAMS
# Pull demographic rates as vectors from data-frames (for vectorised equations)
# NB: create if statements for if there's a difference between males and females?
immunity_F <- age_pars_F %>% pull(imm)
immunity_M <- age_pars_M %>% pull(imm)
# Imm_b immune birth rate is already defined (proportion of offspring born with maternal immunity)
net_off_F <- age_pars_F %>% pull(net_off_F)
net_off_M <- age_pars_M %>% pull(net_off_M)
mort_F <- age_pars_F %>% pull(mort)
mort_M <- age_pars_M %>% pull(mort)
ppr_mort_F <- age_pars_F %>% pull(ppr_mort)
ppr_mort_M <- age_pars_M %>% pull(ppr_mort)
birth <- age_pars_F %>% pull(birth)


###############################
## DEMOGRAPHICS LOOP ##
###############################

for(w in 2:TimeStop_dynamics){
  
  if(w == 2){
    fIm_prev <- fIm_init ; mIm_prev <- mIm_init
    fS_prev <- fS_init ; mS_prev <- mS_init
    fE_prev <- fE_init ; mE_prev <- mE_init
    fI_prev <- fI_init ; mI_prev <- mI_init
    fR_prev <- fR_init ; mR_prev <- mR_init
  }
  
  ## BIRTHS ## 
  
  # Only females > 18M can give birth (birth rate =0 for all other age groups)
  # No M/F differentiation for births
  Im_births <- sum(birth* Imm_b* fR_prev) # immune births 
  S_births <- sum(birth*(1-Imm_b)* fR_prev) + sum(birth*fS_prev) # Sus births = non-immune births to R mothers, and births to S mothers.
  
  E_births <- 0; I_births <- 0; R_births <- 0 # set EIR births to 0
  
  
  ## SEIR DEMOGRAPHICS ##
  
  # Immune Offspring Demographics (f/m)
  fIm_new <- fIm_prev*(immunity_F)*(1-net_off_F)*(1-mort_F) # decline in mat immunity, offtake and mortality
  mIm_new <- mIm_prev*(immunity_M)*(1-net_off_M)*(1-mort_M)
  
  fIm_cur <- c(0.5*Im_births, fIm_new[1:max_age_F-1]) # 0.5*births for F/M, # subset demographics remove last age-group
  mIm_cur <- c(0.5*Im_births, mIm_new[1:max_age_M-1]) # 0.5*births for F/M, # subset demographics remove last age-group
  
  ##
  # Susceptible Demographics
  fS_new <- fS_prev*(1-net_off_F)*(1-mort_F) + # survival (those not offtake, and not died)
    fIm_prev*(1-immunity_F) # add offspring which have lost immunity
  
  mS_new <- mS_prev*(1-net_off_M)*(1-mort_M) +
    mIm_prev*(1-immunity_M) # add offspring which have lost immunity
  
  # update fS and mS matrices
  fS_cur <- c(0.5*S_births, fS_new[1:max_age_F-1]) # 0.5* births are F, lose last age group
  mS_cur <- c(0.5*S_births, mS_new[1:max_age_M-1])# 0.5* births are M, lose last age group
  
  ##
  # Exposed Demographics:
  fE_new <- fE_prev*(1-net_off_F)*(1-mort_F) # F offtake and mortality
  mE_new <- mE_prev*(1-net_off_M)*(1-mort_M) # M offtake and mortality
  
  fE_cur <- c(0.5*E_births,fE_new[1:max_age_F-1]) # F add births (E_births = 0)  and remove last age group
  mE_cur <- c(0.5*E_births,mE_new[1:max_age_M-1]) # M add births (E_births = 0) and remove last age group
  
  ##
  # Infectious Demographics:
  fI_new <- fI_prev*(1-net_off_F)*(1-mort_F) # F offtake and mortality (**ppr_mortality in disease loop?)
  mI_new <- mI_prev*(1-net_off_M)*(1-mort_M) # M offtake and mortality
  
  fI_cur <- c(0.5*I_births,fI_new[1:max_age_F-1]) # F add births (I_births = 0) and remove last age group
  mI_cur <- c(0.5*I_births,mI_new[1:max_age_M-1]) # M add births (I_births = 0) and remove last age group
  
  # Recovered Demographics:
  fR_new <- fR_prev*(1-net_off_F)*(1-mort_F) # offtake and mortality (**ppr_mortality in disease loop?)
  mR_new <- mR_prev*(1-net_off_M)*(1-mort_M)
  
  fR_cur <- c(0.5*R_births,fR_new[1:max_age_F-1]) # F add births (R_births = 0) and remove last age group
  mR_cur <- c(0.5*R_births,mR_new[1:max_age_M-1]) # M add births (R_births = 0) and remove last age group
  
  ## TRANSMISSION LOOP ##
  
 

  ## OUTPUT....CREATE OUTPUTS THAT DON'T DEPEND ON TRACKING MATRIX >>>>
  # reduce output so that rather than storing entire matrices we just produce summary statistics: need to decide what stats are of interest (06/02/23)
  
  # summary stats (for sensitivity analysis)
  if(output=="summary"){
    
    
    # total population size
    fpop <- fIm_cur+fS_cur+fE_cur+fI_cur+fR_cur
    mpop <- mIm_cur+mS_cur+mE_cur+mI_cur+mR_cur
    mpop <- c(mpop, rep(0,max_age_F-max_age_M))
    pop_tot <- fpop+mpop
    sum_pop <- sum(fpop)+sum(mpop)
    
    # population growth (yearly calculation)
    # proportion immune
    fimmune <- fIm_cur+fR_cur
    mimmune <- mIm_cur+mR_cur
    sum_immune <- sum(fimmune)+sum(mimmune)
    prop_immune <- sum_immune / sum_pop
    
    # proportion infectious
    sum_inf <- sum(fI_cur)+sum(mI_cur)
    prop_inf <- sum_inf/sum_pop
    
    # proportion in each age-group
    Kid_tot <- pop_tot[Kid] ; sum_Kid <- sum(Kid_tot) ; pKid <- sum_Kid/sum_pop
    You_tot <- pop_tot[You] ; sum_You <- sum(You_tot) ; pYou <- sum_You/sum_pop
    # KidYou_tot <- Kid_tot+You_tot ; sum_KidYou <- sumKid+sumYou ; pKidYou <- sumKidYou/sum_pop
    Juv_tot <- pop_tot[Juv] ; sum_Juv <- sum(Juv_tot) ; pJuv <- sum_Juv/sum_pop
    Sub_tot <- pop_tot[Sub] ; sum_Sub <- sum(Sub_tot) ; pSub <- sum_Sub/sum_pop
    Adu_tot <- pop_tot[Adu_F] ; sum_Adu <- sum(Adu_tot) ; pAdu <- sum_Adu/sum_pop
    # also add for female and male stratification (of adults/all) ??
    
    # proportion female
    F_tot <- fIm_cur+fS_cur+fE_cur+fI_cur+fR_cur
    F_sum <- sum(F_tot)
    pF <- F_sum/sum_pop
    
    summary_df[w,] <- c(w, sum_pop, prop_immune, prop_inf, pKid,pYou,pJuv,pSub,pAdu,pF)
    
  }else if(output=="counts"){
    # matrix storing total pop in each disease state.
    # totals <- data.frame(
    #   "Im" = apply(fIm_mat, 2, sum)+apply(mIm_mat, 2, sum),
    #   "S" = apply(fS_mat, 2, sum)+apply(mS_mat, 2, sum),
    #   "E" = apply(fE_mat, 2, sum)+apply(mE_mat, 2, sum),
    #   "I" = apply(fI_mat, 2, sum)+apply(mI_mat, 2, sum),
    #   "R" = apply(fR_mat, 2, sum)+apply(mR_mat, 2, sum)
    # ) %>%
    #   mutate("all" = apply(.,1,sum), # total population size
    #          "imm_frac" = (R+Im)/all, # immune proportion of population
    #          "time" = 1:TimeStop_dynamics) # timestep
  }

  # update time and state vectors: 
  
  fIm_prev <- fIm_cur ; mIm_cur <- mIm_cur
  fS_prev <- fS_cur ; mS_prev <- mS_cur
  fE_prev <- fE_cur ; mE_prev <- mE_cur
  fI_prev <- fI_cur ; mI_prev <- mI_cur
  fR_prev <- fR_cur ; mR_prev <- mR_cur
  
}

summary_df <- summary_df %>%
  mutate(w = as.numeric(w))

ggplot(summary_df,aes(x=w,y=sum_pop))+
  geom_line(size = 1.5)+
  labs(title = "Total Population",
       x="time (weeks)", 
       y = "total population")+
  theme_bw()


## melt summary_df to long format
summary_long <- summary_df %>%
  gather(key="stat", value="prop", -w)


## create df for different stats:

# age-sex
summary_agesex <- summary_long %>%
  filter(!stat %in% c("sum_pop",
                      "prop_inf",
                      "prop_immune"))

## Plots:
# age-sex proportions
ggplot(summary_agesex, aes(x=w, y=prop, group=stat, col=stat))+
  geom_line()

# immunity
ggplot(summary_df, aes(x=w, y=prop_immune))+
  geom_line()

