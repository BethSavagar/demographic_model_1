RSA_func <- function(
    imm_decay_corrected,
    var_input_full,
    fix_age_data_full,
    f_list, # initial state of female population
    m_list, # initial state of male population
    TimeStop_dynamics, # 1 year, weekly timestep for demographic component
    TimeStop_transmission, # 1 day, hourly timestep for transission component
    output, # model output: tracker or summary stats
    summary_df, #
    clean_environment
){
  
  # use demographic matrix: 
  var_input_set <- var_input_full
  # this script uses the fix_age_data df (fixed parameters + init pop state) and  var_input_set (variable demographic rates) to generate the demographic_pars df which contains the demographic rates for each week-long sex-age group
  # source("scripts/parameters/set-pars-RSA3.R") # removing this script seems potentially to have fixed the issue? 
  # output is demographic pars dataframe (for sex-age-groupings) and starting population m_list, f_list
  
  fix_age_data <- fix_age_data_full %>% select(parameter, "value" = all_of(`fixdata`))
  
  
  # ---------------------------
  ## FIXED PARAMETERS ## Flock size, initial pop state
  
  ## Flock Size ##
  N_tot <- fix_age_data %>% filter(parameter=="N_tot") %>% pull(value) # number of animals in population (flock or village)?
  
  ## Flock Structure ##
  # set proportion of male/female: kid (<6m), sub (<12m), adu (12m+)
  
  kid_f_prop <- fix_age_data %>% filter(parameter=="kid_f_prop") %>% pull(value) 
  sub_f_prop <- fix_age_data %>% filter(parameter=="sub_f_prop") %>% pull(value) 
  adu_f_prop <- fix_age_data %>% filter(parameter=="adu_f_prop") %>% pull(value) 
  
  kid_m_prop <- fix_age_data %>% filter(parameter=="kid_m_prop") %>% pull(value) 
  sub_m_prop <- fix_age_data %>% filter(parameter=="sub_m_prop") %>% pull(value) 
  adu_m_prop <- fix_age_data %>% filter(parameter=="adu_m_prop") %>% pull(value) 
  
  age_p_f <- c("kid_f_p"=kid_f_prop,"sub_f_p"=sub_f_prop,"adu_f_p"=adu_f_prop) # sex-age group proportions (INITIAL)
  age_p_m <- c("kid_m_p"=kid_m_prop,"sub_m_p"=sub_m_prop,"adu_m_p"=adu_m_prop)
  
  ## Immune Fraction ##
  # proportion initially in R state
  pR <- fix_age_data %>% filter(parameter=="pR") %>% pull(value) 
  
  ## Age Group (limits) ##
  wk2mnth <- 4.345 # weeks per month
  
  kid_max <- fix_age_data %>% filter(parameter=="kid_max") %>% pull(value) 
  kid_max_wks <- round(kid_max*wk2mnth)
  sub_max <- fix_age_data %>% filter(parameter=="sub_max") %>% pull(value) 
  sub_max_wks <- round(sub_max*wk2mnth)
  
  # if(SA == T){
  #   max_age_F <- var_input_set %>% pull(adu_f_max_yrs)*52
  #   max_age_M <- var_input_set %>% pull(adu_m_max_yrs)*52
  # }
  
  if(SA == T){
    max_age_F <- var_input_set["adu_f_max_yrs"]*52
    max_age_M <- var_input_set["adu_m_max_yrs"]*52
  }

  # 
  # if (SA) {
  #   if (is.data.frame(var_input_set) && !is.null(var_input_set)) {
  #     if ("adu_f_max_yrs" %in% colnames(var_input_set) && "adu_m_max_yrs" %in% colnames(var_input_set)) {
  #       max_age_F <- var_input_set["adu_f_max_yrs"] * 52
  #       max_age_M <- var_input_set %>% pull(adu_m_max_yrs) * 52
  #     } else {
  #       stop("Columns adu_f_max_yrs and adu_m_max_yrs not found in var_input_set")
  #     }
  #   } else {
  #     stop("var_input_set is not a valid data frame")
  #   }
  # }
  
  Kid <- 1:kid_max_wks # Kid: 1-6m (1-26w)
  Sub <- (kid_max_wks+1):sub_max_wks # Sub: 6-12m (26-52w)
  Adu_F <- (sub_max_wks+1):max_age_F # Adult F: 12m+
  Adu_M <- (sub_max_wks+1):max_age_M # Adult M: 12m+
  
  ## Maternal Immunity ##
  
  # - Bodjo et al (following ElArbi)
  # - waning of maternal immunity for first 4months (17 wk)
  # see imm_decay_corrected <- read_csv("data/imm_decay_bodjo_v2.csv") # "scripts/demographic-data/mat-imm-decay.R" for workings
  Imm_b <- imm_decay_corrected %>% 
    filter(wk ==0) %>% 
    pull(imm_corrected) # Imm_b = # proportion of young born to immune mothers that gain maternal antibodies
  
  # ---------------------------
  # ## VARIABLE PARAMETERS ## demographic rates
  
  ##### RSA params
  off_1 <- 0
  ppr_mort_1 <- 0
  ppr_mort_2 <- 0
  # if(SA==T){
  #   off_F <- var_input_set %>% pull("NET_offtake_f")
  #   off_M <- var_input_set %>% pull("NET_offtake_m")
  #   mort_1 <- var_input_set %>% pull("mortality_y")
  #   mort_2 <- var_input_set %>% pull("mortality_a")
  #   # mort_end <- var_demo_data %>% filter(parameter=="mortality_end") %>% pull(value) # natural mortality rate for final age group (per week)
  #   birth_r <- var_input_set %>% pull("birth_rate")
  #   
  #   min_age_offtake <- var_input_set %>% pull("min_age_offtake")
  #   min_age_repro <- var_input_set %>% pull("min_age_repro")
  # }
  
  if(SA==T){
    off_F <- var_input_set["NET_offtake_f"]
    off_M <- var_input_set["NET_offtake_m"]
    off_M2 <- var_input_set["NET_offtake_m2"]
    mort_1 <- var_input_set["mortality_y"]
    mort_2 <- var_input_set["mortality_a"]
    # mort_end <- var_demo_data %>% filter(parameter=="mortality_end") %>% pull(value) # natural mortality rate for final age group (per week)
    birth_r <- var_input_set["birth_rate"]
    
    min_age_offtake <- var_input_set["min_age_offtake"]
    min_age_repro <- var_input_set["min_age_repro"]
  }
  
  
  # convert dynamics to weekly rates:
  if(rates == "yrly"){
    off_1 <- 1-((1-off_1)^(1/52))
    off_F <- 1-((1-off_F)^(1/52))
    off_M <- 1-((1-off_M)^(1/52))
    off_M2 <- 1-((1-off_M2)^(1/52))
    mort_1 <- 1-((1-mort_1)^(1/52))
    mort_2 <- 1-((1-mort_2)^(1/52))
    birth_r <- birth_r / 52
  }

  min_offtake_wks <- round(min_age_offtake*wk2mnth)
  max_M2off_wks <- round(24*wk2mnth) # most male offtake before 12-18months 
  min_repro_wks <- round(min_age_repro*wk2mnth)
  
 
  
  #####################################
  
  demographic_pars <- data.frame(
    age_weeks = 1:max_age_F) %>% # nrow = age in weeks
    mutate(age_cat = ifelse(age_weeks <= kid_max_wks, "Kid",
                            ifelse(age_weeks <= sub_max_wks, "Sub", "Adu"))) %>%
    # fill in maternal immunity
    left_join(imm_decay_corrected %>% select(wk, "imm" = imm_corrected), c("age_weeks" = "wk")) %>%
    
    mutate(imm = ifelse(is.na(imm),0,imm)) %>%
    ## Join Demographics Data >>>
    mutate(net_off_F = ifelse(age_weeks<min_offtake_wks, off_1, off_F),
           net_off_M = ifelse(age_weeks<min_offtake_wks, off_1,
                              ifelse(age_weeks<max_M2off_wks, off_M2, off_M)), # add additional offtake rate for males <2y 
           # mort_F = ifelse(age_weeks<=kid_max, mort_1, mort_2),
           # mort_F = ifelse(age_weeks == max_age_F, 1, mort_F),
           mort_F = ifelse(age_weeks<=kid_max_wks, mort_1, 
                           ifelse(age_weeks == max_age_F, 1, mort_2)),
           mort_M = ifelse(age_weeks<=kid_max_wks, mort_1, 
                           ifelse(age_weeks == max_age_M, 1, mort_2)),
           ppr_mort = ifelse(age_weeks<=kid_max_wks, ppr_mort_1,ppr_mort_2), # ppr_mortality
           birth = ifelse(age_weeks<min_repro_wks,0,birth_r), # birth rate (only Adu_F|)
           
           age_p_F = ifelse(age_cat=="Kid", kid_f_prop,
                            ifelse(age_cat=="Sub", sub_f_prop, adu_f_prop)),
           age_p_M = ifelse(age_cat=="Kid", kid_m_prop,
                            ifelse(age_cat=="Sub", sub_m_prop, 
                                   ifelse(age_cat =="Adu" & age_weeks<=max_age_M, adu_m_prop,0))),
           n_weeks_F = ifelse(age_cat=="Kid", kid_max_wks,
                              ifelse(age_cat=="Sub", sub_max_wks - kid_max_wks, 
                                     ifelse(age_cat =="Adu" & age_weeks<=max_age_F, max_age_F - sub_max_wks,0))),
           n_weeks_M = ifelse(age_cat=="Kid", kid_max_wks,
                              ifelse(age_cat=="Sub", sub_max_wks - kid_max_wks, 
                                     ifelse(age_cat =="Adu" & age_weeks<=max_age_M, max_age_M - sub_max_wks,0)))
    ) %>%
    
    # divide initial population between age groups according to proportions in age_p_F
    mutate(pop_init_F = (age_p_F*N_tot)/n_weeks_F,
           pop_init_M = (age_p_M*N_tot)/n_weeks_M, 
           pop_init_M = ifelse(is.na(pop_init_M), 0, pop_init_M)) %>%
    # select relevant variables   
    select(age_weeks,
           age_cat,
           imm,
           birth,
           mort_F,
           mort_M,
           ppr_mort,
           net_off_F,
           net_off_M,
           pop_init_F,
           pop_init_M
    )
  
  
  ################################
  ## INITIAL POPULATION STATES ##
  ################################
  
  # pR = proportion initially immune (due to prior infection)
  # define SEIR vectors for male and female age groups
  # add input param for pIm, pS, pE, pI, pR at t=0 - 27/04/23
  
  fIm_init <- rep(0,max_age_F); mIm_init <- rep(0,max_age_F)
  # fIm_init <- demographic_pars %>% pull(pop_init_F)*pIm
  # mIm_init <- demographic_pars %>% pull(pop_init_M)*pIm
  fS_init <- demographic_pars %>% pull(pop_init_F) *(1-pR) # all S except already recovered
  mS_init <- demographic_pars %>% pull(pop_init_M) *(1-pR) # all S except already recovered
  fE_init <- rep(0,max_age_F); mE_init <- rep(0,max_age_F)
  # fE_init <- demographic_pars %>% pull(pop_init_F)*pE
  # mE_init <- demographic_pars %>% pull(pop_init_M)*pE
  fI_init <- rep(0,max_age_F); mI_init <- rep(0,max_age_F)
  # fI_init <- demographic_pars %>% pull(pop_init_F)*pI 
  # mI_init <- demographic_pars %>% pull(pop_init_M)*pI
  fR_init <- demographic_pars %>% pull(pop_init_F)*pR
  mR_init <- demographic_pars %>% pull(pop_init_M)*pR
  
  f_list <- list("fIm"=fIm_init,
                 "fS"=fS_init,
                 "fE"=fE_init,
                 "fI"=fI_init,
                 "fR"=fR_init)
  m_list <- list("mIm"=mIm_init,
                 "mS"=mS_init,
                 "mE"=mE_init,
                 "mI"=mI_init,
                 "mR"=mR_init)
  
  
  
  
  if (clean_environment == T) {
    rm(fix_age_data,kid_f_prop,sub_f_prop,adu_f_prop,kid_m_prop,sub_m_prop,adu_m_prop,kid_max,sub_max, max_age_F,max_age_M,fIm_init,fS_init,fE_init,fI_init,fR_init,mIm_init,mS_init,mE_init,mI_init,mR_init,off_1,off_F,off_M,mort_1,mort_2,birth_r,ppr_mort_1,ppr_mort_2)
  }
  
  # ---------------------------------
  ## TRANSMISSION PARAMETERS:
  if(transmission == T){
    source(here("scripts", "parameters", "set-pars-transmission-fixed.R"))
  }
  
  # -----------------------
  ## SUMMARY STATS ##
  
  # f_list/m_list is initial state for Females and Males, output defines what stats are provided in output, sumamry_df is a df to keep output in.
  summary_df <- summary_demos(w = 1, f_list, m_list, output, summary_df, Kid, Sub, Adu_F)
  
  output_df <- dynmod_func(
    f_list, # initial state of female population
    m_list, # initial state of male population
    TimeStop_dynamics, # 1 year, weekly timestep for demographic component
    TimeStop_transmission, # 1 day, hourly timestep for transission component
    output, # model output: tracker or summary stats
    demographic_pars, # df containing demographic rates (birth rate, mortality etc) for every week-long age group
    summary_df, #
    Imm_b,
    Kid,
    Sub,
    Adu_F
  )
  
  # output_df contains system dynamics from week 1 - TimeStop_dynamics
  pR_noIm_temp <- output_df %>% pull(pR_noIm)
  agesex_dynamics <- output_df # %>% mutate(set = i)

  ## output is pop growth from t=0 to end
  
  if(output %in% c("summary", "summary_all")){
    res <- output_df %>%
      filter(w==TimeStop_dynamics) %>%
      mutate(pop_growth = output_df[nrow(output_df),"sum_pop"] / output_df[1,"sum_pop"],
             tenyr_growth = (output_df[t1, "sum_pop"]) / output_df[t2, "sum_pop"],
             tenyr_anngrowth = (output_df[t1, "sum_pop"] - (output_df[t2, "sum_pop"]))/10
             )
  }
  
  # if(dynamics == T){
  #  res <- output_df
  # }
  # 
  
  return(res)
  
  
  # below not required as output for RSA - only pop growth & age-sex structure in first instance.
  
  # # 
  # if(dynamics == T){
  #   pop_dynamics <- pop_dynamics %>% rbind(agesex_dynamics) %>% as.data.frame()
  # }
  
  # RSAoutput <- RSAoutput %>%
  #   rbind(res)
  
  # if(turnover == T){
  #   pR_noIm_df <- pR_noIm_df %>%
  #     rbind(pR_noIm_temp)
  # }
  

}

