App_func <- function(
    imm_decay_corrected, # immunity decay rate
    var_input_full, # demographics
    fix_age_data_full, # state vars
    f_list, # initial state of female population
    m_list, # initial state of male population
    TimeStop_dynamics, # 1 year, weekly timestep for demographic component
    TimeStop_transmission, # 1 day, hourly timestep for transission component
    output, # model output: tracker or summary stats
    summary_df, # empty output dataframe
    clean_environment, # cleaning tool
    Vstart, # time of vaccination
    Vprog,
    fixdata,
    vardata
){
  
  #########################
  ## 1. PARAMETERISATION ##
  #########################
  # fixdata <- "cgiar.shp"
  # update state vars and demographics data 
  var_input_set <- var_input_full
  fix_age_data <- fix_age_data_full %>% 
    select(parameter, all_of(`fixdata`))
  
  if(lhs & class(var_input_full) == "numeric"){
    var_input_set <- var_input_full %>%
      as.data.frame() %>%
      rownames_to_column(var = "parameter") %>%
      rename("value"=".")
    names(var_input_set) <- c("parameter", fixdata)
  }
  if(applied_example & class(var_input_full) == "numeric"){
    var_input_set <- var_input_full %>%
      as.data.frame() %>%
      rownames_to_column(var = "parameter") %>%
      rename("value"=".")
    names(var_input_set) <- c("parameter", fixdata)
  }
  
  
  ###
  # -------------------
  ## State Variables ##
  # -------------------
  
  ## Flock Size ##
  N_tot <- fix_age_data %>% filter(parameter=="N_tot") %>% pull(`fixdata`) # number of animals in population (flock or village)?
  
  ## Flock Structure ##
  # set proportion of male/female: kid (<6m), sub (<12m), adu (12m+)
  
  kid_f_prop <- fix_age_data %>% filter(parameter=="kid_f_prop") %>% pull(`fixdata`) 
  sub_f_prop <- fix_age_data %>% filter(parameter=="sub_f_prop") %>% pull(`fixdata`) 
  adu_f_prop <- fix_age_data %>% filter(parameter=="adu_f_prop") %>% pull(`fixdata`) 
  
  kid_m_prop <- fix_age_data %>% filter(parameter=="kid_m_prop") %>% pull(`fixdata`) 
  sub_m_prop <- fix_age_data %>% filter(parameter=="sub_m_prop") %>% pull(`fixdata`) 
  adu_m_prop <- fix_age_data %>% filter(parameter=="adu_m_prop") %>% pull(`fixdata`) 
  
  age_p_f <- c("kid_f_p"=kid_f_prop,"sub_f_p"=sub_f_prop,"adu_f_p"=adu_f_prop) # sex-age group proportions (INITIAL)
  age_p_m <- c("kid_m_p"=kid_m_prop,"sub_m_p"=sub_m_prop,"adu_m_p"=adu_m_prop)
  
  ## Immune Fraction ##
  pR <- fix_age_data %>% 
    filter(parameter=="pR") %>% 
    pull(`fixdata`)  # proportion initially in R state
  
  ## Age Group Limits ##
  
  wk2mnth <- 4.345 # weeks per month
  
  # kids
  kid_max <- fix_age_data %>% filter(parameter=="kid_max") %>% pull(`fixdata`) 
  kid_max_wks <- round(kid_max*wk2mnth)
  # sub-adults
  sub_max <- fix_age_data %>% filter(parameter=="sub_max") %>% pull(`fixdata`) 
  sub_max_wks <- round(sub_max*wk2mnth)
  # adults
  max_age_F <- var_input_set %>% filter(parameter == "adu_f_max_yrs") %>% pull(`vardata`) *52
  max_age_M <- var_input_set %>% filter(parameter == "adu_m_max_yrs") %>% pull(`vardata`) *52
  # weeks in age class
  Kid <- 1:kid_max_wks # Kid: 1-6m (1-26w)
  Sub <- (kid_max_wks+1):sub_max_wks # Sub: 6-12m (26-52w)
  Adu_F <- (sub_max_wks+1):max_age_F # Adult F: 12m+
  Adu_M <- (sub_max_wks+1):max_age_M # Adult M: 12m+
  
  ## Maternal Immunity ## (Bodjo et al) 
  # see imm_decay_corrected <- read_csv("data/imm_decay_bodjo_v2.csv") & "scripts/demographic-data/mat-imm-decay.R" for workings
  Imm_b <- imm_decay_corrected %>% 
    filter(wk ==0) %>% 
    pull(imm_corrected) # Imm_b = # proportion of young born to immune mothers that gain maternal antibodies
  
  # -------------------
  ## Demographics ##
  # -------------------
  
  ## ppr_mortality not incorporated
  ppr_mort_1 <- 0
  ppr_mort_2 <- 0
  
  ## set youth offtake to 0 unless included in demographics set
  if(!"NET_offtake_y" %in% var_input_set$parameter){
    off_1 <- 0
  }
  
  ## Pull Demographics ##
  
  if("NET_offtake_y" %in% var_input_set$parameter){
    off_1 <- var_input_set %>% filter(parameter == "NET_offtake_y") %>% pull(`vardata`)
  }
  off_F <- var_input_set %>% filter(parameter == "NET_offtake_f") %>% pull(`vardata`)
  off_M <- var_input_set  %>% filter(parameter == "NET_offtake_m") %>% pull(`vardata`)
  off_M2 <- var_input_set  %>% filter(parameter == "NET_offtake_m2") %>% pull(`vardata`)
  mort_1 <- var_input_set  %>% filter(parameter == "mortality_y") %>% pull(`vardata`)
  mort_2 <- var_input_set  %>% filter(parameter == "mortality_a") %>% pull(`vardata`)
  # mort_end <- var_demo_data %>% filter(parameter=="mortality_end") %>% pull(value) # natural mortality rate for final age group (per week)
  birth_r <- var_input_set  %>% filter(parameter == "birth_rate")  %>% pull(`vardata`)
  
  min_age_offtake <- var_input_set  %>% filter(parameter == "min_age_offtake") %>% pull(`vardata`)
  min_age_repro <- var_input_set  %>% filter(parameter == "min_age_repro") %>% pull(`vardata`)
  
  
  ## Convert to weekly rates ##
  # (if yearly):
  if(rates == "yrly"){
    off_1 <- 1-((1-off_1)^(1/52))
    off_F <- 1-((1-off_F)^(1/52))
    off_M <- 1-((1-off_M)^(1/52))
    off_M2 <- 1-((1-off_M2)^(1/52))
    mort_1 <- 1-((1-mort_1)^(1/52))
    mort_2 <- 1-((1-mort_2)^(1/52))
    birth_r <- birth_r / 52
  }
  
  #(if fortnightly:
  if(rates == "2wkly"){
    off_1 <- 1-((1-off_1)^(1/2))
    off_F <- 1-((1-off_F)^(1/2))
    off_M <- 1-((1-off_M)^(1/2))
    off_M2 <- 1-((1-off_M2)^(1/2))
    mort_1 <- 1-((1-mort_1)^(1/2))
    mort_2 <- 1-((1-mort_2)^(1/2))
    birth_r <- birth_r / 2
  } 
  
  # Convert ages to wks
  min_offtake_wks <- round(min_age_offtake*wk2mnth)
  max_M2off_wks <- round(24*wk2mnth) # most male offtake before 12-18months 
  min_repro_wks <- round(min_age_repro*wk2mnth)
  
  
  ## Construct Demographics DF ##
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
  
  #########################
  ## 2. SIMULATION SETUP ##
  #########################
  
  #--------------------------
  ## Initialise Population ##
  #--------------------------
  
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
  
  
  ## Clean Environment
  
  if (clean_environment == T) {
    rm(fix_age_data,kid_f_prop,sub_f_prop,adu_f_prop,kid_m_prop,sub_m_prop,adu_m_prop,kid_max,sub_max, max_age_F,max_age_M,fIm_init,fS_init,fE_init,fI_init,fR_init,mIm_init,mS_init,mE_init,mI_init,mR_init,off_1,off_F,off_M,mort_1,mort_2,birth_r,ppr_mort_1,ppr_mort_2)
  }
  
  
  
  ###
  #--------------------------
  ## Transmission parameters ##
  #--------------------------
  
  # Only if transmission is incorporated
  if(transmission == T){
    source(here("scripts", "parameters", "set-pars-transmission-fixed.R"))
  }
  
  
  ###################
  ## 3. RUN DYNMOD ##
  ###################
  
  ## Update output "summary_df" ##
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
    Adu_F,
    Vstart,
    Vprog
  )
  
  # only works for summary all needs updating for summary (only)
  if(output %in% c("summary", "summary_all")){
    # res <- output_df %>%
    #   filter(w==TimeStop_dynamics) %>%
    #   mutate(pop_growth = output_df[nrow(output_df),"sum_pop"] / output_df[1,"sum_pop"],
    #          tenyr_growth = (output_df[t1, "sum_pop"]) / output_df[t2, "sum_pop"]
    #          )
    res <- GSA_output(output_df,Vstart)
  }
  
  if(output %in% c("dynamics", "pop_tracker")){
    res <- output_df
  }
  
  if(output %in% c("mort_only")){
    
    ######
    ## F Mortality
    ######
    
    # Female 0-12m Mortality
    outMort <- output_df %>% mutate(age = rep(1:(nrow(output_df)/TimeStop_dynamics), TimeStop_dynamics))
    F0init <- outMort %>% filter(w == 1, age == 1) %>% pull("fpop")
    F12end <- outMort %>% filter(w == 52, age == 52) %>% pull("fpop")
    annMortF012 <- 1 - F12end / F0init # annual mortality
    mortF012 <- annMortF012
    
    # Female 12m+ Mortality
    F12init <- outMort %>% filter(w == 53, age == 53) %>% pull("fpop")
    Fend <- outMort %>% filter(w == 104, age == 104) %>% pull("fpop")
    annMortF12end <- 1 - Fend / F12init # annual mortality
    mortF12end <- annMortF12end
    
    # Female 0-6m
    F6end <- outMort %>% filter(w == 26, age == 26) %>% pull("fpop")
    monMortF06 <- 1 - (F6end/F0init)
    annMortF06 <- 1- (1-monMortF06)^2
    mortF06 <- annMortF06
    
    # Female 6-12m
    F6init <- outMort %>% filter(w == 26, age == 26) %>% pull("fpop")
    monMortF612 <- 1 - (F12end/F6init)
    annMortF612 <- 1- (1-monMortF612)^2
    mortF612 <- annMortF612
    
    ######
    ## M Mortality
    ######
    
    # Male 0-12m Mortality
    M0init <- outMort %>% filter(w == 1, age == 1) %>% pull("mpop")
    M12end <- outMort %>% filter(w == 52, age == 52) %>% pull("mpop")
    annMortM012 <- 1 - M12end / M0init # annual mortality
    mortM012 <- annMortM012
    
    # Male 12m+ Mortality
    M12init <- outMort %>% filter(w == 53, age == 53) %>% pull("mpop")
    Mend <- outMort %>% filter(w == 104, age == 104) %>% pull("mpop")
    annMortM12end <- 1 - Mend / M12init # annual mortality
    mortM12end <- annMortM12end
    
    
    # Male 0-6m
    M6end <- outMort %>% filter(w == 26, age == 26) %>% pull("mpop")
    monMortM06 <- 1 - (M6end/M0init)
    annMortM06 <- 1- (1-monMortM06)^2
    mortM06 <- annMortM06
    
    # Male 6-12m
    M6init <- outMort %>% filter(w == 26, age == 26) %>% pull("mpop")
    monMortM612 <- 1 - (M12end/M6init)
    annMortM612 <- 1- (1-monMortM612)^2
    mortM612 <- annMortM612
    
    res <- c("mortF012"=mortF012, "mortF12end"=mortF12end, "mortM012"=mortM012, "mortM12end"=mortM12end,
             "mortF06"=mortF06, "mortF612"=mortF612, "mortM06"=mortM06, "mortM612"=mortM612)
    
  }
  
  if(output %in% c("off_only")){
    
    ######
    ## F Offtake
    ######
    
    # Female 0-12m Offtake
    outOff <- output_df %>% mutate(age = rep(1:(nrow(output_df)/TimeStop_dynamics), TimeStop_dynamics))
    F0init <- outOff %>% filter(w == 1, age == 1) %>% pull("fpop")
    F12end <- outOff %>% filter(w == 52, age == 52) %>% pull("fpop")
    annOffF012 <- 1 - F12end / F0init # annual offtake
    offF012 <- annOffF012
    
    # Female 12m+ Offtake
    F12init <- outOff %>% filter(w == 53, age == 53) %>% pull("fpop")
    Fend <- outOff %>% filter(w == 104, age == 104) %>% pull("fpop")
    annOffF12end <- 1 - Fend / F12init # annual offtake
    offF12end <- annOffF12end
    
    # Female 0-6m
    F6end <- outOff %>% filter(w == 26, age == 26) %>% pull("fpop")
    monOffF06 <- 1 - (F6end/F0init)
    annOffF06 <- 1- (1-monOffF06)^2
    offF06 <- annOffF06
    
    # Female 6-12m
    F6init <- outOff %>% filter(w == 26, age == 26) %>% pull("fpop")
    monOffF612 <- 1 - (F12end/F6init)
    annOffF612 <- 1- (1-monOffF612)^2
    offF612 <- annOffF612
    
    ######
    ## M offtake
    ######
    
    # Male 0-12m offtake
    M0init <- outOff %>% filter(w == 1, age == 1) %>% pull("mpop")
    M12end <- outOff %>% filter(w == 52, age == 52) %>% pull("mpop")
    annOffM012 <- 1 - M12end / M0init # annual offtake
    offM012 <- annOffM012
    
    # Male 12m+ offtake
    M12init <- outOff %>% filter(w == 53, age == 53) %>% pull("mpop")
    Mend <- outOff %>% filter(w == 104, age == 104) %>% pull("mpop")
    annOffM12end <- 1 - Mend / M12init # annual offtake
    offM12end <- annOffM12end
    
    # Male 0-6m
    M6end <- outOff %>% filter(w == 26, age == 26) %>% pull("mpop")
    monOffM06 <- 1 - (M6end/M0init)
    annOffM06 <- 1- (1-monOffM06)^2
    offM06 <- annOffM06
    
    # Male 6-12m
    M6init <- outOff %>% filter(w == 26, age == 26) %>% pull("mpop")
    monOffM612 <- 1 - (M12end/M6init)
    annOffM612 <- 1- (1-monOffM612)^2
    offM612 <- annOffM612
    
    offinit <- outOff %>% filter(w == 1, age == 1) %>% pull("sum_pop")
    offend <- outOff %>% filter(w == 52, age == 52) %>% pull("sum_pop")
    offAll <- 1-(offend / offinit)
    
    res <- c("offF012"=offF012, "offF12end"=offF12end, "offM012"=offM012, "offM12end"=offM12end,
             "offF06"=offF06, "offF612"=offF612, "offM06"=offM06, "offM612"=offM612, "offAll"=offAll)
    
  }
  
  return(res)
  
}

