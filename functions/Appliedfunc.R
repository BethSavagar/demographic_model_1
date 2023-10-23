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
    select(parameter, "value" = all_of(`fixdata`))
  
  if(lhs & class(var_input_full) == "numeric"){
    var_input_set <- var_input_full %>%
      as.data.frame() %>%
      rownames_to_column(var = "parameter") %>%
      rename("value"=".")
    }
  if(applied_example & class(var_input_full) == "numeric"){
    var_input_set <- var_input_full %>%
      as.data.frame() %>%
      rownames_to_column(var = "parameter") %>%
      rename("value"=".")
  }
  
  # -------------------
  ## State Variables ##
  # -------------------
  
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
  pR <- fix_age_data %>% 
    filter(parameter=="pR") %>% 
    pull(value)  # proportion initially in R state
  
  ## Age Group Limits ##
  
  wk2mnth <- 4.345 # weeks per month
  
  # kids
  kid_max <- fix_age_data %>% filter(parameter=="kid_max") %>% pull(value) 
  kid_max_wks <- round(kid_max*wk2mnth)
  # sub-adults
  sub_max <- fix_age_data %>% filter(parameter=="sub_max") %>% pull(value) 
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
  
  # resolve issues with pulling data from var_input_set:
  if(lhs){
    fixdata = "value"
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
    Vstart
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
  
  if(output == "dynamics"){
    res <- output_df
  }
  
  return(res)

}

