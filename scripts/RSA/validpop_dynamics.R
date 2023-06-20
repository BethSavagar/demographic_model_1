
if(!exists(set)){
  set <- validRSA2 %>% pull(set)
}

validpop <- c()
for(i in set){
  source("scripts/parameters/set-pars-RSA.R")
  # output is demographic pars dataframe (for sex-age-groupings) and starting population m_list, f_list
  
  
  # ---------------------------------
  ## TRANSMISSION PARAMETERS:
  transmission <- F # include transmission? T = Yes, F = No
  
  # transmission_pars <- "fixed"
  # transmission_filepath <- paste0("scripts/parameters/set-pars-transmission-",transmission_pars,".R")
  # source(transmission_filepath)
  source(here("scripts", "parameters", "set-pars-transmission-fixed.R"))
  #print(c(i,off_F ,off_M ,mort_1,mort_2,birth_r))
  
  ## Clean Parameters Environment? ##
  clean_environment <- T
  if (clean_environment == T) {
    rm(var_demo_data,fix_age_data,kid_f_prop,sub_f_prop,adu_f_prop,kid_m_prop,sub_m_prop,adu_m_prop,kid_max,sub_max, max_age_F,max_age_M,fIm_init,fS_init,fE_init,fI_init,fR_init,mIm_init,mS_init,mE_init,mI_init,mR_init,off_1,off_F,off_M,mort_1,mort_2,birth_r,ppr_mort_1,ppr_mort_2)
  }
  
  # -----------------------
  ## SUMMARY STATS ##
  
  # create summary data frame to store summary stats for each timestep
  summary_df <- output_func(TimeStop_dynamics, output)
  
  # nb need to add if statement to summary (if output == summary, if output == counts)
  summary_df <- summary_demos(w = 1, f_list, m_list, output, summary_df)
  
  output_df <- dynmod_func(
    f_list,# initial state of female population
    m_list,# initial state of male population
    TimeStop_dynamics,# 1 year, weekly timestep for demographic component
    TimeStop_transmission,# 1 day, hourly timestep for transission component
    output,# model output: tracker or summary stats
    demographic_pars,
    summary_df
  )
  
  output_df <- output_df %>% replace(is.na(.), 0)
  
  res <- output_df %>%
    pull(sum_pop)
  # plot(res)

  validpop <- validpop %>%
    rbind(res)

}

validpop_long <- validpop %>% 
  as.data.frame() %>%
  #rename_with(as.character(seq(1,520,1)), everything())
  mutate(set = set) %>% 
  gather(key="w", value = "pop", -set) %>%
  mutate(w = str_remove(w,"V"), 
         w = as.numeric(w))

# ggplot(validpop_long, aes(x=w, y=pop, col = as.factor(set)))+geom_line()+theme(legend.position = "none")

       