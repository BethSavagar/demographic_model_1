# Check age-sex structure dynamics based on "valid_parameters" from 22062023 & RSA 2.2

# Run RSA_analysis_2.2 to generate tenyr_pars df
# set var_input_set to tenyr_pars df and use this with RSA_test2.Rmd script
# in the script set dynamics to TRUE, set turnover to FALSE
# dataframe pop_dynamics will contain full dynamics for 92 set which had 10yr growth between 0.95-1.05 using the 22062023 parameter set...
# for 6 parameters with pop growth within 1.05-0.95 in last 10 years see valid_as_pars_df in RSA_analysis_2.2

# CONVERT DATAFRAME TO LONG FORMAT

dynamics_long <- pop_dynamics %>%
  select(-c(prop_immune, pR_noIm, prop_inf)) %>%
           gather(key="par", value = "val", -c(set,w))

# subset <- sample(1:nrow(var_input_set), 10)

dynamics_subset <- dynamics_long %>% filter(set %in% subset)

# CHECK OVERALL POPULATION DYNAMICS 

ggplot(dynamics_long %>% filter(par=="sum_pop"), aes(x=w,y=val,group=set,col=factor(set)))+
  geom_line()

# check population growth :
dynamics_long %>% filter(par=="sum_pop", w %in% c(t1,t2)) %>% arrange(set)

# CHECK AGE-SEX DYNAMICS

ggplot(dynamics_subset %>% filter(!par%in% c("sum_pop", "pF")), 
       aes(x=w,y=val,group=set,col=factor(set)))+
  geom_line()+
  facet_wrap(~par, scales = "free")


ggplot(dynamics_long %>% filter(!par%in% c("sum_pop", "pF")), 
       aes(x=w,y=val,group=set,col=factor(set)))+
  geom_line()+
  facet_wrap(~par, scales = "free")
