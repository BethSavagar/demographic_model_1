Outlist <- readRDS(file = "output/Applied/applied_output_lhs_ALL-2023-10-26.RData")
# Outlist <- readRDS(file = "output/Applied/applied_output_lhs2_ALL-2023-10-27.RData")


Out_dynamics_df <- c()
for (i in 1:(length(datasets)-1)){
  data <- datasets[i]
  new <- Outlist[[i]] %>% select(pop_growth, fiveyr_growth = tenyr_growth) %>% gather(key = "param", value = value) %>% mutate(prof = data)
  Out_dynamics_df <- rbind(Out_dynamics_df,new)
}

Out_dynamics_df <- Out_dynamics_df %>%
  mutate(
    prof = factor(prof, 
                     levels = c(
                       "cgiar.shp",
                       "cgiar.goat",
                       "lesnoff.T",
                       "oc.goat.aridP",
                       "oc.goat.semiaridP",
                       "oc.goat.semiaridM",
                       "oc.goat.subhumidM",
                       "oc.goat.humidM",
                       "oc.shp.aridP",
                       "oc.shp.semiaridP",
                       "oc.shp.semiaridM",
                       "oc.shp.subhumidM",
                       "oc.shp.humidM"  
                     ))
  )

ggplot(Out_dynamics_df, aes(x = prof, y = value, col = prof)) + 
  geom_boxplot() +
  facet_wrap(~ param, ncol = 1, scales = "free") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

Out_agesex_df <- c()
for (i in 1:(length(datasets)-1)){
  data <- datasets[i]
  new <- Outlist[[i]] %>% select(starts_with("pf"), starts_with("pm")) %>% gather(key = "param", value = value) %>% mutate(prof = data)
  Out_agesex_df <- rbind(Out_agesex_df,new)
}

Out_agesex_df <- Out_agesex_df %>%
  mutate(
    prof = factor(prof, 
                  levels = c(
                    "cgiar.shp",
                    "cgiar.goat",
                    "lesnoff.T",
                    "oc.goat.aridP",
                    "oc.goat.semiaridP",
                    "oc.goat.semiaridM",
                    "oc.goat.subhumidM",
                    "oc.goat.humidM",
                    "oc.shp.aridP",
                    "oc.shp.semiaridP",
                    "oc.shp.semiaridM",
                    "oc.shp.subhumidM",
                    "oc.shp.humidM"  
                  ))
  )

ggplot(Out_agesex_df, aes(x = param, y = value, col = param)) + 
  geom_boxplot() +
  facet_wrap(~ prof, ncol = 3, scales = "free") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


####################
## PROFILE RANGES ##
####################

# Plots to show ranges for demographic parameters of Applied-profile-LHS compared to valid-GSA
# For some reason certain parameters are missing for certain profiles - unknown why but generally ok (27.10/23)

# Load profiles parameter ranges and original parameter ranges
profile_ranges <- read.csv("data/Applied_parameters/demographics-lhs2.csv")
# Load complete df of valid parameters from RSA nad GSA
validpars_GSA <- read_csv("data/GSA_parameters/GSA_valid-pars-only.csv", col_names = T)
validpars_RSA <- read_csv("data/GSA_parameters/GSA_var_input-PRCC.csv", col_names=T)

# Collate all valid parameters from GSA RSA
validpars_ALL <- rbind(validpars_GSA, validpars_RSA) %>%
  # select pars to compare
  select(
    "off_mA",
    "off_f",      
    "mort_Y",     
    "mort_A",     
    "birth_rate", 
    # "max_yrs_F",  
    # "max_yrs_M",  
    "min_off",
    "min_repro",
    "off_mY"
  )

validpars_ALLt <- as.data.frame(t(validpars_ALL)) %>% 
  rownames_to_column(var = "param") %>% 
  gather(key = set, value = value, -param)

profile_ranges_long <- profile_ranges %>% 
  gather(key = prof, value = value, -parameter) %>% 
  mutate(prof = gsub("\\.min|\\.max", "", prof)) %>%
  mutate(param = 
         case_when(parameter == "NET_offtake_m" ~ "off_mA",
                   parameter == "NET_offtake_f" ~ "off_f",      
                   parameter == "mortality_y" ~ "mort_Y",     
                   parameter == "mortality_a" ~ "mort_A",     
                   parameter == "min_age_offtake" ~ "min_off",
                   parameter == "min_age_repro" ~ "min_repro",
                   parameter == "NET_offtake_m2" ~ "off_mY"), 
         param = ifelse(is.na(param), parameter, param))

profile_GSA_compare <- rbind(
  profile_ranges_long %>% select(prof, value, param),
  validpars_ALLt %>% mutate(prof = "GSA") %>% select(prof,value,param)) %>%
  filter(param %in% c(
    "off_mY",
    "off_mA",
    "off_f",
    "mort_Y",
    "mort_A",
    "birth_rate", 
    "min_off", 
    "min_repro"
  )) %>%
  mutate(prof = factor(prof, 
                       levels = c(
    "GSA",
    "cgiar.shp",
    "cgiar.goat",
    "lesnoff.T",
    "oc.goat.aridP",
    "oc.goat.semiaridP",
    "oc.goat.semiaridM",
    "oc.goat.subhumidM",
    "oc.goat.humidM",
    "oc.shp.aridP",
    "oc.shp.semiaridP",
    "oc.shp.semiaridM",
    "oc.shp.subhumidM",
    "oc.shp.humidM"
  )
  ))
  


ggplot(data = profile_GSA_compare, aes(x=factor(prof),y=value, col = prof))+
  geom_line()+
  geom_point()+
  facet_wrap(~param, scales = "free")+ 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
