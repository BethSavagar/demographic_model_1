# Identify whether flock profiles overlap with behavioural parameter sets from pastoral-flocks SA
getwd()

# Load profiles parameter ranges and original parameter ranges
profile_ranges <- read.csv("data/Applied_parameters/demographics-lhs.csv")
profile_vals <- read.csv("data/Applied_parameters/demographics.csv")
GSA_ranges <- read.csv("data/GSA_parameters/GSA_var_input.csv")

# Profiles:
# Select parameters for comparison

profile_ranges_comparison <- profile_ranges %>%
  filter( parameter %in% c(
    
    # "NET_offtake_y", # Set to 0 for all pastoral-SA
    "NET_offtake_m",
    "NET_offtake_f",
    "mortality_y",
    "mortality_a",
    # "mortality_end", # Set to 1 for all pastoral-SA
    "birth_rate",
    # "ppr_mortality_y", # Set to 0 for all pastoral-SA
    # "ppr_mortality_a", # Set to 0 for all pastoral-SA
    # "adu_f_max_yrs", # Not important?
    # "adu_m_max_yrs", # Not important?
    "min_age_offtake",
    "min_age_repro",
    "NET_offtake_m2"
  )
    
  )



profile_vals_comparison <- profile_vals %>%
  filter( parameter %in% c(
    
    # "NET_offtake_y", # Set to 0 for all pastoral-SA
    "NET_offtake_m",
    "NET_offtake_f",
    "mortality_y",
    "mortality_a",
    # "mortality_end", # Set to 1 for all pastoral-SA
    "birth_rate",
    # "ppr_mortality_y", # Set to 0 for all pastoral-SA
    # "ppr_mortality_a", # Set to 0 for all pastoral-SA
    # "adu_f_max_yrs", # Not important?
    # "adu_m_max_yrs", # Not important?
    "min_age_offtake",
    "min_age_repro",
    "NET_offtake_m2"
  )
  
  )



# Load complete df of valid parameters from RSA nad GSA
validpars_GSA <- read_csv("data/GSA_parameters/GSA_valid-pars-only.csv", col_names = T)
validpars_RSA <- read_csv("data/GSA_parameters/GSA_var_input-PRCC.csv", col_names=T)
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

validpars_ALLt <- as.data.frame(t(validpars_ALL))


## CGIAR PROFILE

Cgiarmin <- profile_ranges_comparison %>% select(cgiar.goat.min) %>% pull
Cgiarmax <- profile_ranges_comparison %>% select(cgiar.goat.max) %>% pull

test <- validpars_ALLt %>% select("V1":"V10")

pars_comparison <- validpars_ALLt>=Cgiarmin & validpars_ALLt<=Cgiarmax


profile_vals_comparison <- profile_vals_comparison %>% 
  t() %>% 
  as.data.frame() %>%
  rownames_to_column(var = "profile") %>%
  slice(-1)

validpars_ALL <- validpars_ALL %>% mutate(profile = "valid") %>% select(profile, everything())
colnames(profile_vals_comparison) <- colnames(validpars_ALL)

validpars_profilevals <- rbind(validpars_ALL %>% mutate(profile = "valid"), 
                               profile_vals_comparison) %>%
  as_tibble() %>% mutate(off_mA = as.numeric(off_mA),
                         off_f = as.numeric(off_f),           
                         mort_Y = as.numeric(mort_Y),
                         mort_A = as.numeric(mort_A),
                         birth_rate = as.numeric(birth_rate),
                         min_off = as.numeric(min_off),
                         min_repro = as.numeric(min_repro),
                         off_mY = as.numeric(off_mY))

validpars_profilevals <- validpars_profilevals %>% 
  mutate(profile2 = ifelse(profile == "valid", "valid","other")) %>% 
  select(profile2, everything()) %>%
  filter(profile != "lesnoff.T") # exclude because lesnoff data is in fortnightly rates

ggpairs(validpars_profilevals,
       columns = 3:9,
       aes(color = as.factor(profile2),
           alpha = 0.2))



# Load profiles parameter ranges and original parameter ranges
profiles_lhs <- read_csv("output/applied_pars_lhs_cgiarshp-2023-10-10.csv")

# Profiles:
# Select parameters for comparison

profile_lhs_comparison <- profiles_lhs %>%
  select(c(
    
    # "NET_offtake_y", # Set to 0 for all pastoral-SA
    "NET_offtake_m",
    "NET_offtake_f",
    "mortality_y",
    "mortality_a",
    # "mortality_end", # Set to 1 for all pastoral-SA
    "birth_rate",
    # "ppr_mortality_y", # Set to 0 for all pastoral-SA
    # "ppr_mortality_a", # Set to 0 for all pastoral-SA
    # "adu_f_max_yrs", # Not important?
    # "adu_m_max_yrs", # Not important?
    "min_age_offtake",
    "min_age_repro",
    "NET_offtake_m2"
  )
  ) %>% mutate(profile = "cgiarshp") %>%
  select(profile, everything())

colnames(profile_lhs_comparison) <- colnames(validpars_ALL)

validpars_cgiar <- rbind(validpars_ALL, 
                         profile_lhs_comparison) %>%
  as_tibble() %>% mutate(off_mA = as.numeric(off_mA),
                         off_f = as.numeric(off_f),           
                         mort_Y = as.numeric(mort_Y),
                         mort_A = as.numeric(mort_A),
                         birth_rate = as.numeric(birth_rate),
                         min_off = as.numeric(min_off),
                         min_repro = as.numeric(min_repro),
                         off_mY = as.numeric(off_mY))
ggpairs(validpars_cgiar,
        columns = 2:9,
        aes(color = as.factor(profile),
            alpha = 0.2))





# 
# # # select minimums & maximums from profiles
# profile_mins <- profile_ranges %>%
#   select(ends_with(".min")) 
# 
# profile_maxs <- profile_ranges %>%
#   select(ends_with(".max"))
# 
# # profile names
# profiles <- profile_ranges %>%
# select(ends_with("min"), ends_with("max")) %>%
#   colnames() %>%
#   gsub(".max","",.) %>%
#   gsub(".min","",.) %>% 
#   unique()
# 
# # store T or F depending on whether profile parameters are within original parameter range
# tracker <- profile_ranges
# 
# profile_mins>=GSA_ranges$min.1 
# profile_maxs<=GSA_ranges$max.1
# 
# 
# tracker.min <- apply(profile_mins, 2, function(x)
#   x <- ifelse(x>=GSA_ranges$min.1, 1, 0)
# )
# 
# 
# tracker.max <- apply(profile_maxs, 2, function(x)
#   x <- ifelse(x<=GSA_ranges$max.1, 1, 0)
# )
# 
# 
# tracker <- cbind(tracker.min, tracker.max)





