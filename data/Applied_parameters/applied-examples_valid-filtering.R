# Load profiles parameter ranges and original parameter ranges
profile_ranges <- read.csv("data/Applied_parameters/demographics-filter.csv")
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
validpars_ALLt <- as.data.frame(t(validpars_ALL)) 

# Vactor of profiles:
profs <- profile_ranges %>% filter(!profile == "lesnoff.T") %>% distinct(profile) %>% pull() 
validrow2 <- c()
anyvalid <- list()

for (i in 1:length(profs)){
  
  newp <- profs[i]
  
  # filter profile df to parameters of interest
  profv1 <- profile_ranges %>% 
    filter(profile == newp) %>% 
    # Filter parameters of interest:
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
  
  # Filter profile dataframe to contain only reported parameters
  profv2 <- profv1 %>% filter(reported == "reported")
  
  rownames(validpars_ALLt) <- profv1 %>% pull(parameter)
  
  # Subset RSA/GSA dataframe to contain only pars for filtering profile comparison
  validpars.filter <- validpars_ALLt %>% 
    rownames_to_column(var="parameter") %>%
    filter(parameter %in% profv2$parameter)
  
  # Transform RSA/GSA dataframe for comparison
  validpars.compare <- t(validpars.filter) %>%
    as.data.frame() %>%
    slice(-1) %>%
    mutate_all(function(x) as.numeric(as.character(x)))
  colnames(validpars.compare) <- profv2$parameter
  
  # Define par.mins and par.maxs as vectors with the min and max values for each parameter
  par.mins <- profv2$cgiar.shp.min
  par.maxs <- profv2$cgiar.shp.max
  
  # filter the valid (GSA.RSA) parameter dataframe by each demographic parameter in turn
  # return a dataframe with any valid parameter sets that satisfy all filtering conditions
  
  for(j in 1:ncol(validpars.compare)){
    
    if(j == 1){
      validpars.old <- validpars.compare
    }
    # update validpars df based on valid results of prior filtering round.
    validpars.new <- validpars.old %>%
      # colnames(test.T)[1] >= par.mins[1] 
      filter(validpars.old[,j] >= par.mins[j] & validpars.old[,j] <= par.maxs[j])
    
    validrow1 <- nrow(validpars.new)
    validrow2 <- c(validrow2, validrow1)
    validpars.old <- validpars.new
    
    # any valid is list containing dataframe of valid parameter sets for each demographic profile
    anyvalid[[i]] <- validpars.old
  }
  
}





# Testing filtering method: 
test.filtered <- test.T %>%
  # colnames(test.T)[1] >= par.mins[1] 
  filter(
         mortality_y >= par.mins[1] & mortality_y <= par.maxs[1],
         mortality_a >= par.mins[2] & mortality_a <= par.maxs[2],
         birth_rate >= par.mins[3] & birth_rate <= par.maxs[3],
         min_age_repro >= par.mins[4] & min_age_repro <= par.maxs[4]
         )
  