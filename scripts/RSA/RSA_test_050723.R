# Export necessary objects, including 'fix_age_data_full'
# export_vars <- c("imm_decay_corrected", "var_input_set_full", "fix_age_data_full", "fixdata", "f_list",
#                  "m_list", "TimeStop_dynamics", "TimeStop_transmission", "output",
#                  "summary_df", "clean_environment")

cores=detectCores()
cl <- makeCluster(cores[1]-1) # for running locally
registerDoParallel(cl)
help <- c()

RSAout <- foreach (i = 1:nrow(var_input_backup), 
           .combine = "rbind") %dopar% {
             
             var_input_full <- var_input_backup[i,]

             
             RSA_func(
               
               imm_decay_corrected,
               var_input_full,
               # var_input_backup[2,],
               fix_age_data_full,
               f_list, # initial state of female population
               m_list, # initial state of male population
               TimeStop_dynamics, # 1 year, weekly timestep for demographic component
               TimeStop_transmission, # 1 day, hourly timestep for transission component
               output, # model output: tracker or summary stats
               summary_df, #
               clean_environment
             )
           }
stopCluster(cl)




outout <- c()

for(i in 1:nrow(var_input_backup)){
  var_input_full <- var_input_backup[i,]
  
  #var_input_full <- var_input_backup[3,]
  
  RSAout <- RSA_func(
    
    imm_decay_corrected,
    var_input_full,
    # var_input_full = var_input_backup[i,]
    fix_age_data_full,
    f_list, # initial state of female population
    m_list, # initial state of male population
    TimeStop_dynamics, # 1 year, weekly timestep for demographic component
    TimeStop_transmission, # 1 day, hourly timestep for transission component
    output, # model output: tracker or summary stats
    summary_df, #
    clean_environment
  )
  
  outout <- outout %>%
    rbind(RSAout)
}

mat <- matrix(1:20, nrow=4)
RSAout <- 
  foreach (z = 1:nrow(mat), .combine = "rbind") %dopar% {
   sum(mat[z,])
  }

