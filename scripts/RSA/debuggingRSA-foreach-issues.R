cores=detectCores()
cl <- makeCluster(cores[1]-1) # for running locally
registerDoParallel(cl)


test_vec <- foreach(i=4:1, .combine='c') %dopar% {
  #Sys.sleep(3 * i)
  i
}

stopCluster(cl)



###########################
###########################

# this code works... 

cores=detectCores()
cl <- makeCluster(cores[1]-1) # for running locally
registerDoParallel(cl)


new <- foreach (i = 1:10, .combine = "rbind") %dopar% {
                     var_input_backup[i,]
                   }
stopCluster(cl)
new == var_input_backup

cores=detectCores()
cl <- makeCluster(cores[1]-1) # for running locally
registerDoParallel(cl)


new <- foreach (i = 1:10, .combine = "rbind") %dopar% {
  h <- c(unlist(var_input_backup[i,]), "idx"=i)
  h
}
stopCluster(cl)



cores=detectCores()
cl <- makeCluster(cores[1]-1) # for running locally
registerDoParallel(cl)


new <- foreach (i = 1:10, .combine = "rbind") %dopar% {
  h <- c(unlist(var_input_backup[i,]), "idx"=i)
  # sum(h)
}
stopCluster(cl)


#####################
######################


cores=detectCores()
cl <- makeCluster(cores[1]-1) # for running locally
registerDoParallel(cl)

RSAout <- foreach (i = 1:nrow(var_input_backup), 
                   .packages = c("dplyr"),
           .combine = "rbind") %dopar% {
             
             # tried defining all objects so accessible in global environment and environment of each foreach loop
             # .GlobalEnv$imm_decay_corrected <- imm_decay_corrected
             # .GlobalEnv$var_input_backup <- var_input_backup
             # .GlobalEnv$fix_age_data_full <- fix_age_data_full
             # .GlobalEnv$f_list <- f_list # initial state of female population
             # .GlobalEnv$m_list <- m_list # initial state of male population
             # .GlobalEnv$TimeStop_dynamics <- TimeStop_dynamics # 1 year, weekly timestep for demographic component
             # .GlobalEnv$TimeStop_transmission <- TimeStop_transmission # 1 day, hourly timestep for transission component
             # .GlobalEnv$output <- output # model output: tracker or summary stats
             # .GlobalEnv$summary_df <- summary_df #
             # .GlobalEnv$clean_environment <- clean_environment
             # .GlobalEnv$fixdata <- fixdata
             # .GlobalEnv$SA <- SA
             
             var_input_full <- unlist(var_input_backup[i,])

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




