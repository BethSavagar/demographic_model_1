
filepath <- "/storage/users/bsavagar/dynmod/"

source(paste0(filepath,"scripts/setup.R"))
list.files(paste0(filepath,"functions"), full.names = TRUE) %>% map(source)
#
###############################################################################################################
# Set Up for Parallel Computing
# 
cores=detectCores()
cl <- makeCluster(30) #not to overload your computer
registerDoParallel(cl)
########
# File saving:
#
tdate <- Sys.Date()
filename <- paste0("RSA_output_", tdate, ".RData")

Output_df <-
  foreach (i = 1:nrow(pars_ls), .combine = "rbind") %dopar% {
    dynmod_func(
      f_list, # initial state of female population
      m_list, # initial state of male population
      TimeStop_dynamics, # 1 year, weekly timestep for demographic component
      TimeStop_transmission, # 1 day, hourly timestep for transission component
      output, # model output: tracker or summary stats
      demo_pars_ls[[i,]],
      summary_df
    )
    
  }

epi_List[[i]] <- Runs_tracker
print(i)
saveRDS(epi_List, paste0(filepath, "outputs/vaccine_overwrite_R.RData"))
}
)
tdate <- Sys.Date()
filename <- paste0("vaccination_output_Rand", tdate, ".RData")
# save RData
saveRDS(epi_List, paste0(filepath, "outputs/", filename))
# write.csv(Runs_tracker, "~/folder/runs_tracker.csv")
