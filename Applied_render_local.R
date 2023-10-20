###############################################################################################################
## Flock Dynamics - Applied Profile ~ Baseline/Basic Example
###############################################################################################################
# Script rendering applied-profiles examples
# Author: Beth Savagar
# Date: 19.10.23

############
## SETUP ##
############

filepath <- "/Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/3. Population_Dynamics_Model/pop-dynamics_April2020/demographic_model_1/"


## Libraries
source(paste0(filepath,"scripts/setup.R")) 

## Functions:
source(paste0(filepath, "functions/Appliedfunc.R")) # wrapper function
source(paste0(filepath, "functions/demos_summary.R")) # summary stats for output
source(paste0(filepath, "functions/dynmod-vac.R")) # flock dynamics model function
source(paste0(filepath, "functions/output.R")) # what to include in output df
source(paste0(filepath, "functions/GSA_outputs.R")) # summary stats accounting for vac

## Load parameters
applied_example <- T
source(paste0(filepath, "scripts/applied/applied_load_data.R")) 

## Dataset
datasets <- fix_age_data_full %>% select(-parameter) %>% colnames()
dataset <- datasets[1]
data_filename <- gsub("\\.","",dataset)

## Set filenames:

tdate <- Sys.Date()
filename1 <- paste0("applied_output_eg_", data_filename, "-", tdate, ".csv")
filename1.rds <- paste0("applied_output_eg_", data_filename, "-", tdate, ".RData")
filename2 <- paste0("applied_pars_eg_", data_filename, "-",tdate, ".csv")
filename3 <- paste0("applied_outputSummary_eg_", data_filename, "-",tdate, ".csv")



######################
## MODEL SETUP ##
######################

## Simulation:
TimeStop_dynamics <- 10*52 # 10 years (based on earlier simulations)
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
Vstart <- 5*52 # 20 years
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0

## Model Output
output <- "dynamics" # Output options: "summary" (stats with age-group prop), "summary_all" (stats with age-sex group prop), "dynamics" (pop metrics over time)


############################
## Demographic parameters ##
############################
lhs <- F


##################
## RUN MODEL ##
##################
GSAoutput <- c(); 
Out <- list()
# pR_noIm_df <- c(); 
# pop_dynamics <- c()

summary_df <- output_func(TimeStop_dynamics, output) # create empty dataframe to store output

turnover <- F; 
dynamics <- T; 
transmission <- F; 
clean_environment <- T

# timepoints for population growth
t2 <- 5*52 # 5 year pop growth
t1 <- TimeStop_dynamics

# Format demographic parameter sets:
var_input_backup <- var_demo_data_full %>% column_to_rownames("parameter") %>% t() %>% as.data.frame()
# fixdata <- dataset
# vardata <- dataset
# 
# if(dataset == "lesnoff.T"){
#   rates <- "wkly"
# }else{
#   rates <- "yrly"
# }


for(i in 1:length(datasets)){
  dataset <- datasets[i]
  data_filename <- gsub("\\.","",dataset)
  
  ## Set filenames:
  
  tdate <- Sys.Date()
  filename1 <- paste0("applied_output_eg_", data_filename, "-", tdate, ".csv")
  filename1.rds <- paste0("applied_output_eg_", data_filename, "-", tdate, ".RData")
  filename2 <- paste0("applied_pars_eg_", data_filename, "-",tdate, ".csv")
  filename3 <- paste0("applied_outputSummary_eg_", data_filename, "-",tdate, ".csv")
  
  fixdata <- dataset
  vardata <- dataset
  if(applied_example){
      vardata <- "value"
  }
  
  if(dataset == "lesnoff.T"){
    rates <- "wkly"
  }else{
    rates <- "yrly"
  }
  
  var_input_full <- unlist(var_input_backup[i,])
  
  Out[[i]] <- 
    App_func(
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
      clean_environment,
      Vstart,
      fixdata,
      vardata
    )
  
  
  
  
  
}


####################
## OUTPUT SUMMARY ##
####################

# Out %>% mutate(set = rep(1:nrow(var_input_backup), each = TimeStop_dynamics))

# Summary statistics: pop growth & immunity metrics
Out_summary <- sapply(Out, function(x)
  GSA_output(x,Vstart)
  ) %>%
  t() %>%
  as.data.frame() %>%
  mutate(prof = datasets, 
         fiveyr_growth = tenyr_growth)

dynplot <- do.call(rbind, Out) %>% mutate(prof = rep(datasets, each = TimeStop_dynamics))

##############
## PLOTTING ##
##############

dynplot <- dynplot %>%
  mutate(prof = factor(
  prof,
  levels = c(
    "cgiar.goat",
    "cgiar.shp",
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


dynplot_i <- dynplot %>% filter(prof %in% c("oc.shp.aridP",
                                            "oc.shp.semiaridP",
                                            "oc.shp.semiaridM",
                                            "oc.shp.subhumidM",
                                            "oc.shp.humidM"))

## AGE STRUCTURE
age_struc <- dynplot_i %>%
  select(w, prof, starts_with("pf"), starts_with("pm")) %>%
  gather(key = "age_group", value="prop", -c(w,prof))

plot_age <- ggplot(age_struc, aes(x = w, y = prop,col = age_group,group = age_group)) + 
  geom_line() + 
  facet_wrap( ~ prof,nrow=1) +
  labs(x = "week", y = "population-propotion", title = "Age Structure, 10 years") +
  theme_bw()+
  theme(legend.position = "right")
  # theme(legend.position = "bottom") +
  # guides(color = guide_legend(nrow = 1))

plot_growth <- ggplot(dynplot_i %>% filter(w<500), aes(x=w, y=sum_pop/75))+
  geom_line()+
  facet_wrap(~prof,nrow=1)+
  labs(x="week", y="population-growth", title = "Population growth, 10 years")+
  theme_bw()

plot_growth_free <- ggplot(dynplot_i %>% filter(w<500), aes(x=w, y=sum_pop/75))+
  geom_line()+
  facet_wrap(~prof, scales = "free",nrow=1)+
  labs(x="week", y="population-growth", title = "Population growth, 10 years")+
  theme_bw()

plot_immune <- ggplot(dynplot_i, aes(x=w, y=prop_immune))+
  geom_line()+
  facet_wrap(~prof,nrow=1)+
  labs(x="week", y="proportion-immune", title = "Immune decay, 10 years")+
  coord_cartesian(xlim=c(0,500))+
  theme_bw()

ggarrange(plot_age, plot_growth, plot_immune, ncol=1)


########################
## PLOTTING SUMMARIES ##
########################

Out_summary_long <- Out_summary %>%
  select(imm_6m, imm_12m, imm70_w, fiveyr_growth, prof) %>%
  gather(Out_summary, value = "value", -prof) %>% 
  rename(stat = Out_summary) %>%
  mutate(stat = factor(stat, levels = c("fiveyr_growth", "imm_6m", "imm_12m", "imm70_w")))


ggplot(Out_summary_long, aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


my_colors <- RColorBrewer::brewer.pal(4, "Dark2")[1:4]

A <- ggplot(Out_summary_long %>% filter(stat %in% c("fiveyr_growth")), aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  scale_fill_manual(values = my_colors[1])+
  labs(x = "", y = "Population growth")

B <- ggplot(Out_summary_long %>% filter(stat %in% c("imm_6m", "imm_12m")), aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  coord_cartesian(ylim = c(0,0.9))+
  scale_fill_manual(values = my_colors[2:3])+
  labs(x = "", y = "Proportion immune")


C <- ggplot(Out_summary_long %>% filter(stat %in% c("imm70_w")), aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  scale_fill_manual(values = my_colors[4])+
  labs(x = "", y = "Weeks to 70%")

ggarrange(A,B,C, ncol = 1)
