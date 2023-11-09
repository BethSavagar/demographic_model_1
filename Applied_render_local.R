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

# Select dataset:
data <- "" # "-lhs" for LHS, "" for demographics examples, 
demfile <- paste0("demographics", data, ".csv")
statefile <- paste0("state_vars", data, ".csv")

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

######################
## MODEL SETUP ##
######################

## Simulation:
TimeStop_dynamics <- 20*52 # 10 years (based on earlier simulations)
TimeStop_transmission <- 24 # 1 day, hourly timestep for transmission component
min_pop <- 1 # set minimum size of population, if pop drops below then set to 0

## Model Output
output <- "dynamics" # Output options: "summary" (stats with age-group prop), "summary_all" (stats with age-sex group prop), "dynamics" (pop metrics over time)


#######################
## Vaccination Setup ##
#######################
# see applied_vaccination
pVs <- seq(0.7,1,0.1)
pV <- 0.8 # within flock coverage of each campaign (0.7-1)
source("scripts/applied/applied_vaccination.R")
Vstart <- Vprog %>% filter(Vround==1) %>% pull(Vweek)


############################
## Demographic parameters ##
############################
lhs <- F


##################
## RUN MODEL ##
##################
GSAoutput <- c(); 
Out <- list()
vOut <- list()
# pR_noIm_df <- c(); 
# pop_dynamics <- c()

summary_df <- output_func(TimeStop_dynamics, output) # create empty dataframe to store output

turnover <- F; 
dynamics <- T; 
transmission <- F; 
clean_environment <- T

# timepoints for population growth
t2 <- TimeStop_dynamics/2 # 5 year pop growth
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

for(n in 1:length(pVs)){
  #######################
  ## Vaccination Setup ##
  #######################
  # see applied_vaccination
  pV <- pVs[n] # within flock coverage of each campaign (0.7-1)
  source("scripts/applied/applied_vaccination.R")
  Vstart <- Vprog %>% filter(Vround==1) %>% pull(Vweek)
  
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
    
    if(dataset %in% c("lesnoff.T")){
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
        Vprog,
        fixdata,
        vardata
      )
  }
  
  vOut[[n]] <- Out
}

saveRDS(vOut, file = "output/Applied/applied_output_vaccination_EXAMPLE-2023-10-31.RData")


v70 <- vOut[[1]]
v80 <- vOut[[2]]
v90 <- vOut[[3]]
v100 <- vOut[[4]]






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
    "oc.shp.humidM",
    "oc.shp.humidM.valid"
  )
))

# dynplot_i <- dynplot %>% filter(prof %in% c("cgiar.goat","cgiar.shp"))
# 
# dynplot_i <- dynplot %>% filter(prof %in% c("lesnoff.T"))
# 
dynplot_i <- dynplot %>% filter(prof %in% c("oc.goat.aridP",
                                            "oc.goat.semiaridP",
                                            "oc.goat.semiaridM",
                                            "oc.goat.subhumidM",
                                            "oc.goat.humidM"))
# 
# dynplot_i <- dynplot %>% filter(prof %in% c("oc.shp.aridP",
#                                             "oc.shp.semiaridP",
#                                             "oc.shp.semiaridM",
#                                             "oc.shp.subhumidM",
#                                             "oc.shp.humidM",
#                                             "oc.shp.humidM.valid"))

## AGE STRUCTURE
age_struc <- dynplot_i %>%
  select(w, prof, starts_with("pf"), starts_with("pm")) %>%
  gather(key = "age_group", value="prop", -c(w,prof))

plot_age <- ggplot(age_struc, aes(x = w, y = prop,col = age_group,group = age_group)) + 
  geom_line() + 
  facet_wrap( ~ prof,nrow=1) +
  labs(x = "week", y = "population-propotion", title = "Age Structure, 5 years") +
  theme_bw()+
  theme(legend.position = "right")
  # theme(legend.position = "bottom") +
  # guides(color = guide_legend(nrow = 1))

plot_growth <- ggplot(dynplot_i %>% filter(w<500), aes(x=w, y=sum_pop/min(sum_pop)))+
  geom_line()+
  facet_wrap(~prof,nrow=1)+
  labs(x="week", y="population-growth", title = "Population growth, 5 years")+
  theme_bw()

plot_growth_free <- ggplot(dynplot_i %>% filter(w<500), aes(x=w, y=sum_pop/min(sum_pop)))+
  geom_line()+
  facet_wrap(~prof, scales = "free",nrow=1)+
  labs(x="week", y="population-growth", title = "Population growth, 5 years")+
  theme_bw()

plot_immune <- ggplot(dynplot_i, aes(x=w, y=prop_immune))+
  geom_line()+
  facet_wrap(~prof,nrow=1)+
  labs(x="week", y="proportion-immune", title = "Immune decay, 5 years")+
  coord_cartesian(xlim=c(0,500))+
  theme_bw()

ggarrange(plot_age, plot_growth, plot_immune, ncol=1)


########################
## PLOTTING SUMMARIES ##
########################

Out_summary_long <- Out_summary %>%
  select(imm_V0, imm_6m, imm_12m, imm70_w, finyr_growth, fiveyr_growth, prof) %>%
  gather(Out_summary, value = "value", -prof) %>% 
  rename(stat = Out_summary) %>%
  mutate(stat = factor(stat, levels = c("finyr_growth", "fiveyr_growth", "imm_V0", "imm_6m", "imm_12m", "imm70_w")))


ggplot(Out_summary_long, aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


my_colors <- RColorBrewer::brewer.pal(4, "Dark2")[1:4]

ggplot(Out_summary_long %>% filter(stat %in% c("finyr_growth")), aes(x = prof, y = as.numeric(value), fill = stat)) + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  scale_fill_manual(values = my_colors[1])+
  labs(x = "", y = "Population growth")

A <- ggplot(Out_summary_long %>% filter(stat %in% c("fiveyr_growth")), aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  scale_fill_manual(values = my_colors[1])+
  labs(x = "", y = "Population growth")

B <- ggplot(Out_summary_long %>% filter(stat %in% c("imm_V0","imm_6m", "imm_12m")), aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  coord_cartesian(ylim = c(0,0.9))+
  # scale_fill_manual(values = my_colors[2:4])+
  labs(x = "", y = "Proportion immune")

B2 <- ggplot(Out_summary_long %>% filter(stat %in% c("imm_V0","imm_6m", "imm_12m")), aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  coord_cartesian(ylim = c(0,0.9))+
  scale_fill_manual(values = my_colors[2:4])+
  labs(x = "", y = "Proportion immune")

C <- ggplot(Out_summary_long %>% filter(stat %in% c("imm70_w")), aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  scale_fill_manual(values = my_colors[5])+
  labs(x = "", y = "Weeks to 70%")

ggarrange(A,B,C, ncol = 1)



######################
## VACCINATION PLOT ##
######################
vacplot_i <- dynplot %>% mutate(source = ifelse(prof %in% c("cgiar.shp", "cgiar.goat"), "cgiar",
                                ifelse(prof %in% c("lesnoff.T"), "lesnoff.T",
                                       ifelse(prof %in% c("oc.shp.aridP",
                                                               "oc.shp.semiaridP",
                                                               "oc.shp.semiaridM",
                                                               "oc.shp.subhumidM",
                                                               "oc.shp.humidM",
                                                               "oc.shp.humidM.valid"), "OC.shp", "OC.goat")))) %>%
  filter(prop_immune>0)


plot_vaci <- ggplot(vacplot_i, aes(x=w, y=prop_immune))+
  geom_point(aes(col = prof), size = 0.5)+
  facet_wrap(~source,nrow=2)+
  labs(x="week", y="proportion-immune", title = "Immune decay, 5 years")+
  coord_cartesian(xlim=c(260,500))+
  theme_bw()
plot_vaci
