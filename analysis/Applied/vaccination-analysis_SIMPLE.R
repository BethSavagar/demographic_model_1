###############################################################################################################
## Analysis of vaccination output
###############################################################################################################
# Script to analyse GSCE vaccination output at multiple coverage levels
# Author: Beth Savagar
# Date: 31.10.23

library(tidyverse)

############
## SETUP ##
############

source("functions/GSA_outputs.R")
vOut <- readRDS(file = "output/Applied/applied_output_vaccination_EXAMPLE-2023-10-31.RData")

# split into vacciantion coverage lists for processing
v70 <- vOut[[1]]
v80 <- vOut[[2]]
v90 <- vOut[[3]]
v100 <- vOut[[4]]

pVs <- seq(0.7,1,0.1)
datasets <- c(
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

Vstart <- 520
TimeStop_dynamics <- 20*52
t1 <- 20*52
t2 <- t1/2

####################
## OUTPUT SUMMARY ##
####################


dynplot_df <- c()

for(x in 1:length(vOut)){
  list_x <- vOut[[x]]
  dynplot_x <- do.call(rbind, list_x) %>% 
    mutate(prof = rep(datasets, each = TimeStop_dynamics),
           pV = pVs[x])
  dynplot_df <- rbind(dynplot_df, dynplot_x)
}


summary_df <- c()

for(j in 1:length(vOut)){
  list_x <- vOut[[j]]
  
  # Summary statistics: pop growth & immunity metrics
  summary_x <- sapply(list_x, function(x)
    GSA_output(x,Vstart)
  ) %>%
    t() %>%
    as.data.frame() %>%
    mutate(prof = datasets, 
           pV = pVs[j])
  
  summary_df <- rbind(summary_df, summary_x)
}

##############
## PLOTTING ##
##############

dynplot <- dynplot_df %>%
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

# AGE STRUCTURE
# age_struc <- dynplot_df %>%
#   select(w, prof, starts_with("pf"), starts_with("pm")) %>%
#   gather(key = "age_group", value="prop", -c(w,prof))
# 
# plot_age <- ggplot(age_struc, aes(x = w, y = prop,col = age_group,group = age_group)) +
#   geom_line() +
#   facet_wrap( ~ prof,ncol=3) +
#   labs(x = "week", y = "population-propotion", title = "Age Structure, 5 years") +
#   theme_bw()+
#   theme(legend.position = "right")

# plot_growth <- ggplot(dynplot_i, aes(x=w, y=sum_pop/75))+
#   geom_line()+
#   facet_wrap(~prof,nrow=1)+
#   labs(x="week", y="population-growth", title = "Population growth, 5 years")+
#   theme_bw()
# 
# plot_growth_free <- ggplot(dynplot_i, aes(x=w, y=sum_pop/75))+
#   geom_line()+
#   facet_wrap(~prof, scales = "free",nrow=1)+
#   labs(x="week", y="population-growth", title = "Population growth, 5 years")+
#   theme_bw()


plot_immune <- ggplot(dynplot_i %>% filter(w>500), aes(x=w, y=prop_immune))+
  geom_line(aes(col = as.factor(pV)))+
  geom_hline(yintercept = 0.7, linetype = "dashed")+
  facet_wrap(~prof,nrow=1)+
  labs(x="week", y="proportion-immune", title = "Immune decay, 10 years")+
  theme_bw()

plot_immune 

# ggarrange(plot_age, plot_growth, plot_immune, ncol=1)


########################
## PLOTTING SUMMARIES ##
########################

summary_long <- summary_df %>%
  select(imm_V0, imm_6m, imm_12m, imm70_w, tenyr_growth, finyr_growth, prof,pV) %>%
  gather(summary_df, value = "value", -c(prof,pV)) %>% 
  rename(stat = summary_df) %>%
  mutate(stat = factor(stat, levels = c("finyr_growth","tenyr_growth", "imm_V0", "imm_6m", "imm_12m", "imm70_w")))

ggplot(summary_long, aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap( ~ stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Annual Population Growth Rate (based on growth in final year)

A <- ggplot(summary_long %>% 
         filter(stat == "finyr_growth",
                pV == 1), 
       aes(x = prof, y = as.numeric(value))) + 
  geom_col(position = position_dodge(width = 0.9))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(x=" ", y = "Population Growth, Final Yr")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

A

B <- ggplot(summary_long %>% filter(stat %in% c("imm_V0","imm_6m", "imm_12m")), aes(x = prof, y = as.numeric(value), fill = stat))+ geom_col(position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = 0.7, linetype = "dashed")+
  facet_grid(stat ~ pV, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  coord_cartesian(ylim = c(0,0.9))+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "", y = "Proportion immune")

B

# C <- ggplot(summary_long %>% filter(stat %in% c("imm70_w")), aes(x = prof, y = as.numeric(value), fill = as.factor(pV))) + geom_col(position = position_dodge(width = 0.9)) +
#   facet_wrap( ~ stat, scales = "free")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
#         legend.position = "none")+
#   labs(x = "", y = "Weeks to 70%")

# ggarrange(A,B,C, ncol = 1)



######################
## VACCINATION PLOT ##
######################
vacplot_i <- dynplot_df %>% mutate(source = ifelse(prof %in% c("cgiar.shp", "cgiar.goat"), "cgiar",
                                ifelse(prof %in% c("lesnoff.T"), "lesnoff.T",
                                       ifelse(prof %in% c("oc.shp.aridP",
                                                               "oc.shp.semiaridP",
                                                               "oc.shp.semiaridM",
                                                               "oc.shp.subhumidM",
                                                               "oc.shp.humidM",
                                                               "oc.shp.humidM.valid"), "OC.shp", "OC.goat")))) %>%
  filter(prop_immune>0)


plot_vaci <- ggplot(vacplot_i, aes(x=w, y=prop_immune))+
  geom_point(aes(col = prof), size = 0.5, shape = 18)+
  geom_hline(yintercept = 0.7, linetype = "dashed")+
  facet_grid(pV~source)+
  labs(x="Week", y="Proportion Immune", title = "Immune decay, 10 years", col = "Profile")+
  coord_cartesian(xlim = c(500,800))+
  theme_bw()
plot_vaci




##################################################
##################################################
##################################################


Out_summary_long <- summary_long %>% filter(pV == 1)


my_colors <- RColorBrewer::brewer.pal(4, "Dark2")[1:4]

A <- ggplot(Out_summary_long %>% filter(stat %in% c("finyr_growth")), aes(x = prof, y = as.numeric(value), fill = stat)) + geom_col(position = position_dodge(width = 0.9)) +
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
