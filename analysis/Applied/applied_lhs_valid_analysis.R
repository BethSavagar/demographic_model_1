library(tidyverse)
library(knitr)
library(ggpubr)

validOut <- readRDS(file = "output/Applied/applied_output_lhs2_VALID-2023-10-30.RData")
fix_age_data_full <- read_csv("data/Applied_parameters/state_vars.csv",col_names=T)
datasets <- fix_age_data_full %>% select(-c(parameter, oc.shp.humidM.valid)) %>% colnames()

# number of valid outcomes of 1000 parameter sets for each profile
valid_tally <- cbind(datasets, "nvalid" = sapply(validOut, nrow)) 
kable(valid_tally)
valid_id <- which(sapply(validOut, nrow)>0)
validOnly <- validOut[valid_id]

datasets_valid <- datasets[valid_id]

Out_dynamics_df <- c()
for (i in 1:(length(datasets_valid)-1)){
  data <- datasets_valid[i]
  new <- validOnly[[i]] %>% select(pop_growth, fiveyr_growth = tenyr_growth) %>% gather(key = "param", value = value) %>% mutate(prof = data)
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
for (i in 1:(length(datasets_valid)-1)){
  data <- datasets_valid[i]
  new <- validOnly[[i]] %>% select(starts_with("pf"), starts_with("pm")) %>% gather(key = "param", value = value) %>% mutate(prof = data)
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

library(RColorBrewer)
# Load the Dark2 palette
palette_dark2 <- brewer.pal(8, "Dark2")

# Replace the last color with dark gray using a hexadecimal code
palette_dark2[length(palette_dark2)] <- "#333333"

# Now you can use the modified palette for your plots
palette_dark2


# Plots to show ranges for demographic parameters of Applied-profile-LHS-valid compared to valid-GSA

# Load profiles parameter ranges and original parameter ranges
validpars_app <- readRDS("output/Applied/applied_output_pars_VALID-2023-10-30.RData")
# subset to only profiles with valid output:
validpars_App <- validpars_app[valid_id]
# Load complete df of valid parameters from RSA nad GSA
validpars_GSA <- read_csv("data/GSA_parameters/GSA_valid-pars-only.csv", col_names = T)
validpars_RSA <- read_csv("data/GSA_parameters/GSA_var_input-PRCC.csv", col_names=T)

# Collate all valid parameters from GSA RSA
validpars_SA <- rbind(validpars_GSA, validpars_RSA) %>%
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

# pairs plot of valid pars from RSA GSA
library(GGally)
ggpairs(validpars_SA)

# add cols for distinguishing SA pars: 
validpars_SA <- validpars_SA %>% mutate(profile = "SA", prof2 = "SA")

# Create df with valid pars and profile identifier:
prof_count <- valid_tally %>% as.data.frame() %>% pull(nvalid) %>% as.numeric()
prof_rep <- rep(datasets, as.numeric(prof_count))
pars_df <- as.data.frame(do.call(rbind, validpars_App))

validPars_df <- cbind(pars_df, profile = prof_rep) %>%
  as.data.frame() %>%
  # add profile identifier for faceting
  mutate(prof2 = if_else(profile %in% c(
    "oc.goat.aridP",
    "oc.goat.semiaridP",
    "oc.goat.subhumidM"), 
    "goat", "sheep")) %>%
  # remove columns which aren't parameters
  select(-c(tenyr_15age, tenyr_05age, tenyr_growth))

# join the RSA-GSA pars with the Applied pars:

validPars_df <- rbind(validpars_SA,
                      validPars_df %>% select(colnames(validpars_SA))) %>% 
  as.data.frame()

# PAIRS PLOTS #

ggpairs(validPars_df %>% filter(prof2 != "sheep",
                                profile != "oc.shp.semiaridM"),
        columns = 1:8,
        legend = 1,
        aes(colour = profile,
            alpha = 0.5),
        upper = "blank")+
  scale_colour_manual(values = c("#1B9E77","#D95F02","#7570B3","#333333"))+
  scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#333333"))+
  theme(legend.position = "bottom")

ggpairs(validPars_df %>% filter(prof2 != "goat",
                                profile != "oc.shp.semiaridM"),
        columns = 1:8,
        legend = 1,
        aes(colour = profile,
            alpha = 0.5),
        upper = "blank")+
  scale_colour_manual(values = c("#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#333333"))+

  scale_fill_manual(values = c("#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#333333"))+
  theme(legend.position = "bottom")

######################################
## Vaccination Uncertainty Examples ##
######################################

valid_df <- as.data.frame(do.call(rbind, validOnly))

validOnly_df <- cbind(valid_df, profile = prof_rep) %>%
  as.data.frame() %>%
  # add profile identifier for faceting
  mutate(prof2 = if_else(profile %in% c(
    "oc.goat.aridP",
    "oc.goat.semiaridP",
    "oc.goat.subhumidM"), 
    "goat", "sheep")) %>%
  # remove columns which aren't parameters
  select(tenyr_growth, 
         imm_V0,
         imm_6m,
         imm_12m,
         imm70_w,
         profile,
         prof2)

validOnly_long <- validOnly_df %>% gather(key = "stat", value = "value", -c(profile, prof2))

head(validOnly_long)

mypal <- brewer.pal(5,"Dark2")

A <- ggplot(validOnly_long %>% filter(!stat %in% c("imm70_w", "tenyr_growth")), 
       aes(x = profile, y = as.numeric(value), fill = stat))+ 
  geom_boxplot() +
  facet_wrap(~stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  scale_fill_manual(values = mypal[1:3])

B <- ggplot(validOnly_long %>% filter(stat %in% c("imm70_w")), 
            aes(x = profile, y = as.numeric(value), fill = stat))+ 
  geom_boxplot() +
  facet_wrap(~stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  scale_fill_manual(values = mypal[4])


C <- ggplot(validOnly_long %>% filter(stat %in% c("tenyr_growth")), 
                 aes(x = profile, y = as.numeric(value), fill = stat))+ 
  geom_boxplot() +
  facet_wrap(~stat, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")+
  scale_fill_manual(values = mypal[1:3])

ggarrange(A,B,C, ncol = 1)