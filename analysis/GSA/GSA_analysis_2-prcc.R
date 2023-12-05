### RSA ANALYSIS 3 - 25/08/2023 ###

custom_theme <- theme_bw() + theme(text = element_text(family = "Times New Roman", size = 9))
theme_set(custom_theme)
plotsave <- F

################
# setup
################
library(tidyverse)
library(sensobol)
library(epiR)
library(ggpubr)



################
# Load Datasets
################

# population dynamics: 
GSAoutput <- read.csv("output/GSA_output/GSA_output_sobol_2023-08-30.csv")
# parameter sets:
sobol_input <- read.csv("output/GSA_output/GSA_pars-set_sobol_2023-08-30.csv")
# behavioural parameters with +/- 15% growth saved as GSA_var_input-PRCC in data folder for GSA analysis with PRCC

# take a look at the data: 
str(GSAoutput) # output contains population and immunity dyanmics and age-sex structure from sobol sampling of refined parameter ranges. 
str(sobol_input) # contains parameter sets for each output
summary(sobol_input)


################
# Clean Data 
################

# add set id to output df:
GSAoutput <- GSAoutput %>%
  mutate(GSAset = 1:nrow(GSAoutput))

# tidy parameters dataframe: 
sobol_input <- sobol_input %>%
  select(NET_offtake_m, NET_offtake_m2, NET_offtake_f, everything()) %>%
  rename(off_mY = NET_offtake_m2,
         off_mA = NET_offtake_m,
         off_f = NET_offtake_f,
         mort_Y = mortality_y,
         mort_A = mortality_a,
         max_yrs_F = adu_f_max_yrs,
         max_yrs_M = adu_m_max_yrs,
         min_off = min_age_offtake,
         min_repro = min_age_repro
  ) %>%
  mutate(GSAset = 1:nrow(sobol_input))

## If needed creation of parameter dataframe with outputs of interest:
# 
# GSA_all <- sobol_input %>%
#   left_join(GSAoutput %>% select(c("GSAset","pop_growth","tenyr_growth","imm_6m","imm_12m","imm70_w")), by = c("GSAset")) %>%
#   select(-GSAset)


#########################################################
# Analysis 1 - Identify behavioral parameter GSAsets
#########################################################

# Define age-sex structure conditions: 

## Min and max proportions for each sex-age group (see age-sex SS)

pfKid.min <- 0.05; pfKid.max <- 0.19
pfSub.min <- 0.06; pfSub.max <- 0.19
pfAdu.min <- 0.21; pfAdu.max <- 0.62

pmKid.min <- 0.05; pmKid.max <- 0.16
pmSub.min <- 0.04; pmSub.max <- 0.15
pmAdu.min <- 0.01; pmAdu.max <- 0.15

####################
## GSA ANALYSIS 1 ##
####################
params <- colnames(sobol_input %>% select(-GSAset))

# (i) overall population growth
GSAoutput <- GSAoutput %>%
  mutate(pop_growth = replace_na(pop_growth,0),
         tenyr_growth = replace_na(tenyr_growth,0),)

# PRCC on complete data frame

# Total pop growth
dat.1 <- sobol_input %>% select(-GSAset) %>% cbind(GSAoutput %>% select(pop_growth))
prcc_popgrowth <- epi.prcc(dat.1, sided.test = 2, conf.level = 0.95)

GSAgrowth_prccplot <- ggplot(prcc_popgrowth, aes(x=est, y=var))+
  geom_col()+
  labs(x="PRCC", y="Demographic Parameter")
GSAgrowth_prccplot

GSAgrowth_prcctable <- prcc_popgrowth %>% 
  select(var,est,p.value) %>% 
  transmute(var = var,
            popgrowth=round(est,digits=3),
            p= round(p.value, digits=3)) %>% 
  arrange(desc(abs(popgrowth)))

GSAgrowth_prcctable

filename_GSAgrowthprcc <- "GSAgrowth_prccplot.png"

if(plotsave){
  ggsave(paste0("plots/GSA/",filename_GSAgrowthprcc), plot = GSAgrowth_prccplot, width = 7.5, height = 7.5, units = "cm")
}

# Immunity at 6m
dat.2 <- sobol_input %>% select(-GSAset) %>% cbind(GSAoutput %>% select(imm_6m))
prcc_imm6m <- epi.prcc(dat.2, sided.test = 2, conf.level = 0.95)

# Immunity at 12m
dat.3 <- sobol_input %>% select(-GSAset) %>% cbind(GSAoutput %>% select(imm_12m))
prcc_imm12m <- epi.prcc(dat.3, sided.test = 2, conf.level = 0.95)

# Immunity below 70%
dat.4 <- sobol_input %>% select(-GSAset) %>% cbind(GSAoutput %>% select(imm70_w))
prcc_imm70 <- epi.prcc(dat.4, sided.test = 2, conf.level = 0.95)

# Bar plot comparing prcc for each output
prcc.all <- rbind(prcc_popgrowth %>% select(var,est) %>% mutate(output = "popgrowth"),
                 prcc_imm6m %>% select(var,est) %>% mutate(output = "imm6m"),
                 prcc_imm12m %>% select(var,est) %>% mutate(output = "imm12m"),
                 prcc_imm70 %>% select(var,est) %>% mutate(output = "imm70")) %>%
  mutate(output = factor(output, levels = c("imm6m", "imm12m", "imm70", "popgrowth")))

GSAall_prcc <- ggplot(prcc.all, aes(x=est, y=var, group = output, fill = output))+
  geom_col(position = "dodge")+
  labs(x="PRCC", y="Demographic Parameter", fill = "Output", title = "")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
GSAall_prcc

GSAall_prcc2 <- ggplot(prcc.all, aes(x=est, y=var, group = output, fill = output))+
  geom_col(position = "dodge")+
  facet_wrap(~output)+
  labs(x="PRCC", y="Demographic Parameter", fill = "Output", title = "")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
GSAall_prcc2

filename_GSAallprcc <- "GSAall_prccplot.png"
filename_GSAallprcc2 <- "GSAall_prccplot2.png"

if(plotsave){
  ggsave(paste0("plots/GSA/",filename_GSAallprcc), plot = GSAall_prcc, width = 15, height = 7.5, units = "cm")
  ggsave(paste0("plots/GSA/",filename_GSAallprcc2), plot = GSAall_prcc2, width = 15, height = 7.5, units = "cm")

}


##########################################################################
####################
## GSA ANALYSIS 2 ##
####################
##########################################################################

## SETUP ##

# Analysis on only behavioural parameter sets

GSAoutput_ext <- GSAoutput %>%
  mutate(tenyr_growth = replace_na(tenyr_growth, 0),
         # add variables to identify parameter sets with growth between 5% and 15%
         tenyr_15 = ifelse(tenyr_growth>=0.85 & tenyr_growth <=1.15, 1, 0),
         tenyr_05 = ifelse(tenyr_growth>=0.95 & tenyr_growth <=1.05, 1, 0)) %>%
  
  # add variables to identify parameter sets with growth between 5% and 15% AND age-sex conditions
  mutate(
    tenyr_15age = ifelse(
      tenyr_15 == 1 &
      pfKid >= pfKid.min & pfKid <= pfKid.max &
      pfSub >= pfSub.min & pfSub <= pfSub.max &
      pfAdu >= pfAdu.min & pfAdu <= pfAdu.max &
      
      pmKid >= pmKid.min & pmKid <= pmKid.max &
      pmSub >= pmSub.min & pmSub <= pmSub.max &
      pmAdu >= pmAdu.min & pmAdu <= pmAdu.max,1,0),
    
    tenyr_05age = ifelse(
      tenyr_05 == 1 &
        pfKid >= pfKid.min & pfKid <= pfKid.max &
        pfSub >= pfSub.min & pfSub <= pfSub.max &
        pfAdu >= pfAdu.min & pfAdu <= pfAdu.max &
        
        pmKid >= pmKid.min & pmKid <= pmKid.max &
        pmSub >= pmSub.min & pmSub <= pmSub.max &
        pmAdu >= pmAdu.min & pmAdu <= pmAdu.max,1,0)
  )

# How many parameter sets are behavioural for growth of 5%, 15% and growth of 5%-15% with age-sex structure.

GSAoutput_ext %>% group_by(tenyr_15, tenyr_05) %>% count()
GSAoutput_ext %>% group_by(tenyr_15age) %>% count()
GSAoutput_ext %>% group_by(tenyr_05age) %>% count()

## ID which parameter sets are within 15% growth and age-sex conditions

sobol_par_subset <- sobol_input %>% left_join(GSAoutput_ext %>% select(c(pop_growth,tenyr_growth,imm_6m,imm_12m,imm70_w,tenyr_15age,GSAset)), by = c("GSAset")) %>% filter(tenyr_15age==1) 

par_subset <- sobol_par_subset %>%
  select(-c(pop_growth,tenyr_growth,imm_6m,imm_12m,imm70_w,tenyr_15age,GSAset))

pairs(par_subset, pch=18)
ggpairs(par_subset)

# PRCC on subset

# 6m Imm
dat.1b <- sobol_par_subset %>% select(-c(GSAset,
                                        "pop_growth",
                                        "tenyr_growth",
                                        "imm_12m",
                                        "imm70_w",
                                        "tenyr_15age"))

prccb_imm6m <- epi.prcc(dat.1b, sided.test = 2, conf.level = 0.95)

dat.2b <- sobol_par_subset %>% select(-c(GSAset,
                                        "pop_growth",
                                        "tenyr_growth",
                                        "imm_6m",
                                        "imm70_w",
                                        "tenyr_15age"))

prccb_imm12m <- epi.prcc(dat.2b, sided.test = 2, conf.level = 0.95)

dat.3b <- sobol_par_subset %>% select(-c(GSAset,
                                        "pop_growth",
                                        "tenyr_growth",
                                        "imm_6m",
                                        "imm_12m",
                                        "tenyr_15age"))

prccb_imm70 <- epi.prcc(dat.3b, sided.test = 2, conf.level = 0.95)

dat.4b <- sobol_par_subset %>% select(-c(GSAset,
                                        "pop_growth",
                                        "imm_6m",
                                        "imm_12m",
                                        "imm70_w",
                                        "tenyr_15age"))

prccb_growth <- epi.prcc(dat.4b, sided.test = 2, conf.level = 0.95)

GSAsubset_prccgrowth <- ggplot(prccb.growth, aes(x=var, y=est, group = output, fill = output))+
  geom_col(position = "dodge")+
  labs(x="par", y="prcc", title = "VALID ONLY")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
GSAsubset_prccgrowth

# Bar plot comparing prcc for each output
prccb.all <- rbind(prccb_imm6m %>% select(var,est) %>% mutate(output = "imm6m"),
                  prccb_imm12m %>% select(var,est) %>% mutate(output = "imm12m"),
                  prccb_imm70 %>% select(var,est) %>% mutate(output = "imm70"), 
                  prccb_growth %>% select(var,est) %>% mutate(output = "pop_growth"))


GSAsubset_prccimmunity <- ggplot(prcc.all %>% filter(!output == "popgrowth"), 
       aes(x=est, y=var, group = output, fill = output))+
  geom_col(position = "dodge")+
  labs(x="PRCC", y="Demographic Parameter", fill = "Output", title = "")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
GSAsubset_prccimmunity

GSAsubset_prccall <- ggplot(prcc.all, aes(x=est, y=var, group = output, fill = output))+
  geom_col(position = "dodge")+
  labs(x="PRCC", y="Demographic Parameter", fill = "Output", title = "")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
GSAsubset_prccall

GSAsubset_prccall2 <- ggplot(prcc.all, aes(x=est, y=var, group = output, fill = output))+
  geom_col(position = "dodge")+
  facet_wrap(~output, nrow =1)+
  labs(x="PRCC", y="Demographic Parameter", fill = "Output", title = "")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
GSAsubset_prccall2

filename_GSAsubset_prccall <- "GSAsubset_prccall.png"
filename_GSAsubset_prccall2 <- "GSAsubset_prccall2.png"
filename_GSAsubset_prccimmunity <- "GSAsubset_prccimmunity.png"

if(plotsave){
  ggsave(paste0("plots/GSA/",filename_GSAsubset_prccall), plot = GSAsubset_prccall, width = 12, height = 7.5, units = "cm")
  ggsave(paste0("plots/GSA/",filename_GSAsubset_prccall2), plot = GSAsubset_prccall2, width = 20, height = 10, units = "cm")
  ggsave(paste0("plots/GSA/",filename_GSAsubset_prccimmunity), plot = GSAsubset_prccimmunity, width = 12, height = 7.5, units = "cm")
  
}

prcc_summary <- prccb_imm6m %>% 
  select(var,est,p.value) %>% 
  transmute(var = var,
            est6m=round(est,digits=3),
            p6m= round(p.value, digits=3)) %>%
  cbind(prccb_imm12m %>% 
          select(var,est,p.value) %>% 
          transmute(est12m=round(est,digits=3),
                    p12m= round(p.value, digits=3))) %>%
  cbind(prccb_imm70 %>% 
          select(var,est,p.value) %>% 
          transmute(est70=round(est,digits=3),
                    p70= round(p.value, digits=3))) %>%
  arrange(desc(abs(est70)))

knitr::kable(prcc_summary)

ggarrange(A, B, ncol = 2)

prcc.all2 <- rbind(prcc.all %>% mutate(Cond = "ALL"),
                   prccb.all %>% mutate(Cond = "VALID"),
                   prccb_growth %>% select(var,est) %>% 
                     mutate(output = "popgrowth",
                            Cond="VALID")
                   )
# Plot of PRCC for each output (immunity nad popgrowth), for Valid and All par sets
prcc_comparison <- ggplot(prcc.all2, aes(x=est, y=var, group = Cond, fill = Cond))+
  geom_col(position = "dodge")+
  facet_wrap(~output, nrow = 1)+
  labs(x="par", y="prcc", title = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

filename_prcc_comparison <- "prcc_comparison.png"

if(plotsave){
  ggsave(paste0("plots/GSA/",filename_prcc_comparison), plot = prcc_comparison, width = 20, height = 10, units = "cm")

  
}
##########################################################


# Plot the variation in output parameters:


out_ALL <- GSAoutput %>% select(pop_growth,
                                tenyr_growth,
                                imm_6m,
                                imm_12m,
                                imm70_w) %>%
  gather(key="metric",value="metric_val")

ggplot(out_ALL, aes(x=metric,y=metric_val))+geom_boxplot()+facet_wrap(~metric, scales = "free")

ggplot(out_ALL, aes(x=metric_val))+geom_density()+facet_wrap(~metric, scales = "free")




out_VALID <- sobol_par_subset %>% select(pop_growth,
                                tenyr_growth,
                                imm_6m,
                                imm_12m,
                                imm70_w) %>%
  gather(key="metric",value="metric_val")

ggplot(out_VALID, aes(x=metric,y=metric_val))+geom_boxplot()+facet_wrap(~metric, scales = "free")

ggplot(out_VALID, aes(x=metric_val))+geom_density()+facet_wrap(~metric, scales = "free")



