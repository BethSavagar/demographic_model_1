### Analysis 2 - Conditional on final ten year pop growth ###
library(tidyverse)
library(knitr)
# RSA analysis_2 
# Similar to analysis_1 but which additional analysis of pop growth over final 10yrs
# Running RSA_test.Rmd on 01-06-2023 with 10'000 lhs parameter sets and 25 year simulations
# 1) Analysis of population growth for:
##  a) total time
##  b) last 10 years 
# 2) Sex-age structure on population.
# 3) Population dynamics

## RSAoutput <- read.csv("output/RSAoutput_morevars_220623.csv")
## var_input_set <- parameter boundaries `min.3` and `max.3`
#############################

# I - Identify valid populations
## - stable populations (with TENYR growth +/- 0.5) 
RSAoutput <- RSAoutput %>%
  mutate(set = 1:nrow(RSAoutput))
# Subset populations which are maintained
validRSA <-  RSAoutput %>%
  filter(pop_growth != 0) 
# - TENYR: subset populations are stable i.e. with TENYR growth of +/- 5% (final pop: ~50)
tenyr_growth <- validRSA %>%
  filter(tenyr_growth>=0.95, tenyr_growth <=1.05)
# try with much wider range of growth:
# filter(tenyr_growth>=0.85, tenyr_growth <=1.15)
# 
# var_input_set <- var_input_set %>%
#   select(NET_offtake_m, NET_offtake_m2, NET_offtake_f, everything()) %>%
#   rename(off_mA = NET_offtake_m,
#          off_mY = NET_offtake_m2,
#          off_f = NET_offtake_f,
#          mort_Y = mortality_y,
#          mort_A = mortality_a,
#          max_yrs_F = adu_f_max_yrs,
#          max_yrs_M = adu_m_max_yrs,
#          min_off = min_age_offtake,
#          min_repro = min_age_repro
#   )

tenyr_set <- tenyr_growth %>%
  pull(set)

tenyr_pars <-  var_input_set[tenyr_set,]

# plot parameter sets which have overall growth and 10 year growth which is ~ stable
pairs(tenyr_pars, pch = 18)

tenyr_pars_long <- gather(as.data.frame(tenyr_pars) %>% 
                            mutate(set=1:nrow(tenyr_pars)), key="par", value = "val", -set)

# ggplot(tenyr_pars_long, aes(x=val))+geom_density()+facet_wrap(~par, scales="free")
# ggplot(tenyr_pars_long, aes(x=par,y=val))+geom_boxplot()+facet_wrap(~par, scales="free")

var_input_long <- gather(as.data.frame(var_input_set) %>% mutate(set=1:nrow(var_input_set)), key="par", value = "val", -set)

var_input_all <- rbind(var_input_long %>% mutate(stage = "PRE"), 
                       tenyr_pars_long %>% mutate(stage = "10Y")) %>%
  mutate(stage = ordered(stage, levels = c("PRE", "10Y")))

# ggplot(var_input_all, aes(x=val))+geom_density(aes(col=stage), linewidth = 1)+facet_wrap(~par, scales="free")
# ggplot(var_input_all, aes(x=par,y=val))+geom_boxplot(aes(col=stage))+facet_wrap(~par, scales="free")

# What is the population structure of the valid parameters?
### population structure ###

# for parameters where population is stable over final 10 years
tenyr_agesex <- tenyr_growth %>% 
  select(starts_with("pf")|starts_with("pm")|starts_with(("pF"))) %>%
  gather(key=stat, value=prop) %>%
  mutate(sex = ifelse(stat %in% c("pmKid","pmSub","pmAdu"), "M", "F"))

ggplot(tenyr_agesex, aes(x=stat, y=prop, col = sex))+geom_boxplot()
tenyr_agesex %>% group_by(sex, stat) %>% summarise(mean=mean(prop)) %>% kable()

### population dynamics ###
# 
# set <- allgrowth_set 
# source("scripts/RSA/validpop_dynamics.R")
# allgrowth_dynamics <- ggplot(validpop_long, aes(x=w, y=pop, col = as.character(set)))+geom_line()
# 
# set <- tenyr_set 
# source("scripts/RSA/validpop_dynamics.R")
# tenyr_dynamics <- ggplot(validpop_long, aes(x=w, y=pop, col = as.character(set)))+geom_line()+theme(legend.position="none")
# 
# ggarrange(allgrowth_dynamics, tenyr_dynamics, ncol = 1)


###########################
## ADDITIONAL CONDITIONS ##

# using 10yr growth df

## Min and max proportions for each sex-age group
pF.min <- 0.5; pF.max <- 0.8

pfKid.min <- 0.08; pfKid.max <- 0.18
pfSub.min <- 0.11; pfSub.max <- 0.15
pfAdu.min <- 0.24; pfAdu.max <- 0.42

pmKid.min <- 0.08; pmKid.max <- 0.16
pmSub.min <- 0.09; pmSub.max <- 0.14
pmAdu.min <- 0.08; pmAdu.max <- 0.15
#pmAdu.min <- 0.05; pmAdu.max <- 0.15
pmAdu.min2 <- 0.08; pmAdu.max2 <- 0.25

#######################################################
# FOR RELAXED AGE-SEX PARS #
#######################################################

valid_agesex2 <- tenyr_growth %>%
  filter(pfKid >= pfKid.min, pfKid <= pfKid.max, 
         pfSub >= pfSub.min, pfSub <= pfSub.max,
         pfAdu >= pfAdu.min, pfAdu <= pfAdu.max,
         
         pmKid >= pmKid.min, pmKid <= pmKid.max,
         pmSub >= pmSub.min, pmSub <= pmSub.max,
         pmAdu >= pmAdu.min2, pmAdu <= pmAdu.max2)

valid_as_pars2 <- valid_agesex2 %>%
  pull(set)

valid_as_pars2_df <-  var_input_set[valid_as_pars2,]

pairs(valid_as_pars2_df, pch = 18)


AS_pars2_long <- gather(as.data.frame(valid_as_pars2_df) %>% 
                            mutate(set=1:nrow(valid_as_pars2_df)), key="par", value = "val", -set)

# ggplot(AS_pars2_long, aes(x=val))+geom_density()+facet_wrap(~par, scales="free")
# ggplot(AS_pars2_long, aes(x=par,y=val))+geom_boxplot()+facet_wrap(~par, scales="free")

var_input_all <- var_input_all %>% rbind(AS_pars2_long %>% mutate(stage = "relaxed")) %>%
  mutate(stage = ordered(stage, levels = c("PRE", "10Y", "relaxed")))

# ggplot(var_input_all, aes(x=val))+geom_density(aes(col=stage), linewidth = 1)+facet_wrap(~par, scales="free")
# ggplot(var_input_all, aes(x=par,y=val))+geom_boxplot(aes(col=stage))+facet_wrap(~par, scales="free")

#######################################################
# FOR STRICT AGE-SEX PARS #
#######################################################

valid_agesex <- tenyr_growth %>%
  filter(pfKid >= pfKid.min, pfKid <= pfKid.max, 
         pfSub >= pfSub.min, pfSub <= pfSub.max,
         pfAdu >= pfAdu.min, pfAdu <= pfAdu.max,
         
         pmKid >= pmKid.min, pmKid <= pmKid.max,
         pmSub >= pmSub.min, pmSub <= pmSub.max,
         pmAdu >= pmAdu.min, pmAdu <= pmAdu.max)

valid_as_pars <- valid_agesex %>%
  pull(set)

valid_as_pars_df <- var_input_set[valid_as_pars,] 

# valid_as_pars_df <- valid_as_pars_df %>%
#   select(NET_offtake_m, NET_offtake_m2, NET_offtake_f, everything()) %>%
#   rename(off_mA = NET_offtake_m,
#        off_mY = NET_offtake_m2,
#        off_f = NET_offtake_f,
#        mort_Y = mortality_y,
#        mort_A = mortality_a,
#        max_yrs_F = adu_f_max_yrs,
#        max_yrs_M = adu_m_max_yrs,
#        min_off = min_age_offtake,
#        min_repro = min_age_repro
# )


set <- 1:nrow(valid_as_pars_df)
pairs(valid_as_pars_df, pch = 18, #bg = hcl.colors(length(set), "Temps")[set], col = hcl.colors(length(set), "Temps")[set]
      ) 

AS_pars_long <- gather(as.data.frame(valid_as_pars_df) %>% 
                          mutate(set=1:nrow(valid_as_pars_df)), key="par", value = "val", -set)

# ggplot(AS_pars_long, aes(x=val))+geom_density()+facet_wrap(~par, scales="free")
# ggplot(AS_pars_long, aes(x=par,y=val))+geom_boxplot()+facet_wrap(~par, scales="free")

var_input_all <- var_input_all %>% rbind(AS_pars_long %>% mutate(stage = "strict")) %>%
  mutate(stage = ordered(stage, levels = c("PRE", "10Y", "relaxed", "strict")))

ggplot(var_input_all, aes(x=val))+geom_density(aes(col=stage), linewidth = 1)+facet_wrap(~par, scales="free")
ggplot(var_input_all, aes(x=par,y=val))+geom_boxplot(aes(col=stage))+facet_wrap(~par, scales="free")

#######################################################
# 
# plot_map <- gather(as.data.frame(valid_as_pars_df) %>% mutate(set=1:nrow(valid_as_pars_df)), key="par", value = "val", -set)
# plot_map2 <- gather(as.data.frame(valid_as_pars2_df) %>% mutate(set=1:nrow(valid_as_pars_df)), key="par", value = "val", -set)
# ggplot(plot_map %>% filter(par %in% c("adu_f_max_yrs", "adu_m_max_yrs", "min_age_offtake", "min_age_repro")), aes(x=par, y=val))+geom_boxplot()
# ggplot(plot_map %>% filter(!par %in% c("adu_f_max_yrs", "adu_m_max_yrs", "min_age_offtake", "min_age_repro")), aes(x=par, y=val))+geom_boxplot()
# ggplot(plot_map, aes(x=par, y=val))+geom_point(aes(col = as.factor(set)))#+geom_line(aes(group = set, col = as.factor(set)))
# 

#####################################################
#####################################################
## Using a wider range of population growth/decline: 
#####################################################
#####################################################

# IV - Populations with any growth pattern which satisfy age-sex conditions. 

## Min and max proportions for each sex-age group
pF.min <- 0.5; pF.max <- 0.8

pfKid.min <- 0.08; pfKid.max <- 0.18
pfSub.min <- 0.11; pfSub.max <- 0.15
pfAdu.min <- 0.24; pfAdu.max <- 0.42

pmKid.min <- 0.08; pmKid.max <- 0.16
pmSub.min <- 0.09; pmSub.max <- 0.14
#pmAdu.min <- 0.08; pmAdu.max <- 0.15
pmAdu.min3 <- 0.05; pmAdu.max3 <- 0.15
#pmAdu.min2 <- 0.08; pmAdu.max2 <- 0.25

#######################################################
# FOR STRICT AGE-SEX PARS #
#######################################################

valid_agesex3 <- validRSA %>%
  filter(pfKid >= pfKid.min, pfKid <= pfKid.max, 
         pfSub >= pfSub.min, pfSub <= pfSub.max,
         pfAdu >= pfAdu.min, pfAdu <= pfAdu.max,
         
         pmKid >= pmKid.min, pmKid <= pmKid.max,
         pmSub >= pmSub.min, pmSub <= pmSub.max,
         pmAdu >= pmAdu.min3, pmAdu <= pmAdu.max3)


valid_agesex3 %>% pull(tenyr_growth) %>% summary()
agesex3_growth <- valid_agesex3 %>%
  # +/- 10%
  filter(tenyr_growth>=0.85, tenyr_growth <=1.15)


valid_as_pars3 <- agesex3_growth %>%
  pull(set)

valid_as_pars3_df <-  var_input_set[valid_as_pars3,]

pairs(valid_as_pars3_df, pch = 18)


AS_pars3_long <- gather(as.data.frame(valid_as_pars3_df) %>% 
                          mutate(set=1:nrow(valid_as_pars3_df)), key="par", value = "val", -set)

# ggplot(AS_pars3_long, aes(x=val))+geom_density()+facet_wrap(~par, scales="free")
# ggplot(AS_pars3_long, aes(x=par,y=val))+geom_boxplot()+facet_wrap(~par, scales="free")

var_input_all <- var_input_all %>% rbind(AS_pars3_long %>% mutate(stage = "strict_15%")) %>%
  mutate(stage = ordered(stage, levels = c("PRE", "10Y", "relaxed", "strict", "strict_15%")))

ggplot(var_input_all, aes(x=val))+geom_density(aes(col=stage), linewidth = 1)+facet_wrap(~par, scales="free")
ggplot(var_input_all, aes(x=par,y=val))+geom_boxplot(aes(col=stage))+facet_wrap(~par, scales="free")



## ===============================================
## CORRELATIONS ##
library(ggpubr)

# original data: 

RSAoutput <- RSAoutput %>% mutate(tenyr_growth = replace_na(tenyr_growth, 0))

# join input parameters with output of interest
RCC <- cbind(var_input_set, 
             "tenyr_growth" = RSAoutput %>% 
               pull(tenyr_growth)) %>% 
  as.data.frame()

RCC_long <- RCC %>% gather(key = "par", value = "par_val", -tenyr_growth)


ggplot(RCC_long, aes(x=par_val, y=tenyr_growth))+geom_point()+facet_wrap(~par, scales = "free")+
  # stat_cor(method = "pearson")+
  stat_cor(method = "spearman", col = "blue", geom="label", aes(size = 8))+
  theme(legend.position='none')

#############################
# with age-sex conditional data

# join input parameters with output of interest
RCC_valid <- cbind(valid_as_pars3_df, 
             "tenyr_growth" = agesex3_growth %>% 
               pull(tenyr_growth)) %>% 
  as.data.frame()

RCC_valid <- RCC_valid %>% mutate(tenyr_growth = replace_na(tenyr_growth, 0))


RCC_valid_long <- RCC_valid %>% gather(key = "par", value = "par_val", -tenyr_growth)
RCC_valid_long %>% group_by(par) %>% summarise(corr = cor(par_val, tenyr_growth, method = "spearman")) %>% knitr::kable()

ggplot(RCC_valid_long, aes(x=par_val, y=tenyr_growth))+geom_point()+facet_wrap(~par, scales = "free")+
  # stat_cor(method = "pearson")+
  stat_cor(method = "spearman", col = "blue", geom="label", aes(size = 8))+
  theme(legend.position='none')



