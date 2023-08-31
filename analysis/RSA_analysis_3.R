### RSA ANALYSIS 3 - 25/08/2023 ###

################
# Setup
################
library(tidyverse)


################
# Load Datasets
################

# population dynamics: 
RSAoutput <- read.csv("output/RSA_output/RSA_output_2023-08-24.csv")
# parameter sets:
var_input_set <- read.csv("output/RSA_output/RSA_pars-set_2023-08-24.csv")
# behavioural parameters with +/- 15% growth saved as GSA_var_input-PRCC in data folder for GSA analysis with PRCC

# take a look at the data: 
str(RSAoutput)
str(var_input_set)
summary(var_input_set)


################
# Clean Data 
################

# add set id to output df:
RSAoutput <- RSAoutput %>%
  mutate(set = 1:nrow(RSAoutput))

# tidy parameters dataframe: 
var_input_set <- var_input_set %>%
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
  mutate(set = 1:nrow(var_input_set))



#########################################################
# Analysis 1 - Identify behavioral parameter sets
#########################################################

# Define age-sex structure conditions: 

## Min and max proportions for each sex-age group (see age-sex SS)

pfKid.min <- 0.05; pfKid.max <- 0.19
pfSub.min <- 0.06; pfSub.max <- 0.19
pfAdu.min <- 0.21; pfAdu.max <- 0.62

pmKid.min <- 0.05; pmKid.max <- 0.16
pmSub.min <- 0.04; pmSub.max <- 0.15
pmAdu.min <- 0.01; pmAdu.max <- 0.15

#####################################

RSAoutput_ext <- RSAoutput %>%
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

RSAoutput_ext %>% group_by(tenyr_15, tenyr_05) %>% count()
RSAoutput_ext %>% group_by(tenyr_15age) %>% count()
RSAoutput_ext %>% group_by(tenyr_05age) %>% count()


## Refine parameter dataframe:

RSAparameters <- var_input_set %>%
  left_join(RSAoutput_ext %>%
              select(tenyr_growth,
                     tenyr_15,
                     tenyr_05,
                     tenyr_15age,
                     tenyr_05age,
                     set), by = c("set"))

RSApars_15age <- RSAparameters %>% 
  filter(tenyr_15age==1)  %>%
  select(-c(tenyr_15age,
            tenyr_15,
            tenyr_05,
            tenyr_15age,
            tenyr_05age,
            set))

# pairs plot:

pairs(RSApars_15age, pch = 18)
ggpairs(RSApars_15age)
ggplot(RSApars_15age, aes(x=off_mA, y=tenyr_growth))+geom_point()

RSApars_15ageX <- RSAparameters %>% 
  filter(tenyr_15age==0)  %>%
  select(-c(tenyr_15age,
            tenyr_15,
            tenyr_05,
            tenyr_15age,
            tenyr_05age,
            set))


KStest_df <- data.frame(par=character(10), D=numeric(10), p=numeric(10), stringsAsFactors=F)
for(j in 1:10){  
  k <- ks.test(RSApars_15age[,j], RSApars_15ageX[,j])
  KStest_df$par[j] <- names(RSApars_15age)[j]
  KStest_df$D[j]       <- k$statistic
  KStest_df$p[j]       <- k$p.value
} 

kable(KStest_df)

# Density Plot & Boxplot comparing pre & post condition distributions. 

RSApars_long <- gather(as.data.frame(RSAparameters) %>% 
                               mutate(set=1:nrow(RSAparameters)) %>% 
                               select(-c(starts_with("tenyr"))), key="par", value = "val", -set) %>%
  mutate(condition = "PRE")

RSApars_15age_long <- gather(as.data.frame(RSApars_15age) %>% 
                          mutate(set=1:nrow(RSApars_15age)) %>% 
                            select(-c(tenyr_growth)), key="par", value = "val", -set) %>%
  mutate(condition = "POST")

RSA_long_comparison <- as.data.frame(rbind(RSApars_long, RSApars_15age_long)) %>%
  mutate(
    condition = factor(condition, levels = c("PRE", "POST")),
    par = factor(par, levels = c("max_yrs_F",
                                  "max_yrs_M",
                                  "min_off", 
                                  "min_repro",
                                  "birth_rate",
                                  "mort_Y",
                                  "mort_A", 
                                  "off_mY",
                                  "off_mA",
                                  "off_f")))

# Density plot
ggplot(RSA_long_comparison, aes(x=val))+
  geom_density(aes(col=condition), linewidth = 1)+
  facet_wrap(~par, scales="free")
# Box plot
ggplot(RSA_long_comparison, aes(x=par,y=val))+
  geom_boxplot(aes(col=condition))+
  facet_wrap(~par, scales="free")


# Age-Sex structure


# for parameters where population is stable over final 10 years
agesex_structure <- RSAoutput_ext %>% 
  filter(tenyr_15age==1) %>%
  select(starts_with("pf")|starts_with("pm")|starts_with(("pF"))) %>%
  gather(key=stat, value=prop) %>%
  mutate(sex = ifelse(stat %in% c("pmKid","pmSub","pmAdu"), "M", "F"))

ggplot(agesex_structure, aes(x=stat, y=prop, col = sex))+geom_boxplot()
agesex_structure %>% group_by(sex, stat) %>% summarise(mean=mean(prop)) %>% kable()

