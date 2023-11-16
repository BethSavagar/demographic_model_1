library(tidyverse)
library(knitr)
library(ggpubr)

# ANALYSIS OF LESNOFF LHS PARAMETER SETS:

lesnoff_ranges <- var_demo_data_full %>% 
  select(parameter,
         lesnoff.yr.min,
         lesnoff.yr.max,
         RSA.min,
         RSA.max) %>% 
  gather(key = "datasource", value = "value", -parameter) %>%
  mutate(datasource = gsub("\\.min|\\.max","", datasource))

L1 <- ggplot(lesnoff_ranges %>% filter(parameter %in% c("adu_f_max_yrs", "adu_m_max_yrs","min_age_offtake","min_age_repro")), aes(x=parameter, y=value, col = datasource))+
  geom_point(position=position_dodge(width=0.2))+
  geom_line(size = 1, position=position_dodge(width=0.2))+
  labs(x = "Parameter")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        text = element_text(size=16))

L2 <- ggplot(lesnoff_ranges %>% filter(!parameter %in% c("adu_f_max_yrs", "adu_m_max_yrs","min_age_offtake","min_age_repro", "ppr_mortality_y", "ppr_mortality_a")), 
       aes(x=parameter, y=value, col = datasource))+
  geom_point(position=position_dodge(width=0.2))+
  geom_line(size = 1, position=position_dodge(width=0.2))+
  labs(x = "Parameter")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size=16))

ggarrange(L1,L2)

################################
## POP & AGE-SEX DYNAMICS ##
################################

# Out is a list of df which track full population dynamics for 1000 (lhs_n) parameter combinations over 20y periods

# Sample for plotting
dyn_sample <- sample(1:lhs_n, 10, replace = F)

# Transform Out (list) into a df of dynamics with each set identified:
Out_dynamics <- Out %>%
  do.call(rbind, .) %>% 
  mutate(set = rep(1:lhs_n, each = TimeStop_dynamics))

# Population Growth plot:
growth_dynplot <- ggplot(Out_dynamics %>% filter(set %in% dyn_sample), 
       aes(x = w, y = sum_pop, group = set)) + 
  labs(x="weeks", 
       y = "Population Size",
       title = "Population growth over 20 years, Lesnoff, (LHS Parameter Sets)")+
  geom_line()+
  theme_bw()

# AgeSex Dynamics df: 
Out_agesex <- Out_dynamics %>% 
  select(c("w", "set", starts_with("pf"), starts_with("pm"))) %>% 
  gather(key = "par", value = value, -c(w,set)) %>%
  mutate(par = factor(par, levels = c("pfAdu", "pfSub", "pfKid", 
                                         "pmAdu","pmSub","pmKid",
                                         "pF")))

# Age-Sex Plot:
agesex_dynplot <- ggplot(Out_agesex %>% filter(set %in% dyn_sample), 
       aes(x = w, y = value, group = set, col = par)) + 
  geom_line()+
  facet_wrap(~par)+
  labs(x="Weeks", 
       y = "Proportion",
       title = "Age-Sex dynamics, Lesnoff, (LHS Parameter Sets)")+
  theme_bw()

growth_dynplot
agesex_dynplot
ggarrange(growth_dynplot, agesex_dynplot, ncol = 1)

################################
## POP & AGE-SEX SUMMARY ##
################################

# Out2 is a dataframe of summary data for each parameter set 

## Plot population growth stats: 
# - Total Growth (20y), 
# - Midyr Growth (10y), 
# - Finyr Growth (1y)

# df of population growth stats:
popgrowth <- Out2 %>% 
  select(ends_with("growth")) %>% 
  gather(key = "par", value = "prop")

growth_summplot <- ggplot(popgrowth, aes(x=par, y = prop))+
  geom_boxplot()+
  facet_wrap(~par, scales = "free")+
  labs(x = "Growth Stat", 
       y = "Pop Growth",
       title = "Growth statistics for Lesnoff data, LHS parameter sets")+
  theme_bw()
growth_summplot

finyr_summplot <- ggplot(Out2, aes(finyr_growth))+
  geom_histogram(colour = "black",
                 alpha = 0.5)+
  geom_vline(xintercept = 1)+
  labs(x="Final year growth", 
       y = "Count", 
       title = "Distribution of final year growth, Lesnoff, (LHS paremeter sets)")+
  theme_bw()

finyr_summplot

## twoyr growth
twoyr_growth <- sapply(Out, function(x)
  x[nrow(x),"sum_pop"] / x[(nrow(x)-(2*52)), "sum_pop"]
)

Out2 <- Out2 %>%
  mutate(twoyr_growth = replace_na(twoyr_growth, 0))

twoyr_summplot <- ggplot(Out2, aes(twoyr_growth))+
  geom_histogram(colour = "black",
                 alpha = 0.5)+
  geom_vline(xintercept = 1)+
  labs(x="Two year growth", 
       y = "Count", 
       title = "Distribution of final TWO year growth, Lesnoff, (LHS paremeter sets)")+
  theme_bw()

twoyr_boxplot <- ggplot(Out2, aes(y = twoyr_growth))+
  geom_boxplot()+
  labs(x= "", 
       y = "Two year growth", 
       title = "Final TWO year growth, Lesnoff, (LHS paremeter sets)")+
  theme_bw()

twoyr_summplot

twoyr_boxplot

summary(Out2 %>% filter(twoyr_growth>0) %>% select(twoyr_growth)) %>% kable()


## Analyse age-sex dynamics:

# - Age-Sex plots
agesex <- Out2 %>% 
  select(starts_with("pf"), starts_with("pm")) %>% 
  gather(key = "par", value = "prop")

agesex_summplot <- ggplot(agesex, aes(x=par, y = prop))+
  geom_boxplot(fill = "grey", 
               alpha = 0.5)+
  labs(x="Age-Sex Group", 
       y = "Proportion",
       title = "Stable Age-Sex structure, Lesnoff, (LHS parameter sets)")+
  theme_bw()
  
agesex_summplot

ggarrange(growth_summplot, finyr_summplot, agesex_summplot, ncol = 1)

#------------------------------------------------------------------------------
## VALID RESULTS ##



##################################
## Valid Age-Sex Conditions ##
##################################

## Min and max proportions for each sex-age group (see age-sex SS)
pfKid.min <- 0.05; pfKid.max <- 0.19
pfSub.min <- 0.06; pfSub.max <- 0.19
pfAdu.min <- 0.21; pfAdu.max <- 0.62

pmKid.min <- 0.05; pmKid.max <- 0.16
pmSub.min <- 0.04; pmSub.max <- 0.15
pmAdu.min <- 0.01; pmAdu.max <- 0.15
  

#########################################################
# Retain behavioral parameter sets
#########################################################

# add set id to output df:
Out2 <- Out2 %>%
  mutate(set = 1:nrow(Out2))

# Edit Out to contain identifier variables for pop growth (5%,15%) and pop growth with age-sex structure 
Out_ext <- Out2 %>%
  mutate(midyr_growth = replace_na(midyr_growth, 0),
         # add variables to identify parameter sets with growth between 5% and 15%
         midyr_15 = ifelse(midyr_growth>=0.85 & midyr_growth <=1.15, 1, 0),
         midyr_05 = ifelse(midyr_growth>=0.95 & midyr_growth <=1.05, 1, 0)) %>%
  
  # add variables to identify parameter sets with growth between 5% and 15% AND age-sex conditions
  mutate(
    midyr_15age = ifelse(
      midyr_15 == 1 &
        pfKid >= pfKid.min & pfKid <= pfKid.max &
        pfSub >= pfSub.min & pfSub <= pfSub.max &
        pfAdu >= pfAdu.min & pfAdu <= pfAdu.max &
        
        pmKid >= pmKid.min & pmKid <= pmKid.max &
        pmSub >= pmSub.min & pmSub <= pmSub.max &
        pmAdu >= pmAdu.min & pmAdu <= pmAdu.max,1,0),
    
    midyr_05age = ifelse(
      midyr_05 == 1 &
        pfKid >= pfKid.min & pfKid <= pfKid.max &
        pfSub >= pfSub.min & pfSub <= pfSub.max &
        pfAdu >= pfAdu.min & pfAdu <= pfAdu.max &
        
        pmKid >= pmKid.min & pmKid <= pmKid.max &
        pmSub >= pmSub.min & pmSub <= pmSub.max &
        pmAdu >= pmAdu.min & pmAdu <= pmAdu.max,1,0)
  )

Out_ext %>% group_by(midyr_15age, midyr_05age) %>% count() %>% kable()

##################
## Valid Output ##
##################

Out_valid <- Out_ext %>%
  filter(midyr_15age == 1 |
           midyr_05age == 1)

## Refine parameter dataframe:
Out_parameters <- var_input_backup %>%
  mutate(set = 1:nrow(var_input_backup)) %>%
  left_join(Out_ext %>%
              select(midyr_growth,
                     midyr_15,
                     midyr_05,
                     midyr_15age,
                     midyr_05age,
                     set), by = c("set"))

pars_valid <- Out_parameters %>% 
  filter(midyr_15age == 1 |
           midyr_05age == 1)  %>%
  select(-c(midyr_15,
            midyr_05,
            set))

library(GGally)
pairs(pars_valid %>% select(-c(midyr_growth,midyr_05age,midyr_15age)))




