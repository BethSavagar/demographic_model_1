library(tidyverse)
library(knitr)
library(ggpubr)

dyn_sample <- sample(1:lhs_n, 10, replace = F)


Out_dynamics <- Out %>%
  do.call(rbind, .) %>% 
  mutate(set = rep(1:lhs_n, each = TimeStop_dynamics))

ggplot(Out_dynamics %>% filter(set %in% dyn_sample), aes(x = w, y = sum_pop, group = set)) + 
  geom_line()

Out_agesex <- Out_dynamics %>% 
  select(c("w", "set", starts_with("pf"), starts_with("pm"))) %>% 
  gather(key = "par", value = value, -c(w,set))

ggplot(Out_agesex %>% filter(set %in% dyn_sample), aes(x = w, y = value, group = set, col = par)) + 
  geom_line()+
  facet_wrap(~par)


### out 2


## Analyse population growth:

popgrowth <- Out2 %>% 
  select(ends_with("growth")) %>% 
  gather(key = "par", value = "prop")

ggplot(popgrowth, aes(x=par, y = prop))+
  geom_boxplot()

ggplot(Out2, aes(finyr_growth))+
  geom_histogram()+
  geom_vline(xintercept = 1)


## Analyse age-sex dynamics:
# - Age-Sex plots

agesex <- Out2 %>% 
  select(starts_with("pf"), starts_with("pm")) %>% 
  gather(key = "par", value = "prop")

ggplot(agesex, aes(x=par, y = prop))+geom_boxplot()
  
  
## Do any meet original profile criteria:


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
ggpairs(pars_valid %>% select(-c(midyr_growth,midyr_05age,midyr_15age)))




