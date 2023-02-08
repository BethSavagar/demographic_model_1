# load baobab data for testing
library(tidyverse)

source("~/OneDrive - Royal Veterinary College/PPR Collaborations/Data Bank/Baobab_data (Apr2021)/look-at-data.R")

median_rate

off_1 <- 0 # NET offtake rate <12M (per week) - NO trade of animals <12m

int <- median_rate %>% filter(parameter=="hint") %>% pull(rate_wk)
off <- median_rate %>% filter(parameter=="hoff") %>% pull(rate_wk)
off_F <- int-off # NET offtake rate FEMALE >12M (per week) 
off_M <- int-off# NET offtake rate MALE >12M (per week)
mort_1 <- median_rate %>% filter(parameter=="hdea") %>% pull(rate_wk) # natural mortality rate <6M (per week)
mort_2 <- median_rate %>% filter(parameter=="hdea") %>% pull(rate_wk) # natural mortality rate >6M  (per week)
# see https://math.stackexchange.com/questions/1122085/how-to-calculate-a-monthly-mortality-rate for calc of weekly rate from yearly mortality of 0.3, 0.1 respectively.
mort_end <- 1 # natural mortality rate for final age group (per week)
ppr_mort_1 <- 0 # natural mortality rate <6M (per week)
ppr_mort_2 <- 0 # natural mortality rate >6M  (per week)
birth_r <- median_rate %>% filter(parameter=="reprod") %>% pull(rate_wk) # only animals >18M
age_p_f <- c(0,0,0.15,0.15,0.5) # proportion of population in each age group (initial)
age_p_m <- c(0,0,0.1,0.05,0.05)
