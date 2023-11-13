#######################
## Vaccination Setup ##
#######################

## GSCE Vaccination: 

# vaccination starts at halfway point of simulation
V1 <- 0.5*TimeStop_dynamics # first vaccination campaign
V_rounds <- 4 # number of campaigns (GSCE recommended)
# add variable: V_schedule <- "ann", "bi"
V_breaks <- 52 # timsteps between campaigns: 1 year gaps with 1 week timestep

V_starts <- vector(length=V_rounds)
for(v in 1:V_rounds){
  V_starts[v] <- V1+(v-1)*V_breaks
}

V_age_min <- round(4 *4.345,0) # 4 months (4.345 weeks per month)
V_age_max <- round(12*4.345,0) # 12 months (4.345 weeks per month) for partial campaigns


Vprog <- data.frame(
  Vround = 1:V_rounds,
  Vtype = c("full","full","partial", "partial"),
  Vweek = V_starts,
  Vmin = V_age_min,
  Vmax = c(NA, NA, V_age_max, V_age_max),
  pV = rep(pV, V_rounds)
)
