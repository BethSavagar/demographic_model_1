
# Flock Dynamics Births:



# ---------------------
## ORIGINAL - variable ## 

# Within a given timestep: 

# 1. Update weekly birth rate: 
# "birth": a vector containing the weekly birth rate for females. The vector length = max age F but is set to 0 for females < min age reproduction.
# seasonal:
if(seasonal & # seasonal is a logical which identifies whether model simualtes seasonality or not
   w>= t2/2){ # w>= t2/2 implements seasonal births from this timepoint (1/4 of the full time)
  if(w %in% birthpeak_w){ # birthpeak_w is a vector which contains the weeks of birth peaks (over the full simulation period i.e. 1, 53...)
    birth <- birthH # birthH: vector containing the weekly birth rate in peak weeks. 
  }else{
    birth <- birthL # birthL: vector contains the weekly birth rate in non-peak weeks. 
  }
}else{
  birth <- birthW  # birthW: vector  containing the weekly birth rate when no seasonality. 
}
# 2. Simulate births:
# Immune births:
Im_births <- sum(birth* Imm_b* fR_prev) #Imm_b is proportion kids that gain maternal immunity, fR_prev is a vector containing the number of recovered females in each weeklong agegroup
# Susceptible births:
S_births <- sum(birth*(1-Imm_b)* fR_prev) + sum(birth*fS_prev) # as above by fS_prev is a vector containing the number of susceptible females in each weeklong agegroup




# ---------------------
## BIRTHS - fixed ## 

# for fixed births, instead of birth vector containing birth rate for females in each age group...
# births is the number of births per week>>>

# NON-SEASONAL: 
# birthsW = number of births per week 
# - the number of births per week
# - calculated from initial adult female population (>12m) * annual birth rate divided by 52 for weekly births)

# SEASONAL:
# birthsHw 
# - number of births per week during a peek week 
# - calculated at beginning of the simulation: nbirths-per-year* proportion-births-in-peak / nweeks-in-peak
# birthsLw 
# - number of births during low week
# - calculated: nbirths-per-year* proportion-births-in-low / nweeks-in-low

# Set births per week:

if(seasonal & w>= t2/2){ 
  if(w %in% birthpeak_w){ # birthpeak_w is a vector which contains the weeks of birth peaks (over the full simulation period i.e. 1, 53...)
    births <- birthsHw # birthsHw: no. births in peak week
  }else{
    births <- birthsLw # birthsHw: no. births in low week. 
  }
}else{
  births <- birthsW  # birthsW: no. births per week with no seasonality:
}

pfR = (sum(fR_prev)/sum(fRprev+fSprev))
pfS = (sum(fS_prev)/sum(fRprev+fSprev))

Im_births <- birthsw* Imm_b* pfR # immune births, pfR is proportion of females in Immune (R) state
S_births <- birthsw-Im_births # susceptible births = total births per week - immune births per week