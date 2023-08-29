# demographic_model_1
Repository for first model of population demographics. Including matrix model and discrete compartmental model for age structured population

testing if git is synced 19-Jan-23


## Branches
- Master: master branch with only tested, clean working code on it
- model_dev_1: updated with code as of 26/04
    - all preliminary demographic model testing is contained on this branch
    - original pop structure: kid, you, juv, sub, adu compartments included
    - In RSA_dev branches the age-sex structure is reduced to Kid, Sub, Adu, and is sex structured
    
 - RSA : branch for regional sensitivity analysis testing
    - This also contains streamlined model code
    - Age-sex compartments of Kid, Sub, Adu only
    - simplified associated .csv files (fix_input.csv, var_input.csv)

# Update Fri 2 Jun

- RSA_test.Rmd has parameters for 10000 LHS, 25year simulation, used fro analysis of 10 year pop growth
- analysis include adding condition of population age-sex structure
- RSA_analysis_1 : contains analysis of population with growht 0.95-1/05 over entire simulation
- RSA_analysis_2: has analysis of population with growth 0.95-1.05 in last TEN YEARS