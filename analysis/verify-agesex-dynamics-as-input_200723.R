# Analyse age-sex population dynamics for valid_as_pars_ pars from ("output/RSA_output/RSA_output_2023-07-12.csv")
# Verify whether stable population dynamics are observed when these parameters are used as inputs....

dynamics_agesex <- read_csv("output/RSA_output/agesex_dynamics_from_120723.csv")

rand <- sample(1:90, 10)

dynamics_agesex_subset <- dynamics_agesex %>% 
  filter(set %in% rand) %>%
  select(w,starts_with("pf"), starts_with("pm"), set) %>%
  gather(key=par, value = prop, -c(set,w)) 


ggplot(dynamics_agesex_subset, aes(x=w,y=prop,group=set,col=set))+
  geom_line()+
  facet_wrap(~par)



dynamics_agesex_growth <- dynamics_agesex %>% 
  filter(set %in% rand) %>%
  select(w, sum_pop, set) %>%
  gather(key=par, value = prop, -c(set,w)) 


ggplot(dynamics_agesex_growth, aes(x=w,y=prop,group=set,col=set))+
  geom_line()


pop_growth <- data.frame(
  "end" = dynamics_agesex %>% filter(w == max(w)) %>% pull(sum_pop),
  "y15" = dynamics_agesex %>% filter(w == 15 * 52) %>% pull(sum_pop)
) %>% mutate(growth = end/y15)


ggplot(pop_growth, aes(x=1:90, y=growth)) + geom_point()
