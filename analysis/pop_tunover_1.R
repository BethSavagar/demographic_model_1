# Turnover...


pop_turnover <- pR_noIm_df %>% 
  as.data.frame() %>%
  mutate(set=1:nrow(pR_noIm_df)) %>%
  gather(key="time", value = "pR_noIm", -set) %>% 
  arrange(set) %>%
  mutate(time=rep(1:TimeStop_dynamics, lhs_n)) 

mean_turnover <- pop_turnover %>%
  group_by(time) %>%
  summarise(pR_noIm = mean(pR_noIm)) %>%
  mutate(set = "mean")

immunity_dynamics <- pop_turnover %>%
  rbind(mean_turnover)

immdyn_subset <- immunity_dynamics # %>% filter(set %in% 1:1000)

ggplot(immdyn_subset, aes(x=time/52, y=pR_noIm, group = as.factor(set)))+
  geom_line(col = "grey")+
  geom_line(data = immunity_dynamics %>% filter(set =="mean"), aes(x=time/52, y=pR_noIm), col = "black")+
  labs(y = "Proportion Immune (no offspring)", x = "Time (yrs)")+
  coord_cartesian(xlim = c(0,15))+
  theme_bw()

turnover_time <- pop_turnover %>%
  group_by(set) %>%
  filter(pR_noIm==0) %>%
  summarise(turnover_time = min(time))

pR_below70 <- pop_turnover %>%
  group_by(set) %>%
  filter(pR_noIm<0.7) %>%
  summarise(turnover_time = min(time))

pR_decay <- rbind(turnover_time %>% mutate(immunity = "0"),
                  pR_below70 %>% mutate(immunity = "70%"))

ggplot(turnover_time, aes(x=set, y=turnover_time))+geom_point()

ggplot(turnover_time, aes(y=turnover_time))+geom_boxplot()

ggplot(pR_decay, aes(y=turnover_time, col = immunity))+
  geom_boxplot(size = 1)+
  facet_wrap(~immunity, scales = "free", ncol = 1)+
  labs(y = "Time (wks)")+
  theme_bw()+
  coord_flip()

ggplot(pR_decay, aes(x=immunity,y=turnover_time, col = immunity))+
  geom_boxplot(size = 1)+
  facet_wrap(~immunity, scales = "free", ncol = 1)+
  labs(x = "Proprtion Immune", y = "Time (wks)")+
  theme_bw()+
  coord_flip()


######################################
######################################
## Turnover for pops iwth 10yr growth


# - TENYR: subset populations are stable i.e. with TENYR growth of +/- 5% (final pop: ~50)


tenyr_growth <- RSAoutput %>%
  filter(tenyr_growth>=0.95, tenyr_growth <=1.05)

tenyr_set <- tenyr_growth %>%
  pull(set)

tenyr_pars <-  var_input_set[tenyr_set,]

tenyr_immunity <- pR_noIm_df[tenyr_set, ]


pop_turnover_10y <- tenyr_immunity %>% 
  as.data.frame() %>%
  mutate(set=1:nrow(tenyr_immunity)) %>%
  gather(key="time", value = "pR_noIm", -set) %>% 
  arrange(set) %>%
  mutate(time=rep(1:TimeStop_dynamics, nrow(tenyr_immunity))) 

mean_turnover_10y <- pop_turnover_10y %>%
  group_by(time) %>%
  summarise(pR_noIm = mean(pR_noIm)) %>%
  mutate(set = "mean")

immunity_dynamics_10y <- pop_turnover_10y %>%
  rbind(mean_turnover_10y)

immdyn_subset_10y <- immunity_dynamics_10y # %>% filter(set %in% 1:1000)

ggplot(immdyn_subset_10y, aes(x=time/52, y=pR_noIm, group = as.factor(set)))+
  geom_line(col = "grey")+
  geom_line(data = immunity_dynamics_10y %>% filter(set =="mean"), aes(x=time/52, y=pR_noIm), col = "black")+
  labs(y = "Proportion Immune (no offspring)", x = "Time (yrs)")+
  coord_cartesian(xlim = c(0,15))+
  theme_bw()

turnover_time_10y <- pop_turnover_10y %>%
  group_by(set) %>%
  filter(pR_noIm==0) %>%
  summarise(turnover_time = min(time))

pR_below70_10y <- pop_turnover_10y %>%
  group_by(set) %>%
  filter(pR_noIm<0.7) %>%
  summarise(turnover_time = min(time))

pR_decay_10y <- rbind(turnover_time_10y %>% mutate(immunity = "0"),
                  pR_below70_10y %>% mutate(immunity = "70%"))

ggplot(turnover_time_10y, aes(x=set, y=turnover_time))+geom_point()

ggplot(turnover_time_10y, aes(y=turnover_time))+geom_boxplot()

ggplot(pR_decay_10y, aes(y=turnover_time, col = immunity))+
  geom_boxplot(size = 1)+
  facet_wrap(~immunity, scales = "free", ncol = 1)+
  labs(y = "Time (wks)")+
  theme_bw()+
  coord_flip()

ggplot(pR_decay_10y, aes(x=immunity,y=turnover_time, col = immunity))+
  geom_boxplot(size = 1)+
  facet_wrap(~immunity, scales = "free", ncol = 1)+
  labs(x = "Proprtion Immune", y = "Time (wks)")+
  theme_bw()+
  coord_flip()

