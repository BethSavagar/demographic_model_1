
# immunity 6m
colnames(dat.2)

growth_plot <- dat.1 %>% gather(key="par",value="val",-pop_growth)
ggplot(growth_plot, aes(x=val, y=pop_growth))+
  geom_point()+
  facet_wrap(~par, scales = "free")


imm6m_plot <- dat.2 %>% gather(key="par",value="val",-imm_6m)
ggplot(imm6m_plot, aes(x=val, y=imm_6m))+
  geom_point()+
  facet_wrap(~par, scales = "free")

####

imm6m_plot <- dat.1b %>% gather(key="par",value="val",-imm_6m)
ggplot(imm6m_plot, aes(x=val, y=imm_6m))+
  geom_point()+
  facet_wrap(~par, scales = "free")


imm12m_plot <- dat.2b %>% gather(key="par",value="val",-imm_12m)
ggplot(imm12m_plot, aes(x=val, y=imm_12m))+
  geom_point()+
  facet_wrap(~par, scales = "free")

imm70_plot <- dat.3b %>% gather(key="par",value="val",-imm70_w)
ggplot(imm70_plot, aes(x=val, y=imm70_w))+
  geom_point()+
  facet_wrap(~par, scales = "free")+
  stat_cor(method = "pearson")


# growthb_plot <- dat.4b %>% gather(key="par",value="val",-tenyr_growth)
# ggplot(growthb_plot, aes(x=val, y=tenyr_growth))+
#   geom_point()+
#   facet_wrap(~par, scales = "free")+
#   stat_cor(method = "pearson")


# dat.b.master <- rbind(imm6m_plot %>% 
#                         rename(imm_val = imm_6m) %>% 
#                         mutate(imm = "imm6m"),
#                       imm12m_plot %>% 
#                         rename(imm_val = imm_12m) %>% 
#                         mutate(imm = "imm12m"),
#                       imm70_plot %>% 
#                         rename(imm_val = imm70_w) %>% 
#                         mutate(imm = "imm70"))
# 
# ggplot(dat.b.master, aes(x=val,y=imm_val))+
#   geom_point()+
#   facet_grid(par~imm, scales = "free")

