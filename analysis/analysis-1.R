# analysis and plots of demographic model


## plotting and analysis

summary_df <- summary_df %>%
  mutate(w = as.numeric(w))

ggplot(summary_df,aes(x=w,y=sum_pop))+
  geom_line(size = 1.5)+
  labs(title = "Total Population",
       x="time (weeks)", 
       y = "total population")+
  theme_bw()


## melt summary_df to long format
summary_long <- summary_df %>%
  gather(key="stat", value="prop", -w)


## create df for different stats:

colors_agesex <- RColorBrewer::brewer.pal(6,"Set1")
colors_agesex[6] <- "black"
  
# age-sex
summary_agesex <- summary_long %>%
  filter(!stat %in% c("sum_pop",
                      "prop_inf",
                      "prop_immune")) %>%
  mutate(stat = ordered(stat, 
                        levels = c("pKid", "pYou", "pJuv", "pSub", "pAdu", "pF")))


## Plots:
# age-sex proportions
ggplot(summary_agesex, aes(x=w, y=prop, group=stat, col=stat))+
  geom_line(size=1)+
  # scale_color_brewer(palette = "Set1")+
  scale_color_manual(values = colors_agesex)+
  labs(x = "weeks", y = "Proportion of total population", col = "age-sex cat")+
  theme_bw()

# immunity
ggplot(summary_df, aes(x=w, y=prop_immune))+
  geom_line()