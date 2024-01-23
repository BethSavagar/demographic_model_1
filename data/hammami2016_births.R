library(readr)
library(tidyverse)
library(knitr)

custom_theme <- theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 9),
        panel.grid.minor = element_blank())
theme_set(custom_theme)
  

hammami2016_demographics <- read_delim("data/pone.0161769.s002.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

hammami2016_demographics %>% View()

hammami_monthBirths <- hammami2016_demographics %>% 
  filter(as.numeric(cyclebir) >= 1989) %>%
  mutate(phasebir = as.numeric(phasebir)) %>% 
  group_by(phasebir) %>% 
  count() %>%
  ungroup() %>%
  mutate(pbirths = n/sum(n)) %>%
  rename("month" = phasebir)

hammami_monthBirths <- hammami2016_demographics %>% 
  filter(as.numeric(cyclebir) >= 1989) %>%
  mutate(month = as.numeric(phasebir)) %>% 
  group_by(cyclebir, month) %>% 
  count() %>%
  ungroup() %>%
  group_by(month) %>%
  summarise(meanMonth = mean(n)) %>%
  ungroup() %>% 
  mutate(propMonth = meanMonth/sum(meanMonth))

sumBirths = sum(hammami_monthBirths$meanMonth)
kable(hammami_monthBirths)

hammami_birthplot <- ggplot(hammami_monthBirths, 
       aes(x=month, y = meanMonth))+
  geom_col() + 
  scale_x_continuous(breaks = 1:12, labels = month.abb )+
  scale_y_continuous(sec.axis = sec_axis(~ . / sumBirths, name = "Proportion births per month"))+
  labs(x="Month", 
       y="Mean births per month",
       # title = "Births per month (1989-1995)",
       # subtitle = "Louga Sheep, Ndiagne district, Senegal"
       )

# filename_hammamiBirths <- "hammami_birthplot.png"
# ggsave(paste0("plots/for-methods/",filename_hammamiBirths), plot = hammami_birthplot, width = 15, height = 10, units = "cm")

birthSeason <- hammami_monthBirths %>%
  mutate(season = ifelse(month %in% c(1,12), "peak", "baseline")) %>%
  group_by(season) %>%
  summarise(pBirths_season = sum(propMonth))

kable(birthSeason)



