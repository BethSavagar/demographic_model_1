library(readr)
library(tidyverse)
library(knitr)

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

sumBirths = sum(hammami_monthBirths$n)
kable(hammami_monthBirths)

ggplot(hammami_monthBirths, 
       aes(x=month, y = n))+
  geom_col() + 
  scale_x_continuous(breaks = 1:12, labels = month.abb )+
  scale_y_continuous(sec.axis = sec_axis(~ . / sumBirths, name = "Birth Proportion"))+
  theme_minimal()+
  labs(x="Month", 
       y="Birth Count",
       title = "Births per month (1989-1995)",
       subtitle = "Louga Sheep, Ndiagne district, Senegal")

birthSeason <- hammami_monthBirths %>%
  mutate(season = ifelse(month %in% c(1,12), "peak", "baseline")) %>%
  group_by(season) %>%
  summarise(pBirths_season = sum(pbirths))

kable(birthSeason)



