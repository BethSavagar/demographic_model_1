# lesnoff check

library(readr)
lesnoff_check <- read_csv("data/Applied_parameters/demographics-lesnoff_check.csv")

lesnoff_long <- lesnoff_check %>% 
  filter(parameter %in% 
           c(
             "NET_offtake_y",
             "NET_offtake_m" ,
             "NET_offtake_m2" ,
             "NET_offtake_f"  ,
             "mortality_y" ,
             "mortality_a"  ,
             "birth_rate"
           )) %>%
  gather(key="prof", value = "val", -parameter) %>%
  mutate(profid = gsub(".min", "", prof),
         profid = gsub(".max", "", profid))


ggplot(lesnoff_long %>% filter(profid %in% c("lesnoff.yr", "RSA")), 
       aes(x=parameter, y = val, col = profid), group = profid)+geom_point()+geom_line()
