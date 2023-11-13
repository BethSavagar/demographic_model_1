## maternal immunity
# compute decay in maternal immunity based on empirical data

library(tidyverse)

# Bodjo: Tested duration of maternal immunity in 112 lambs up to 150 days after birth
bodjo <- data.frame(
  ref = "bodjo",
  day = seq(15,150,15),
  imm = c(0.92,0.893,0.84,0.705,0.32,0.053,0.045,0.045,0.018,0.018))

# based on results calculate weekly decline in maternal immunity, following Bodjo data
bodjo_wk <- bodjo %>% 
  # add wk number
  mutate(wk = round(day/7)) %>%
  # add first entry (with 100% immunity)
  rbind(c("bodjo", 0,1,0)) %>%
  # class of variables
  mutate(day = as.numeric(day),
         imm = as.numeric(imm),
         wk = as.numeric(wk)) %>%
  # set imm for >4months to 0, following consensus of papers
  mutate(imm_data = imm,
         imm = ifelse(wk>17,0,imm_data)) %>%
  arrange(day) %>%
  select(ref, day, imm_data, wk, imm)

imm_decay <- data.frame(
  wk = numeric(),
  imm = numeric()
)

for(i in 2:nrow(bodjo_wk)){
  
  imm_dif <- bodjo_wk[i,"imm"] - bodjo_wk[i-1,"imm"]
  wk_dif <- bodjo_wk[i,"wk"] - bodjo_wk[i-1,"wk"] 
  decay = imm_dif / wk_dif
  
  imm_decay <- imm_decay %>%
    rbind(
      cbind(
        "wk" = c(bodjo_wk[i-1,"wk"]:bodjo_wk[i,"wk"]),
        "imm" = seq(bodjo_wk[i-1,"imm"],bodjo_wk[i,"imm"], by = decay)
      )
    )
}

# correct immune decay so that probability of remaining immune at week 2 = 0.92 etc
# i.e. Previously immunity from week 2-3 = 0.92 retain immunity... should be 92% immune AT week 2..
imm_decay_corrected <- imm_decay %>%
  distinct() 

imm_corrected <- c(1)
for(i in 2:nrow(imm_decay_corrected)){
  imm_prop <- imm_decay_corrected[i,"imm"]/imm_decay_corrected[i-1,"imm"]
  
  if(is.na(imm_prop)){
    imm_prop <- 0
  }
  imm_corrected <- c(imm_corrected, imm_prop)
}

imm_decay_corrected <- imm_decay_corrected %>%
  mutate(imm_corrected = imm_corrected)


# plot of empirical data - red points - from Bodjo, against computed decay - black line + points - for model.
ggplot(imm_decay, aes(x=wk, y=imm))+
  geom_line()+
  geom_point()+
  geom_point(data = bodjo_wk, aes(x=wk, y = imm_data), col="red")

# write.csv(imm_decay_corrected, "data/imm_decay_bodjo_v2.csv", row.names = F)
