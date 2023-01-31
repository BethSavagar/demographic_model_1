## maternal immunity

bodjo <- data.frame(
  ref = "bodjo",
  day = seq(15,150,15),
  imm = c(0.92,0.893,0.84,0.705,0.32,0.053,0.045,0.045,0.018,0.018))

bidjeh <- data.frame(
  ref = "bidjeh",
  day = c(30,120,150,180),
  imm = c(0.885,0.613,0.405,0.077)
)

Markus <- data.frame(
  ref = "markus",
  day = c(28,42,56,70,84),
  imm = c(1,0.833,0.5,0.167,0)
)

mat_imm <- 
  as.data.frame(rbind(bodjo,
                      bidjeh,
                      Markus))
ggplot(mat_imm, aes(x=day, y = imm, col = ref))+geom_line()
