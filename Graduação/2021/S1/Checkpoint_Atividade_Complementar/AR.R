
fat <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/Checkpoint_Atividade_Complementar/faturamento.csv")

fat2 <- fat[fat$loja=="Loja 1",]
fat3 <- fat2[13:396,]
fat3$GMV1 <- fat2[12:395,]$GMV
fat3$GMV2 <- fat2[11:394,]$GMV
fat3$GMV3 <- fat2[10:393,]$GMV
fat3$GMV4 <- fat2[9:392,]$GMV
fat3$GMV5 <- fat2[8:391,]$GMV
fat3$GMV6 <- fat2[7:390,]$GMV
fat3$GMV7 <- fat2[6:389,]$GMV
fat3$GMV8 <- fat2[5:388,]$GMV
fat3$GMV9 <- fat2[4:387,]$GMV
fat3$GMV10 <- fat2[3:386,]$GMV
fat3$GMV11 <- fat2[2:385,]$GMV
fat3$GMV12 <- fat2[1:384,]$GMV

View(fat3)
write.csv("faturamento3.csv")