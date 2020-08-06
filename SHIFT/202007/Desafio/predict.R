baba <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/202007/Desafio/train_BABA.csv")

treino <- baba[1:365,]
teste  <- baba[366:396,]

mod <- lm(venda~outmg, data=treino)

p<-predict(mod, newdata=teste)

df <- as.data.frame(cbind(1:31,p))
names(df)<-c("id","venda")

write.table(df, 'predict.csv', row.names = FALSE, col.names = FALSE, sep=",")

baba$data <- as.Date(as.character(baba$date), "%Y%m%d")
