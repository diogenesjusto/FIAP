##############################################################################
# CRÉDITOS: Fernando Rocha, Kauê Ribeiro Camilo e Reinaldo Pereira Piveta    #
##############################################################################

baba <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/desafio/baba/BABA.csv")
 
# TRATAMENTO DOS DADOS
baba$weekday_z <- ifelse(baba$weekday=="domingo" | baba$weekday=="sabado", 1 , 0 ) 
baba <- cbind(baba,dummy(baba$mes))
 
# ANÁLISE CORRELAÇÃO
cor(treino$venda, treino$weekday_z)
cor(treino$venda, treino$margem)
cor(treino$venda, treino$desconto)
cor(treino$venda, treino$babaagosto)
 
# SEPARAÇÃO DAS BASES
treino <- baba[1:365,]
teste <- baba[366:396,]
 
# MODELO DE REGRESSÃO MULTIVARIADA
mod1 <- lm(venda ~ weekday_z + margem + desconto + babaagosto, data=treino)
summary(mod1)
 
# PREVISÃO
p<-predict(mod1, newdata=teste)
 
# SALVAR ARQUIVO
df <- as.data.frame(cbind(1:31,p))
names(df)<-c("id","venda")
write.table(df, 'predict2.csv', row.names = FALSE, col.names = FALSE, sep=",")
