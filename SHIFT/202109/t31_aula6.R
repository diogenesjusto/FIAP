# Aula 6 - Classificadores - A.D.
# 0. Carga das bibliotecas
library(party)
library(caret)
# 1. Carga
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")
# 2. Tratamento de dados
tit$Sex_f <- as.factor(tit$Sex)
tit$Pclass_f <- as.factor(tit$Pclass)
# 3. Separação Treino e Teste
set.seed(33)
va<-sample(nrow(tit)) # tamanho da base 
treino <-tit[va[1:600],]
teste  <-tit[va[601:891],]
# 4. Modelagem
mod <- ctree(Survived~Sex_f+Pclass_f, data=treino)
plot(mod, type="simple")
# 5. Previsão em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<0.45,0,1)
# 6. Análise do erro de previsão de um classificador
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))






