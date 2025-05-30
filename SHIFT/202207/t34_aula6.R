# Aula 6 - avalia��o de classificadores 
install.packages("caret")

###########################################
# Teoria de Probabilidades - Arvores de decis�o
#0. Carga de pacotes
library(party)
library(caret)
#1. Carga de dados
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")
#2. Transforma��es
tit$Pclass_F = as.factor(tit$Pclass)
tit$Sex_F = as.factor(tit$Sex)
tit$Embarked_F = as.factor(tit$Embarked)
#3. Separa��o Treino e Teste (split)
set.seed(33)
va<-sample(nrow(tit))
treino<-tit[va[1:600],]
teste <-tit[va[601:891],]
#4. Constru��o modelo: �rvore de Decis�o
mod <- ctree(Survived~Pclass_F+Sex_F+Age, data=treino)
plot(mod, type='simple')
#5. Previs�o em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.45,0,1) # Ponto de corte => Threeshold
real <- teste$Survived
#6. Avalia��o de Modelos de classifica��o
caret::confusionMatrix(as.factor(prev), as.factor(real))

## An�lise de Probabilidade Esperada
hist(p)
table(prev,real)

# Vari�vel categ�rica
class(tit$Sex)
class(tit$Sex_F)
levels(tit$Sex_F)
tit$Sex_F
