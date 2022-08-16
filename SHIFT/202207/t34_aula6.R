# Aula 6 - avaliação de classificadores 
install.packages("caret")

###########################################
# Teoria de Probabilidades - Arvores de decisão
#0. Carga de pacotes
library(party)
library(caret)
#1. Carga de dados
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")
#2. Transformações
tit$Pclass_F = as.factor(tit$Pclass)
tit$Sex_F = as.factor(tit$Sex)
tit$Embarked_F = as.factor(tit$Embarked)
#3. Separação Treino e Teste (split)
set.seed(33)
va<-sample(nrow(tit))
treino<-tit[va[1:600],]
teste <-tit[va[601:891],]
#4. Construção modelo: Árvore de Decisão
mod <- ctree(Survived~Pclass_F+Sex_F+Age, data=treino)
plot(mod, type='simple')
#5. Previsão em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.45,0,1) # Ponto de corte => Threeshold
real <- teste$Survived
#6. Avaliação de Modelos de classificação
caret::confusionMatrix(as.factor(prev), as.factor(real))

## Análise de Probabilidade Esperada
hist(p)
table(prev,real)

# Variável categórica
class(tit$Sex)
class(tit$Sex_F)
levels(tit$Sex_F)
tit$Sex_F
