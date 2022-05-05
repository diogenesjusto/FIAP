install.packages("caret")
install.packages("dummies")

library(caret)
library(party)
library(dummies)

tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")

mod <- ctree(Survived~as.factor(Sex), data=tit)
plot(mod, type="simple")

# Transformações de dados
tit <- cbind(tit, dummy(tit$Sex))
tit$Sex_F <- as.factor(tit$Sex)
tit$Pclass_F <- as.factor(tit$Pclass)

# Separação Treino e Teste
set.seed(33)
va <- sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]

# Modelo de árvore
mod <- ctree(Survived~Sex_F+Pclass_F, data=treino)

# Previsão em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5,0,1)

# Matriz de Confusão
confusionMatrix(as.factor(prev), as.factor(teste$Survived) )


# Variável Categórica
#  Dummies

