library(party)
# carregar os dados do titanic
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/train.csv")
# tratamento de variáveis
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)
# separação treino e teste
set.seed(33)
va<-sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]
# Modelagem
mod <- ctree(Survived~SibSp+Embarked+, data=treino)
plot(mod, type="simple")
# Validação em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5, 0, 1)
# Matriz de confusão
table(prev, teste$Survived)
# Métricas - 1a - Acurácia = [(#acertos+)+(#acertos-)]/#tot
acc <- (54+164)/291
# Métricas - 2a - Precision = (#acertos+)/[(#acertos+)+FP]
precision <- 54/(54+3)
# Métricas - 3a - Recall = (#acertos+)/[(#acertos+)+FN]
recall <- 54/(54+70)

# Métricas de previsão para classificadores
# package: caret
library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))


# Execução
# carregar os dados do titanic
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/train.csv")
# tratamento de variáveis
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)
# separação treino e teste
set.seed(33)
va<-sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]
# Modelagem
mod <- ctree(Survived~SibSp+Embarked+Age+Pclass_f, data=treino)
# Validação em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5, 0, 1)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))
