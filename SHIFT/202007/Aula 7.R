# Aula 7 - Clusterização
m <- mtcars
plot(m$mpg ~ m$wt)
m$wt2 <- m$wt*6
plot(m$mpg ~ m$wt2)
set.seed(33)
k <- kmeans(m[,c("mpg", "wt2")],5)
plot(m$mpg ~ m$wt2, col=k$cluster, pch=k$cluster)

# aplicando a "nova" feature em um modelo supervisionado
summary(lm(hp~wt+as.factor(k$cluster), data=m))

###################################
# Regressão Logística
library(e1071)
library(caret)
# carregar os dados do titanic
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/train.csv")
# tratamento de variáveis
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)
t[is.na(t$Age),]$Age <- mean( t[!is.na(t$Age),]$Age )
# separação treino e teste
set.seed(33)
va<-sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]
# Modelagem - Regressão Logística
mod <- glm(Survived~Sex+Age+Pclass, data=treino, family = binomial())
# Validação em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5, 0, 1)
# Matriz de confusão
confusionMatrix(as.factor(prev), as.factor(teste$Survived))

####################
# RANDOM FOREST
library(randomForest)
library(caret)
# carregar os dados do titanic
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/train.csv")
# tratamento de variáveis
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)
t$Survived_f <- as.factor(t$Survived)
t[is.na(t$Age),]$Age <- mean( t[!is.na(t$Age),]$Age )
# separação treino e teste
set.seed(33)
va<-sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]
# Modelagem - RandomForest
set.seed(33)
mod <- randomForest(Survived_f~Sex+Age+Pclass, data=treino, ntree=200)
# Validação em teste
p <- predict(mod, newdata=teste)
# Matriz de confusão
confusionMatrix(p, (teste$Survived_f))

# Calibragem do RandomForest
plot(mod) # análise do erro de aproximação x ntree
# Análise de importância de features
varImpPlot(mod)
# RandomForest opera como Classificador e Regressor
# Basta informa o tipo de variável target
