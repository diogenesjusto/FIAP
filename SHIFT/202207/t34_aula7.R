# Aula 7, penúltima, snif snif
# Clusterização
m <- mtcars
plot(m$mpg~m$wt)

set.seed(33)
k <- kmeans(m[,c("mpg","wt")],12)
plot(m$mpg~m$wt,col=k$cluster, pch=k$cluster)

# Ajustes de escala
install.packages("scales")
library(scales)
m$mpg2 <- rescale(m$mpg,c(0,1))
m$wt2 <- rescale(m$wt,c(0,1))
plot(m$mpg2~m$wt2)

set.seed(33)
k <- kmeans(m[,c("mpg2","wt2")],4)
plot(m$mpg~m$wt,col=k$cluster, pch=k$cluster)

# Uso da "nova variável" em um modelo supervisionado
summary(lm(mpg~wt,data=m))

m$cluster <- as.factor(k$cluster)
summary(lm(mpg~wt+cluster,data=m))

#################################################
install.packages("e1071")
# Regressão Logística
library(caret)
library(e1071)

tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")

# Transformações de dados
tit <- cbind(tit, dummy(tit$Sex))
tit$Sex_F <- as.factor(tit$Sex)
tit$Pclass_F <- as.factor(tit$Pclass)
MeanAge <- mean(tit[!is.na(tit$Age),]$Age)
tit[is.na(tit$Age),]$Age <- MeanAge

# Separação Treino e Teste
set.seed(33)
va <- sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]

# Modelo de Regr Logística
mod <- glm(Survived~Sex_F+Pclass_F+Age, data=treino,family=binomial())

# Previsão em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5,0,1)

# Matriz de Confusão
confusionMatrix(as.factor(prev), as.factor(teste$Survived) )


#################################################
install.packages("randomForest")
# Random Forest
library(caret)
library(randomForest)

tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")

# Transformações de dados
tit <- cbind(tit, dummy(tit$Sex))
tit$Sex_F <- as.factor(tit$Sex)
tit$Pclass_F <- as.factor(tit$Pclass)
MeanAge <- mean(tit[!is.na(tit$Age),]$Age)
tit[is.na(tit$Age),]$Age <- MeanAge
tit$Survived_F <- as.factor(tit$Survived)

# Separação Treino e Teste
set.seed(33)
va <- sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]

# Modelo Random Forest
set.seed(33)
mod <- randomForest(Survived_F~Sex_F+Pclass_F+Age, data=treino,ntree=400)

# Previsão em teste
p <- predict(mod, newdata=teste)
prev <- p

# Matriz de Confusão
confusionMatrix(as.factor(prev), as.factor(teste$Survived) )

# Análise/ajustes no RandomForest
plot(mod)
varImpPlot(mod)

#################################################
# SVM
library(caret)
library(e1071)

tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")

# Transformações de dados
tit <- cbind(tit, dummy(tit$Sex))
tit$Sex_F <- as.factor(tit$Sex)
tit$Pclass_F <- as.factor(tit$Pclass)
MeanAge <- mean(tit[!is.na(tit$Age),]$Age)
tit[is.na(tit$Age),]$Age <- MeanAge
tit$Survived_F <- as.factor(tit$Survived)

# Separação Treino e Teste
set.seed(33)
va <- sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]

# Modelo SVM
mod <- svm(Survived_F~Sex_F+Pclass_F+Age, kernel="linear", data=treino)

# Previsão em teste
p <- predict(mod, newdata=teste)
prev <- p

# Matriz de Confusão
confusionMatrix(as.factor(prev), as.factor(teste$Survived) )

# Análise/ajustes no RandomForest
plot(mod)
varImpPlot(mod)
