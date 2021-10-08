# Aula 7 - Clusterização
m <- mtcars
plot(m$mpg ~ m$wt)
set.seed(33)
k <- kmeans(m[,c("mpg","wt")],8)
plot(m$mpg ~ m$wt, col=k$cluster, pch=k$cluster)

# Normalização ou ajuste de escala
m$wt6 <- m$wt*6
plot(m$mpg ~ (m$wt6))

set.seed(33)
k <- kmeans(m[,c("mpg","wt6")],4)
plot(m$mpg ~ m$wt, col=k$cluster, pch=k$cluster)

# Pacote Rescale
install.packages("scales")
library(scales)
m$mpg_s <- rescale(m$mpg, c(0,1))
m$wt_s <- rescale(m$wt, c(0,1))

set.seed(33)
k <- kmeans(m[,c("mpg_s","wt_s")],4)
plot(m$mpg_s ~ m$wt_s, col=k$cluster, pch=k$cluster)

# Incorporando cluster em um mod pred
m$cluster = as.factor(k$cluster)

summary(lm(mpg~wt, data=m))
summary(lm(mpg~wt+cluster, data=m))

# Outros algoritmos

# Regressão Logística
# 0. Carga das bibliotecas
library(e1071)
library(caret)
# 1. Carga
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")
# 2. Tratamento de dados
tit$Sex_f <- as.factor(tit$Sex)
tit$Pclass_f <- as.factor(tit$Pclass)
tit[is.na(tit$Age),]$Age <- mean(tit[!is.na(tit$Age),]$Age)

# 3. Separação Treino e Teste
set.seed(33)
va<-sample(nrow(tit)) # tamanho da base 
treino <-tit[va[1:600],]
teste  <-tit[va[601:891],]
# 4. Modelagem = Regressão Logística =>GLM,family = binomial()
mod <- glm(Survived~Sex_f+Pclass_f+Age, data=treino, family=binomial())
# 5. Previsão em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<0.35,0,1)
# 6. Análise do erro de previsão de um classificador
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))

# Random Forest
# 0. Carga das bibliotecas
library(randomForest)
library(caret)
# 1. Carga
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")
# 2. Tratamento de dados
tit$Survived_f <- as.factor(tit$Survived)
tit$Sex_f <- as.factor(tit$Sex)
tit$Pclass_f <- as.factor(tit$Pclass)
tit[is.na(tit$Age),]$Age <- mean(tit[!is.na(tit$Age),]$Age)

# 3. Separação Treino e Teste
set.seed(33)
va<-sample(nrow(tit)) # tamanho da base 
treino <-tit[va[1:600],]
teste  <-tit[va[601:891],]
# 4. Modelagem = RandomForest
set.seed(33)
mod <- randomForest(Survived_f~Sex_f+Pclass_f+Age, data=treino, ntree=200)
plot(mod)
varImpPlot(mod)
# 5. Previsão em teste
p <- predict(mod, newdata=teste)
prev <- p
# 6. Análise do erro de previsão de um classificador
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))


# SVM
# 0. Carga das bibliotecas
library(e1071)
library(caret)
# 1. Carga
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")
# 2. Tratamento de dados
tit$Survived_f <- as.factor(tit$Survived)
tit$Sex_f <- as.factor(tit$Sex)
tit$Pclass_f <- as.factor(tit$Pclass)
tit[is.na(tit$Age),]$Age <- mean(tit[!is.na(tit$Age),]$Age)

# 3. Separação Treino e Teste
set.seed(33)
va<-sample(nrow(tit)) # tamanho da base 
treino <-tit[va[1:600],]
teste  <-tit[va[601:891],]
# 4. Modelagem = SVM
set.seed(33)
mod <- svm(Survived_f~Sex_f+Pclass_f+Age, data=treino)
# 5. Previsão em teste
p <- predict(mod, newdata=teste)
prev <- p
# 6. Análise do erro de previsão de um classificador
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))

