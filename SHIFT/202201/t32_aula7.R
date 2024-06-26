# Clusteriza��es
# => K-Means

m <- mtcars
plot(m$mpg~m$wt)

# Execu��o do K means com escolha de K centr�ides
set.seed(33)
k <- kmeans(m[,c('mpg', 'wt')], 7)

# Visualiza��o dos K clusters
plot(m$mpg~m$wt, col=k$cluster, pch=k$cluster)

# Ajuste de escala = colocar os dados na mesma escala
m$wt_2 <- m$wt*6
plot(m$mpg~m$wt_2)

set.seed(33)
k <- kmeans(m[,c('mpg', 'wt_2')], 4)

# Visualiza��o dos K clusters
plot(m$mpg~m$wt, col=k$cluster, pch=k$cluster)

# Ajuste de escalas com o pacote scales
install.packages("scales")
library(scales)
m$mpg_s <- rescale(m$mpg, c(0,1))
m$wt_s <- rescale(m$wt, c(0,1))
plot(m$mpg_s~m$wt_s)

### Passeios por algoritmos
######

# Regress�o Log�stica
# Ex.: Mod. Cr�dito
# 0. Carga de bibliotecas
library(e1071)
library(caret)
# 1. Carga de dados
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Tratamento de dados
tit$Pclass_f <- as.factor(tit$Pclass) 
tit[is.na(tit$Age),]$Age <- mean( tit[!is.na(tit$Age),]$Age )

# 3. Separa��o de treino e teste
set.seed(33)
va<-sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]

# 4. Modelagem de dados (treinamento do modelo)
# par�metro family=binomial indica que � uma regress�o log�stica
mod <- glm(Survived~Age+Sex+Pclass_f, data=treino, family = binomial())

# 5.Previs�o em teste
p <- predict(mod, newdata = teste)
prev <- ifelse(p<.3,0,1)

# 6.An�lise do erro de previs�o
confusionMatrix(as.factor(prev), as.factor(teste$Survived) )


# RandomForest
# 0. Carga de bibliotecas
library(randomForest)
library(caret)
# 1. Carga de dados
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Tratamento de dados
tit$Pclass_f <- as.factor(tit$Pclass) 
tit[is.na(tit$Age),]$Age <- mean( tit[!is.na(tit$Age),]$Age )

# 3. Separa��o de treino e teste
set.seed(33)
va<-sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]

# 4. Modelagem de dados (treinamento do modelo)
set.seed(33)
mod <- randomForest(as.factor(Survived)~Age+Sex+Pclass_f, data=treino, ntree=650)
# An�lise de RandomForest
plot(mod)
varImpPlot(mod)

# 5.Previs�o em teste
p <- predict(mod, newdata = teste)

# 6.An�lise do erro de previs�o
confusionMatrix(p, as.factor(teste$Survived) )



# SVM Support Vector Machine
# 0. Carga de bibliotecas
library(e1071)
library(caret)
# 1. Carga de dados
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Tratamento de dados
tit$Pclass_f <- as.factor(tit$Pclass) 
tit[is.na(tit$Age),]$Age <- mean( tit[!is.na(tit$Age),]$Age )

# 3. Separa��o de treino e teste
set.seed(33)
va<-sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]

# 4. Modelagem de dados (treinamento do modelo)
mod <- svm(as.factor(Survived)~Age+Sex+Pclass_f, data=treino)

# 5.Previs�o em teste
p <- predict(mod, newdata = teste)

# 6.An�lise do erro de previs�o
confusionMatrix(p, as.factor(teste$Survived) )
