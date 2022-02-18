# Clusterizações
# => K-Means

m <- mtcars
plot(m$mpg~m$wt)

# Execução do K means com escolha de K centróides
set.seed(33)
k <- kmeans(m[,c('mpg', 'wt')], 7)

# Visualização dos K clusters
plot(m$mpg~m$wt, col=k$cluster, pch=k$cluster)

# Ajuste de escala = colocar os dados na mesma escala
m$wt_2 <- m$wt*6
plot(m$mpg~m$wt_2)

set.seed(33)
k <- kmeans(m[,c('mpg', 'wt_2')], 4)

# Visualização dos K clusters
plot(m$mpg~m$wt, col=k$cluster, pch=k$cluster)

# Ajuste de escalas com o pacote scales
install.packages("scales")
library(scales)
m$mpg_s <- rescale(m$mpg, c(0,1))
m$wt_s <- rescale(m$wt, c(0,1))
plot(m$mpg_s~m$wt_s)

### Passeios por algoritmos
######

# Regressão Logística
# Ex.: Mod. Crédito
# 0. Carga de bibliotecas
library(e1071)
library(caret)
# 1. Carga de dados
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Tratamento de dados
tit$Pclass_f <- as.factor(tit$Pclass) 
tit[is.na(tit$Age),]$Age <- mean( tit[!is.na(tit$Age),]$Age )

# 3. Separação de treino e teste
set.seed(33)
va<-sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]

# 4. Modelagem de dados (treinamento do modelo)
# parâmetro family=binomial indica que é uma regressão logística
mod <- glm(Survived~Age+Sex+Pclass_f, data=treino, family = binomial())

# 5.Previsão em teste
p <- predict(mod, newdata = teste)
prev <- ifelse(p<.3,0,1)

# 6.Análise do erro de previsão
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

# 3. Separação de treino e teste
set.seed(33)
va<-sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]

# 4. Modelagem de dados (treinamento do modelo)
set.seed(33)
mod <- randomForest(as.factor(Survived)~Age+Sex+Pclass_f, data=treino, ntree=650)
# Análise de RandomForest
plot(mod)
varImpPlot(mod)

# 5.Previsão em teste
p <- predict(mod, newdata = teste)

# 6.Análise do erro de previsão
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

# 3. Separação de treino e teste
set.seed(33)
va<-sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]

# 4. Modelagem de dados (treinamento do modelo)
mod <- svm(as.factor(Survived)~Age+Sex+Pclass_f, data=treino)

# 5.Previsão em teste
p <- predict(mod, newdata = teste)

# 6.Análise do erro de previsão
confusionMatrix(p, as.factor(teste$Survived) )
