# Aula 7 - Clusterização - KMeans
m <- mtcars

plot(m$mpg ~m$wt)

# Utilizamos seed pois o Kmeand clássico tem partida aleatória 
#(posicionamento dos centróides)
set.seed(33)
k <- kmeans(m[,c("wt","mpg")], 7)

# Visualização dos clusters
plot(m$mpg ~m$wt, col=k$cluster, pch=k$cluster)

# Ajuste de escala (duas variáveis na mesma escala)
m$wt2 <- m$wt*6

set.seed(33)
k <- kmeans(m[,c("wt2","mpg")], 4)
plot(m$mpg ~m$wt, col=k$cluster, pch=k$cluster)

# Pacote para ajuste de escala
install.packages("scales")
library(scales)
m$wt2 <- rescale(m$wt,c(0,1))
m$mpg2 <- rescale(m$mpg,c(0,1))
plot(m$mpg2 ~m$wt2, col=k$cluster, pch=k$cluster)


# Outros algoritmos
# 1. Regressão Logística
install.packages("e1071")
library(e1071)
library(caret)
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")
mi <- mean( tit[!is.na(tit$Age),]$Age )
tit[is.na(tit$Age),]$Age <- mi
# Separação Treino e Teste
set.seed(33)
va <- sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]
# Modelo de Regressão Logística
mod <- glm(Survived~Sex+Age, data=treino, family=binomial() )
# Previsão em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5,0,1)
# Matriz de Confusão
confusionMatrix(as.factor(prev), as.factor(teste$Survived) )


# Outros algoritmos
# 2. Random Forest
install.packages("randomForest")
library(randomForest)
library(caret)
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")
mi <- mean( tit[!is.na(tit$Age),]$Age )
tit[is.na(tit$Age),]$Age <- mi
tit$Survived_f <- as.factor(tit$Survived)
# Separação Treino e Teste
set.seed(33)
va <- sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]
# Modelo RandomForest
set.seed(33)
mod <- randomForest(Survived_f~Sex+Age, data=treino,ntree=90 )
# Ajuste fino de parâmetros de RF
plot(mod)

# Previsão em teste
p <- predict(mod, newdata=teste)
prev <- p
# Matriz de Confusão
confusionMatrix(as.factor(prev), as.factor(teste$Survived) )
# Análise de importância das variáveis
varImpPlot(mod)


# Outros algoritmos
# 3. SVM - Support Vector Machines
library(e1071)
library(caret)
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")
mi <- mean( tit[!is.na(tit$Age),]$Age )
tit[is.na(tit$Age),]$Age <- mi
# Separação Treino e Teste
set.seed(33)
va <- sample(nrow(tit))
treino <- tit[va[1:600],]
teste  <- tit[va[601:891],]
# Modelo SVM
mod <- svm(Survived~Sex+Age, data=treino )
# Previsão em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.2,0,1)
# Matriz de Confusão
confusionMatrix(as.factor(prev), as.factor(teste$Survived) )








