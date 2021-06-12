# Análise de conglomerados - Cluster
m <- mtcars

plot(m$mpg ~ m$wt)

# Seed define que a distribuição aleatória dos k-centróides
# Possa ser repetida
set.seed(86)
k <- kmeans(m[,c("mpg","wt")],6)
# Listagem de obs por cluster
k$cluster
# Gráfico com clusters identificados
plot(m$mpg ~ m$wt, col=k$cluster, pch=k$cluster)

# Colocar as variáveis na mesma escala
m$wt2 <- m$wt*6

# ajuste de escala utilizando o pacote rescale
install.packages("scales")
library(scales)
m$mpg_rs <- rescale(m$mpg, to = c(0.01, 0.09))
m$wt_rs <- rescale(m$wt, to = c(0.01, 0.09))

# Possa ser repetida
set.seed(86)
k <- kmeans(m[,c("mpg_rs","wt_rs")],4)
# Listagem de obs por cluster
k$cluster
# Gráfico com clusters identificados
plot(m$mpg ~ m$wt, col=k$cluster, pch=k$cluster)


# Capturar nomes das variáveis
names(m)
# Capturar os nomes das linhas
row.names(m)

# Usando função apply
# Calculand\o Somente Numerics
d <- m
d$col <- "X"
is.numeric(d$col)
d[,sapply(d, is.numeric)]
sapply( d[,sapply(d, is.numeric)], sd)

#############
# Mais algoritmos 
# REGRESSÃO LOGÍSTICA
install.packages("e1071")
library(e1071)
# 1. Carga de dados
d <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/dados_arvore.csv")
# preenchimento de idades nulos
d[is.na(d$Age),]$Age <- mean(d[!is.na(d$Age),]$Age)
set.seed(33)
va <- sample(nrow(d))
treino <- d[va[1:600],]
teste  <- d[va[601:891],]
# glm com family binomial aplica o ajuste pela função logística acumulado => regressão logística
mod <- glm(Survived~Sex+Pclass+Age, data=treino, family=binomial())
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5,0,1)
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived) )

#############
# Mais algoritmos 
# RANDOM FOREST
install.packages("randomForest")
library(randomForest)
# 1. Carga de dados
d <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/dados_arvore.csv")
# preenchimento de idades nulos
d[is.na(d$Age),]$Age <- mean(d[!is.na(d$Age),]$Age)
d$Survived_F <- as.factor(d$Survived)
set.seed(33)
va <- sample(nrow(d))
treino <- d[va[1:600],]
teste  <- d[va[601:891],]
# random Forest: dada a característica de sorteio aleatório de árvores, para reproduzir seus resultados é necessário definir a semente de aleatoriedade
set.seed(33)
mod <- randomForest(Survived_F~Sex+Pclass+Age, data=treino, ntree=200)
p <- predict(mod, newdata=teste)
table(p, teste$Survived_F)
confusionMatrix(p, teste$Survived_F )

# RandomForest - análise de convergência
plot(mod)
# Gráfico de análise de importância de variáveis
varImpPlot(mod)


#############
# Mais algoritmos 
# SVM
library(e1071)
# 1. Carga de dados
d <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/dados_arvore.csv")
# preenchimento de idades nulos
d[is.na(d$Age),]$Age <- mean(d[!is.na(d$Age),]$Age)
d$Survived_F <- as.factor(d$Survived)
set.seed(33)
va <- sample(nrow(d))
treino <- d[va[1:600],]
teste  <- d[va[601:891],]
# svm
mod <- svm(Survived_F~Sex+Pclass+Age, data=treino)
p <- predict(mod, newdata=teste)
table(p, teste$Survived_F)
confusionMatrix(p, teste$Survived_F )
