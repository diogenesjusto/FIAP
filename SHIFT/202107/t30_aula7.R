# Aprendizagem Não Supervisionada
# Clusterização: K-Means
m <- mtcars
plot(m$mpg~m$wt)

# Geração clusters
k <- kmeans(m[,c("mpg","wt")],7)
plot(m$mpg~m$wt, col=k$cluster, pch=k$cluster)

# Pacote para transformar escalas
install.packages("scales")
library(scales)

m$mpg_s <- rescale(m$mpg, to=c(0, 1))
m$wt_s <- rescale(m$wt, to=c(0, 1))

plot(m$mpg_s~m$wt_s)

# Execução do kmeans a partir de uma semente de aleatoriedade
set.seed(33)
k <- kmeans(m[,c("mpg_s","wt_s")],5)
plot(m$mpg_s~m$wt_s, col=k$cluster, pch=k$cluster)

m$cluster <- as.factor(k$cluster)

# Utilização da nova variável "cluster" 
# em um modelo (aprend. sup) preditivo
summary(lm(mpg~wt+cluster, data=m))


# Passeio por algoritmos:
# I. Regressão Logística
# 0. Bibliotecas
library(e1071)
library(caret)

# 1. Carga de dados
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Transformações de dados
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)
mi <- mean(t[!is.na(t$Age),]$Age)
t[is.na(t$Age),]$Age <-mi

# 3. Separação Treino-Teste
set.seed(33)
va <- sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]

# 4. Modelagem
mod <- glm(Survived~Sex_f+Age+Pclass_f, data=treino, family = binomial())

#plot(mod, type="simple")
# 5. Previsão em teste
p <- predict(mod, newdata=teste)

# 6. Análise do Erro de Previsão
prev <- ifelse(p>=.5,1,0)

# Matriz de Confusão
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))


# II. Random Forest
# 0. Bibliotecas
library(randomForest)
library(caret)

# 1. Carga de dados
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Transformações de dados
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)
t$Survived_f <- as.factor(t$Survived)
  
mi <- mean(t[!is.na(t$Age),]$Age)
t[is.na(t$Age),]$Age <-mi

# 3. Separação Treino-Teste
set.seed(33)
va <- sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]

# 4. Modelagem
set.seed(33)
mod <- randomForest(Survived_f~Sex_f+Age+Pclass_f, data=treino, 
                    ntree=750)
# Gráfico de importância de variáveis
varImpPlot(mod)
# Análise do tamanho da floresta para ajuste fino
plot(mod)

#plot(mod, type="simple")
# 5. Previsão em teste
p <- predict(mod, newdata=teste)

# 6. Análise do Erro de Previsão
prev <- p

# Matriz de Confusão
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))


# III. SVM
# 0. Bibliotecas
library(e1071)
library(caret)

# 1. Carga de dados
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Transformações de dados
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)
t$Survived_f <- as.factor(t$Survived)

mi <- mean(t[!is.na(t$Age),]$Age)
t[is.na(t$Age),]$Age <-mi

# 3. Separação Treino-Teste
set.seed(33)
va <- sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]

# 4. Modelagem
mod <- svm(Survived_f~Sex_f+Age+Pclass_f, data=treino)

# 5. Previsão em teste
p <- predict(mod, newdata=teste)

# 6. Análise do Erro de Previsão
prev <- p

# Matriz de Confusão
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))
