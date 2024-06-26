# Aprendizagem N�o Supervisionada
# Clusteriza��o: K-Means
m <- mtcars
plot(m$mpg~m$wt)

# Gera��o clusters
k <- kmeans(m[,c("mpg","wt")],7)
plot(m$mpg~m$wt, col=k$cluster, pch=k$cluster)

# Pacote para transformar escalas
install.packages("scales")
library(scales)

m$mpg_s <- rescale(m$mpg, to=c(0, 1))
m$wt_s <- rescale(m$wt, to=c(0, 1))

plot(m$mpg_s~m$wt_s)

# Execu��o do kmeans a partir de uma semente de aleatoriedade
set.seed(33)
k <- kmeans(m[,c("mpg_s","wt_s")],5)
plot(m$mpg_s~m$wt_s, col=k$cluster, pch=k$cluster)

m$cluster <- as.factor(k$cluster)

# Utiliza��o da nova vari�vel "cluster" 
# em um modelo (aprend. sup) preditivo
summary(lm(mpg~wt+cluster, data=m))


# Passeio por algoritmos:
# I. Regress�o Log�stica
# 0. Bibliotecas
library(e1071)
library(caret)

# 1. Carga de dados
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Transforma��es de dados
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)
mi <- mean(t[!is.na(t$Age),]$Age)
t[is.na(t$Age),]$Age <-mi

# 3. Separa��o Treino-Teste
set.seed(33)
va <- sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]

# 4. Modelagem
mod <- glm(Survived~Sex_f+Age+Pclass_f, data=treino, family = binomial())

#plot(mod, type="simple")
# 5. Previs�o em teste
p <- predict(mod, newdata=teste)

# 6. An�lise do Erro de Previs�o
prev <- ifelse(p>=.5,1,0)

# Matriz de Confus�o
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))


# II. Random Forest
# 0. Bibliotecas
library(randomForest)
library(caret)

# 1. Carga de dados
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Transforma��es de dados
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)
t$Survived_f <- as.factor(t$Survived)
  
mi <- mean(t[!is.na(t$Age),]$Age)
t[is.na(t$Age),]$Age <-mi

# 3. Separa��o Treino-Teste
set.seed(33)
va <- sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]

# 4. Modelagem
set.seed(33)
mod <- randomForest(Survived_f~Sex_f+Age+Pclass_f, data=treino, 
                    ntree=750)
# Gr�fico de import�ncia de vari�veis
varImpPlot(mod)
# An�lise do tamanho da floresta para ajuste fino
plot(mod)

#plot(mod, type="simple")
# 5. Previs�o em teste
p <- predict(mod, newdata=teste)

# 6. An�lise do Erro de Previs�o
prev <- p

# Matriz de Confus�o
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))


# III. SVM
# 0. Bibliotecas
library(e1071)
library(caret)

# 1. Carga de dados
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Transforma��es de dados
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)
t$Survived_f <- as.factor(t$Survived)

mi <- mean(t[!is.na(t$Age),]$Age)
t[is.na(t$Age),]$Age <-mi

# 3. Separa��o Treino-Teste
set.seed(33)
va <- sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]

# 4. Modelagem
mod <- svm(Survived_f~Sex_f+Age+Pclass_f, data=treino)

# 5. Previs�o em teste
p <- predict(mod, newdata=teste)

# 6. An�lise do Erro de Previs�o
prev <- p

# Matriz de Confus�o
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))
