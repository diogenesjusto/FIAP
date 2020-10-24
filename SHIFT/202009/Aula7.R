# Aula 7 - Clusterização
m <- mtcars
plot(m$mpg~m$wt)

# Calculando k-means
set.seed(33)
k <- kmeans(m[,c("mpg", "wt")],6)
plot(m$mpg~m$wt, col=k$cluster, pch=k$cluster)

# Colocando na mesma escala
m$wt_2 <- m$wt * 6
plot(m$mpg~m$wt_2)

# Recalculando o kmeans
set.seed(33)
k <- kmeans(m[,c("mpg", "wt_2")],4)
plot(m$mpg~m$wt, col=k$cluster, pch=k$cluster)

# Utilizando cluster como variável 
summary(lm(mpg~wt, data=m))
summary(lm(mpg~wt+as.factor(k$cluster), data=m))

# Aula 7 - outros algoritmos de ML
# Regressão Logística
library(e1071)
tt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv", stringsAsFactors = TRUE)

# 2. Análise descritiva/tratamento
tt$Pclass_F <- as.factor(tt$Pclass)
ma <- mean(tt[!is.na(tt$Age),]$Age)
tt[is.na(tt$Age),]$Age <- ma
tt$Survived_F <- as.factor(tt$Survived)

# 3. Split (treino / teste)
set.seed(33)
va<-sample(891)
treino <- tt[va[1:600],]
teste  <- tt[va[601:891],]

mod <- glm(Survived~Sex+Pclass_F+Age, data=tt, family=binomial())
p <- predict(mod, newdata=teste)
pred <- ifelse(p<.5,0,1)

# Matriz de confusão
table(pred, teste$Survived)
confusionMatrix(as.factor(teste$Survived), as.factor(pred))

########################
# RANDOM FOREST
library(randomForest)
tt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv", stringsAsFactors = TRUE)

# 2. Análise descritiva/tratamento
tt$Pclass_F <- as.factor(tt$Pclass)
ma <- mean(tt[!is.na(tt$Age),]$Age)
tt[is.na(tt$Age),]$Age <- ma
tt$Survived_F <- as.factor(tt$Survived)

# 3. Split (treino / teste)
set.seed(33)
va<-sample(891)
treino <- tt[va[1:600],]
teste  <- tt[va[601:891],]

set.seed(33)
mod <- randomForest(Survived_F~Sex+Pclass_F+Age, data=tt, ntree=500)
plot(mod)
varImpPlot(mod)

p <- predict(mod, newdata=teste)
pred <- p

# Matriz de confusão
table(pred, teste$Survived)
confusionMatrix(as.factor(teste$Survived), as.factor(pred))

########################
# SVM
library(e1071)
tt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv", stringsAsFactors = TRUE)

# 2. Análise descritiva/tratamento
tt$Pclass_F <- as.factor(tt$Pclass)
ma <- mean(tt[!is.na(tt$Age),]$Age)
tt[is.na(tt$Age),]$Age <- ma
tt$Survived_F <- as.factor(tt$Survived)

# 3. Split (treino / teste)
set.seed(33)
va<-sample(891)
treino <- tt[va[1:600],]
teste  <- tt[va[601:891],]

set.seed(33)
mod <- svm(Survived_F~Sex+Pclass_F+Age, data=tt)

p <- predict(mod, newdata=teste)
pred <- p

# Matriz de confusão
table(pred, teste$Survived)
confusionMatrix(as.factor(teste$Survived), as.factor(pred))



