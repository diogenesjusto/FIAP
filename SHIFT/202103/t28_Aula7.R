# Aula 7 - Análise de Conglomerados (clustering)
m <- mtcars

# Visualização gráfica de 2 variáveis através do gráf. de dispersão
plot(m$mpg ~ m$wt)
# Cálculo do kmeans (sorteio aleatório dos k-centróides iniciais)
set.seed(33)
k <- kmeans(m[,c("mpg","wt")], 7)
# Um método para escolha do k-centróides pode ser o método de elbow

# Visualização gráfica dos clusters
plot(m$mpg ~ m$wt, col=k$cluster, pch=k$cluster)

# "Normalização simples" - colocar na mesma ordem numérica de grandeza
m$wt_2 <- m$wt * 6
plot(m$mpg ~ m$wt_2)

# Execução do k-means com as variáveis normalizadas
set.seed(33)
k <- kmeans(m[,c("mpg","wt_2")], 4)

# Visualização gráfica dos clusters
plot(m$mpg ~ m$wt, col=k$cluster, pch=k$cluster)

# Resultado de um k-means sendo aplicado em um método supervisionado
# 1. inicialmente sem o resultado do kmeans
summary(lm(mpg~wt, data=m))
# 2. incluindo a nova variável (o novo conhecimento) gerado a partir
# do clustering (simplificado)
summary(lm(mpg~wt+ as.factor(k$cluster), data=m))

# 2.a parte - Mais algoritmos
# Regressão Logística (package: e1071)
# 0. Pacotes utilizados
library(caret)
library(e1071)

# 1. Carga de dados
dt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Tratamento de dados (feature eng.)
dt$Sex_f <- as.factor(dt$Sex)
dt$Pclass_f <- as.factor(dt$Pclass)
med <- mean(dt[!is.na(dt$Age),]$Age)
dt[is.na(dt$Age),]$Age <- med

# 3. Separação de treino e teste
set.seed(33)
va <- sample(nrow(dt))
treino <- dt[va[1:600],]
teste  <- dt[va[601:891],]

# 4. Modelagem (regressão logística, através do parâmetro , family=binomial() )
mod <- glm(Survived~Sex_f+Pclass_f+Age, data=treino, family=binomial())

# 5. Previsão em teste
p <- predict(mod, newdata=teste)
# Ajuste fino no threshold (ponto de corte)
# visando adequar a distribuição de probabilidade esperada
prev <- ifelse(p<.3, 0, 1)

# 6. Análise de previsões (classificador)
# Matriz de confusão "na mão"
table(prev, teste$Survived )
# Matriz de confusão utilizando package
confusionMatrix(as.factor(prev), as.factor(teste$Survived))

######################################################
# 2.a parte - Mais algoritmos
# RandomForest (package: randomForest)
# 0. Pacotes utilizados
library(caret)
library(randomForest)

# 1. Carga de dados
dt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Tratamento de dados (feature eng.)
dt$Survived_f <- as.factor(dt$Survived)
dt$Sex_f <- as.factor(dt$Sex)
dt$Pclass_f <- as.factor(dt$Pclass)
med <- mean(dt[!is.na(dt$Age),]$Age)
dt[is.na(dt$Age),]$Age <- med

# 3. Separação de treino e teste
set.seed(33)
va <- sample(nrow(dt))
treino <- dt[va[1:600],]
teste  <- dt[va[601:891],]

# 4. Modelagem (randomForest )
mod <- randomForest(Survived_f~Sex_f+Pclass_f+Age, data=treino, ntree=600)
# análise de importância de variáveis para RF
varImpPlot(mod)
# análise do "tamanho da floresta" como ajuste fino
plot(mod)

# 5. Previsão em teste
prev <- predict(mod, newdata=teste)

# 6. Análise de previsões (classificador)
# Matriz de confusão "na mão"
table(prev, teste$Survived )
# Matriz de confusão utilizando package
confusionMatrix(as.factor(prev), as.factor(teste$Survived))

