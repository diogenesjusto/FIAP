# Aula 8 
######################################################
# 2.a parte - Mais algoritmos
# SVM (package: e1071)
# 0. Pacotes utilizados
library(caret)
library(e1071)

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

# 4. Modelagem ( SVM )
mod <- svm(Survived_f~Sex_f+Pclass_f+Age, data=treino)

# 5. Previsão em teste
prev <- predict(mod, newdata=teste)

# 6. Análise de previsões (classificador)
# Matriz de confusão "na mão"
table(prev, teste$Survived )
# Matriz de confusão utilizando package
confusionMatrix(as.factor(prev), as.factor(teste$Survived))

