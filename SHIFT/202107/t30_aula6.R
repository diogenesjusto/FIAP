# Aula 6 - Árvores de Decisão
# 0. Bibliotecas
library(party)
library(caret)

# 1. Carga de dados
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Transformações de dados
t$Sex_f <- as.factor(t$Sex)
t$Pclass_f <- as.factor(t$Pclass)

# 3. Separação Treino-Teste
set.seed(33)
va <- sample(891)
treino <- t[va[1:600],]
teste  <- t[va[601:891],]

# 4. Modelagem
mod <- ctree(Survived~Pclass_f+SibSp+as.factor(Embarked), data=t)

#plot(mod, type="simple")
# 5. Previsão em teste
p <- predict(mod, newdata=teste)

# 6. Análise do Erro de Previsão
prev <- ifelse(p>=.45,1,0)
# Análise de distribuição de probabilidade esperada (target)
hist(p)

# Matriz de Confusão
table(prev, teste$Survived)
confusionMatrix(as.factor(prev), as.factor(teste$Survived))


