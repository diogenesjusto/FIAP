d <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/dados_arvore.csv")

# Aplicando em um modelo preditivo
set.seed(33)
va <- sample(891)
treino <- d[va[1:600],]
teste  <- d[va[601:891],]

# Modelo preditivo baseado em árvore de decisão
mod <- ctree(Survived~Age+SibSp+Embarked, data=treino)
# Previsão em teste
p <- predict(mod, newdata=teste)
# Transforma probabilidade (p) em previsão 0|1
prev <- ifelse(p>.5,1,0)

# Análise do erro
# Matriz de confusão calculada "na mão"
table(teste$Survived, prev)
# Matriz de confusão por função
install.packages("caret")
library(caret)
confusionMatrix(as.factor(prev),as.factor(teste$Survived))

# Video/Palestra QCon 2016 - Classificador de Fraude
# https://www.infoq.com/br/presentations/estatistica-e-matematica-aplicadas-a-data-science/
