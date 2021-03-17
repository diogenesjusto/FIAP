# Instalação e carga de pacotes para árvore de decisão
install.packages("party")
library(party)
# Instalação e carga de pacotes para Random Forest
install.packages("randomForest")
library(randomForest)

d <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/dados_arvore.csv")

set.seed(33)
va <- sample(891)
treino <- d[va[1:600],]
teste  <- d[va[601:891],]

# Modelos de árvore de decisão (ctree) com diferentes variáveis
#mod <- ctree(Survived~Age, data=treino)
# Transformação de variável chr em categórica => as.factor
#mod <- ctree(Survived~ Age+as.factor(Sex), data=treino)
# Modelos de RandomForest com diferentes variáveis
mod <- randomForest(Survived~Age, data=treino)

p <- predict(mod, newdata=treino)
pred <- ifelse(p<.5,0,1)
# Cálculo da acurácia=>taxa de erro = 1 - acurácia
# erro de modelagem
emod <- 1 - sum(ifelse(pred==treino$Survived,1,0))  / 600

p <- predict(mod, newdata=teste)
pred <- ifelse(p<.5,0,1)
# Taxa de erro (em previsão) = 1 - acurácia
# erro previsão
eprev <- 1 - sum(ifelse(pred==teste$Survived,1,0))  / 291

