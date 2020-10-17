# Árvore de decisão no R
# 0. Carga de bibliotecas
library(party)
library(caret)

# 1. Carga de dados
# Aula 6 - Classificadores
tt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv", stringsAsFactors = TRUE)

# 2. Análise descritiva/tratamento
tt$Pclass_F <- as.factor(tt$Pclass)

# Variável categórica
levels(tt$Pclass_F)
# Inlcuir nova categoria
levels(tt$Pclass_F) <- c(levels(tt$Pclass_F), "4")

# 3. Split (treino / teste)
set.seed(33)
va<-sample(891)
treino <- tt[va[1:600],]
teste  <- tt[va[601:891],]

mod <- ctree(Survived~Sex+Pclass_F+Age, data=tt)
plot(mod, type="simple")
p <- predict(mod, newdata=teste)
pred <- ifelse(p<.5,0,1)

# Matriz de confusão
table(pred, teste$Survived)
confusionMatrix(as.factor(teste$Survived), as.factor(pred))


