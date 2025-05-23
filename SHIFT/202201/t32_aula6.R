# Aula 6 - Classificadores
# 0 - Bibliotecas
library(party)
library(dummies)
library(caret)

# 1 - Carga de dados
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")

# 2 - Tratamento de dados
# Vari�veis categ�ricas no R => Factor
tit$Sex_f <- as.factor(tit$Sex)
tit$Pclass_f <- as.factor(tit$Pclass)
tit <- cbind(tit, dummy(tit$Pclass))

# 3 - Separa��o treino x teste
set.seed(33)
va <- sample(nrow(tit))
treino <- tit[va[1:691],]
teste  <- tit[va[692:891],]

# 4 - Modelo preditivo
mod <- ctree(Survived~Sex_f+ Age+Pclass_f, data=tit)
plot(mod, type="simple")

# 5 - Previs�o em teste
p <- predict(mod, newdata=teste)

# 6 - An�lise de erro de previs�o
prev <- ifelse(p<.5,0,1)
cbind(prev, teste$Survived)
table(teste$Survived, prev)

confusionMatrix(as.factor(prev), as.factor(teste$Survived) )

# Distribui��o de vari�vel esperada
prop.table(table(tit$Survived)) # => Distr. Observada
prop.table(table(prev)) # => Distr. prob. esperada
hist(p)
