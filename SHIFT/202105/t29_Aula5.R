# Machine learning
# 1. Carga de dados
m <- mtcars

# 2. Separação treino-teste
set.seed(33)
va <- sample(32)
treino <- m[va[1:24],]
teste  <- m[va[25:32],]

plot(m$mpg~m$wt)
# 3. Modelagem
# mod <- lm(mpg~wt, data=treino)
# mod <- lm(mpg~log(wt), data=treino)
# Sequência de transformações polinomiais para simular
# o overfit
# mod <- lm(mpg~poly(wt,2), data=treino)
# mod <- lm(mpg~poly(wt,3), data=treino)
# mod <- lm(mpg~poly(wt,4), data=treino)
# mod <- lm(mpg~poly(wt,8), data=treino)
# mod <- lm(mpg~poly(wt,15), data=treino)
mod <- lm(mpg~wt+as.factor(cyl), data=treino)
summary(mod)

# 4. Análise do erro de previsão
p <- predict(mod, newdata = teste)
sse <- sum((p- teste$mpg)^2) # Sum of Squared Errors

# Transformações lineares
m$wt2 <- log(m$wt)
cor(m$wt, m$mpg)
cor(m$wt2, m$mpg)
plot(m$mpg~m$wt)
plot(m$mpg~m$wt2)


############################################################
# Teoria de probabilidades
d <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/dados_arvore.csv")

# Probabilidade de sobrevivência dos passageiros embarcados
nrow(d[d$Survived==1,])/nrow(d)
prop.table( table(d$Survived) )
# Usando SQL no R
install.packages("sqldf")
library(sqldf)
sqldf("select Survived, count(*) from d group by Survived")

# Eventos combinados
prop.table( table( d[,c("Survived", "Sex")] ))

# Uso de árvore de decisão para análise de prob. condicional
install.packages("party")
library(party)
mod <- ctree(Survived~Sex, data=d)
plot(mod)

library(party)
# Árvore de decisão com 1 variável
mod <- ctree(Survived~as.factor(Sex), data=d)
plot(mod, type="simple")

# Árvore de decisão com 2 variáveis
mod <- ctree(Survived~as.factor(Sex)+as.factor(Pclass), data=d)
plot(mod, type="simple")

# Árvore de decisão com 3 variáveis
mod <- ctree(Survived~as.factor(Sex)+as.factor(Pclass)+Age, data=d)
plot(mod, type="simple")