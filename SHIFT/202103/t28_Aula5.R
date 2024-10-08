# Aula 5 - Machine Learning - Min(perda) evitando sobreajuste
m <- mtcars

# Prepara��o das amostras de treino e teste aleat�rias
set.seed(33)
va<-sample(32)
# Separa��o das bases de treino e teste
treino <- m[va[1:24],]
teste <- m[va[25:32],]
# Modelagem estat�stica (treinamento)
# mod <- lm(mpg~wt, data=treino)
# Transforma��o Log.
# mod <- lm(mpg~log(wt), data=treino)
# Transf. Polinomial
# mod <- lm(mpg~poly(wt,2), data=treino)
# mod <- lm(mpg~poly(wt,3), data=treino)
# mod <- lm(mpg~poly(wt,7), data=treino)
mod <- lm(mpg~poly(wt,13), data=treino)
# An�lise do modelo
summary(mod)
# Previs�o em teste
p <- predict(mod, newdata=teste)
# C�lculo do erro de previs�o
#(neste caso utilizaremos sse - sum of squared errors)
sse <- sum((p-teste$mpg)^2)

# Transforma��o log em vari�vel com cresc exponencial
# ANTES DA TRANSF
# Gr�fico dispers�o
plot(m$mpg~m$wt)
# Coef. correla��o linear
cor(m$mpg, m$wt)

# DEPOIS DA TRANSF
# Gr�fico dispers�o
plot(m$mpg~log(m$wt))
# Coef. correla��o linear
cor(m$mpg, log(m$wt))


# TEORIA DE PROBABILIDADES
dt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# Quantidade de passageiros sobreviventes e mortos
table(dt$Survived)
# Calc de prop
prop.table( table(dt$Survived) )
# Calc de prop
prop.table( table(dt$Sex) )

# Emular SQL no R
install.packages("sqldf")
library(sqldf)
sqldf("select Survived, count(*) from dt group by Survived")

# C�lculo de �rvores de (probabilidades) de decis�o no R
install.packages("party")
library(party)

# as.factor = trasnforma��o de uma vari�vel em categ�rica
mod <- ctree(Survived~as.factor(Sex)+Pclass, data=dt)

plot(mod, type="simple")
