# Aula 5 - transformações lineares
m <- mtcars

# Separação treino - teste
set.seed(33)
va <- sample(32)
treino <- m[va[1:24],]
teste <-  m[va[25:32],]

# Mod. Regressao Linear
# mod <- lm(mpg~wt, data=treino) # RLS
# mod <- lm(mpg~log(wt), data=treino) # RLS T.Log
# mod <- lm(mpg~poly(wt,2), data=treino) # RL T.Poly
# mod <- lm(mpg~poly(wt,4), data=treino) # RL T.Poly 4
# mod <- lm(mpg~poly(wt,8), data=treino) # RL T.Poly 8
mod <- lm(mpg~poly(wt,14), data=treino) # RL T.Poly 14

# Analise das estatísticas da regressão
summary(mod)

# Previsão em teste
p <- predict(mod, newdata = teste)
# Analise do erro de previsao - 
sse <- sum((p-teste$mpg)^2)

# Transformações Lineares
cor(m$mpg, m$wt)
cor(m$mpg, log(m$wt))
plot(m$mpg~m$wt)
plot(m$mpg~log(m$wt)) 

##########################
# Probabilidades
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")

# Calculo de probabilidade
# contagem de passageiros sobreviventes
sobrev <- nrow(tit[tit$Survived==1,])
# Probabilidade de sobrevivência
sobrev / nrow(tit)
# Usando SQL no R
install.packages("sqldf")
library(sqldf)
sqldf("select Survived, count(*) from tit group by Survived")

# Probabilidades com table
table(tit$Survived)
prop.table(table(tit$Survived))

table(tit$Sex)
prop.table(table(tit$Sex))
prop.table(table(tit$Pclass))

table(tit[,c("Survived", "Sex")])

# Árvore de Decisão
install.packages("party")
library(party)

mod <- ctree(Survived~as.factor(Sex)+Pclass+Age, data=tit)
plot(mod, type="simple")

