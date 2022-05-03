# Aula 5 - transformações lineares
m <- mtcars

plot(m$mpg~m$wt)

# Geração das amostras aleatórias de treino e teste
set.seed(33)
va <- sample(nrow(m))
treino <- m[va[1:24],] 
teste  <- m[va[25:32],] 

# Modelos
#mod <- lm(mpg~wt, data=treino)  #RegLinearSimples
#mod <- lm(mpg~log(wt), data=treino)  #Transf LogLinear
#mod <- lm(mpg~poly(wt,2), data=treino)  #Transf Polinomial o2
#mod <- lm(mpg~poly(wt,3), data=treino)  #Transf Polinomial o3
#mod <- lm(mpg~poly(wt,4), data=treino)  #Transf Polinomial o4
#mod <- lm(mpg~poly(wt,7), data=treino)  #Transf Polinomial o7
# Simulação overfit
mod <- lm(mpg~poly(wt,15), data=treino)  #Transf Polinomial o15
summary(mod)

# Erro de previsão
p <- predict(mod, newdata = teste)
cbind(p, teste$mpg, p-teste$mpg)

# Sum of Squared Errors
sse <- sum((p-teste$mpg)^2)
# Root Mean Squared Errors
rmse <- sqrt(mean((p-teste$mpg)^2))

# transformações lineares
# transform. logarítmica
plot(m$mpg~log(m$wt))
cor(m$mpg, m$wt)
cor(m$mpg, log(m$wt))


#########################################
#########################################
#########################################
# Teoria de probabilidades
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")

# Probabilidade de sobrevivência no titanic?
nrow(tit[tit$Survived==1,])/nrow(tit)

# Usando SQL no R
install.packages("sqldf")
library(sqldf)
df<-sqldf("select Survived, count(*) from tit group by Survived")

# Table
table(tit$Survived)
prop.table(table(tit$Survived))
prop.table(table(tit[,c("Survived","Sex")]))

# Prob Sex==F ou M
prop.table(table(tit$Sex))

# Algoritmo árvores de decisão
install.packages("party")
library(party)

mod <- ctree(Survived~as.factor(Sex)+Pclass, data=tit)
plot(mod, type="simple")


