# aula 5 - Overfit
# 0. Bibliotecas
# 1. Carga de dados
m <- mtcars
# 2. Separa��o treino e teste
set.seed(33)
va <- sample(nrow(m))
treino <-m[va[1:24],]
teste <-m[va[25:32],]
# 3.Modelo
# mod <- lm(mpg~wt,data=treino) RLS
#mod <- lm(mpg~log(wt),data=treino) Transf LogLin 
#mod <- lm(mpg~poly(wt,2),data=treino)Poly 2
#mod <- lm(mpg~poly(wt,3),data=treino)Poly 3
mod <- lm(mpg~poly(wt,18),data=treino)
# Estat�sticas do modelo
summary(mod)
# 4. Avalia��o da previs�o
p <- predict(mod, newdata=teste)
erro <- (p-teste$mpg)^2
sse <- sum(erro)

# An�lise das rela��es entre as vari�veis
plot(m$mpg ~ m$wt)
# Transf. Linear - log-lin
m$wt_log <- log(m$wt)
plot(m$mpg ~ m$wt_log)
cor(m$mpg, m$wt)
cor(m$mpg, m$wt_log)


###########################################
###########################################
###########################################
# Teoria de Probabilidades - Arvores de decis�o

tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

table(tit$Survived)/891
prop.table(table(tit$Survived))
prop.table(table(tit$Sex))

table(tit[,c('Sex', 'Survived')])

# Vari�veis categ�ricas
tit$Sex_F = as.factor(tit$Sex)
tit$Pclass_F = as.factor(tit$Pclass)

# �rvore de Decis�o no R
install.packages("party")
library(party)
mod <- ctree(Survived~Pclass_F+Sex_F, data=tit)
plot(mod, type='simple')

summary(lm(Survived~Pclass_F, data=tit))
