# Landscape de M.L.
m <- mtcars          # Carga de dados
set.seed(33)
va<-sample(32)
treino<-m[va[1:24] ,]             # Separação treino / test (split)
teste <-m[va[25:32],]
#mod <- lm(mpg~wt, data=treino)    # Modelo linear
#mod <- lm(mpg~log(wt), data=treino)    # Modelo transf. log-linear
#mod <- lm(mpg~poly(wt,2), data=treino)    # Modelo transf. poli o=2
#mod <- lm(mpg~poly(wt,3), data=treino)    # Modelo transf. poli o=3
mod <- lm(mpg~poly(wt,15), data=treino)    # Modelo transf. poli o=6
summary(mod)
p<-predict(mod, newdata=teste)    # Previsão
mse<-(sum((p-teste$mpg)^2))       # MSE (MeanSquaredError)


# Log-linear (transformação linear)
plot(m$mpg~m$hp)
plot(m$mpg~log(m$wt))
cor(m$mpg, m$hp)
cor(m$mpg, log(m$hp))


## 2.a parte - CLASSIFICADORES
# PROBABILIDADES
# carregar os dados do titanic
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/train.csv")

# Probabilidade de um passageiro ser do gênero feminino
# Pf = #fem/#total
nrow(t[t$Sex=="female",])/nrow(t)
# SQL no R
install.packages("sqldf")
library(sqldf)
ndf <- sqldf("select PassengerId, Name from t where Sex='female'")
# Table
table(t$Sex)
prop.table(table(t$Sex))

# Probabilidade de ser mulher e ter sobrevivido
nrow(t[t$Sex=="female" & t$Survived==1,])/nrow(t[t$Sex=="female",])

# Probabilidade de sobrevivência
nrow(t[t$Survived==1,])/nrow(t)

# Árvore de decisão
install.packages("party")
library(party)

mod <- ctree(Survived~as.factor(Sex)+Pclass, data=t)
plot(mod, type="simple")

t[is.na(t$Age),]$Age <- 26

cor(t[, sapply(t, is.numeric) ])





