# Aula 5 - Transf Lineares + Classificadores

m <- mtcars

plot(m$mpg~m$wt)
cor(m$mpg,m$wt)
# Transf. lineares - Log-Linear
cor(m$mpg, log(m$wt))
plot(m$mpg~log(m$wt))

# Model ML
m <- mtcars
set.seed(33)
va<-sample(32)
treino <- m[va[1:24],]
teste  <- m[va[25:32],]
#mod<-lm(mpg~wt, data=treino)
#mod<-lm(mpg~log(wt), data=treino)
mod<-lm(mpg~poly(wt,15), data=treino)
summary(mod)
p<-predict(mod, newdata=teste)
sse<- sum((p-teste$mpg)^2)

# Classificadores
tt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv", stringsAsFactors = TRUE)

# Probabilidades de sobrevivência e morte
nrow(tt[tt$Survived==0,])/nrow(tt)
# Package sql
library(sqldf)
dfr<-sqldf("select Survived, count(*) from tt group by 1")
# Table 
table(tt$Survived)
# Tabela proporções ou probabilidades
prop.table(table(tt$Survived))
prop.table(table(tt$Sex))

# Probabilidade de ser de um gênero e ter sobrevivido
nrow(tt[tt$Survived==1 & tt$Sex=="female",])/nrow(tt[tt$Sex=="female",])
nrow(tt[tt$Survived==1 & tt$Sex=="male",])/nrow(tt[tt$Sex=="male",])

# Árvore de decisão no R
library(party)

set.seed(33)
va<-sample(891)
treino <- tt[va[1:600],]
teste  <- tt[va[601:891],]

mod <- ctree(Survived~Sex+Pclass, data=tt)
plot(mod, type="simple")
p <- predict(mod, newdata=teste)
pred <- ifelse(p<.5,0,1)

cbind(pred, teste$Survived)
# Matriz de confusão
table(pred, teste$Survived)
