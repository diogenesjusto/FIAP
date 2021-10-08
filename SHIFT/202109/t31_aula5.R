# Aula 5 - ML
m <- mtcars

# Tratamento das variáveis
m$cyl_F <- as.factor(m$cyl)

m <- cbind(m, dummy(m$cyl))

set.seed(33)
va <- sample(32)
treino <- m[va[1:24],]
teste  <- m[va[25:32],]

#mod <- lm(mpg~wt, data=treino)
#mod <- lm(mpg~log(wt), data=treino)   # transf log-lin
#mod <- lm(mpg~poly(wt,2), data=treino)   # transf polinomial
# Simulação do overfitting = fenômeno do sobreajuste
#mod <- lm(mpg~poly(wt,3), data=treino)   # transf polinomial
#mod <- lm(mpg~poly(wt,4), data=treino)   # transf polinomial
#mod <- lm(mpg~poly(wt,9), data=treino)   # transf polinomial
#mod <- lm(mpg~poly(wt,14), data=treino)   # transf polinomial
#mod <- lm(mpg~wt+cyl, data=treino) # cyl numerico
mod <- lm(mpg~wt+cyl_F, data=treino) # cyl categorico=cyl_F
summary(mod)

p <- predict(mod, newdata=teste)
sse <- sum((p-teste$mpg)^2)

# Análise de log-lineariedade
cor(m$mpg, m$wt)
# aplicando o log
cor(m$mpg, log(m$wt))
# análise visual
plot(m$mpg~log(m$wt))

# Variável categórica
v1 <- c(1,3,5,5)
as.factor(v1)

# Criação de dummies atra´ves de um package
install.packages("dummies")
library(dummies)
dummy(as.factor(v1))



##################################33
# Classificadores
# Teoria de probabilidades

tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# Contar # sobreviventes
nrow(tit[tit$Survived==1,])/nrow(tit)
# Análise probabilidades
table(tit$Survived)
prop.table(table(tit$Survived))
# Usando sql no R
install.packages("sqldf")
library(sqldf)
nt <- sqldf("select Survived,count(*) from tit group by 1")

# Probabilidades de eventos combinado
prop.table(table(tit[,c("Sex", "Survived")]))

# Árvore de probabilidades condicionais
install.packages("party")
library(party)
mod <- ctree(Survived~as.factor(Sex)+Pclass+Age, data=tit)
plot(mod, type="simple")

