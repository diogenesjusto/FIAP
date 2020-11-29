# Aula 5 - M.L. - Transf Lineares
m <- mtcars

set.seed(33) # def da semente de aleatoriedade
va <- sample(32) # ger de amostra aleatória de números de 1 a 32

treino <- m[va[1:24],]
teste  <- m[va[25:32],]

#mod <- lm(mpg~wt, data=treino) - RLS
#mod <- lm(mpg~log(wt), data=treino) # Trsn Log_lin
#mod <- lm(mpg~poly(wt,2), data=treino) # Trsn Polinomial de ordem 2
#mod <- lm(mpg~poly(wt,3), data=treino) # Trsn Polinomial de ordem 3
#mod <- lm(mpg~poly(wt,5), data=treino) # Trsn Polinomial de ordem 5
#mod <- lm(mpg~poly(wt,8), data=treino) # Trsn Polinomial de ordem 8
#mod <- lm(mpg~poly(wt,12), data=treino) # Trsn Polinomial de ordem 12
#mod <- lm(mpg~poly(wt,14), data=treino) # Trsn Polinomial de ordem 14
#mod <- lm(mpg~wt+cyl, data=treino)
mod <- lm(mpg~wt+as.factor(cyl), data=treino) # cyl como var categorica => factor

summary(mod)
p <- predict(mod, newdata = teste)
sse <- sum( (p-teste$mpg)^2) #sum of squared errors
sse

# Transf Linear - log-linear
m$wt_log <- log(m$wt)
plot(m$mpg~m$wt_log)
cor(m$mpg, m$wt)
cor(m$mpg, m$wt_log)

# Var cat - dummy
plot(m$mpg~m$wt, col=m$cyl)


# Teoria Probabilidades
dt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv", stringsAsFactors = TRUE)

# Cálculo da propabilidade
nrow(dt[dt$Sex=="female",])/nrow(dt)

# Utilizando sql no R
install.packages("sqldf")
library(sqldf)
sqldf("select Sex, count(*) from dt group by 1")

# Table
table(dt$Sex)
prop.table(table(dt$Sex))
prop.table(table(dt$Survived))

# Matriz de contingência do titanic
table(dt$Sex, dt$Survived)
prop.table(table(dt$Sex, dt$Survived))

# Árvore de decisão e probabilidade condicional
install.packages("party")
library(party)

mod <- ctree(Survived~as.factor(Pclass)+Sex+Age, data=dt)
plot(mod, type="simple")

