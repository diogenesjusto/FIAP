# Aula 5 - Transformações lineares
m <- mtcars
# Etapa preparação dos dados e transf
# Amostras Treino e Teste
set.seed(33)
va <- sample(32)
treino <- m[va[1:24],]
teste  <- m[va[25:32],]
# Modelo
# mod <- lm(mpg~wt, data=treino)
# mod <- lm(mpg~log(wt), data=treino)
# mod <- lm(mpg~poly(wt,2), data=treino)
# mod <- lm(mpg~poly(wt,3), data=treino)
# mod <- lm(mpg~poly(wt,5), data=treino)
# mod <- lm(mpg~poly(wt,8), data=treino)
# mod <- lm(mpg~poly(wt,15), data=treino)
mod <- lm(mpg~wt+as.factor(cyl), data=treino)
summary(mod)
# Previsão em teste
p <- predict(mod, newdata = teste)
# Análise erro de previsão
sse <- sum((p-teste$mpg)^2)


# Transf. Lineares: log
cor(m$mpg, m$wt)
cor(m$mpg, log(m$wt))
plot(m$mpg~log(m$wt))

# Matriz de Correlação
cor(m)
install.packages("corrplot")
library(corrplot)
corrplot(cor(m))

# Transformação de uma variável numérica em cat
# Função as.factor
m$cyl
as.factor(m$cyl)
# Levels = categorias
levels(as.factor(m$cyl))
# Criação de faixas de valores
m$fx_mpg <- cut(m$mpg, 3)
levels(m$fx_mpg) <- c("f1", "f2", "f3")

# Criação de dummies
install.packages("dummies")
library(dummies)
dummy(m$cyl)
m <- cbind(m, dummy(m$cyl))

# Teoria das probabilidades
t <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# Probabilidade de sobrevivência
nrow(t[t$Survived==1,])/nrow(t)

# Tabela de probabilidades
prop.table(table(t$Survived))

# Usando sql no R 
install.packages("sqldf")
library(sqldf)
dfs <- sqldf("select Survived, count(*)
      from t
      group by Survived")


# Matriz de contingência
table(t[,c('Sex','Survived')])
# Tabela de probabilidades
prop.table(table(t[,c('Sex','Survived')]))

# Árvore de decisão 
install.packages("party")
library(party)
mod <- ctree(Survived~as.factor(Sex)+as.factor(Pclass), data=t)
plot(mod, type="simple")
