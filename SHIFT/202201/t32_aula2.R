# Aula 2 - Estat�stica descritiva
a <- c(2,3,4,9)
# m�dia aritm�tica
mean(a)
# mediana
median(a)
# desvio padr�o
sd(a)

# Obten��o de uma amostra representativa
install.packages("ggplot2")
library(ggplot2)
d <- diamonds

# amostra 1as linhas de um dataframe
head(d)
View(head(d,12))
tail(d)
# amostra da linha 10 at� linha 15
d[10:15,]
# amostra das linhas 10, 13 e 15
d[c(10,13,15),]

# Estat�sticas descritivas da pop
mean(d$price)
median(d$price)
sd(d$price)

# Obten��o de amostra de 3K
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)

# obter uma amostra aleat�ria
sample(3)  # gera 3 n�meros de 1 a 3 em ordem aleat�ria

set.seed(33) # defini��o da semente para gera��o do algoritmo de dados aleat�rios
sample(3)

set.seed(33)
va <- sample(53940)
a3 <- d[va[1:3000],]
mean(a3$price)
median(a3$price)
sd(a3$price)

# Estat�sticas descritivas de um objeto
summary(d)
# Propor��es em uma vari�vel categ�rica
table(d$cut)
prop.table(table(d$cut))
prop.table(table(a3$cut))

# Histograma
hist(d$price)
# 4x gr�ficos em uma tela
par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)
par(mfrow=c(1,1))

# Box-plot
boxplot(d$price)
boxplot(d$price ~ d$clarity)

# Criar fun��o de detec��o de outlier
fctOutiler <- function(x) 
{
  z <- (mean(x)-x)/sd(x)
  return(ifelse(abs(z)>3,TRUE,FALSE) )
}

# Cria��o de uma nova vari�vel
# com o resultado do teste de outlier
d$outlier <- fctOutiler(d$price)

# Gr�ficos de dispers�o
m <- mtcars
plot(m$mpg ~ m$wt)

# Coef. de correla��o linear de pearson
cor(m$wt, m$mpg)
# Matriz de correla��o
cor(m)





