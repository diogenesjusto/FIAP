# Aula 2 - estat�stica descritiva
install.packages("ggplot2")
library(ggplot2)

d <- diamonds
head(d)

# M�dia
mean(d$price)
# Mediana
median(d$price)
# DesvioPadr�o
sd(d$price)

# Obten��o de Amostras
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)

#Gera��o de amostra aleat�ria
set.seed(33) # Define semente de aleatoriedade
sample(3)    # Gera 3 n�meros de ordem aleat�ria

set.seed(33)
va <- sample( nrow(d) ) 
a3 <- d[va[1:3000],]

mean(a3$price)
median(a3$price)
sd(a3$price)

# Histograma
hist(d$price)
# Divis�o da janela gr�fica
par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Boxplot
par(mfrow=c(1,1))
boxplot(d$price)
# Usando boxplot para analisar distribui��o de dados
# de uma mesma vari�vel por diferentes categoria
boxplot(d$price~d$color)
boxplot(d$price~d$clarity)

# Estat�sticas descritivas de um objeto
summary(d)
# C�lculo de distribui��o de valores de uma vari�vel categ�rica
prop.table(table(d$cut))
prop.table(table(a3$cut))

# Estat�stica Descritiva de 2 vari�veis
# Gr�fico de dispers�o
m <- mtcars
plot(m$mpg ~ m$wt)

# Coeficiente de correla��o linear
cor(m$mpg, m$wt)
cor(m)



