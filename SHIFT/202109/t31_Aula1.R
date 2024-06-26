# Aula 2 - estat�stica descritiva
# Utilizamos <- como atribui��o
a <- 3 + 4
# Utilizamos c() para criar uma vari�vel
v1 <- c(100,105,110,400)
# M�dia
mean(v1)
# Mediana
median(v1)
# Desvio Padr�o
sd(v1)

# Adicionando pacotes
install.packages("ggplot2")
library(ggplot2)

d <- diamonds
# Visualizar somente as 1as 200 linhas
head(d,200)
View( head(d,200) )
# Visualizar somente as Ultimas 200 linhas
tail(d,200)
# Visualizar uma "parte" de um dataframe
d[200:205,1:3]

# Amostragem
# Descri��o da Popula��o d
mean(d$price)
median(d$price)
sd(d$price)

# Obten��o e descri��o de amostra de 3000 obs
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)

# Obten��o de amostra aleat�ria
# Gera��o de n�meros aleat�rios
# set.seed(x) - define a semente x de aleatoriedade
set.seed(33)
va <- sample(nrow(d))
a3 <- d[va[1:3000],]

mean(a3$price)
median(a3$price)
sd(a3$price)

# Comando summary - estat�sticas descritivas para um dataframe
summary(d)

# Histograma
hist(d$price)
# Dividir tela gr�fica em 4
par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Zerar par�metro mfrow
par(mfrow=c(1,1))
# Boxplot
boxplot(a3$price)
boxplot(a3$price~a3$color)

# Estat�sticas descritivas de 2vari�veis
m <- mtcars
plot(m$mpg ~ m$wt)
plot(m$mpg ~ m$cyl, col=4)

# Coeficiente de Correla��o Linear (m�todo Pearson)
cor(m$mpg, m$wt)
