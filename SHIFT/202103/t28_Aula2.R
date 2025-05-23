# Aula 2 - Estat�stica Descritiva
a <- 3 + 4

m <- mtcars
head(m)  # visualiza��o das 1as 6 linhas de um dataframe

m$mpg    # $ � utilizado para acessar um atributo de um objeto
mean(m$mpg) # C�lculo da m�dia
median(m$mpg) # C�lculo da mediana
max(m$mpg)-min(m$mpg) # amplitude
sd(m$mpg)             # desvio padr�o
plot(m$mpg)           # gr�fico

write.csv(m, file="m.csv")
df <- read.csv("m.csv")

install.packages("ggplot2")
library(ggplot2)

d <- diamonds
head(d)

# Descrever a popula��o
mean(d$price)
median(d$price)
sd(d$price)

# Obten��o de amostras
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)

# Gera��o de um n�mero aleat�rio
set.seed(33)
sample(3)

# Gera��o de um vetor de n�meros aleat�rios do tamanho da popula��o
set.seed(33)
va <- sample(53940)
a3 <- d[va[1:3000],]  # filtro p obten��o de amostra aleat�ria
mean(a3$price)
median(a3$price)
sd(a3$price)

# Histograma - representado atrav�s de um gr�f de barras,
# demonstrando a distribui��o dos dados
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Par�metro de janela de visualiza��o
# visualiza��o de mais de um gr�fico na mesma janela
par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Comando summary
summary(d$price)
summary(a1$price)
summary(a2$price)
summary(a3$price)

# Boxplot
boxplot(d$price)
# Boxplot - an�lise de segmentos
boxplot(d$price ~ d$clarity)

# Gr�ficos de dispers�o
plot(m$mpg ~ m$wt)
# Coef de correla��o linear
cor(m$mpg, m$wt)

plot(m$mpg ~ m$hp)
cor(m$mpg, m$hp)
# Matriz de correla��o
cor(m)
