# Aula 2 - estatística descritiva
install.packages("ggplot2")
library(ggplot2)

d <- diamonds
head(d)

# Média
mean(d$price)
# Mediana
median(d$price)
# DesvioPadrão
sd(d$price)

# Obtenção de Amostras
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)

#Geração de amostra aleatória
set.seed(33) # Define semente de aleatoriedade
sample(3)    # Gera 3 números de ordem aleatória

set.seed(33)
va <- sample( nrow(d) ) 
a3 <- d[va[1:3000],]

mean(a3$price)
median(a3$price)
sd(a3$price)

# Histograma
hist(d$price)
# Divisão da janela gráfica
par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Boxplot
par(mfrow=c(1,1))
boxplot(d$price)
# Usando boxplot para analisar distribuição de dados
# de uma mesma variável por diferentes categoria
boxplot(d$price~d$color)
boxplot(d$price~d$clarity)

# Estatísticas descritivas de um objeto
summary(d)
# Cálculo de distribuição de valores de uma variável categórica
prop.table(table(d$cut))
prop.table(table(a3$cut))

# Estatística Descritiva de 2 variáveis
# Gráfico de dispersão
m <- mtcars
plot(m$mpg ~ m$wt)

# Coeficiente de correlação linear
cor(m$mpg, m$wt)
cor(m)



