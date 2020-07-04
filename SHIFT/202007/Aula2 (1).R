# Aula 2 - Estatística Descritiva
install.packages("ggplot2")
library(ggplot2)
d <- diamonds
# Média (aritmética)
mean(d$price)
# Mediana
median(d$price)
# Desvio Padrão
sd(d$price)
# Histograma
hist(d$price)

# Geração de Amostras
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)
hist(a1$price)

a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)
hist(a2$price)

# Geração de amostra aleatória
sample(3)
# Reproduzir uma amostra aleatória a partir de uma "semente"
set.seed(33)
sample(3)

# Geração da amostra aleatória de 3K, obtida a partir da pop
set.seed(33)
va <- sample(nrow(d))
a3 <- d[va[1:3000],]
mean(a3$price)
median(a3$price)
sd(a3$price)
hist(a3$price)

# Estatísticas descritivas de um dataframe
summary(d)
summary(a3)

# Box plot
boxplot(d$price)
hist(d$price)
boxplot(d$price~d$clarity)

# Gráfico de dispersão
m <- mtcars
# Primeiras 6 linhas de um dataframe
head(m)
# Últimas 6 linhas de um dataframe
tail(m)
# VisualizaçAo em formato de tabela
View(tail(m))
# Gráfico de dispersão
plot(m$mpg~m$wt)
# Coeficiente de correlação linear
# Valores possíveis: -1 e 1
cor(m$mpg,m$wt)
# Matriz de correlação
cor(m)
