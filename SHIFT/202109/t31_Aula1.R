# Aula 2 - estatística descritiva
# Utilizamos <- como atribuição
a <- 3 + 4
# Utilizamos c() para criar uma variável
v1 <- c(100,105,110,400)
# Média
mean(v1)
# Mediana
median(v1)
# Desvio Padrão
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
# Descrição da População d
mean(d$price)
median(d$price)
sd(d$price)

# Obtenção e descrição de amostra de 3000 obs
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)

# Obtenção de amostra aleatória
# Geração de números aleatórios
# set.seed(x) - define a semente x de aleatoriedade
set.seed(33)
va <- sample(nrow(d))
a3 <- d[va[1:3000],]

mean(a3$price)
median(a3$price)
sd(a3$price)

# Comando summary - estatísticas descritivas para um dataframe
summary(d)

# Histograma
hist(d$price)
# Dividir tela gráfica em 4
par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Zerar parâmetro mfrow
par(mfrow=c(1,1))
# Boxplot
boxplot(a3$price)
boxplot(a3$price~a3$color)

# Estatísticas descritivas de 2variáveis
m <- mtcars
plot(m$mpg ~ m$wt)
plot(m$mpg ~ m$cyl, col=4)

# Coeficiente de Correlação Linear (método Pearson)
cor(m$mpg, m$wt)
