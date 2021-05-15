# - Aula 2 - Estatística Descritiva
m <- mtcars
m$mpg

# Média
mean(m$mpg)
# Mediana
median(m$mpg)
# Amplitude
max(m$mpg)-min(m$mpg)
# Desvio Padrão
sd(m$mpg)

# Visualizar 1as 6 linhas
head(m,6)
View(head(m))
# Visualizar ultimas 6 linhas
tail(m)

# Carregar dataframe diamonds do package GGplot2
install.packages("ggplot2")
library(ggplot2)
d <- diamonds
head(d)

# Descrever a população (d)
mean(d$price)
median(d$price)
sd(d$price)

# Obtenção de recortes de um dataframe
c(1,3,7)
# filtrar colunas 1, 3 e 7
d_3c <- d[,c(1,3,7)]
# filtrar 1as 10 linhas
d_10l <- head(d,10)
# filtrar 1as 10 linhas
d_10l <- d[1:10,]

# Geração de amostra de 3k linhas
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

# Geração de amostra de 3k linhas
a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)

# Comando sample gera números aleatórios
sample(3)
# Reprodução de uma amostra aleatório: definição da semente
set.seed(33)
sample(3)

# Geração de uma amostra aleatória
set.seed(33)
va <- sample(nrow(d))
a3 <- d[va[1:3000],]

mean(a3$price)
median(a3$price)
sd(a3$price)

# Geração de estatísticas descritivas
summary(d)
summary(a3)

# Descrição de variáveis categóricas
prop.table(table(d$cut))
prop.table(table(a3$cut))

# Histograma
hist(d$price)

# R: múltiplos gráficos em uma janela
par(mfrow=(c(2,2)))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Função para cálculo de outlier
fctOutlier <- function(x) {
  z <- (x-mean(x))/sd(x)
  # abs é o cálculo do módulo - isto é, sem sinal
  # return é o valor retornado pela função
  return(abs(z)>3)
}

fctOutlier(c(1,1,1,1,1,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1000010))

# Criar coluna Outlier no dataframe
d$outlier <- fctOutlier(d$price)

# box-plot
boxplot(d$price)
# box-plot para analisar distribuições segmentadas por categorias
boxplot(d$price~d$clarity)

# Estatísticas para 2 variáveis
m <- mtcars

# Gráfico de 2 variáveis - dispersão (scatterplot)
plot(m$mpg~m$wt)

# Coeficiente de correlação linear
cor(m$mpg, m$wt)
