# Script da aula 2 da turma 33
a <- 1+2
a
# Instalação do pacote ggplot2
install.packages("ggplot2")
library("ggplot2")

# Acessar o dataset
d <- diamonds

# Visualizar as 1as 6 linhas do dataframe
head(d)
# Últimas linhas do dataframe
tail(d,8)

# Estatísticas descritivas
# Média
ed <- mean(d$price)
# Mediana
ed[2] <- median(d$price)
# Desv Pad
ed[3] <- sd(d$price)

# Geração de amostra dos dados
a1 <- d[1:3000,]

eda1 <- mean(a1$price)
eda1[2] <- median(a1$price)
eda1[3] <- sd(a1$price)

a2 <- d[3001:6000,]

eda2 <- mean(a2$price)
eda2[2] <- median(a2$price)
eda2[3] <- sd(a2$price)

# Geração de amostras aleatórias
# Set.seed = define uma semente para geração da amostra aleatória
set.seed(33)
va <- sample(53940)

a3 <- d[va[1:3000],]

eda3 <- mean(a3$price)
eda3[2] <- median(a3$price)
eda3[3] <- sd(a3$price)


tab <- cbind(ed, eda1, eda2, eda3)

# Histograma
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price, col="blue")

# Boxplot para representar distribuição de dados
boxplot(d$price)
boxplot(d$price ~ d$clarity)

# Comando summary
summary(d)
summary(a3)

# Gráfico de dispersão
m <- mtcars
plot(m$mpg ~ m$wt)

# Coef de Corr Linear
cor(m$mpg, m$wt)
# Matriz correlação
cor(m)
