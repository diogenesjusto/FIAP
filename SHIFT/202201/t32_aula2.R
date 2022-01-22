# Aula 2 - Estatística descritiva
a <- c(2,3,4,9)
# média aritmética
mean(a)
# mediana
median(a)
# desvio padrão
sd(a)

# Obtenção de uma amostra representativa
install.packages("ggplot2")
library(ggplot2)
d <- diamonds

# amostra 1as linhas de um dataframe
head(d)
View(head(d,12))
tail(d)
# amostra da linha 10 até linha 15
d[10:15,]
# amostra das linhas 10, 13 e 15
d[c(10,13,15),]

# Estatísticas descritivas da pop
mean(d$price)
median(d$price)
sd(d$price)

# Obtenção de amostra de 3K
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)

# obter uma amostra aleatória
sample(3)  # gera 3 números de 1 a 3 em ordem aleatória

set.seed(33) # definição da semente para geração do algoritmo de dados aleatórios
sample(3)

set.seed(33)
va <- sample(53940)
a3 <- d[va[1:3000],]
mean(a3$price)
median(a3$price)
sd(a3$price)

# Estatísticas descritivas de um objeto
summary(d)
# Proporções em uma variável categórica
table(d$cut)
prop.table(table(d$cut))
prop.table(table(a3$cut))

# Histograma
hist(d$price)
# 4x gráficos em uma tela
par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)
par(mfrow=c(1,1))

# Box-plot
boxplot(d$price)
boxplot(d$price ~ d$clarity)

# Criar função de detecção de outlier
fctOutiler <- function(x) 
{
  z <- (mean(x)-x)/sd(x)
  return(ifelse(abs(z)>3,TRUE,FALSE) )
}

# Criação de uma nova variável
# com o resultado do teste de outlier
d$outlier <- fctOutiler(d$price)

# Gráficos de dispersão
m <- mtcars
plot(m$mpg ~ m$wt)

# Coef. de correlação linear de pearson
cor(m$wt, m$mpg)
# Matriz de correlação
cor(m)





