# aula2 - análise descritiva
install.packages("ggplot2")
library(ggplot2)
d <- diamonds

# Amostra Linhas iniciais
head(d)
# Amostra Linhas finais
tail(d)
# Contagem de linhas/obs
nrow(d)
# Contagem de colunas/var
ncol(d)

# Estatísticas descritivas
# média
mean(d$price)
# mediana
median(d$price)
# desv.pad
sd(d$price)

# Geração amostra
a1 <- d[1:3000,]
# est. descritivas da amostra
mean(a1$price)
median(a1$price)
sd(a1$price)

a2 <- d[c(1:1500,50000:51500),]
# est. descritivas da amostra
mean(a2$price)
median(a2$price)
sd(a2$price)

# Geração de amostra aleatória
set.seed(33)
sample(3)

set.seed(33)
va <- sample(53940)
a3 <- d[va[1:3000],]
mean(a3$price)
median(a3$price)
sd(a3$price)

# Histograma
hist(d$price)

par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Estati Descr
summary(d)
summary(a3)

# Boxplot
boxplot(d$price ~ d$color)

# Cálculo Escore Z e outlier
fctOutlier <- function(x) {
  EscoreZ <- (x-mean(x))/sd(x)
  return(abs(EscoreZ)>3)
}

fctOutlier( c(0,1,2,1,1,2,1,1,1,1,0,0,0,1,2,3,1,1,1,2,1,2,1,2,1,2,2,2,2,1000) )

d$OutlierPrice <- fctOutlier(d$price)

#Gráfico de dispersão ou scatterplot
m <- mtcars
plot(m$mpg ~ m$wt)

# Coef. de corrl. linear
cor(m$mpg, m$wt)
# Matriz de correlação
cor(m)

