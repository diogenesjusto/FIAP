# Aula 2 - Estatística Descritiva
install.packages("ggplot2")
library(ggplot2)
d <- ggplot2::diamonds

# Est Descrit
mean(d$price)   # Média aritmética
median(d$price) # Mediana
sd(d$price)     # DesvPadrão

# Amostragem
#d[<linhas>,<colunas>]
#d[<obs>,<variables>]
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

a2 <- d[3001:6000,] 
mean(a2$price)
median(a2$price)
sd(a2$price)

# Amostragem aleatória
set.seed(33)
va <- sample(53940)
a3 <- d[va[1:3000],]

mean(a3$price)
median(a3$price)
sd(a3$price)

# Histograma
par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Boxplot
par(mfrow=c(1,1))
boxplot(d$price~d$cut)
boxplot(d$price~d$color)

# Outlier - Função
fctOutlier <- function(x) {
  z <- (x-mean(x))/sd(x)
  return(abs(z)>3)
}
vt <- c(1,2,1,2,2,2,2,2,2,2,1,1,1,1,1,2,1,2,1,2,1,2000,1,2,1,2,1)
fctOutlier(vt)

# Criar coluna nova com resultado do teste de outlier
d$OL_Price <- fctOutlier(d$price)
# Visualizar 20 ocorrências de outliers
head(d[d$OL_Price==TRUE,],20)
View(head(d[d$OL_Price==TRUE,],20))

# Visualizar 1as n linhas
head(d,20)
# Visualizar últimas n linhas
tail(d,20)
# Est Descritivas do data frame "inteiro"
summary(d)

# Distribuição de uma variável categórica
prop.table(table(d$cut))
prop.table(table(a3$cut))

# Tabela Dinâmica no R
install.packages("rpivotTable")
library(rpivotTable)
rpivotTable::rpivotTable(head(d[d$OL_Price==TRUE,],20))

# Est Descritivas para mais de uma variável
m <- mtcars
# Gráfico de dispersão / scatterplot
plot(m$mpg ~ m$wt)

# Calc. do coef de correlacao linear
cor(m$mpg, m$wt)
# Matriz de correlação
cor(m)



