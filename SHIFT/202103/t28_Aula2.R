# Aula 2 - Estatística Descritiva
a <- 3 + 4

m <- mtcars
head(m)  # visualização das 1as 6 linhas de um dataframe

m$mpg    # $ é utilizado para acessar um atributo de um objeto
mean(m$mpg) # Cálculo da média
median(m$mpg) # Cálculo da mediana
max(m$mpg)-min(m$mpg) # amplitude
sd(m$mpg)             # desvio padrão
plot(m$mpg)           # gráfico

write.csv(m, file="m.csv")
df <- read.csv("m.csv")

install.packages("ggplot2")
library(ggplot2)

d <- diamonds
head(d)

# Descrever a população
mean(d$price)
median(d$price)
sd(d$price)

# Obtenção de amostras
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)

# Geração de um número aleatório
set.seed(33)
sample(3)

# Geração de um vetor de números aleatórios do tamanho da população
set.seed(33)
va <- sample(53940)
a3 <- d[va[1:3000],]  # filtro p obtenção de amostra aleatória
mean(a3$price)
median(a3$price)
sd(a3$price)

# Histograma - representado através de um gráf de barras,
# demonstrando a distribuição dos dados
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

# Parâmetro de janela de visualização
# visualização de mais de um gráfico na mesma janela
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
# Boxplot - análise de segmentos
boxplot(d$price ~ d$clarity)

# Gráficos de dispersão
plot(m$mpg ~ m$wt)
# Coef de correlação linear
cor(m$mpg, m$wt)

plot(m$mpg ~ m$hp)
cor(m$mpg, m$hp)
# Matriz de correlação
cor(m)
