# Aula 2 - geração de amostras
# Instalação do package 
install.packages("ggplot2")
# Carregar na memória
library(ggplot2)

# Cópia de um dataframe em memória
d <- diamonds

# Visualização do dataframe
View(d)
# 1as linhas de um dataframe
head(d)
# Ultimas linhas de um dataframe
tail(d)
# Filtro de linhas e colunas de um df
d[50:60,4:7]

# Geração de amostras
a1 <- d[1:3000,]

# Estatísticas descritivas
# População (d)
mean(d$price)  # Média (arit.)
median(d$price) # Mediana
sd(d$price)     # Desvio Padrão
# Amostra (a1)
mean(a1$price)  # Média (arit.)
median(a1$price) # Mediana
sd(a1$price)     # Desvio Padrão

a2 <- d[3001:6000,]
# Amostra (a2)
mean(a2$price)  # Média (arit.)
median(a2$price) # Mediana
sd(a2$price)     # Desvio Padrão

# Geração de amostras aleatória
sample(3)
# Reprodução de amostras aleatória
set.seed(33)
sample(3)

# Obtenção de amostra aleatória a partir da pop.
set.seed(33)
va <- sample(nrow(d)) # nrow => número de linhas(obs) de um df
a3 <- d[va[1:3000],]  # filtro de 3000 linhas (aleatórias)

mean(a3$price)  # Média (arit.)
median(a3$price) # Mediana
sd(a3$price)     # Desvio Padrão

# Resumo de estatísticas descritivas
summary(d)

# Avaliação de representatividade de uma variável categ
prop.table(table(d$cut)) # Proporções da população
prop.table(table(a3$cut)) # Proporções da amostra

# Ferramentas gráficas 
# Separar janela gráfica em 4 partes
par(mfrow=c(2,2))
# Histograma
hist(d$price) #pop
hist(a1$price) #a1
hist(a2$price) #a2
hist(a3$price) #a3

par(mfrow=c(1,1))
# Box-plot
boxplot(d$price)
# Box-plot para análise segmentada
boxplot(d$price~d$color)

# Cálculo de Outlier - através da criação de uma função
fctOutlier <- function(x) {
  z <- (x-mean(x))/sd(x)
  return(abs(z)>3)
}

#validação/teste da função
fctOutlier(c(1,2,1,2,1,2,1,2,1,2,2,1,2,1,2,1,2,200))

# Criar nova variável com o resultado da função outlier(price)
d$OutlierPreco <- fctOutlier(d$price)

# Relação entre variáveis
m <- mtcars

# Gráfico de dispersão (scatterplot)
plot(m$mpg ~ m$wt)

# Coeficiente de correlação linear
cor(m$mpg, m$wt)
