# 1. Como faço para categorizar novos objetos
# nos clusters que foram definidos previamente?

# Clusteriza????o
m <- mtcars
plot(m$mpg~m$wt)

# Antes de executar kmeans
# por ele utilizar premissa de plano cartesiano
# precisamos adequar as escalas
# Alternativas: linear maior para escala linear menor
# linear para log ou vice-versa
install.packages("scales")
library(scales)

# Package FlexClust é mais completo que a função KMeans
# e permite o método predict (apesar de não exatamente uma "previsão")
install.packages("flexclust")
library(flexclust)

m$mpg_s <- rescale(m$mpg, c(0,1))
m$wt_s <- rescale(m$wt, c(0,1))
plot(m$mpg_s~m$wt_s)

# Como kmeans padr??o tem in??cio aleat??rio
# ?? interessante utilizar semente para garantir
# que consigamos reproduzir os resultados do kmeans
set.seed(33)
k<-kcca(m[1:31,c('mpg_s','wt_s')],k=5)

p<-predict(k,newdata=m[32,c('mpg_s','wt_s')])

# P é um vetor com o número do cluster dos objetos em newdata


# 2. Como faço para gerar quartis sobre variáveis
# segmentando por categorias, tal como fazemos boxplots
# por categorias?
library(ggplot2)
d<-diamonds
tapply(d$price, d$cut, quantile)

# Alterando as faixas de probabilidade
tapply(d$price, d$cut, quantile, probs=c(0.1,.2,.8,0.9))


