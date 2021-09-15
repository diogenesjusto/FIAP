# Aula 14-09
# Exercício de clusterização
# K-Means no R
m <- mtcars
plot(m$mpg ~m$wt)

set.seed(33)
k <- kmeans(m[,c("mpg", "wt")], 5)
plot(m$mpg ~m$wt, col=k$cluster, pch=k$cluster)

# Ajustes de escala
library(scales)
m$mpg_s <- rescale(m$mpg, c(0,1))
m$wt_s <- rescale(m$wt, c(0,1))

set.seed(33)
k <- kmeans(m[,c("mpg_s", "wt_s")], 4)
plot(m$mpg_s ~m$wt_s, col=k$cluster, pch=k$cluster)
