# Revis??o 
# Carregar biblioteca ggplot2
library(ggplot2)
d<-diamonds
head(d)
# Amostra 1
a1<-d[1:3000,]
# Estat??sticas descritiva do df
summary(d)
summary(a1)
sd(d$price)
sd(a1$price)
# An??lise gr??fico da distribui????o de uma vari??vel
hist(d$price)
hist(a1$price)
# Amostra aleat??ria
# Semente de aleatoriedade - tornar a amostra reproduz??vel
set.seed(33)
va<-sample(53940)
a2<-d[va[1:3000],]

summary(a2)
sd(a2$price)
hist(a2$price)

# Coeficiente de correla????o Linear
# Gr??fico de dispers??o (scatterplot)
m<-mtcars
plot(m$mpg~m$wt)
# Coeficiente de correla????o linear (de pearson)
# entre duas vari??veis
cor(m$mpg,m$wt)
# Matriz de correla????o
# C??lc coef. correla????o linear entre todas vari??veis
cor(m)
library(corrplot)
corrplot(cor(m))

mc <- as.data.frame(cor(m))
mc[abs(mc$mpg)>.7,]

# Regress??o Linear
# M??todo m??nimos quadrados => m??todo anal??tico
# Algoritmo Gradiente Descendente => Implementa????o num??rica do m??todo dos min. quadr.
mod <- lm(mpg~wt, data=m)
summary(mod)
# Este modelo linear seria
# y = a*x + b
# mpg = -5.34 * wt + 37.28

