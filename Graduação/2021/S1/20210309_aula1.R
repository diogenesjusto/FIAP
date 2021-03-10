# Aula 1 - 20210309 - Revisão Amostragem
# install.packages("ggplot2")
# Carregar package na memória
library(ggplot2)
# Carregar dataframe no objeto d
d <- diamonds

# Distribuição de price
hist(d$price)

# Gerar amostra de 3K que representa a população
a1 <- d[1:3000,]
hist(a1$price)
a2 <- d[3001:6000,]
hist(a2$price)
# amostras a1 e a2 tem um viés da ordem natural dos dados

# Geração de uma amostra aleatória
# Seed => identifica uma amostra aleatória específica
set.seed(33)
va<-sample(53940)
a3 <- d[va[1:3000],]
hist(a3$price)

par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)
hist(a3$price)

summary(d$price)
summary(a1$price)
summary(a2$price)
summary(a3$price)

sd(d$price)
sd(a3$price)

#########################################################
# Landscape de Machine Learning (simplificado)
# 1. carga de dados
m <- mtcars

# 2. separação de amostras de treino e teste
set.seed(33)
va<-sample(32)
treino <- m[va[1:24],]
teste  <- m[va[25:32],]

# 3. modelo (de regressão linear)
#mod <- lm(mpg~wt+hp, data=treino)
#mod <- lm(mpg~wt+hp+am+drat+qsec+vs+am+cyl, data=treino)
#mod <- lm(mpg~log(wt)+log(hp), data=treino)
mod <- lm(mpg~poly(wt,15), data=treino)

summary(mod) # Erro de modelagem => Residual standard error

# 4. previsão (predição) de mpg
p <- predict(mod, newdata = teste)
# Loss-Function => erro de previsão (erro de generalização)
erro <- sum((p-teste$mpg)^2) # sse - sum of squared errors

