# Regressao Linear
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/pib.csv")

# Separacao dados treino e teste
treino <- pib[1:124,]
teste  <- pib[125:136,]

# Modelo
mod <- lm(PIB~BRL, data=treino)

# Previsao
pib_prev <- predict(mod,newdata = teste)

par(mfrow=c(1,2))
# Em treino
plot(treino$PIB, type="l")
lines(mod$fitted.values, col=4)
# Em teste
plot(teste$PIB, type="l")
lines(pib_prev, col=4)
