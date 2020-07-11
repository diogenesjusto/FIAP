d <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/pib.csv")
treino <- d[1:132,]
teste  <- d[133:138,]

# Regressao Linear Simples
mod<-lm(PIB~BRL, data=treino)
summary(mod)

# Regressao Linear Multipla
mod<-lm(PIB~BRL+BRP, data=treino)
summary(mod)

# Autoregressivo
mod<-lm(PIB~PIBi1+PIBi2+PIBi12, data=treino)
summary(mod)

# Multivariado com sazonalidade
mod<-lm(PIB~BRL+BRP+D2+D5+D6+D7+D8+D9+D11, data=treino)
summary(mod)

# Previsões de dados
p <- predict(mod, newdata = teste)

# Comparação da previsão com dados reais: erro de previsão
# Root Mean Squared Error
RMSE <- sqrt(mean((p-teste$PIB)^2))

# Erro percentual (métrica do RMSE)
RMSE/mean(teste$PIB)

# Gráfico Valores previstos x reais
plot(p, type ='l')
lines(teste$PIB, col=3)


