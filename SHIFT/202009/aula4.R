pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

treino <- pib[1:132,]
teste  <- pib[133:138,]

# Modelo Regressão Linear Simples
mod <-lm(PIB~BRL,data=treino)
# Modelo Regressao Linear Multipla
mod <-lm(PIB~BRL+BRP ,data=treino)
# Model Autoregressivo de ordem 2
mod <-lm(PIB~PIBi1 +PIBi2 ,data=treino)
summary(mod)
# Model Autoregressivo de ordem 2+saz4+saz12
mod <-lm(PIB~PIBi1 +PIBi2+PIBi4+PIBi12,data=treino)
summary(mod)
# Modelo Multivariado com dummies de sazonalidade
mod <-lm(PIB~BRL+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11,data=treino)
summary(mod)

# Modelo do Vini
mod <-lm(PIB~BRL+SPP+RJL+PRT+D2+D3+D5+D6+D7+D8+D9,data=treino)
summary(mod)

# Previsão em teste
p <- predict(mod, newdata=teste)
# Comparação previsto x real
cbind(p, teste$PIB, p-teste$PIB)
# erro previsão
sse=sum((p-teste$PIB)^2)
rmse=sqrt(mean((p-teste$PIB)^2))
