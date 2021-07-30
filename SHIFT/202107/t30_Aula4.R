# Aula 4 - Regressões no R
# 1. Carga de dados do PIB
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")
# 2. Separação de amostras de treino e teste
treino <- pib[1:132,]
teste  <- pib[133:138,]

# 3.1 Modelo de Regressão Linear Simples
# mod <- lm(PIB~BRP, data=treino)
# 3.2 Modelo de Regressão Linear Multivariado
# mod <- lm(PIB~BRP+BRL, data=treino)
# 3.3 Modelo Autoregressivo
# mod <- lm(PIB~PIBi1+PIBi2+PIBi4+PIBi12, data=treino)
# 3.4 Modelo Multivariado com Dummies de Sazonalidades
mod <- lm(PIB~BRP+BRL+D2+D5+D6+D7+D8+D9+D11, data=treino)
summary(mod)   # Estatísticas da regressão

# 4. Erro de Previsão
p <- predict(mod, newdata = teste)
# Valores previstos, valores reais, erro absoluto, erro relativo (%)
cbind(p, teste$PIB, p-teste$PIB, (p-teste$PIB)/teste$PIB )  # Comparando valores previstos x reais
# sse = sum of squared errors
sse <- sum((p-teste$PIB)^2)
# rmse = root mean squared errors
rmse <- sqrt(mean((p-teste$PIB)^2))
