# Aula 4 - Regressões no R
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# Separação das amostras de treino e teste
treino <- pib[1:132,]
teste  <- pib[133:138,]

# RLS
#mod <- lm(PIB ~ BRP, data=treino )
# RLM
mod <- lm(PIB ~ BRL + BRP, data=treino )
# AR
#mod <- lm(PIB ~ PIBi1 + PIBi2, data=treino )
# RLM com Dummies
#mod <- lm(PIB ~ BRP+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11, data=treino )

# estatísticas da regressão
summary(mod)

# Previsão em teste
p <- predict(mod, newdata = teste)
# Avaliação previsto x real
cbind( p, teste$PIB,  p-teste$PIB )
# Métricas de erro
# sse = sum of squared errors - soma do quadrado dos erros
sse <- sum((p-teste$PIB)^2)
# rmse = root mean squared errors - raiz quadrada, da média dos erros quadrados
rmse <- sqrt(mean(p-teste$PIB)^2)
