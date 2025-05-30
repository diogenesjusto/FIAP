# Aula 4 - Regress�es no R
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# Separa��o das amostras de treino e teste
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

# estat�sticas da regress�o
summary(mod)

# Previs�o em teste
p <- predict(mod, newdata = teste)
# Avalia��o previsto x real
cbind( p, teste$PIB,  p-teste$PIB )
# M�tricas de erro
# sse = sum of squared errors - soma do quadrado dos erros
sse <- sum((p-teste$PIB)^2)
# rmse = root mean squared errors - raiz quadrada, da m�dia dos erros quadrados
rmse <- sqrt(mean(p-teste$PIB)^2)
