# Regressão Linear no R
dp <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# Sep Treino e Teste
treino <- dp[1:132,]
teste  <- dp[133:138,]

# Regressão Linear Simples
mod <- lm(PIB~SPT, data=treino)
# Estatísticas (parâmetros) da regressão
summary(mod)
# Erro de previsão
p <- predict(mod, newdata = teste)
# Visualização previsto x real
cbind(p,teste$PIB,p-teste$PIB,(p-teste$PIB)/teste$PIB)
# Métricas para erro de previsão
RMSE=sqrt(mean((p-teste$PIB)^2))   # Root Mean Squared Error
SSE=sum((p-teste$PIB)^2)           # Sum of Squared Errors
# Gráfico de erro de previsão
plot(teste$PIB, type="l") # dados reais
lines(p, col=2)

# Regressão Linear Múltipla
mod <- lm(PIB~SLP+SPP, data=treino)
summary(mod)
# Erro de previsão
p <- predict(mod, newdata = teste)
SSE=sum((p-teste$PIB)^2)           # Sum of Squared Errors

# Auto Regressivo
mod <- lm(PIB~PIBi1+PIBi2+PIBi4+PIBi12, data=treino)
summary(mod)
# Erro de previsão
p <- predict(mod, newdata = teste)
SSE=sum((p-teste$PIB)^2)           # Sum of Squared Errors
SSE

# Multivariado com Dummies de Sazonalidade
mod <- lm(PIB~BRL+BRP+D2+D4+D5+D6+D7+D8+D9+D11, data=treino)
summary(mod)
# Erro de previsão
p <- predict(mod, newdata = teste)
SSE=sum((p-teste$PIB)^2)           # Sum of Squared Errors
SSE

# Modelo "vida loka"
mod <- lm(PIB~., data=treino[,3:38])
summary(mod)
# Erro de previsão
p <- predict(mod, newdata = teste)
SSE=sum((p-teste$PIB)^2)           # Sum of Squared Errors
SSE

# Dummies
install.packages("dummies")
library(dummies)
dummy(substr(dp$ANO_MES,1,3))


