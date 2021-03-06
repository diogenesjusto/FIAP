# Regressão Linear
# 1. Carga de dados
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# 2. Separação base treino e teste
treino <- pib[1:132,]
teste  <- pib[133:138,]

# 3. Modelagem estatística/machine learning
# 3.1 Regressão Linear Simples (uma variável exógena)
# mod1 <- lm(PIB~BRP, data=treino) 
# 3.2 Regressão Linear Multivariada (mais de uma variável exógena)
# mod1 <- lm(PIB~BRP+BRL, data=treino) 
# 3.3 Modelo autoregressivo
mod1 <- lm(PIB~PIBi1+PIBi2+PIBi4+PIBi12, data=treino) 
# 3.4 Modelo multivariado com dummy de sazonalidade
mod1 <- lm(PIB~BRP+BRL+D2+D5+D6+D7+D8+D9+D11, data=treino) 

# 4. Análise da regressão
# estatísticas da regressão
summary(mod1)

# 5. Previsão
p <- predict(mod1, newdata = teste)

# 6. erro previsão
cbind(teste$PIB,p, teste$PIB-p)
# métricas para erro de previsão
# Root Mean Squared Error
RMSE <- sqrt(mean( (teste$PIB-p)^2 ))
# Sum of Squared Errors
SSE <- sum((teste$PIB-p)^2 )
