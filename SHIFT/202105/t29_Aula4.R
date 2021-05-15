# Aula 4 - Regressões no R
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# Separação Treino e Teste
treino <- pib[1:132,]
teste  <- pib[133:138,]

##################################################################
# 1. Modelo de RLS: 
mod <- lm(PIB~BRL, data=treino)
# Visualização das estatísticas descritivas da regressão
summary(mod)

# Previsão sobre dados teste
p <- predict(mod, newdata=teste)

# Comparação de dados previstos x reais
cbind(p, teste$PIB, p-teste$PIB, (p-teste$PIB)/teste$PIB)

# Erro de previsão
sse <- sum((p-teste$PIB)^2)    # sum of squared errors - amplifica métrica de erro para análise variações mais sensíveis
rmse <- sqrt(mean((p-teste$PIB)^2))  # root mean squared errors - mesma ordem de grandeza da variável

##################################################################
# 2. Regressão Linear Multivariada
mod <- lm(PIB~BRL+BRP, data=treino)
# Visualização das estatísticas descritivas da regressão
summary(mod)

# Previsão sobre dados teste
p <- predict(mod, newdata=teste)

# Erro de previsão
sse <- sum((p-teste$PIB)^2)    # sum of squared errors - amplifica métrica de erro para análise variações mais sensíveis

##################################################################
# 3. Modelo Autoregressivos
mod <- lm(PIB~PIBi1+PIBi2+PIBi4+PIBi12, data=treino)
# Visualização das estatísticas descritivas da regressão
summary(mod)

# Previsão sobre dados teste
p <- predict(mod, newdata=teste)

# Erro de previsão
sse <- sum((p-teste$PIB)^2)    # sum of squared errors - amplifica métrica de erro para análise variações mais sensíveis

##################################################################
# 4. Modelo RLM com uso de dummies para tratamento de sazonalidades
mod <- lm(PIB~BRL+BRP+D2+D5+D6+D7+D8+D9+D11, data=treino)
# Visualização das estatísticas descritivas da regressão
summary(mod)

# Previsão sobre dados teste
p <- predict(mod, newdata=teste)

# Erro de previsão
sse <- sum((p-teste$PIB)^2)    # sum of squared errors - amplifica métrica de erro para análise variações mais sensíveis

###################################################################
# Construção de variáveis dummy
pib$MES <- substr(pib$ANO_MES, 1,3)

install.packages("dummies")
library(dummies)
pib <- cbind(pib, dummy(pib$MES))
