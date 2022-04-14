# Modelos regressivos
df <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# Separação Treino e Teste
treino <- df[1:132,]
teste  <- df[133:138,]

# RegressÕES 
# mod <- lm(PIB~BRL, data=treino)   # RLS
#mod <- lm(PIB~SLP+SPP, data=treino) # RLM
#mod <- lm(PIB~PIBi1+PIBi2+PIBi4+PIBi12, data=treino) # AR
mod <- lm(PIB~SLP+SPP+D1+D5+D9+D11, data=treino) # RLM+S12

# Estatísticas da regressão
summary(mod)
# Previsão em teste
p <- predict(mod, newdata = teste)

# Visualização de dados Previstos | Reais | Erro Absoluto | Erro Relativo%
cbind(p, teste$PIB, p- teste$PIB,(p- teste$PIB)/teste$PIB)

# Erro Previsão
# SSE = sum of squared errors
sse = sum((p- teste$PIB)^2)
