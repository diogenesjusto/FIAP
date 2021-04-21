# Carga dos dados de PIB e tráfego desde o github
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# Separação Treino e Teste
treino <- pib[1:126,]
teste <- pib[127:138,] 

# Modelo Regressão Linear (assume linearidade)
mod <- lm(PIB~BRP+BRL+RJT+PRT, data=treino)
# Análise de uma regressão linear
summary(mod)
# Previsão em teste
p <- predict(mod, newdata = teste)
# Cálculo do erro previsão (generalização)
erro <- sum((p-teste$PIB)^2) # sse - sum of squared errors


# Teste de multicolinearidade
cor(treino$BRL,treino$BRP)
cor(treino$RJT,treino$PRT)
# Teste de variation-inflation: vif < 4 
library(car)
vif(mod)
mod <- lm(PIB~BRP+BRL, data=treino)
vif(mod)
mod <- lm(PIB~RJT+PRT, data=treino)
vif(mod)
# Conclusão: RJT + PRT juntas geram um problema de multicolinearidade
# Teste 1: BRs + RJT
mod <- lm(PIB~BRP+BRL+RJT, data=treino)
vif(mod)
# Rejeitamos a hipótese
# Teste 2: BRs + PRT
mod <- lm(PIB~BRP+BRL+PRT, data=treino)
vif(mod)
