# SERIES TEMPORAIS
# Packages
library(dummies)

# Carga dos dados de PIB e tráfego desde o github
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# Exploratória - PIB
plot(pib$PIB, type="l")

# Tratamento de dados - criação dummy
pib$MES <- substring(pib$ANO_MES,1,3)
# uso de dummies para representar sazonalidades
pib <- cbind(pib, dummy(pib$MES))

# Separação Treino e Teste
treino <- pib[1:126,]
teste <- pib[127:138,] 

# Modelo Regressão Linear (assume linearidade) + Dummies de Sazonalidade
mod <- lm(PIB~SPT+pibjan+pibdez, data=treino)
# Análise de uma regressão linear
summary(mod)

# Modelo Autoregressivo (variável se "explica" por ela mesma no passado)
mod <- lm(PIB~PIBi1+PIBi2, data=treino)
# Análise de um modelo autoregressivo
summary(mod)

# Modelo Autoregressivo (variável se "explica" por ela mesma no passado)
# Incluindo um tratamento de sazonalidade anual (12)
mod <- lm(PIB~PIBi1+PIBi2+PIBi12, data=treino)
# Análise de um modelo autoregressivo
summary(mod)
