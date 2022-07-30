# Regressões Lineares
# carga dos dados
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")
# separação treino e teste
treino <- pib[1:132,]
teste <- pib[133:138,]
# Regressões
# mod <- lm(PIB~BRL, data=treino) # RLS
# mod <- lm(PIB~BRL+BRP, data=treino) # RLM
# mod <- lm(PIB~BRL+PIBi1+PIBi2+PIBi4+PIBi12, data=treino) # AR
mod <- lm(PIB~BRL+BRP+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10, data=treino) # RLM com dummies
# Estatísticas da regressão
summary(mod)
# Previsões em teste
p <- predict(mod, newdata = teste)
# Análise das previsões
cbind(p, teste$PIB, p-teste$PIB,(p-teste$PIB)/teste$PIB)
# Métricas de erro de previsão
sse<-sum((p-teste$PIB)^2) # Sum of Squared Errors
rmse <- sqrt(mean((p-teste$PIB)^2)) # Root Mean Squared Error


# Modelos AutoRegressivos no R com função nativa
mod <-ar(treino$PIB,2) # Modelo AutoregressivoOrdem2
p<-predict(mod, n.ahead = 6)

# Construção de Variáveis Dummies no R
install.packages("fastDummies")
pib$MES <- substr(pib$ANO_MES,1,3)
pib <- cbind(pib, fastDummies::dummy_cols((pib$MES)) )

# Variáveis Categóricas
pib$MES_c <- as.factor(pib$MES)

# SQL "dentro" do R
install.packages("sqldf")
library(sqldf)
df <- sqldf("select PIB, BRL from pib")
