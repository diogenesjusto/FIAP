# Regress�es Lineares
# carga dos dados
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")
# separa��o treino e teste
treino <- pib[1:132,]
teste <- pib[133:138,]
# Regress�es
# mod <- lm(PIB~BRL, data=treino) # RLS
# mod <- lm(PIB~BRL+BRP, data=treino) # RLM
# mod <- lm(PIB~BRL+PIBi1+PIBi2+PIBi4+PIBi12, data=treino) # AR
mod <- lm(PIB~BRL+BRP+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10, data=treino) # RLM com dummies
# Estat�sticas da regress�o
summary(mod)
# Previs�es em teste
p <- predict(mod, newdata = teste)
# An�lise das previs�es
cbind(p, teste$PIB, p-teste$PIB,(p-teste$PIB)/teste$PIB)
# M�tricas de erro de previs�o
sse<-sum((p-teste$PIB)^2) # Sum of Squared Errors
rmse <- sqrt(mean((p-teste$PIB)^2)) # Root Mean Squared Error


# Modelos AutoRegressivos no R com fun��o nativa
mod <-ar(treino$PIB,2) # Modelo AutoregressivoOrdem2
p<-predict(mod, n.ahead = 6)

# Constru��o de Vari�veis Dummies no R
install.packages("fastDummies")
pib$MES <- substr(pib$ANO_MES,1,3)
pib <- cbind(pib, fastDummies::dummy_cols((pib$MES)) )

# Vari�veis Categ�ricas
pib$MES_c <- as.factor(pib$MES)

# SQL "dentro" do R
install.packages("sqldf")
library(sqldf)
df <- sqldf("select PIB, BRL from pib")
