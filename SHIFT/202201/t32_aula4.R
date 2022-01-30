# Carga de dados do PIB
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# Separação amostras treino e teste
treino <- pib[1:132,]
teste  <- pib[133:138,]

# mod <- lm(PIB~BRL, data=treino) # RLS
# mod <- lm(PIB~BRL+BRP, data=treino) # RLM
# mod <- lm(PIB~PIBi1+PIBi2+PIBi4+PIBi12, data=treino) # AR2
mod <- lm(PIB~SPT+SPP+D2+D5+D6+D7, data=treino) # RLM+SAZ

# Analisando estat. regressão
summary(mod)

# Previsão em teste
p <- predict(mod, newdata = teste)

# Análise do erro de previsão
cbind(p, teste$PIB, p- teste$PIB,(p- teste$PIB)/teste$PIB)

# rmse - root mean squared error
rmse<-sqrt(mean((p- teste$PIB)^2))
# sse - sum of squared error
sse<- sum(((p- teste$PIB)^2))
