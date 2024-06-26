# Regress�o Linear
# 1. Carga de dados
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# 2. Separa��o base treino e teste
treino <- pib[1:132,]
teste  <- pib[133:138,]

# 3. Modelagem estat�stica/machine learning
# 3.1 Regress�o Linear Simples (uma vari�vel ex�gena)
# mod1 <- lm(PIB~BRP, data=treino) 
# 3.2 Regress�o Linear Multivariada (mais de uma vari�vel ex�gena)
# mod1 <- lm(PIB~BRP+BRL, data=treino) 
# 3.3 Modelo autoregressivo
mod1 <- lm(PIB~PIBi1+PIBi2+PIBi4+PIBi12, data=treino) 
# 3.4 Modelo multivariado com dummy de sazonalidade
mod1 <- lm(PIB~BRP+BRL+D2+D5+D6+D7+D8+D9+D11, data=treino) 

# 4. An�lise da regress�o
# estat�sticas da regress�o
summary(mod1)

# 5. Previs�o
p <- predict(mod1, newdata = teste)

# 6. erro previs�o
cbind(teste$PIB,p, teste$PIB-p)
# m�tricas para erro de previs�o
# Root Mean Squared Error
RMSE <- sqrt(mean( (teste$PIB-p)^2 ))
# Sum of Squared Errors
SSE <- sum((teste$PIB-p)^2 )
