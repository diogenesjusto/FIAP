d <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/dados_arvore.csv")

# Etapa exploratória: P = E / T
# T = # de pessoas, neste caso nrow(d) ou 891
# Probabilidades de sobrevivência
prop.table(table(d$Survived))
#         "      de passageiro ser um gênero
prop.table(table(d$Sex))

# Probabilidades de eventos combinados
prop.table(table(d$Survived, d$Sex))

# Análise de árvore de decisão
mod <- ctree(Survived~Sex+Pclass+Age, data=d)
# Visualização gráfica da árvore de decisão
plot(mod, type="simple")

# Aplicando em um modelo preditivo
set.seed(33)
va <- sample(891)
treino <- d[va[1:600],]
teste  <- d[va[601:891],]

mod <- ctree(Survived~Sex+Pclass+Age, data=treino)

p <- predict(mod, newdata=treino)
pred <- ifelse(p<.5,0,1)
# Cálculo da acurácia=>taxa de erro = 1 - acurácia
# erro de modelagem
emod <- 1 - sum(ifelse(pred==treino$Survived,1,0))  / 600

p <- predict(mod, newdata=teste)
pred <- ifelse(p<.5,0,1)
# Taxa de erro (em previsão) = 1 - acurácia
# erro previsão
eprev <- 1 - sum(ifelse(pred==teste$Survived,1,0))  / 291
