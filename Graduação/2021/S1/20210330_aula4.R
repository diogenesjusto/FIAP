# Carga dos dados de PIB e tráfego desde o github
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# Matriz de correlação (linear)
cor(pib[,3:15])

# Gráfico para analisar a relação entre duas variáveis
# Gráfico de dispersão | Scatterplot
plot(pib$PIB~pib$SPT)

# Separação Treino e Teste
treino <- pib[1:126,]
teste <- pib[127:138,] 

# Modelo Regressão Linear (assume linearidade)
mod <- lm(PIB~SPT, data=treino)
# Análise de uma regressão linear
summary(mod)

