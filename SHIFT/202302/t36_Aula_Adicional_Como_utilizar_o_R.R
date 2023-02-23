# R para cálculos
1+2

# Como gerar amostras em R
install.packages("ggplot2").  # !pip install 
library(ggplot2). # equivalente ao comando import

df = diamonds

a1 = df[1:3000,]  #equivalente ao comando .iloc do pandas

# Estatísticas descritivas
mean(a1$price). #equivalente no pandas a df.mean
median(a1$price)
sd(a1$price)

hist(a1$price)

# Amostra aleatória
set.seed(33) #equivalente ao parâmetro random_state
va = sample(53940) 
a2 = df[va[1:3000],]
hist(a2$price)

# Análise de correlação linear
cor( df$price, df$x )

# Matriz de correlação
cor( df[,c("price", "x", "y", "z")] )    # Equivalente a utilizar o método .loc do pandas

#########################
# Regressões Lineares
df = read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/Data/pib.csv")

# Amostras de treino e teste | split
treino = df[1:132,]    # Atenção: em Python os índices começam em 0 e em R os índices começam em 1
teste  = df[133:138,]

# Regressões
# mod = lm(PIB~BRL, data=treino)   # RLS - no R utilizamos a fç nativa lm ao invés da biblioteca scikitlearn
# mod = lm(PIB~BRL+BRP, data=treino)   # RLM
# mod = lm(PIB~PIBi1+PIBi2+PIBi4+PIBi12 , data=treino)   # AR
mod = lm(PIB~BRL+BRP+D2+D5+D6+D7+D8+D9+D11 , data=treino)   # RLM + Dummies de Sazonalidade
summary(mod)  # analisar as estatísticas da regressão

# Previsão em teste
p = predict(mod, newdata = teste)

# Análise do erro de previsão
cbind(p, teste$PIB, p-teste$PIB, (p-teste$PIB)/teste$PIB )

# Métrica de análise de erro
sse = sum((p-teste$PIB)^2)
