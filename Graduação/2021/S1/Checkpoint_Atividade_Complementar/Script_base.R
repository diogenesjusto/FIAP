# Carga de dados
df <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/Checkpoint_Atividade_Complementar/faturamento.csv")
# Filtrando loja
dfl1 <- df[df$loja=="Loja 1",]
# analise descritiva
plot(dfl1$GMV, type="l")
# Separação treino teste
treino <- dfl1[1:365,]
teste  <- dfl1[366:396,]
# Modelagem
mod <- lm(GMV~X, data=treino)
summary(mod)
# Previsão
prev <- predict(mod, newdata = teste )
# Avaliação
erro_prev <- prev-teste$GMV
erro_med_prev <-  mean(abs(erro_prev))
