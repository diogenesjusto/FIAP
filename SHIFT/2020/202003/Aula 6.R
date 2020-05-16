# Aula 6 - Classificadores
# Probabilidades
df <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/train.csv")

# Table - contagem
table(df$Survived)
# Prop.Table - prop/prob.
prop.table(table(df$Survived))
# Prop.Table - prop/prob.
prop.table(table(df$Sex))
# Matriz de conting??ncia
prop.table(table(df$Sex,df$Survived))
# Matriz de conting??ncia c totais
addmargins(prop.table(table(df$Sex,df$Survived)))

# ??rvore de decis??o
install.packages("party")
library(party)
# Modelo de ??rvore de decis??o com uma feature
mod <- ctree(Survived~Sex, data=df)
plot(mod, type='simple')
# Modelo de ??rvore de decis??o com duas features
mod <- ctree(Survived~Sex+Pclass, data=df)
plot(mod, type='simple')

# 0 morte     .5   sobrev 1 #
#   7    6    4          3  #
# Bom 3, 7, +- 6  Ruim 4

# 0 morte     .5    sobrev 1 #
#   Bom +-   Ruim  +-   Bom  #
# Modelo de ??rvore de decis??o com duas features
mod <- ctree(Survived~Sex+Pclass+Age, data=df)
plot(mod, type='simple')

# Script completo
# M.L. com ??rvores de decis??o
# 1. Carga de dados
df <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/train.csv")
# 2. Tratamento
# 3. Amostra treino e teste
set.seed(33)
va<-sample(891)
treino <- df[va[1:600],]
teste  <- df[va[601:891],]
# 4. Modelagem
mod <- ctree(Survived~Sex+Pclass, data=treino)
# 5. Previs??o em teste
p<-predict(mod, newdata=teste)
pred<-ifelse(p<.5,0,1)
# 6. Erro de previs??o: para um classificador utilizaremos outras m??tricas (diferentes de sse, mse, etc)
# Compara????o previs??es x valores reais
addmargins(table(pred, teste$Survived))
# Total de acertos: Acertos+ e Acertos-
# Acur??cia : #Acertos / #Total
ACC <- (169+48)/291
# M??trica para F.P. => Precision = TP/(FP+TP)
P <- 48/(4+48)
# M??trica para F.N. => Recal = TP/(FN+TP)
R <- 48/(70+48)




