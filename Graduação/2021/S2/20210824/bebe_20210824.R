baba <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/1_Checkpoint/baba.csv")
b <- baba
# Análise Exploratória
plot(b$venda, type="l")

# hipóteses
# 1. tratar bf
b$D_bf <- ifelse(b$X >= 119 & b$X <= 124, 1, 0)

# 2. saz semanais
boxplot(b$venda ~ (b$weekday))
library(dummies)
b <- cbind(b, dummy(b$weekday))
b$D_Sem_SabDom <- ifelse(b$b1==1|b$b4==1,1,0)

# 3. explorar saz mensal?
boxplot(b$venda ~ (b$mes))
b <- cbind(b, dummy(b$mes))
b$Mes_num <- substring(b$date,5,6)
b$D_Mes_MaiJunJul <- ifelse(b$Mes_num=="05"|b$Mes_num=="06"|b$Mes_num=="07",1,0)
# 4. rel entre venda x desconto x margem
plot(b$venda~b$margem)
abline(lm(b$venda~b$margem))
plot(b$venda~b$desconto)
(lm(b$venda~poly(b$desconto,2)))
# 5. analisar outlier de desc e mg
# 6. agregar dados externos

treino <- b[1:365,]
teste  <- b[366:396,]

mod <- lm(venda~poly(desconto,3)+margem
          +D_bf
          +D_Sem_SabDom
          +, data=treino)
summary(mod)
p<-predict(mod, newdata=teste)

df <- as.data.frame(cbind(1:31,p))
names(df)<-c("id","venda")