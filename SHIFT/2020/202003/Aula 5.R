# Aula 5 - Regress??es Lineares +sobre
m <- mtcars
# Transforma????o e limpeza de vari??veis
m$cyl_F <- as.factor(m$cyl)
# Gera????o de amostras de treino e teste
set.seed(33)
va <- sample(32)
treino <- m[va[1:24],]   # ~66% (80-20, 70-30, etc)
teste  <- m[va[25:32],]  # ~33%
# Modelagem
# mod <- lm(mpg~wt,data=treino)
# mod <- lm(mpg~log(wt),data=treino) # Transf. Log-Linear
# mod <- lm(mpg~poly(wt,2),data=treino) # Polin??mio de 2.o grau

# Simula????o do overfit: modelo sobreajustado nos dados de treino
# por??m que n??o consegue fazer uma boa previs??o em novos dados (teste ou futuros)
#mod <- lm(mpg~poly(wt,3),data=treino) # Polin??mio de 3.o grau
#mod <- lm(mpg~poly(wt,6),data=treino) # Polin??mio de 6.o grau
#mod <- lm(mpg~poly(wt,15),data=treino) # Polin??mio de 11.o grau

# Modelando vari??veis num??ricas => categ??ricas
# mod <- lm(mpg~wt+cyl,data=treino) # 1.o Cyl como var num??rica
mod <- lm(mpg~wt+cyl_F,data=treino) # 2.o Cyl como var categ??rica

summary(mod)
# p=> valores previstos para vari??vel mpg, da base de teste
p <- predict(mod, newdata = teste)
# teste$mpg => valores reais da vari??vel mpg
# tabela comparativa entre valores previstos, valores reais e erro de previs??o
cbind(p, teste$mpg, p-teste$mpg)

sse<-sum((p-teste$mpg)^2)


# Transforma????es lineares
# Comport. que parece acelerar exp a vari??vel wt
plot(m$mpg~m$wt)
# Transf. log-linear
plot(m$mpg~log(m$wt))
cor(m$mpg,m$wt)
cor(m$mpg,log(m$wt))

# V??rias medidas de erro de previs??o
# ----------------------------------
# M??dia dos valores absolutos dos erros de previs??o
# => Est?? na mesma unidade de medida da vari??vel original
mean(abs(p-teste$mpg))
# Erro relativo (erro proporcional ou percentual) - erros de previs??o
# => Est?? em %
mean(abs(p-teste$mpg))/mean(teste$mpg)
# M??dia dos quadrados dos erros de previs??o
# => Est?? ^2 erro absoluto - M.S.E.
mean((p-teste$mpg)^2)
# Soma dos quadrados dos erros de previs??o - S.S.E.
# => Soma dos ^2 | ??til para efeitos
sum((p-teste$mpg)^2)


# Vari??vel num??rica x vari??vel categ??rica
plot(m$mpg~m$cyl)
plot(m$mpg~m$wt)
# Gr??fico de dispers??o com cyl sendo a "cor" dos pontos
plot(m$mpg~m$wt, col=m$cyl)


# Classificadores
p4 = nrow(m[m$cyl==4,])/nrow(m)
p6 = nrow(m[m$cyl==6,])/nrow(m)
p8 = nrow(m[m$cyl==8,])/nrow(m)

# Table => contagem de agrupamentos
# Prop.table => propor????o (percentual)
prop.table(table(m$cyl))
prop.table(table(m$am))
# Contagem de agrupamentos cruzados
table(m$cyl,m$am)
# Propor????es de agrupamentos cruzados
prop.table(table(m$cyl,m$am))
# Adicionar totais
addmargins(prop.table(table(m$cyl,m$am)))



