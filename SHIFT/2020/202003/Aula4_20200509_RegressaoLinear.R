# Regress??o Linear
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/pib.csv")

# Separa????o dados treino e teste - janela temporal pois s??o dados "temporais"
treino <- pib[1:124,]
teste  <- pib[125:136,]

# R.L.S. (somente uma vari??vel ex??gena)
mod <- lm(PIB~BRL, data=treino)
# Par??metros da regress??o
summary(mod)

# C??lculo manual da previs??o de PIB para 1.o m??s da base teste (mai/14)
    # PIB = A*BRL + B
    # PIB = 0.64489 * BRL + 52.43479
    # PIB[mai/14] = 0.64489 * BRL[mai/14] + 52.43479
    # PIB[mai/14] = 0.64489 * 149.68 + 52.43479
    pib_m14 <- (0.64489 * 149.68 + 52.43479)
    # Qual a diferen??a entre o valor previsto de PIB para mai/14 
    # e o valor real conhecido de mai/14?
    #  (pib real mai/14 est?? na base teste = 154.49)
    pib_m14 - 154.49
    # -5.52 ?? erro de previs??o de mai/14
    
# C??lculo previs??o pelo R
pib_prev <- predict(mod,newdata = teste)
# C??lculo de erro de previs??o
erro_medio_absoluto<-mean(abs(pib_prev - teste$PIB))
erro_medio_relativo<-erro_medio_absoluto/mean(teste$PIB)

# R.L.M. (mais de uma vari??vel ex??gena)
mod <- lm(PIB~SLP+SPP, data=treino)
# Par??metros da regress??o
summary(mod)
# C??lculo previs??o pelo R
pib_prev <- predict(mod,newdata = teste)
# C??lculo de erro de previs??o
erro_medio_absoluto<-mean(abs(pib_prev - teste$PIB))
erro_medio_relativo<-erro_medio_absoluto/mean(teste$PIB)

# A.R. (modelo autoregressivo)
mod <- lm(PIB~PIBi1+PIBi2, data=treino)
# Par??metros da regress??o
summary(mod)
# C??lculo previs??o pelo R
pib_prev <- predict(mod,newdata = teste)
# C??lculo de erro de previs??o
erro_medio_absoluto<-mean(abs(pib_prev - teste$PIB))
erro_medio_relativo<-erro_medio_absoluto/mean(teste$PIB)

# M.V. Sazonalidade 
mod <- lm(PIB~BRL+BRP+D2+D5+D6+D7+D8+D9+D11, data=treino)
# Par??metros da regress??o
summary(mod)
# C??lculo previs??o pelo R
pib_prev <- predict(mod,newdata = teste)
# C??lculo de erro de previs??o
erro_medio_absoluto<-mean(abs(pib_prev - teste$PIB))
erro_medio_relativo<-erro_medio_absoluto/mean(teste$PIB)




