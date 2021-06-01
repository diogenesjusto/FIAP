############################################################
# Teoria de probabilidades
install.packages("caret")
library(party)
library(caret)
# 1. Carga de dados
d <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/Gradua%C3%A7%C3%A3o/2021/S1/dados_arvore.csv")

# 2. Tratamento de dados
d$Pclass_f <- as.factor(d$Pclass)
d$Adulto_f <- as.factor(ifelse(d$Age>17,1,0))

# 3. Separação treino e teste
set.seed(33)
va <- sample(nrow(d))
treino <- d[va[1:600],]
teste  <- d[va[601:891],]

# 4. Modelagem
mod <- ctree(Survived~Pclass_f+SibSp+Age, data=treino)

# 5. Previsão em teste
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5,0,1)

# 6. Análise do erro de previsão
# Matriz de confusão "na mão"
table(prev, teste$Survived)
# M.Conf. pacote
confusionMatrix(as.factor(prev), as.factor(teste$Survived) )


# Variáveis categóricas
v1 <- c(1,2,3,4,5)
v2 <- (c(2,2,3,3,4))
df <- as.data.frame(cbind(v1,v2))
df$v2 <- as.factor(df$v2)
rbind(df,c(6,"7"))
# verificar quais categorias de uma variável factor
levels(df$v2)
# Inserção de uma nova categoria na variável factor
levels(df$v2) <- c(levels(df$v2),"7")
rbind(df,c(6,"7"))

# Video da palestra sobre classificador de fraude
# https://www.infoq.com/br/presentations/estatistica-e-matematica-aplicadas-a-data-science/
