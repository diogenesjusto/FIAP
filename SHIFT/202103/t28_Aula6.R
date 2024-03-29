# Vari�vel categ�rica
v1 <- c(1,2,2,4) # num.
v1 <- as.factor(v1) # transf em vari�vel categ�rica
levels(v1)
levels(v1) <- c(levels(v1),"6")

# Vari�veis dummy no R
install.packages("dummies")
library(dummies)
m <- mtcars
m <- cbind(m, dummy(m$cyl))

# Instala��o do pacote para Confusion Matrix
install.packages("caret")
install.packages("e1071")

# Tabela din�mica no R
install.packages("rpivotTable")
library(rpivotTable)
rpivotTable(dt)

# Classificadores
# �rvores de decis�o
# 0. Pacotes utilizados
library(party)
library(caret)
library(e1071)

# 1. Carga de dados
dt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv")

# 2. Tratamento de dados (feature eng.)
dt$Sex_f <- as.factor(dt$Sex)
dt$Pclass_f <- as.factor(dt$Pclass)

# 3. Separa��o de treino e teste
set.seed(33)
va <- sample(nrow(dt))
treino <- dt[va[1:600],]
teste  <- dt[va[601:891],]

# 4. Modelagem (�rvore bin�ria ctree )
mod <- ctree(Survived~Sex_f+Pclass_f+Age, data=treino)
plot(mod, type="simple")

# 5. Previs�o em teste
p <- predict(mod, newdata=teste)
# Ajuste fino no threshold (ponto de corte)
# visando adequar a distribui��o de probabilidade esperada
prev <- ifelse(p<.4, 0, 1)

# 6. An�lise de previs�es (classificador)
# Matriz de confus�o "na m�o"
table(prev, teste$Survived )
# Matriz de confus�o utilizando package
confusionMatrix(as.factor(prev), as.factor(teste$Survived))
