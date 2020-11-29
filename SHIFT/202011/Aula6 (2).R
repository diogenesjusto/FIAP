# 0. Bibliotecas
library(party)
library(caret)
library(dummies)

# 1. Carga dados
dt <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train_titanic.csv", stringsAsFactors = TRUE)

# 2. Tratamento de variáveis
dt[is.na(dt$Age),]$Age <- mean(dt[!is.na(dt$Age),]$Age)
dt$Pclass_F <- as.factor(dt$Pclass)
dt<- cbind(dt, dummy(dt$Pclass_F))

prop.table(table(dt$Survived))

# 3. Separação dados treino e teste
set.seed(33)
va <- sample(nrow(dt))
treino <- dt[va[1:600],]
teste  <- dt[va[601:891],]

# 4. Modelagem
mod <- ctree(Survived~Age+Sex+Pclass, data=treino)
plot(mod, type="simple")
# 5. Previsão
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.4,0,1)
# ifelse(prev==teste$Survived => Cálculo de acertos
head(cbind(prev, teste$Survived, ifelse(prev==teste$Survived,1,0)),20)
# Soma de acertos
sum(ifelse(prev==teste$Survived,1,0))

### Métricas de avaliação de classificadores
### Erro previsão => uma das métricas é ACC
# ACC - Acurácia - acuracy 
# ACC = (sum(Ac Posit)+sum(Ac Negat))/#tot
# ACC = (TP+TN)/#tot
sum(ifelse(prev==teste$Survived,1,0))/nrow(teste)

# Matriz de Confusão - Confusion Matrix
table(prev, teste$Survived)
confusionMatrix(prev, teste$Survived)

# Variável categórica -> factor
levels(dt$Pclass_F)
levels(dt$Pclass_F) <- c(levels(dt$Pclass_F), "4")

# RandomForest
library(randomForest)

modRF <- randomForest(Survived~.-Name-Ticket-Cabin-PassengerId-Pclass-Pclass_F, data=dt)
# Análise de importância de variáveis
varImpPlot(modRF)

