install.packages("randomForest")
library(party)
library(e1071)
library(randomForest)
# Script completo
# M.L.: (1) A.D. (2) RegLogist. (3) Random Forest
# 1. Carga de dados
df <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/SHIFT/2020/202003/train.csv")
# 2. Tratamento Dados
df$Sex <- as.factor(df$Sex)
df$Survived_F <- as.factor(df$Survived)
df$Pclass_F <- as.factor(df$Pclass)
df[is.na(df$Age),]$Age <- mean(df$Age,na.rm=TRUE) # By Michelli
# 3. Amostra treino e teste
set.seed(33)
va<-sample(891)
treino <- df[va[1:600],]
teste  <- df[va[601:891],]
# 4. Modelagem
# mod <- ctree(Survived_F~Sex+Pclass_F, data=treino) # (1) A.D.
# mod <- glm(Survived_F~Sex+Pclass_F+Age, data=treino, family = binomial()) # (2) Reg.Logist.
mod <- randomForest(Survived_F~Sex+Pclass+Age, data=treino, ntree=180) # Rand.Forest
varImpPlot(mod)
plot(mod)
# 5. Previs??o em teste
p<-predict(mod, newdata=teste)
# 6. Erro de previs??o: para um classificador utilizaremos outras m??tricas (diferentes de sse, mse, etc)
# Compara????o previs??es x valores reais
confusionMatrix(p,teste$Survived_F)


