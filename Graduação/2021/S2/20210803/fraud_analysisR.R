library(party)
library(caret)

df <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/fraude_study.csv")

prop.table(table(df$RESULTADO))

set.seed(33)
va <- sample(50000)

train <- df[va[1:30000],]
test <- df[va[30001:50000],]


mod <- ctree(RESULTADO ~ DESC_MUNICIPIO + FATURADO, data=train)

p <- predict(mod, newdata=test)

table(p)
prop.table(table(p))

table(p, test$RESULTADO)
confusionMatrix(p, test$RESULTADO)
