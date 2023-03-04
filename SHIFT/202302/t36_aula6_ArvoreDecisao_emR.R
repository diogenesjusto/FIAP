# Árvore de decisão 
install.packages("party")
library("party")

# Árvore de decisão 
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")

# Construção da árvore de decisão
# com transf. da variável Sex de caractere para categórica
# utilizando a função as.factor
mod <- ctree(Survived~as.factor(Pclass)+as.factor(Sex)+Age, data=tit)
plot(mod, type="simple")
