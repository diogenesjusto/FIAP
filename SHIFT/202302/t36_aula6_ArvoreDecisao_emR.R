# �rvore de decis�o 
install.packages("party")
library("party")

# �rvore de decis�o 
tit <- read.csv("https://raw.githubusercontent.com/diogenesjusto/FIAP/master/dados/train.csv")

# Constru��o da �rvore de decis�o
# com transf. da vari�vel Sex de caractere para categ�rica
# utilizando a fun��o as.factor
mod <- ctree(Survived~as.factor(Pclass)+as.factor(Sex)+Age, data=tit)
plot(mod, type="simple")
