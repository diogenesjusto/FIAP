library(MASS)

m<-mtcars

mod<-lm(mpg~., data=m)
# StepWise (combina????o de colunas)
mod.step<-stepAIC(mod)
  # StepWise (com multiplica????o de colunas)
  # mod.step<-stepAIC(mod,~.^2)
summary(mod.step)
# Eliminar vari??veis de um modelo sem recalcul??-lo 
# mod2 <-update(mod,~. - vs)

# Incluindo novas colunas calculadas
m_log<-log(mtcars)
# Muda nomes das colunas
# names(m_log)<-paste("log",names(m_log),sep="_")
# Elimina coluna alvo (mant??m s?? features)
# m_log$log_mpg<-NULL
# Une os 2 dataframes
m2<-cbind(m,m_log)
# Remove colunas com valores INF
# m2<-cbind(m,m_log[,apply(m_log, 2, function(x) all(is.finite(x)))])
mod2<-lm(mpg~., data=m2)
mod.step<-stepAIC(mod2)
summary(mod2)
