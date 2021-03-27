# Clusteriza????o
m <- mtcars
plot(m$mpg~m$wt)

# Antes de executar kmeans
# por ele utilizar premissa de plano cartesiano
# precisamos adequar as escalas
# Alternativas: linear maior para escala linear menor
# linear para log ou vice-versa
install.packages("scales")
library(scales)

m$mpg_s <- rescale(m$mpg, c(0,1))
m$wt_s <- rescale(m$wt, c(0,1))
plot(m$mpg_s~m$wt_s)

# Como kmeans padr??o tem in??cio aleat??rio
# ?? interessante utilizar semente para garantir
# que consigamos reproduzir os resultados do kmeans
set.seed(33)
k<-kmeans(m[,c('mpg_s','wt_s')],5)

# Visualiza????o dos clusters com cores
plot(m$mpg~m$wt, col=k$cluster)

hist(m$mpg)
hist(m$wt)

# Exemplo de adequa????o de escalas
# log-linear
library(ggplot2)
d<-diamonds
hist(d$price)
hist(log(d$price))

################################
# EXERCICIO - EXECUTAR KMEANS SOBRE DESPESAS POR FORNECEDOR
##############################

## pacotes
lista_pacotes <- c('httr', 
                   'rjson', 
                   'jsonlite', 
                   'readr', 
                   'dplyr', 
                   'tidyr', 
                   'magrittr', 
                   'stringr', 
                   'lubridate', 
                   'tibble')

lapply(lista_pacotes, 
       library, 
       character.only = TRUE)


# dados despesas
dados_despesas <- fromJSON(
  paste(
    readLines( "https://transparencia.tce.sp.gov.br/api/json/despesas/balsamo/2015/1" ),
    collapse = "" )
)

dados_despesas %<>% 
  as_tibble() %>% 
  mutate(dt_emissao_despesa = dmy(dt_emissao_despesa),
         vl_despesa = as.numeric(gsub(",", ".", gsub("\\.", "", vl_despesa))) )

# ajustes dos dados

despesas_por_fornecedor <- dados_despesas %>% 
  group_by(nm_fornecedor) %>% 
  summarise( vl_despesa = mean(vl_despesa),
             qtd = n() )
