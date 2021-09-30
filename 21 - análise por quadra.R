library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(stats)
library(ggplot2)
library(sf)

# caminho <- "pasta do repositório"
setwd(caminho)

######## resumindo por quadra ########
####  ####
# lista de anos com base do IPTU, de 1995 até o ano atual
ListaAnos <- 1995:(year(today()))

for (ano in ListaAnos){
  
  print( paste( "Processando" , ano )  )
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_arrumado.csv.gz")
  
  # cálculos por lote antes de resumir por quadra
  temp <- read_csv2( arquivo , locale=locale(encoding="latin1") ) %>%
    # resumindo por quadra
    group_by( ano , SQ ) %>%
    # combinando por soma ou mediana os atributos
    summarize( 
                across( ValorTerreno:AnoConstr , ~weighted.mean( . , Construído ) , .names = "{.col}_ponderado" ),
                across( Frentes:Ocupado , sum , .names = "{.col}_soma" ),
                across( c(ValorTerreno:Testada,CA_lote:TO_lote) , median , .names = "{.col}_mediana" ),
              ) %>%
    ungroup() %>%
    # CA e TO da quadra toda, valor m2 e ano ponderados
    mutate( 
            CA = Construído_soma/Terreno_soma,
            TO = Ocupado_soma/Terreno_soma
            )
  
  # juntando
  if (ano == 1995){
    IPTU_21_0 <- temp
  } else{
    IPTU_21_0 <- bind_rows( IPTU_21_0 , temp )
  }
  
}

######## juntando geometria e geografia ########
#### juntando geometria à tabela ####
# lendo arquivo geopackage contendo quadras
arquivo2 <- "./00 - dados brutos/geo.gpkg"

quadras <- st_read( arquivo2 , layer = "Quadras fiscais" ) %>%
  # criando coluna SQ
  mutate( SQ = paste0( qd_setor , qd_fiscal ) ) %>%
  # agrupando as geometrias das quadras com subquadras
  group_by( qd_setor , qd_fiscal , qd_tipo , SQ ) %>% 
  summarise() %>% 
  ungroup() %>%

# salvando processamento
st_write( quadras , "./10 - processamentos/geo.gpkg" , "Quadras fiscais sem subquadras" )

# juntando quadras arrumadas ao tabelão
IPTU_21_0 <- IPTU_21_0 %>% inner_join( quadras %>% select(SQ,geom) )

# salvando
arquivo <- paste0( "./20 - info/21 - por quadra - IPTU" , ".csv.gz")
write_csv2( IPTU_21_0 , arquivo )

##############
############## MOVER O DE CIMA PRA O FINAL, PRA EXPORTAR QUADRAS CONTENDO ASSOCIAÇÃO
############## COM AS DIVISÕES
##############








#### associando a diversas divisões ####
IPTU_21_0 <- read_csv2( arquivo )

#### gráficos ####

# novas áreas construídas por ano
IPTU_21_1_ACporano <- IPTU_21_0 %>%
  group_by( ano ) %>%
  summarise( Construído_soma = sum( Construído_soma , na.rm = TRUE ) ) %>%
  ungroup() %>%
  mutate( construído_total = Construído_soma - lag( Construído_soma , 1 ) )

IPTU_21_1_ACporano %>%
  ggplot( aes( x = factor(ano) , y = construído_total/1000000 ) ) +
  geom_bar( stat = "identity" )

