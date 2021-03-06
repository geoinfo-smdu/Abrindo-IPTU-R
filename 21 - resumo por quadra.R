library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(stats)
library(ggplot2)
library(sf)
library(writexl)

# caminho <- "pasta do reposit�rio"

######## resumindo por quadra ########
####  ####
# lista de anos com base do IPTU, de 1995 at� o ano atual
ListaAnos <- (year(today())):1995

for (ano in ListaAnos){
  
  print( paste( "Processando" , ano )  )
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_loteArrumado.csv.gz")
  
  temp <- read_csv2( arquivo , locale=locale(encoding="UTF8") )
  
  # c�lculos por lote antes de resumir por quadra
  temp <- temp %>%
    # resumindo usos e padr�es
#    mutate(
#            TipoUso = ,
#            UsoH = case_when(
#                                  str_detect( TipoUso, "" )
#                                ),
#          ) %>%
    # resumindo por quadra
    group_by( ano , SQ ) %>%
    # combinando por soma ou mediana os atributos
    summarize( 
                across( ValorTerreno:AnoConstr , ~weighted.mean( . , Constru�do ) , .names = "{.col}_ponderado" ),
                across( Frentes:Ocupado , sum , .names = "{.col}_soma" ),
                across( c(ValorTerreno:Testada,CA_lote:TO_lote) , median , .names = "{.col}_mediana" ),
#                UsoH = 
              ) %>%
    ungroup() %>%
    # CA e TO da quadra toda, valor m2 e ano ponderados
    mutate( 
            CA = Constru�do_soma/Terreno_soma,
            TO = Ocupado_soma/Terreno_soma
            )
  
  # juntando
  if (ano == ListaAnos[1]){
    IPTU_21_0 <- temp
  } else{
    IPTU_21_0 <- bind_rows( IPTU_21_0 , temp )
  }
  
}

arquivo <- "./20 - info/21_1 - resumo por quadra (sem geometria).csv.gz"
write_csv2( IPTU_21_0 , arquivo )

######## juntando geometria e geografia ########
#### juntando geometria � tabela ####
# lendo arquivo geopackage contendo quadras
arquivo2 <- "./00 - dados brutos/geo.gpkg"

quadras <- st_read( arquivo2 , layer = "4.06 - Cadastro - Quadras fiscais" ) %>%
  # criando coluna SQ
  mutate( SQ = paste0( qd_setor , qd_fiscal ) ) %>%
  # agrupando as geometrias das quadras com subquadras
  group_by( qd_setor , qd_fiscal , qd_tipo , SQ ) %>% 
  summarise() %>% 
  ungroup()

# salvando processamento
st_write( quadras , "./10 - processamentos/geo.gpkg" , "4.06 - Cadastro - Quadras fiscais sem subquadras" )

arquivo2 <- "./10 - processamentos/geo.gpkg"
quadras <- st_read( arquivo2 , layer = "4.06 - Cadastro - Quadras fiscais sem subquadras" ) 

# juntando quadras arrumadas ao tabel�o
IPTU_21_0 <- IPTU_21_0 %>%
  # juntando a geometria e transformando camada em georreferenciada
  inner_join( quadras %>% select(SQ,geom) ) %>%
  st_as_sf(sf_column_name = "geom") %>%
  

############### FALTA JUNTAR MACROAREA MACROZONA ETC

# salvando
st_write( IPTU_21_0 , "./20 - info/21_2 - resumo por quadra.gpkg" , "geo" )




#### associando a diversas divis�es ####
IPTU_21_0 <- read_csv2( arquivo )

#### gr�ficos ####

# novas �reas constru�das por ano
IPTU_21_1_ACporano <- IPTU_21_0 %>%
  group_by( ano ) %>%
  summarise( Constru�do_soma = sum( Constru�do_soma , na.rm = TRUE ) ) %>%
  ungroup() %>%
  mutate( constru�do_total = Constru�do_soma - lag( Constru�do_soma , 1 ) )

IPTU_21_1_ACporano %>%
  ggplot( aes( x = factor(ano) , y = constru�do_total/1000000 ) ) +
  geom_bar( stat = "identity" )

