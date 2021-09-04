library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)

# CONFIGURAR CAMINHO MANUALMENTE
# caminho <- "pasta do repositório"
setwd(caminho)

########## 21_1 -  ##########

arquivo <- paste0( "./20 - info/", "IPTU_ParâmetrosPorAnoPorQuadra.csv.gz" )
IPTU_21_1_gráficos_bruto <- na.omit( read_csv2( arquivo ) )

IPTU_21_1_gráficos_bruto %>%
  group_by( ano ) %>%
  summarise( construído_total = sum( construído ) ) %>%
  mutate( construído_total = construído_total - lag( construído_total , 1 ) ) %>%
  ggplot( aes( x = factor(ano) , y = construído_total/1000000 ) ) +
  geom_bar( stat = "identity" )












variáveis <- tail( colnames( read_csv2( paste0("./10 - processamentos/IPTU_" , "1995" , "_ResumoQuadra.csv.gz")  , n_max = 1 ) ) , -1 )
arquivo <- paste0("./20 - info/" , "IPTU_ResumoQuadra.csv.gz")

IPTU_21_2_geo_bruto <- read_csv2( arquivo )

# escolher a variável e inserir a posição abaixo conforme a lista variáveis
variávelDesejada <- 3
índices <- seq( from = variávelDesejada, to = ncol(IPTU_21_bruto), by = 12 )

# 
col_index <- seq(1:ncol(IPTU_21_bruto)) 
IPTU_21_bruto %>%
  select( 1 , col_index[ col_index %in% índices != 0 ] ) %>% 
  ggplot( aes( x = factor( str_sub(  ) )  ) )

 
