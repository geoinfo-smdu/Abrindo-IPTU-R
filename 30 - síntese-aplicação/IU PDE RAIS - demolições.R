library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(writexl)

ListaAnos <- 2014:2019

for (ano in ListaAnos){
  
  print( paste( "Processando" , ano )  )
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_loteArrumado.csv.gz")
  
  temp <- read_csv2( arquivo , locale=locale(encoding="UTF8") )
  
  # juntando
  if (ano == ListaAnos[1]){
    demoli��es <- temp
  } else{
    demoli��es <- bind_rows( demoli��es , temp )
  }
  
  rm(temp)
  
}


demoli��es %>%
  group_by( Ano , SQ ) %>%
  summarize( Constru�do = sum( Constru�do ) ) %>%
  ungroup() %>%
  # classificar por SQ e ano
  arrange( SQ , Ano ) %>%
  # calculando varia��o da �rea constru�da e removendo os inv�lidos
  mutate( 
          varia��o_percent = case_when(
                                SQ != lag( SQ , 1 ) ~ NA_real_,
                                TRUE ~ ((Constru�do/lag(Constru�do,1))-1)*100
                              ), 
          varia��o = case_when(
                                SQ != lag( SQ , 1 ) ~ NA_real_,
                                TRUE ~ Constru�do - lag(Constru�do,1)
                              )
        ) %>%
  # filtrando demoli��o
  filter( varia��o < 0 ) %>%
  arrange( varia��o ) %>%
  write_xlsx( "./30 - s�ntese-aplica��o/IU PDE RAIS - demoli��es 2014-2019.xlsx" )
