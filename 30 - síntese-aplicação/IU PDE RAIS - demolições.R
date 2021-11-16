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
    demolições <- temp
  } else{
    demolições <- bind_rows( demolições , temp )
  }
  
  rm(temp)
  
}


demolições %>%
  group_by( Ano , SQ ) %>%
  summarize( Construído = sum( Construído ) ) %>%
  ungroup() %>%
  # classificar por SQ e ano
  arrange( SQ , Ano ) %>%
  # calculando variação da área construída e removendo os inválidos
  mutate( 
          variação_percent = case_when(
                                SQ != lag( SQ , 1 ) ~ NA_real_,
                                TRUE ~ ((Construído/lag(Construído,1))-1)*100
                              ), 
          variação = case_when(
                                SQ != lag( SQ , 1 ) ~ NA_real_,
                                TRUE ~ Construído - lag(Construído,1)
                              )
        ) %>%
  # filtrando demolição
  filter( variação < 0 ) %>%
  arrange( variação ) %>%
  write_xlsx( "./30 - síntese-aplicação/IU PDE RAIS - demolições 2014-2019.xlsx" )
