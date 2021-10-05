library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(stats)
library(ggplot2)
library(sf)
library(readxl)

######## alvarás ########
arquivo <- "./00 - dados brutos/msp_empreendimentos_sisacoe.xlsx"

alvaras <- read_excel( arquivo ) %>%
  # renomeando SQL pra ficar no padrão
  rename( SQL = sql ) %>%
  rename( SQ = sq ) %>%
  # removendo SQLs inválidos (diferente de 12 caracteres)
  filter( nchar(SQL) != 12 ) %>%
  mutate( 
          # corrigindo camada de número
          ac_total = as.numeric( str_replace_all( ac_total , "\\," , "\\." ) ),
          # ano do processo administrativo
          ano_processo_adm = as.numeric( str_sub( processo_adm , 0 , 4 ) ),
          # SQL só números
          SQL = str_replace_all( SQL , "\\." , "" ),
          # SQ pelo SQL, mais confiável
          SQ = str_sub( SQL , 0 , 6 )
        ) %>%
  # só linhas com ano de processo administrativo
  filter( is.na( ano_processo_adm ) == FALSE & ano_processo_adm > 2000 ) %>%
  # pegando só o alvará mais recente pra cada SQL
  group_by( SQL ) %>%
  slice_max( alvara_numero ) %>%
  ungroup()

arquivo <- "./10 - processamentos/IU PDE IPTU - 0 SISACOE.csv"
write_csv2( alvaras , arquivo )
