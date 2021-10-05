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
  group_by( ) %>%
  # renomeando SQL pra ficar no padrão e corrigindo SQ que não têm 6 dígitos
  rename( SQL = sql ) %>%
  rename( SQ = sq ) %>%
  mutate( SQ = case_when(
    nchar( SQ ) == 5 ~ paste0( "0" , SQ ),
    TRUE ~ SQ
  ) 
  ) %>%
  mutate( 
    # corrigindo camada de número
    ac_total = as.numeric( str_replace_all( ac_total , "\\," , "\\." ) ),
    # ano do processo administrativo
    ano_processo_adm = as.numeric( str_sub( processo_adm , 0 , 4 ) ),
    # SQL só números
    SQL = str_replace_all( SQL , "\\." , "" )
  )