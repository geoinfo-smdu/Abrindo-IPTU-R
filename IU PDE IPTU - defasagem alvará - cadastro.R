library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(stats)
library(ggplot2)
library(sf)
library(readxl)

# alvarás
arquivo <- "./00 - dados brutos/msp_empreendimentos_sisacoe.xlsx"

alvarás <- read_excel( arquivo ) %>%
  rename( SQL = sql ) %>%
  mutate( 
    ano_processo_adm = as.numeric( str_sub( processo_adm , 0 , 4 ) ),
    SQL = str_replace_all( SQL , "\\." , "" )
  )


# 2019
ano <- 2019
arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_arrumado.csv.gz")
temp2019 <- read_csv2( arquivo , locale=locale(encoding="latin1") )

# 2020
ano <- 2020
arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_arrumado.csv.gz")
temp <- read_csv2( arquivo , locale=locale(encoding="latin1") )

#### base ####
# diferença 2019-2020
diferença <- temp %>%
  # pegando só as colunas de interesse
  select( SQL , Construído ) %>%
  # juntando os dados de 2019 para comparação
  inner_join( temp2019 %>% select( SQL , Construído ) %>% rename( Construído2019 = Construído ) ) %>%
  # calculando a diferença
  mutate( `diferença2019-2020` = Construído / Construído2019 )

#### teste flexível - só ver mudança positiva de área construída ####
# imóveis onde teve mudança positiva de área construída
iptualvará011 <- diferença %>%
  # pegando diferenças que não são 0 (demolição ou algo do gênero), 1 (ficou igual), ou entre 0 e 1 (diminuiu área construída)
  filter( `diferença2019-2020` !=0 & `diferença2019-2020` != 1 & `diferença2019-2020` > 1  )

# imóveis onde teve mudança positiva de área construída presentes na base do SISACOE
iptualvará012 <- iptualvará011 %>%
  # juntando com os alvarás pra ver quais tem
  inner_join( alvarás %>% distinct() ) %>%
  select( SQL , legislacao_pde , id ) %>%
  distinct() %>%
  mutate( válido = !is.na(id)  )

# totalizando os planos diretores
iptualvará012 %>% group_by(legislacao_pde) %>% summarize( n = n())

#### teste rigoroso - ver o que deveria ser edificação nova (isto é, área construída ir de 0 para X) ####
iptualvará021 <- diferença %>%
  # pegando diferenças que não são 0 (demolição ou algo do gênero), 1 (ficou igual), ou entre 0 e 1 (diminuiu área construída)
  filter( is.infinite( `diferença2019-2020` ) == TRUE  )

iptualvará022 <- iptualvará021 %>%
  # juntando com os alvarás pra ver quais tem
  inner_join( alvarás %>% distinct() ) %>%
  select( SQL , legislacao_pde , id ) %>%
  distinct() %>%
  mutate( válido = !is.na(id)  )

# totalizando os planos diretores
iptualvará022 %>% group_by(legislacao_pde) %>% summarize( n = n())
