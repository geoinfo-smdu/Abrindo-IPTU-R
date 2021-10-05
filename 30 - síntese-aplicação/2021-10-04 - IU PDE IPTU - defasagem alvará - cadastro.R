library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(stats)
library(ggplot2)
library(sf)
library(readxl)

########alvarás ########
arquivo <- "./00 - dados brutos/msp_empreendimentos_sisacoe.xlsx"

alvaras <- read_excel( arquivo ) %>%
  # renomeando SQL pra ficar no padrão
  rename( SQL = sql ) %>%
  rename( SQ = sq ) %>%
  mutate( 
          # corrigindo camada de número
          ac_total = as.numeric( str_replace_all( ac_total , "\\," , "\\." ) ),
          # ano do processo administrativo
          ano_proc_adm = as.numeric( str_sub( processo_adm , 0 , 4 ) ),
          # SQL só números
          SQL = str_replace_all( SQL , "\\." , "" )
        ) %>%
  # pegando só o alvará mais recente pra cada SQL
  group_by( SQL ) %>%
  slice_max( alvara_numero ) %>%
  ungroup() %>%
  # calculando o total de área construída em cada ano de processo administrativo
  group_by( ano_proc_adm ) %>%
  mutate( total_ano_processo = sum( ac_total ) ) %>%
  ungroup() %>%
  # calculando o total de área construída em cada ano de emissão de alvará
  group_by( alvara_ano_emissao ) %>%
  mutate( total_ano_emissao = sum( ac_total ) ) %>%
  ungroup() %>%
  # só colunas necessárias
  select( id , processo_adm , ano_proc_adm , alvara_numero , alvara_ano_emissao , SQ , SQL , 
          legislacao_pde , legislacao_lpuos , ac_total , total_ano_processo , total_ano_emissao ) %>%
  # só linhas válidas
  filter( is.na( ano_proc_adm ) == FALSE )

# ------------------------------------------------------- #
# ------------------------------------------------------- #
# ------------------------------------------------------- #

######## juntando os dados de IPTU nos alvarás ########

#### base de lotes ####
ListaAnos <- 2013:2020

for (ano in ListaAnos){
  
  print( paste( "Processando" , ano )  )
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_arrumado.csv.gz")
  
  temp <- read_csv2( arquivo , locale=locale(encoding="UTF8") ) %>%
    # pegando só as colunas de interesse
    select( Ano , SQ , SQL , Construído  )
  
  # juntando
  if (ano == ListaAnos[1]){
    iptu_lotes <- temp
  } else{
    iptu_lotes <- bind_rows( iptu_lotes , temp )
  }
  
}

iptu_lotes_novaConstr <- iptu_lotes %>%
  # variação da área construída
  arrange( SQL , Ano ) %>%
  mutate(
          variaçãoConstruído = case_when(
                                          lag(SQL,1) == SQL ~ Construído - lag(Construído,1),
                                          TRUE ~ NA_real_
                                        )
        ) %>%
  # remover variações inválidas ( <= 0, NA ) e anos inválidos (antes de 2002)
  filter( is.na( variaçãoConstruído ) == FALSE & variaçãoConstruído >0 & Ano >= 2013 )



#### juntando variação de AC primeiro por lote ####
iptu_alvaras <- (alvaras %>% select(-c("SQ"))) %>%
  left_join( iptu_lotes_novaConstr ) %>%
  mutate( Ano = case_when( is.na(Ano) == TRUE ~ 2012, TRUE ~ Ano ) ) %>%
  group_by( SQL ) %>%
  slice_min( Ano )


# ------------------------------------------------------- #
# ------------------------------------------------------- #
# ------------------------------------------------------- #

######## ano alvará X primeiro ano em que aparece variação na área construída ########

#### gráfico anos do processo administrativo x ano IPTU ####
iptu_alvaras %>%
  # agrupando
  group_by( ano_proc_adm , Ano , total_ano_processo ) %>%
  summarize( novas_AC = sum( variaçãoConstruído ) ) %>%
  ungroup() %>%
  # comparando com o total daquele ano dos processos
  mutate( novas_AC_porcent_ano_processo = novas_AC/total_ano_processo ) %>%
  # gráfico
  ggplot( aes( x = ano_proc_adm , y = Ano , size = novas_AC_porcent_ano_processo ) ) + 
  geom_point( alpha = 0.7 ) +
  scale_x_continuous("Ano do processo administrativo", limits = c(2013,2020) , breaks = seq(2013,2020) ) +
  scale_y_continuous("Primeiro ano no IPTU", limits = c(2013,2020) , breaks = seq(2013,2020)) +
  geom_text( aes( label = paste0(round(novas_AC_porcent_ano_processo,3)*100,"%") ) , vjust = 2 , size = 4  ) + 
  scale_size( range = c(1, 20) ) +
  theme(
        legend.position = "none",
      ) +
  labs( size = "% de novas áreas construídas do ano do processo" )


#### gráfico anos do alvará x ano IPTU ####
iptu_alvaras %>%
  # agrupando
  group_by( alvara_ano_emissao , Ano , total_ano_emissao ) %>%
  summarize( novas_AC = sum( variaçãoConstruído ) ) %>%
  ungroup() %>%
  # comparando com o total daquele ano dos processos
  mutate( novas_AC_porcent_ano_alvará = novas_AC/total_ano_emissao ) %>%
  # gráfico
  ggplot( aes( x = alvara_ano_emissao , y = Ano , size = novas_AC_porcent_ano_alvará ) ) + 
  geom_point( alpha = 0.7 ) +
  scale_x_continuous("Ano do alvará", limits = c(2013,2020) , breaks = seq(2013,2020) ) +
  scale_y_continuous("Primeiro ano no IPTU", limits = c(2013,2020) , breaks = seq(2013,2020)) +
  geom_text( aes( label = paste0(round(novas_AC_porcent_ano_alvará,3)*100,"%") ) , vjust = 2 , size = 4  ) + 
  scale_size( range = c(.1, 20) ) +
  theme(
          legend.position = "bottom",
        ) +
  labs(size="% de novas áreas construídas do ano do alvará")
