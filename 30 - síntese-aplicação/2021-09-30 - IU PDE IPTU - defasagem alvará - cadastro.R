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

alvarás <- read_excel( arquivo ) %>%
  rename( SQL = sql ) %>%
  mutate( 
    ano_processo_adm = as.numeric( str_sub( processo_adm , 0 , 4 ) ),
    SQL = str_replace_all( SQL , "\\." , "" )
  )

######## IPTU ########
ListaAnos <- 2013:2020

for (ano in ListaAnos){
  
  print( paste( "Processando" , ano )  )
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_arrumado.csv.gz")
  
  # cálculos por lote antes de resumir por quadra
  temp <- read_csv2( arquivo , locale=locale(encoding="latin1") ) %>%
    # pegando só as colunas de interesse
    select( Ano , SQL , Construído  ) %>%
    # juntando os alvarás
    inner_join( alvarás ) %>%
    # filtrando onde não bateu alvará e só anos de processo administrativo depois de 2013, já que IPTU só peguei 2013 em diante
    filter( is.na(id) == FALSE )
  
  # juntando
  if (ano == ListaAnos[1]){
    iptu <- temp
  } else{
    iptu <- bind_rows( iptu , temp )
  }
  
}

######## ano alvará X primeiro ano em que aparece variação na área construída ########

#### arrumando a tabela ####
iptu_alvará <- iptu %>%
  # descobrir se em algum ano mudou a área construída, exceto nos casos de id diferente (i.e. não dizem respeito ao mesmo alvará)
  arrange( ano , id ) %>%
  mutate(
          variaçãoConstruído = case_when(
                                          lag(id,1) == id ~ Construído - lag(Construído,1),
                                          TRUE ~ NA_real_
                                        )
        ) %>%
  # remover variações inválidas ( <= 0, NA ) e anos inválidos (antes de 2002)
  filter( is.na( variaçãoConstruído ) == FALSE & variaçãoConstruído >0 & ano_processo_adm >= 2013 ) %>%
  # pegar só a máxima variação pra cada alvará/imóvel, que indica que foi ali que apareceu no IPTU
  group_by( id ) %>%
  slice_max( variaçãoConstruído ) %>%
  ungroup() %>%
  distinct() %>%
  # extraindo só as colunas necessárias
  select( ano_processo_adm , alvara_ano_emissao , Ano , SQL , id , variaçãoConstruído  )
#View() %>%



#### gráfico anos do processo administrativo x ano IPTU ####
iptu_alvará  %>%
  # agrupando
  group_by( ano_processo_adm , Ano ) %>%
  summarize( novas_AC = sum( variaçãoConstruído ) ) %>%
  ungroup() %>%
  # comparando com o total daquele ano dos processos
  group_by( ano_processo_adm ) %>%
  mutate( total_ano = sum( novas_AC ) ) %>%
  ungroup() %>%
  mutate( novas_AC_porcent_ano_processo = novas_AC/total_ano ) %>%
  # gráfico
  ggplot( aes( x = ano_processo_adm , y = Ano , size = novas_AC_porcent_ano_processo ) ) + 
  geom_point( alpha = 0.7 ) +
  scale_x_continuous("Ano do processo administrativo", limits = c(2013,2020) , breaks = seq(2013,2020) ) +
  scale_y_continuous("Primeiro ano no IPTU", limits = c(2013,2020) , breaks = seq(2013,2020)) +
  geom_text( aes( label = paste0(round(novas_AC_porcent_ano_processo,2)*100,"%") ) , vjust = 3.6 , size = 4  ) + 
  scale_size( range = c(.1, 20) ) +
  theme(
    legend.position = "bottom",
  ) +
  labs(size="% de novas áreas construídas do ano do processo")



#### gráfico anos do alvará x ano IPTU ####
iptu_alvará %>%
  # agrupando
  group_by( alvara_ano_emissao , Ano ) %>%
  summarize( novas_AC = sum( variaçãoConstruído ) ) %>%
  ungroup() %>%
  # comparando com o total daquele ano dos processos
  group_by( alvara_ano_emissao ) %>%
  mutate( total_ano = sum( novas_AC ) ) %>%
  ungroup() %>%
  mutate( novas_AC_porcent_ano_alvará = novas_AC/total_ano ) %>%
  # gráfico
  ggplot( aes( x = alvara_ano_emissao , y = Ano , size = novas_AC_porcent_ano_alvará ) ) + 
  geom_point( alpha = 0.7 ) +
  scale_x_continuous("Ano do alvará", limits = c(2013,2020) , breaks = seq(2013,2020) ) +
  scale_y_continuous("Primeiro ano no IPTU", limits = c(2013,2020) , breaks = seq(2013,2020)) +
  geom_text( aes( label = paste0(round(novas_AC_porcent_ano_alvará,2)*100,"%") ) , vjust = 3.6 , size = 4  ) + 
  scale_size( range = c(.1, 20) ) +
  theme(
    legend.position = "bottom",
  ) +
  labs(size="% de novas áreas construídas do ano do alvará")