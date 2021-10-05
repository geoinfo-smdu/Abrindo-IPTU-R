library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(stats)
library(ggplot2)
library(sf)
library(readxl)

######## arrumando arquivo-base ########
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

# --------------------------------------------------- #
# --------------------------------------------------- #
# --------------------------------------------------- #

######## defasagem PDE até alvará ########



















# --------------------------------------------------- #
# --------------------------------------------------- #
# --------------------------------------------------- #

######## defasagem alvará até IPTU ########

alvaras_ano_processo_AC <- alvaras %>%
  group_by( ano_processo_adm , SQ ) %>%
  summarize( novas_AC_processo = sum(ac_total) ) %>%
  ungroup() %>%
  # calculando o total prometido do ano
  group_by( ano_processo_adm ) %>%
  mutate( total_AC_ano_processo = sum(novas_AC_processo) ) %>%
  ungroup()

alvaras_ano_alvara_AC <- alvaras %>%
  group_by( alvara_ano_emissao , SQ ) %>%
  summarize( novas_AC_alvara = sum(ac_total) ) %>%
  ungroup()

ListaAnos <- 2013:2020

for (ano in ListaAnos){
  
  print( paste( "Processando" , ano )  )
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_loteArrumado.csv.gz")
  
  temp <- read_csv2( arquivo , locale=locale(encoding="UTF8") ) %>%
    # pegando só as colunas de interesse
    select( Ano , SQ , SQL , Construído  ) %>%
    # resumindo por quadra
    group_by( Ano, SQ ) %>%
    summarize( Construído = sum(Construído) ) %>%
    ungroup()
  
  # juntando
  if (ano == ListaAnos[1]){
    iptu_lotes <- temp
  } else{
    iptu_lotes <- bind_rows( iptu_lotes , temp )
  }
  
}


iptu_lotes %>%
  arrange( SQ , Ano ) %>%
  mutate(
          # variação na área construída ano a ano em cada quadra
          variaçãoConstruído = case_when(
                                          lag(SQ,1) == SQ ~ Construído - lag(Construído,1),
                                          TRUE ~ 0
                                        )
        ) %>%
  # remover variações inválidas ( <= 0, NA ) e anos inválidos (antes de 2002)
  filter( variaçãoConstruído > 0 ) %>%
  # juntando áreas por quadra por alvará
  left_join( alvaras_ano_processo_AC ) %>%
  # puxando o ano do processo administrativo mais antigo
  group_by( Ano , SQ ) %>%
  slice_min( ano_processo_adm ) %>%
  ungroup() %>% 
  # cumpriu 85% da nova área construída no processo administrativo mais antigo?
  mutate( 
          atingiuAlvara = case_when(
                                      variaçãoConstruído/novas_AC_processo >= 0.85 ~ 1,
                                      TRUE ~ 0 
                                    ),
          
        ) %>%
  # pegando só as áreas previstas no alvará que apareceram no IPTU
  filter( atingiuAlvara == 1 ) %>%
  # agrupando por ano de processo administrativo X ano do IPTU
  group_by( ano_processo_adm , Ano  ) %>%
  summarize( construídoIPTU_x_totalAnoProcesso = (sum( variaçãoConstruído ) / median(total_AC_ano_processo))*100 ) %>%
  ungroup() %>%
  filter( ano_processo_adm < Ano ) %>%
  # gráfico
  ggplot( aes( x = ano_processo_adm , y = Ano , size = construídoIPTU_x_totalAnoProcesso ) ) + 
  geom_point( alpha = 0.7 ) +
  scale_x_continuous("Ano do processo administrativo", limits = c(2007,2020) ,breaks = seq(2006,2019) ) +
  scale_y_continuous("Primeiro ano no IPTU", limits = c(2013,2020) , breaks = seq(2013,2020)) +
  geom_text( aes( label = paste0(round(construídoIPTU_x_totalAnoProcesso,0),"%") ) , vjust = 2.5 , size = 3  ) + 
  scale_size( range = c(1, 15) ) +
  theme(
    legend.position = "bottom",
  ) +
  labs( size = "Do total de áreas construídas pedidas por ano do processo, \n__% aparecem no IPTU-EG a cada ano" )



