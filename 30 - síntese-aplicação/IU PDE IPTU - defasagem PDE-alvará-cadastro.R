library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(stats)
library(ggplot2)
library(sf)
library(readxl)
library(gridExtra)

######## arrumando arquivo-base ########
arquivo <- "./00 - dados brutos/msp_empreendimentos_sisacoe.xlsx"

alvaras <- read_excel( arquivo ) %>%
  # renomeando SQL pra ficar no padr�o
  rename( SQL = sql ) %>%
  rename( SQ = sq ) %>%
  # removendo SQLs inv�lidos (diferente de 12 caracteres)
  filter( nchar(SQL) != 12 ) %>%
  mutate( 
          # corrigindo camada de n�mero
          ac_total = as.numeric( str_replace_all( ac_total , "\\," , "\\." ) ),
          # ano do processo administrativo
          ano_processo_adm = as.numeric( str_sub( processo_adm , 0 , 4 ) ),
          # SQL s� n�meros
          SQL = str_replace_all( SQL , "\\." , "" ),
          # SQ pelo SQL, mais confi�vel
          SQ = str_sub( SQL , 0 , 6 )
        ) %>%
  # s� linhas com ano de processo administrativo
  filter( is.na( ano_processo_adm ) == FALSE & ano_processo_adm > 2000 ) %>%
  # pegando s� o alvar� mais recente pra cada SQL
  group_by( SQL ) %>%
  slice_max( alvara_numero ) %>%
  ungroup()

# --------------------------------------------------- #
# --------------------------------------------------- #
# --------------------------------------------------- #

######## defasagem PDE at� alvar� ########

PDE_alvara <- alvaras %>%
  group_by( ano_processo_adm , legislacao_pde ) %>%
  summarize( n = n_distinct( id ) ) %>%
  ungroup() %>%
  filter( str_detect( legislacao_pde , "n�o presente" ) == FALSE ) %>%
  pivot_wider( names_from = legislacao_pde , values_from = n )


png("./30 - s�ntese-aplica��o/IU PDE IPTU - defasagem PDE-alvar�-cadastro - 1.png")
grid.arrange( tableGrob(PDE_alvara) )
dev.off()
















# --------------------------------------------------- #
# --------------------------------------------------- #
# --------------------------------------------------- #

######## defasagem alvar� at� IPTU ########

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

ListaAnos <- 2012:2020

for (ano in ListaAnos){
  
  print( paste( "Processando" , ano )  )
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_loteArrumado.csv.gz")
  
  temp <- read_csv2( arquivo , locale=locale(encoding="UTF8") ) %>%
    # pegando s� as colunas de interesse
    select( Ano , SQ , SQL , Constru�do  ) %>%
    # resumindo por quadra
    group_by( Ano, SQ ) %>%
    summarize( Constru�do = sum(Constru�do) ) %>%
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
          # varia��o na �rea constru�da ano a ano em cada quadra
          varia��oConstru�do = case_when(
                                          lag(SQ,1) == SQ ~ Constru�do - lag(Constru�do,1),
                                          TRUE ~ 0
                                        )
        ) %>%
  # remover varia��es inv�lidas ( <= 0, NA ) e anos inv�lidos (antes de 2002)
  filter( varia��oConstru�do > 0 ) %>%
  # juntando �reas por quadra por alvar�
  left_join( alvaras_ano_processo_AC ) %>%
  # puxando o ano do processo administrativo mais antigo
  group_by( Ano , SQ ) %>%
  slice_min( ano_processo_adm ) %>%
  ungroup() %>%
  # cumpriu 85% da nova �rea constru�da no processo administrativo mais antigo?
  mutate( 
          atingiuAlvara = case_when(
                                      varia��oConstru�do/novas_AC_processo >= 0.85 ~ 1,
                                      TRUE ~ 0 
                                    ),
          
        ) %>%
  # pegando s� as �reas previstas no alvar� que apareceram no IPTU
  filter( atingiuAlvara == 1 ) %>% 
  # agrupando por ano de processo administrativo X ano do IPTU
  group_by( ano_processo_adm , Ano  ) %>%
  summarize( constru�doIPTU_x_totalAnoProcesso = (sum( varia��oConstru�do ) / median(total_AC_ano_processo))*100 ) %>%
  ungroup() %>%
  filter( ano_processo_adm < Ano ) %>%
  # gr�fico
  ggplot( aes( x = ano_processo_adm , y = Ano , size = constru�doIPTU_x_totalAnoProcesso ) ) + 
  geom_point( alpha = 0.7 ) +
  scale_x_continuous("Ano do processo administrativo", limits = c(2010,2019) ,breaks = seq(2010,2019) ) +
  scale_y_continuous("Primeiro ano no IPTU", limits = c(2012,2020) , breaks = seq(2013,2020)) +
  geom_text( aes( label = paste0(round(constru�doIPTU_x_totalAnoProcesso,0),"%") ) , vjust = 2.5 , size = 3  ) + 
  scale_size( range = c(1, 15) ) +
  theme(
    legend.position = "bottom",
  ) +
  labs( size = "Do total de �reas constru�das pedidas por ano do processo, \n__% aparecem no IPTU-EG a cada ano" )



