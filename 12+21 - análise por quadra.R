library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# caminho <- "pasta do repositório"
setwd(caminho)

# função para leitura
le_IPTU <- function( listaAnos , linhas ){
  
  listaCampos = c( 
    "NUMERO DO CONTRIBUINTE",
    "ANO DO EXERCICIO",
    "NUMERO DO CONDOMINIO",
    "CODLOG DO IMOVEL",
    "CEP DO IMOVEL"
  )
  
  for (ano in listaAnos){
    
    print( paste( "Processando" , ano )  )
    
    arquivo <- paste0( "./00 - dados brutos/IPTU_" , ano , ".csv.gz")
    
    # leitura diferente pra 2016
    if (ano == 2016){
      temp <- read_csv( arquivo , n_max = linhas , locale=locale(encoding="latin1") , show_col_types = FALSE ) %>%
        mutate( across( starts_with( c("VALOR","TESTADA","FATOR")) , ~ str_replace( . ,",",".")  ) ) %>%
        mutate( across(  starts_with( c("VALOR","TESTADA","FATOR") )  , ~ as.numeric(.) )  ) %>%
        mutate( across(  starts_with( c("VALOR","TESTADA") )  , ~ .x/100 )  )
    }
    
    else{
      temp <- read_csv2( arquivo , n_max = linhas , locale=locale(encoding="latin1") , show_col_types = FALSE) 
    }
    
    # extraindo só campos que precisa
    temp <- temp %>%
      select( all_of(listaCampos) , contains("FRENTE") , "AREA DO TERRENO":"FATOR DE OBSOLESCENCIA" )
    
    # renomeando
    names(temp) <- c("SQL", "Ano", "Condo","Codlog","CEP","Frentes", "Terreno","Construído","Ocupado",
                     "ValorTerreno","ValorConstr","AnoConstr","Pavs","Testada","TipoUso",
                     "TipoPadrão","TipoTerr","FatorObsoles"
                    )
    
    # arrumando campo de pavimentos
    temp$Pavs <- as.numeric(temp$Pavs)
    
    # arrumando condomínios pra não contar duplicado
    temp <- temp %>%
      mutate( SQL = case_when(
                                Condo != "00-0" ~ paste0( str_sub( SQL , 0 , 6) , "-C" , Condo ),
                                TRUE ~ SQL
                              )
              ) %>%
      group_by( SQL ) %>%
      mutate(
              Construído = sum(Construído),
              across( Ocupado:Testada, median ),
              FatorObsoles = median(FatorObsoles)
              
            ) %>%
      distinct( SQL , .keep_all = TRUE )
      
    
    arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_arrumado.csv.gz")
    
    write.csv2( temp , arquivo )
    
  }
  
}

######## 12_1 - leitura dos dados do IPTU ##########
#### preparando o arquivo ####
# lista de anos com base do IPTU, de 1995 até o ano atual
ListaAnos <- 1995:(year(today()))

# leitura e junção dos arquivos arrumados
le_IPTU( ListaAnos , Inf )








for (ano in ListaAnos){
  # nome do arquivo e leitura
  arquivo <- paste0( "./00 - dados brutos/IPTU_" , ano , ".csv.gz")
  
  library(stats)
  
  
  
  
  
  temp <- temp %>%
########## 12_2 - pré-processamentos ##########
###### resumindo por quadra ###### 
  # selecionando só colunas de interesse
  select( c( 1 , 21:32 ) ) %>%
    
    # adicionando ano
    mutate( ano = ano ) %>%
    
    # calculando CA e TO
    mutate( CA = `AREA CONSTRUIDA` / `AREA DO TERRENO`  ) %>%
    mutate( TO = `AREA OCUPADA` / `AREA DO TERRENO`  ) %>%
    
    # agrupando por quadra
    mutate( quadra = str_sub( `NUMERO DO CONTRIBUINTE`, 1, 6 ) ) %>%
    group_by( ano, quadra ) %>%
    summarize( terreno = sum(`AREA DO TERRENO`) ,
               construído = sum(`AREA CONSTRUIDA`) ,
               ocupado = sum(`AREA OCUPADA`),
               preço_m2_mediano = median( `VALOR DO M2 DO TERRENO` ),
               preço_m2_ponderado = round( weighted.mean( `VALOR DO M2 DO TERRENO` , `AREA CONSTRUIDA` ) , 0),
               ano_mediano = median( `ANO DA CONSTRUCAO CORRIGIDO` ),
               ano_ponderado = round ( weighted.mean( `ANO DA CONSTRUCAO CORRIGIDO` , `AREA CONSTRUIDA` ) , 0),
               #pav_mediano = round( median( `QUANTIDADE DE PAVIMENTOS` ) ,0),
               testada_mediana = median( `TESTADA PARA CALCULO` ),
               CA_mediano = median( CA ),
               TO_mediano = median( TO )
               
    ) %>%
    
    # CA e TO de quadra
    mutate( CA_quadra = construído / terreno ) %>%
    mutate( TO_quadra = ocupado / terreno )
  
  nome <- paste0("./10 - processamentos/IPTU_" , ano , "_ResumoQuadra.csv.gz")
  write_csv2( temp, nome )
  
  rm(temp)
  
}

###### juntando em uma tabela só ###### 
ListaAnos <- 1995:(year(today()))

mat = matrix(ncol = 0, nrow = 0)
IPTU_12_1_bruto = data.frame(mat)
IPTU_12_2_final = data.frame(mat)
rm(mat)




# corrido - para gráficos
for (ano in ListaAnos){
  ###### nome do arquivo e leitura
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_ResumoQuadra.csv.gz")
  
  library(stats)
  
  IPTU_12_1_bruto <- read_csv2( arquivo )
  
  if (ListaAnos[1] == ano){
    IPTU_12_2_final <- IPTU_12_1_bruto
  }
  
  else{
    IPTU_12_2_final <- bind_rows( IPTU_12_2_final , IPTU_12_1_bruto )
  }
  
  
}

nome <- paste0("./20 - info/IPTU_ParâmetrosPorAnoPorQuadra.csv.gz")
write_csv2( IPTU_12_2_final, nome )




# por quadra - para espacializações
for (ano in ListaAnos){
  ###### nome do arquivo e leitura
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_ResumoQuadra.csv.gz")
  
  library(stats)
  
  IPTU_12_1_bruto <- read_csv2( arquivo )
  
  IPTU_12_1_bruto <- IPTU_12_1_bruto %>% 
    rename_with(   ~paste0( ano , "_" , .)  , -c(1:2)   ) %>%
    select( -c( ano ) )
  
  if (ListaAnos[1] == ano){
    IPTU_12_2_final <- IPTU_12_1_bruto
  }
  
  else{
    IPTU_12_2_final <- full_join( IPTU_12_2_final , IPTU_12_1_bruto )
  }
  
  
}

nome <- paste0("./20 - info/IPTU_ParâmetrosPorQuadra.csv.gz")
write_csv2( IPTU_12_2_final, nome )






rm(IPTU_12_1_bruto)
rm(IPTU_12_2_final)


########## 21_1 -  ##########

arquivo <- paste0( "./20 - info/", "IPTU_ParâmetrosPorAnoPorQuadra.csv.gz" )
IPTU_21_1_gráficos_bruto <- na.omit( read_csv2( arquivo ) )


########## 21_2 - tipos de áreas em 2020 ##########
IPTU_21_2 <- 
  
  
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


