library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# CONFIGURAR CAMINHO MANUALMENTE
# caminho <- "pasta do repositório"
setwd(caminho)

########## 12_1 - leitura dos dados do IPTU ##########

###### lista de anos com base do IPTU, de 1995 até o ano atual
ListaAnos <- 1995:(year(today()))

mat = matrix(ncol = 0, nrow = 0)
IPTU_12_1_final = data.frame(mat)
rm(mat)

################## FAZER LISTA DE CAMPOS DE TODOS PRA COMPATIBILIZAR
################## FAZER LISTA DE CAMPOS DE TODOS PRA COMPATIBILIZAR
################## FAZER LISTA DE CAMPOS DE TODOS PRA COMPATIBILIZAR
################## FAZER LISTA DE CAMPOS DE TODOS PRA COMPATIBILIZAR
################## FAZER LISTA DE CAMPOS DE TODOS PRA COMPATIBILIZAR

################## INSERIR COL TYPES PRA VER SE DIMINUI GASTO DE MEMÓRIA
################## INSERIR COL TYPES PRA VER SE DIMINUI GASTO DE MEMÓRIA
################## INSERIR COL TYPES PRA VER SE DIMINUI GASTO DE MEMÓRIA
################## INSERIR COL TYPES PRA VER SE DIMINUI GASTO DE MEMÓRIA
################## INSERIR COL TYPES PRA VER SE DIMINUI GASTO DE MEMÓRIA

for (ano in ListaAnos){
  ###### nome do arquivo e leitura
  arquivo <- paste0( "./00 - dados brutos/IPTU_" , ano , ".csv.gz")
  
  library(stats)
  
  temp <- read_csv2( arquivo ) %>%
    
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


IPTU_12_1_final <- read_csv2("./10 - processamentos/IPTU_2015_ResumoQuadra.csv.gz")


