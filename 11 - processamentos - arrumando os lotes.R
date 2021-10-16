library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# caminho <- "pasta do repositório"

#### deixando só as variáveis de interesse por ano por lote (considerando o condomínio inteiro como um lote só)  ####
# lista de anos com base do IPTU, de 1995 até o ano atual
ListaAnos <- (year(today())):1995

# leitura e junção dos arquivos arrumados
listaCampos <- c( 
  "NUMERO DO CONTRIBUINTE",
  "ANO DO EXERCICIO",
  "NUMERO DO CONDOMINIO",
  "CODLOG DO IMOVEL",
  "CEP DO IMOVEL"
)

# tabela de tradução de usos e padrões em usoH
usos <- unique(temp2$TipoUso)
padrões <- unique(temp2$TipoPadrão)


for (ano in ListaAnos){
  
  print( paste( "Processando" , ano )  )
  
  arquivo <- paste0( "./00 - dados brutos/IPTU_" , ano , ".csv.gz")
  
  # leitura diferente pra 2016
  if (ano == 2016){
    temp <- read_csv( arquivo , locale=locale(encoding="latin1") ) %>%
      mutate( across( starts_with( c("VALOR","TESTADA","FATOR")) , ~ str_replace( . ,",",".")  ) ) %>%
      mutate( across(  starts_with( c("VALOR","TESTADA","FATOR") )  , ~ as.numeric(.) )  ) %>%
      mutate( across(  starts_with( c("VALOR","TESTADA") )  , ~ .x/100 )  )
  } else{
    temp <- read_csv2( arquivo , locale=locale(encoding="latin1") ) 
  }
  
  # extraindo só campos que precisa
  temp <- temp %>%
    select( all_of(listaCampos) , contains("FRENTE") , "AREA DO TERRENO":"FATOR DE OBSOLESCENCIA" )
  
  # renomeando
  names(temp) <- c("SQL", "Ano", "Condo","Codlog","CEP","Frentes", "Terreno","Construído","Ocupado",
                   "ValorTerreno","ValorConstr","AnoConstr","Pavs","Testada","TipoUso",
                   "TipoPadrão","TipoTerr","FatorObsoles"
  )
  
  # reordenando para facilitar cálculos  
  temp <- temp %>%
    select( c(1:5,8,6,7,9:14,18,15:17) )
  
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_loteArrumado.csv.gz")
  
  
  
  
  # remover depois de consolidar o método
  temp2 <- temp
  temp <- temp[1:100000,]
  
  
  
  
  
  
  
  temp %>%
    mutate(
            # arrumando campo de pavimentos
            Pavs = as.numeric(Pavs),
            # arrumando casos em que diz que não tem frentes pra por pelo menos 1
            Frentes = case_when( Frentes == 0 ~ 1 , TRUE ~ Frentes ),
            # arrumando condomínios pra não contar duplicado
            SQL = str_sub( SQL , 0 , 10 ) , 
            SQL = case_when(
                            Condo != "00-0" ~ paste0( str_sub( SQL , 0 , 6) , "0000" , str_sub(Condo,0,2) ),
                            TRUE ~ paste0( str_sub( SQL , 0 , 10) , str_sub(Condo,0,2) )
                          ),
            
            # traduzindo os usos-padrão
            UsoPadrão = paste( TipoUso , TipoPadrão ),
            # resumindo em usoH
            UsoH = case_when(
                              str_detect( UsoPadrão , "^(Aparta).*(A|B)$" ) == TRUE ~ "14 - Uso Residencial Vertical Baixo Padrão",
                              str_detect( UsoPadrão , "^(Aparta).*(C)$" ) == TRUE ~ "04 - Uso Residencial Vertical Médio Padrão",
                              str_detect( UsoPadrão , "^(Aparta).*(D|E|F)$" ) == TRUE ~ "05 - Uso Residencial Vertical Alto Padrão",
                              str_detect( UsoPadrão , "^Escola" ) == TRUE ~ "11 - Uso Escola",
                              str_detect( UsoPadrão , "^Terreno" ) == TRUE ~ "13 - Terrenos Vagos",
                              str_detect( UsoPadrão , "^Garag.*(Comercial horizontal|Barra)" ) == TRUE ~ "15 - Uso garagens não-residenciais",
                              
                              TRUE ~ "FALTA FAZER, no final vai ser 99"
                            )
          ) %>% View()
    select( c(-UsoPadrão)) %>%
    group_by( SQL ) %>%
    mutate(
            # consolidando um valor por lote/condomínio, mas ainda com várias instâncias, pra depois poder resumir os usos
            Construído = sum(Construído),
            across( Frentes:FatorObsoles, median ),
            # resumindo os usos
            
          ) %>%
    ungroup() %>%
    # removendo instâncias repetidas do mesmo condomínio
    distinct( SQL , .keep_all = TRUE ) %>%
    # extraindo quadra e calculando CA e TO do lote (já com condomínios combinados) para futura comparação
    mutate(
            CA_lote = Construído/Terreno,
            TO_lote = Ocupado/Terreno,
            SQ = str_sub(SQL,0,6),
            ano = ano,
          ) %>% View()
  #write_csv2( file = arquivo )
  
  rm(temp)
  gc()
  
}
