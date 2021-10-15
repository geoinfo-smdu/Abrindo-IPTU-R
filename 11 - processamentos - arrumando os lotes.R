library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# caminho <- "pasta do repositório"

#### deixando só as variáveis de interesse por ano por lote (considerando o condomínio inteiro como um lote só)  ####
# lista de anos com base do IPTU, de 1995 até o ano atual
ListaAnos <- 1995:(year(today()))

# leitura e junção dos arquivos arrumados
listaCampos <- c( 
  "NUMERO DO CONTRIBUINTE",
  "ANO DO EXERCICIO",
  "NUMERO DO CONDOMINIO",
  "CODLOG DO IMOVEL",
  "CEP DO IMOVEL"
)

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
  
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_loteArrumado.csv.gz")
  
  temp %>%
    mutate(
            # arrumando campo de pavimentos
            Pavs = as.numeric(Pavs),
            # arrumando condomínios pra não contar duplicado
            SQL = str_sub( SQL , 0 , 10 ) , 
            SQL = case_when(
                            Condo != "00-0" ~ paste0( str_sub( SQL , 0 , 6) , "0000" , str_sub(Condo,0,2) ),
                            TRUE ~ paste0( str_sub( SQL , 0 , 10) , str_sub(Condo,0,2) )
                          ),
            SQ = str_sub(SQL,0,6),
            ano = ano,
          ) %>%
    group_by( SQL ) %>%
    summarize(
            # consolidando um valor por lote/condomínio, mas ainda com várias instâncias
            Construído = sum(Construído),
            Terreno = max(Terreno),
            Ocupado = max(Ocupado),
            across( ValorTerreno:Testada, median ),
            FatorObsoles = median(FatorObsoles)
          ) %>%
    ungroup() %>%
    # extraindo quadra e calculando CA e TO do lote (já com condomínios combinados) para futura comparação
    mutate(
            CA_lote = Construído/Terreno,
            TO_lote = Ocupado/Terreno
          ) %>%
  write_csv2( file = arquivo )
  
}
