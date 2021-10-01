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
  
  
  temp <- temp %>%
    mutate(
            # arrumando campo de pavimentos
            Pavs = as.numeric(Pavs),
            # arrumando condomínios pra não contar duplicado
            SQL = case_when(
                            Condo != "00-0" ~ paste0( str_sub( SQL , 0 , 6) , "-C" , Condo ),
                            TRUE ~ SQL
                          )
          ) %>%
    group_by( SQL ) %>%
    mutate(
            # consolidando um valor por lote/condomínio, mas ainda com várias instâncias
            Construído = sum(Construído),
            Ocupado = max(Ocupado),
            across( ValorTerreno:Testada, median ),
            FatorObsoles = median(FatorObsoles)
          ) %>%
    ungroup() %>%
    # removendo instâncias repetidas do mesmo condomínio
    distinct( SQL , .keep_all = TRUE ) %>%
    # extraindo quadra e calculando CA e TO do lote para futura comparação
    mutate(
            SQ = str_sub(SQL,0,6),
            ano = ano,
            CA_lote = Construído/Terreno,
            TO_lote = Ocupado/Terreno
          ) 
  
  
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_arrumado.csv.gz")
  
  write_csv2( temp , arquivo )
  
}