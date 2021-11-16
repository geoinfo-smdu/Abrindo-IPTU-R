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
#usos <- unique(temp2$TipoUso)
#padrões <- unique(temp2$TipoPadrão)


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
  
  temp1 <-
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_loteArrumado.csv.gz")
  

  
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
            # traduzindo os usos-padrão depois de tornar tudo maiúscula para evitar erros
            UsoPadrão = toupper(paste( TipoUso , TipoPadrão )),
            # resumindo em usoH
            UsoH = case_when(
                              # residência e termina com A ou B / cortiço = 01
                              str_detect( UsoPadrão , "(^(RESID).*(A|B)$)|(^CORTIÇO)" ) == TRUE ~ "01 - Uso Residencial Horizontal Baixo Padrão",
                              # residência e termina com C = 02, D/E/F = 03
                              str_detect( UsoPadrão , "^(RESID|FLAT.*RESID).*(C)$" ) == TRUE ~ "02 - Uso Residencial Horizontal Médio Padrão",
                              str_detect( UsoPadrão , "^(RESID).*(D|E|F)$" ) == TRUE ~ "03 - Uso Residencial Horizontal Alto Padrão",
                              # apartamentos, prédio predom. resid. com A ou B = 14
                              str_detect( UsoPadrão , "(APARTA|PR.*PREDOM.*RESID).*(A|B)$" ) == TRUE ~ "14 - Uso Residencial Vertical Baixo Padrão",
                              # apartamentos, prédio predom. resid. ou flat com C = 04, D/E/F = 05
                              str_detect( UsoPadrão , "(APARTA|PR.*PREDOM.*RESID|FLAT.*RESID).*(C)$" ) == TRUE ~ "04 - Uso Residencial Vertical Médio Padrão",
                              str_detect( UsoPadrão , "(APARTA|PR.*PREDOM.*RESID|FLAT.*RESID).*(D|E|F)$" ) == TRUE ~ "05 - Uso Residencial Vertical Alto Padrão",
                              # loja, posto de gasolina, outras edificações de comércio ou serviços, escritórios, horizontais ou em barracão = 06
                              str_detect( UsoPadrão , "^(LOJA|OFICI|POSTO|OUTR.*EDIF.*(COMER|SERV)|.*ESCRIT).*(HORIZ|BARRA|ARMAZ)" ) == TRUE ~ "06 - Uso Comércio e Serviço Horizontal",
                              # loja, outras edificações de comércio ou serviços, escritórios verticais = 07
                              str_detect( UsoPadrão , "^(LOJA|OUTR.*EDIF.*(COMER|SERV)|FLAT.*COMER|.*ESCRIT).*(VERTI)" ) == TRUE ~ "07 - Uso Comércio e Serviço Vertical",
                              str_detect( UsoPadrão , "^(IND).*(HORIZ|BARRA|ARMAZ|IND)" ) == TRUE ~ "08 - Uso Industrial",
                              str_detect( UsoPadrão , "^(ARMAZ).*(HORIZ|BARRA|ARMAZ)" ) == TRUE ~ "09 - Uso Armazéns e Depósitos",
                              str_detect( UsoPadrão , "HOTEL|HOSPITAL|ASILO|ESTAÇÃO" ) == TRUE ~ "10 - Uso Especial ( Hotel, Hospital, Cartório, Etc. )",
                              str_detect( UsoPadrão , "ESCOLA" ) == TRUE ~ "11 - Uso Escola",
                              str_detect( UsoPadrão , "TEATRO|TEMPLO|CINEMA|CLUBE" ) == TRUE ~ "12 - Uso Coletivo ( Cinema, Teatro, Clube, Templo, Etc. )",
                              str_detect( UsoPadrão , "^TERRE" ) == TRUE ~ "13 - Terrenos Vagos",
                              str_detect( UsoPadrão , "^GARAG.*(HORIZ|BARRA|ARMAZ|EDIFÍCIO)" ) == TRUE ~ "15 - Uso garagens não-residenciais",
                              
                              TRUE ~ "FALTA FAZER, no final vai ser 99"
                            )
          ) %>% #filter( str_detect(UsoH,"vai ser 99") == TRUE ) %>% View()
    select( c(-UsoPadrão)) %>%
    group_by( SQL ) %>%
    mutate(
            # consolidando um valor por lote/condomínio, mas ainda com várias instâncias, pra depois poder resumir os usos
            Construído = sum(Construído),
            across( Frentes:FatorObsoles, median ),
            # resumindo os usos APENAS dos condomínios
            
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
          ) %>%
  write_csv2( file = arquivo )
  
  rm(temp)
  gc()
  
}
