library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# caminho <- "pasta do reposit�rio"

#### deixando s� as vari�veis de interesse por ano por lote (considerando o condom�nio inteiro como um lote s�)  ####
# lista de anos com base do IPTU, de 1995 at� o ano atual
ListaAnos <- (year(today())):1995

# leitura e jun��o dos arquivos arrumados
listaCampos <- c( 
  "NUMERO DO CONTRIBUINTE",
  "ANO DO EXERCICIO",
  "NUMERO DO CONDOMINIO",
  "CODLOG DO IMOVEL",
  "CEP DO IMOVEL"
)

# tabela de tradu��o de usos e padr�es em usoH
#usos <- unique(temp2$TipoUso)
#padr�es <- unique(temp2$TipoPadr�o)


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
  
  # extraindo s� campos que precisa
  temp <- temp %>%
    select( all_of(listaCampos) , contains("FRENTE") , "AREA DO TERRENO":"FATOR DE OBSOLESCENCIA" )
  
  # renomeando
  names(temp) <- c("SQL", "Ano", "Condo","Codlog","CEP","Frentes", "Terreno","Constru�do","Ocupado",
                   "ValorTerreno","ValorConstr","AnoConstr","Pavs","Testada","TipoUso",
                   "TipoPadr�o","TipoTerr","FatorObsoles"
  )
  
  # reordenando para facilitar c�lculos  
  temp <- temp %>%
    select( c(1:5,8,6,7,9:14,18,15:17) )
  
  temp1 <-
  arquivo <- paste0( "./10 - processamentos/IPTU_" , ano , "_loteArrumado.csv.gz")
  

  
  temp %>%
    mutate(
            # arrumando campo de pavimentos
            Pavs = as.numeric(Pavs),
            # arrumando casos em que diz que n�o tem frentes pra por pelo menos 1
            Frentes = case_when( Frentes == 0 ~ 1 , TRUE ~ Frentes ),
            # arrumando condom�nios pra n�o contar duplicado
            SQL = str_sub( SQL , 0 , 10 ) , 
            SQL = case_when(
                            Condo != "00-0" ~ paste0( str_sub( SQL , 0 , 6) , "0000" , str_sub(Condo,0,2) ),
                            TRUE ~ paste0( str_sub( SQL , 0 , 10) , str_sub(Condo,0,2) )
                          ),
            # traduzindo os usos-padr�o depois de tornar tudo mai�scula para evitar erros
            UsoPadr�o = toupper(paste( TipoUso , TipoPadr�o )),
            # resumindo em usoH
            UsoH = case_when(
                              # resid�ncia e termina com A ou B / corti�o = 01
                              str_detect( UsoPadr�o , "(^(RESID).*(A|B)$)|(^CORTI�O)" ) == TRUE ~ "01 - Uso Residencial Horizontal Baixo Padr�o",
                              # resid�ncia e termina com C = 02, D/E/F = 03
                              str_detect( UsoPadr�o , "^(RESID|FLAT.*RESID).*(C)$" ) == TRUE ~ "02 - Uso Residencial Horizontal M�dio Padr�o",
                              str_detect( UsoPadr�o , "^(RESID).*(D|E|F)$" ) == TRUE ~ "03 - Uso Residencial Horizontal Alto Padr�o",
                              # apartamentos, pr�dio predom. resid. com A ou B = 14
                              str_detect( UsoPadr�o , "(APARTA|PR.*PREDOM.*RESID).*(A|B)$" ) == TRUE ~ "14 - Uso Residencial Vertical Baixo Padr�o",
                              # apartamentos, pr�dio predom. resid. ou flat com C = 04, D/E/F = 05
                              str_detect( UsoPadr�o , "(APARTA|PR.*PREDOM.*RESID|FLAT.*RESID).*(C)$" ) == TRUE ~ "04 - Uso Residencial Vertical M�dio Padr�o",
                              str_detect( UsoPadr�o , "(APARTA|PR.*PREDOM.*RESID|FLAT.*RESID).*(D|E|F)$" ) == TRUE ~ "05 - Uso Residencial Vertical Alto Padr�o",
                              # loja, posto de gasolina, outras edifica��es de com�rcio ou servi�os, escrit�rios, horizontais ou em barrac�o = 06
                              str_detect( UsoPadr�o , "^(LOJA|OFICI|POSTO|OUTR.*EDIF.*(COMER|SERV)|.*ESCRIT).*(HORIZ|BARRA|ARMAZ)" ) == TRUE ~ "06 - Uso Com�rcio e Servi�o Horizontal",
                              # loja, outras edifica��es de com�rcio ou servi�os, escrit�rios verticais = 07
                              str_detect( UsoPadr�o , "^(LOJA|OUTR.*EDIF.*(COMER|SERV)|FLAT.*COMER|.*ESCRIT).*(VERTI)" ) == TRUE ~ "07 - Uso Com�rcio e Servi�o Vertical",
                              str_detect( UsoPadr�o , "^(IND).*(HORIZ|BARRA|ARMAZ|IND)" ) == TRUE ~ "08 - Uso Industrial",
                              str_detect( UsoPadr�o , "^(ARMAZ).*(HORIZ|BARRA|ARMAZ)" ) == TRUE ~ "09 - Uso Armaz�ns e Dep�sitos",
                              str_detect( UsoPadr�o , "HOTEL|HOSPITAL|ASILO|ESTA��O" ) == TRUE ~ "10 - Uso Especial ( Hotel, Hospital, Cart�rio, Etc. )",
                              str_detect( UsoPadr�o , "ESCOLA" ) == TRUE ~ "11 - Uso Escola",
                              str_detect( UsoPadr�o , "TEATRO|TEMPLO|CINEMA|CLUBE" ) == TRUE ~ "12 - Uso Coletivo ( Cinema, Teatro, Clube, Templo, Etc. )",
                              str_detect( UsoPadr�o , "^TERRE" ) == TRUE ~ "13 - Terrenos Vagos",
                              str_detect( UsoPadr�o , "^GARAG.*(HORIZ|BARRA|ARMAZ|EDIF�CIO)" ) == TRUE ~ "15 - Uso garagens n�o-residenciais",
                              
                              TRUE ~ "FALTA FAZER, no final vai ser 99"
                            )
          ) %>% #filter( str_detect(UsoH,"vai ser 99") == TRUE ) %>% View()
    select( c(-UsoPadr�o)) %>%
    group_by( SQL ) %>%
    mutate(
            # consolidando um valor por lote/condom�nio, mas ainda com v�rias inst�ncias, pra depois poder resumir os usos
            Constru�do = sum(Constru�do),
            across( Frentes:FatorObsoles, median ),
            # resumindo os usos APENAS dos condom�nios
            
          ) %>%
    ungroup() %>%
    # removendo inst�ncias repetidas do mesmo condom�nio
    distinct( SQL , .keep_all = TRUE ) %>%
    # extraindo quadra e calculando CA e TO do lote (j� com condom�nios combinados) para futura compara��o
    mutate(
            CA_lote = Constru�do/Terreno,
            TO_lote = Ocupado/Terreno,
            SQ = str_sub(SQL,0,6),
            ano = ano,
          ) %>%
  write_csv2( file = arquivo )
  
  rm(temp)
  gc()
  
}
