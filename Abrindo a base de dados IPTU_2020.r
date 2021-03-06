# Utilizando a biblioteca readr para importar arquivos csv
>library(readr)

# Executando o comando de abertura passo a passo

> IPTU_2020 <- read_delim("D:/GeoSampa/TPCL/IPTU_2020.csv",

# / IPTU_2020 <- / é o comando para denominar o arquivo de IPTU_2020, mas pode chamar como quiser!
# / read_delim("D:/GeoSampa/TPCL/IPTU_2020.csv", / encontrar a pasta do arquivo termine o comando com vírgula e aperte enter

+     ";", quote = "\\\"", escape_double = FALSE,

# / ";", / indica que o delimitador é pontovírgula (semicolon)
# / quote = "\\\"", / indica que se houver algum texto no campo é delimitado com aspas duplas
# / escape_double = FALSE, / não temos caracter \ no arquivo

+     locale = locale(decimal_mark = ",", encoding = "WINDOWS-1252"),

# / locale = locale(decimal_mark = ",", enconding = "WINDOWS-1252"), / aqui indica que o decimar é vírgula e a codificação do arquivo é Windows=1252

+     trim_ws = TRUE)

# / trim_ws = TRUE) / Remover os epaços brancos e clique em enter!

# Forma final do comando
> IPTU_2020 <- read_delim("D:/GeoSampa/TPCL/IPTU_2020.csv", 
+     ";", quote = "\\\"", escape_double = FALSE, 
+     locale = locale(decimal_mark = ",", encoding = "WINDOWS-1252"), 
+     trim_ws = TRUE)

# Após iniciar, aperte enter, o diálogo abaixo vai aparecer, a barra final indicará quando o arquivo for lido. Enquanto isso pegue um café!!!

Parsed with column specification:
cols(
  .default = col_character(),
  `ANO DO EXERCICIO` = col_double(),
  `NUMERO DA NL` = col_double(),
  `NUMERO DO IMOVEL` = col_double(),
  `QUANTIDADE DE ESQUINAS/FRENTES` = col_double(),
  `FRACAO IDEAL` = col_double(),
  `AREA DO TERRENO` = col_double(),
  `AREA CONSTRUIDA` = col_double(),
  `AREA OCUPADA` = col_double(),
  `VALOR DO M2 DO TERRENO` = col_double(),
  `VALOR DO M2 DE CONSTRUCAO` = col_double(),
  `ANO DA CONSTRUCAO CORRIGIDO` = col_double(),
  `QUANTIDADE DE PAVIMENTOS` = col_double(),
  `TESTADA PARA CALCULO` = col_double(),
  `FATOR DE OBSOLESCENCIA` = col_double(),
  `ANO DE INICIO DA VIDA DO CONTRIBUINTE` = col_double(),
  `MES DE INICIO DA VIDA DO CONTRIBUINTE` = col_double(),
  `FASE DO CONTRIBUINTE` = col_double()
)
See spec(...) for full column specifications.
|==========================================================================================================| 100% 1066 MB

# Para ver a tabela, caso queira. Agora beba seu café vagarosamente!!

View(IPTU_2020)

# Calcular média das áreas de todos os terrenos em m² 

> mean (x = IPTU_2020$"AREA DO TERRENO")
[1] 4004.681 #Resultado? Alguém confirma com outro software?

# Sequência de médias, área construída e ocupada e valor/m² de terreno

> mean (x = IPTU_2020$"AREA CONSTRUIDA")
[1] 153.6926
> mean (x = IPTU_2020$"AREA OCUPADA") 
[1] 1292.137
> mean (x = IPTU_2020$"VALOR DO M2 DO TERRENO")
[1] 1906.349
