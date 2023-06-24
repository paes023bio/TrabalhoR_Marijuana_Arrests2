#trabalho R

#################### Importação das bibliotecas ############################
library(readr)
library(dplyr)
library(tidyverse)



#################### Importação do conjunto de dados #######################

# Importando o banco de dados localmente (caso a url não funcione)
# Raw_Data <- read_csv("Diretório do arquivo salvo no computador")

# Importação do banco de dados remoto salvo no diretório do github
Raw_Data <- read_csv(url("https://raw.githubusercontent.com/paes023bio/TrabalhoR_Marijuana_Arrests2/main/Marijuana_Arrests.csv"))
# Variável chamada de Raw_Data por ser os dados "crus" 

View(Raw_Data)
str(Raw_Data)

Raw_Data %>% select(TYPE) %>% head(1000) %>% View()





####################### Tratamento dos dados #############################

# Descrição das variáveis dos dados puros 
'''
TIPO: Indica o tipo de prisão	Texto
ADULT_JUVENILE: Especifica se o detido é um adulto ou um menor	Texto
ANO:	O ano em que ocorreu a prisão	Numérico
DATA HORA:	A data e hora da prisão	Texto
CCN:	Número de hash que permite aos indivíduos determinar se há várias prisões associadas a um evento	Texto
IDADE:	A idade do detido no momento da detenção	Numérico
OFFENSE_DISTRICT:	Bairro onde ocorreu o crime	Texto
OFFENSE_PSA:	A Área de Serviço Policial (PSA) associada ao crime	Texto
OFFENSE_BLOCKX:	Coordenada X da localização aproximada do bloco da ofensa	Numérico
OFFENSE_BLOCKY:	Coordenada Y da localização aproximada do bloco da ofensa	Numérico
DEFENDANT_PSA:	O PSA associado ao réu	Texto
DEFENDANT_DISTRICT:	O distrito associado ao réu	Texto
CORRIDA:	A raça do réu, com base na observação oficial	Texto
ETNIA:	A etnia do réu, com base na observação oficial	Texto
SEXO:	O gênero do réu	Texto
CATEGORIA:	A categoria do delito (por exemplo, posse, distribuição, consumo público)	Texto
DESCRIÇÃO:	Descrição do crime	Texto
ENDEREÇO:	O endereço do local da ofensa	Texto
ARREST_BLOCKX:	Coordenada X da localização aproximada do bloco da prisão	Numérico
ARREST_BLOCKY:	Coordenada Y da localização aproximada do bloco da prisão	Numérico
GIS_ID	ID: do Sistema de Informações Geográficas (GIS) associado ao registro	Texto
O CRIADOR:	O criador do registro	Texto
CRIADA:	A data e a hora em que o registro foi criado	Texto
EDITOR:	O editor do disco	Texto
EDITADO:	A data e a hora em que o registro foi editado pela última vez	Texto
OBJECTID:	Identificador único para cada registro	Numérico
GLOBALID:	Identificador exclusivo global para cada registro	Texto
'''

# Selecionando colunas de interesse
variaveis <- c("TYPE", "ADULT_JUVENILE", "DATETIME", "AGE", "RACE", "ETHNICITY", "SEX", "ADDRESS", "DEFENDANT_DISTRICT", "OFFENSE_DISTRICT")
M_interesse <- Raw_Data %>% select(variaveis)
# M_interesse são os dados com todas as colunas de interesse
# Foram selecionadas as variáveis que parecem relevantes para as análises propostas


# Removendo linhas com informações nulas 
M <- filter(M_interesse, TYPE!=0 & ADULT_JUVENILE!=0 & DATETIME!=0 & AGE!=0 
    & RACE!=0 & ETHNICITY!=0 & SEX!=0 & ADDRESS!=0 & DEFENDANT_DISTRICT!=0 
    & OFFENSE_DISTRICT!=0)
print(head(M, 50), n=50)
'''
A informação de que o detido é maior ou menor de idade é relevante, mas a descrição 
do banco de dados diz que todos menores estão com as informações em branco por questões de privacidade. 
Portanto todos marcados como "Juvenile" em ADULT_JUVENILE foram removidos junto com todas as outras linhas
com alguma entrada Nula, entretanto essa variável pode ser utilizada para obter o número de menores detidos
e seu tipo de delito.
'''

# separando a variável DATATIME em ANO, MES, DIA e HORA
M <- M %>% separate(DATETIME, into = c("ANO", "MES", "DIAHORA"), sep="/")
M <- M %>% separate(DIAHORA, into = c("DIA", "HORA"), sep=" ")
# nesse ponto M possui todas as colunas de interesse já tratadas (entradas nulas removidas e informações concatenadas separadas)
print(head(M, 100), n=100)



#################### Perguntas de interesse #####################################

# 1. Qual é o principal tipo de delito (coluna Type) relacionado com o maior número de apreensões? Onde (coluna address) ocorre as principais apreensões desse tipo de delito? 
M_tratado
# 2. Qual o distrito que ocorre mais apreensões (coluna OFFENSE_DISTRICT)? qual o distrito com o maior número de detentos(coluna DEFENDANT_DISTRICT)?
# 3. Quantas apreensões foram registradas por consumo próprio? Desse número, quais são as porcentagens entre jovens e adultos? 
# 4. A maior parte dos delitos ocorreram em qual período do dia (manhã, tarde, noite)? 
# 5. Qual mês do ano ocorre mais delitos? 
# 6. Qual a idade média de pessoas envolvidas com crimes de tráfego de maconha?
# 7...











