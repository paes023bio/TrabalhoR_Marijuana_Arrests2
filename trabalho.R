#trabalho R

#carregamento dos dados
library(readr)
library(dplyr)

# importação do conjunto de dados Marijuana_Arrests
Marijuana_Arrests <- read_csv("C:/Users/paes2/Desktop/disciplinas/Linguagem R/trabalho/TrabalhoR_Marijuana_Arrests/Marijuana_Arrests.csv")
#obs.: quando for abrir o arquivo colocar o diretório específico do arquivo .csv
View(Marijuana_Arrests)
#  url<- "https://github.com/paes023bio/TrabalhoR_Marijuana_Arrests2/blob/33c8c1db26b6b074fea8ebc4ec959a2f118bfcb7/Marijuana_Arrests.csv"
# db_Marijuana_Arrests<- read_csv(url)

Marijuana_Arrests%>% select(TYPE) %>% head(1000) %>% View()

#descrição das variáveis
# TIPO: Indica o tipo de prisão	Texto
# ADULT_JUVENILE: Especifica se o detido é um adulto ou um menor	Texto
# ANO:	O ano em que ocorreu a prisão	Numérico
# DATA HORA:	A data e hora da prisão	Texto
# CCN:	Número de hash que permite aos indivíduos determinar se há várias prisões associadas a um evento	Texto
# IDADE:	A idade do detido no momento da detenção	Numérico
# OFFENSE_DISTRICT:	Bairro onde ocorreu o crime	Texto
# OFFENSE_PSA:	A Área de Serviço Policial (PSA) associada ao crime	Texto
# OFFENSE_BLOCKX:	Coordenada X da localização aproximada do bloco da ofensa	Numérico
# OFFENSE_BLOCKY:	Coordenada Y da localização aproximada do bloco da ofensa	Numérico
# DEFENDANT_PSA:	O PSA associado ao réu	Texto
# DEFENDANT_DISTRICT:	O distrito associado ao réu	Texto
# CORRIDA:	A raça do réu, com base na observação oficial	Texto
# ETNIA:	A etnia do réu, com base na observação oficial	Texto
# SEXO:	O gênero do réu	Texto
# CATEGORIA:	A categoria do delito (por exemplo, posse, distribuição, consumo público)	Texto
# DESCRIÇÃO:	Descrição do crime	Texto
# ENDEREÇO:	O endereço do local da ofensa	Texto
# ARREST_BLOCKX:	Coordenada X da localização aproximada do bloco da prisão	Numérico
# ARREST_BLOCKY:	Coordenada Y da localização aproximada do bloco da prisão	Numérico
# GIS_ID	ID: do Sistema de Informações Geográficas (GIS) associado ao registro	Texto
# O CRIADOR:	O criador do registro	Texto
# CRIADA:	A data e a hora em que o registro foi criado	Texto
# EDITOR:	O editor do disco	Texto
# EDITADO:	A data e a hora em que o registro foi editado pela última vez	Texto
# OBJECTID:	Identificador único para cada registro	Numérico
# GLOBALID:	Identificador exclusivo global para cada registro	Texto


#limpeaza dos dados
#sugestões: retirar linhas com NA; e colunas que não iram ser usadas, como GIS_ID, EDITOR, OBJECTID...
Marijuana_Arrests_new<- Marijuana_Arrests %>% filter(c(DEFENDANT_PSA!= 0 | DEFENDANT_DISTRICT!=0)) %>% select(c(-CCN, -OFFENSE_BLOCKX, -OFFENSE_BLOCKY,-GIS_ID, -CREATED, -ARREST_BLOCKX, -ARREST_BLOCKY)) 

# Perguntas de interesse:
# 1. Qual é o principal tipo de delito (coluna Type) relacionado com o maior número de apreensões? Onde (coluna address) ocorre as principais apreensões desse tipo de delito?

# 2. Quantas apreensões foram registradas por consumo próprio? Desse número, quais são as porcentagens entre jovens e adultos?
# 3. A maior parte dos delitos ocorreram em qual período do dia (manhã, tarde, noite)? 
# 4. Qual a idade média de pessoas envolvidas com crimes de tráfego de maconha?
# 5...

#estive aqui










