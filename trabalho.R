#trabalho R

#################### Importação das bibliotecas ############################
library(readr)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggthemes) # pacote de temas para os gráficos ggplot2


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
DEFENDANT_PSA:	O PSA associado ao réu	numérico
DEFENDANT_DISTRICT:	O distrito associado ao réu	Texto
RACE:	A raça do réu, com base na observação oficial	Texto
ETNIA:	A etnia do réu, com base na observação oficial	Texto
SEXO:	O gênero do réu	Texto
CATEGORIA:	A categoria do delito (por exemplo, posse, distribuição, consumo público)	Texto
DESCRIÇÃO:	Descrição do crime	Texto
ENDEREÇO:	O endereço do local da ofensa	Texto
ARREST_BLOCKX:	Coordenada X da localização aproximada do bloco da prisão	Numérico
ARREST_BLOCKY:	Coordenada Y da localização aproximada do bloco da prisão	Numérico
GIS_ID	ID: do Sistema de Informações Geográficas (GIS) associado ao registro	Texto
O CRIADOR:	O criador do registro	Texto
CREATED:	A data e a hora em que o registro foi criado	Texto
EDITOR:	O editor do disco	Texto
EDITADO:	A data e a hora em que o registro foi editado pela última vez	Texto
OBJECTID:	Identificador único para cada registro	Numérico
GLOBALID:	Identificador exclusivo global para cada registro	Texto
'''

# Selecionando colunas de interesse
variaveis <- c("TYPE", "ADULT_JUVENILE", "DATETIME", "AGE", "RACE", "ETHNICITY", "SEX", "ADDRESS", "DEFENDANT_DISTRICT", "OFFENSE_DISTRICT")
Interest_Data <- Raw_Data %>% select(variaveis)
# Interest_Data são os dados com todas as colunas de interesse
# Foram selecionadas as variáveis que parecem relevantes para as análises propostas


# Removendo linhas com informações nulas 
M <- filter(Interest_Data, TYPE!=0 & ADULT_JUVENILE!=0 & DATETIME!=0 & AGE!=0 
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


# Colocando as entradas da coluna TYPE como título
head(M$TYPE)
M <- M %>% mutate(TYPE = str_to_title(M$TYPE))
print(head(M, 100), n=25)
'''
Em TYPE aparecem "Public consumption" (c minúsculo)  e "Public Consumption" (C maiúsculo). 
Devido isso a coluna foi transformada em título (Palavras começando com maiúsculo) 
'''

M
# M é a base de dados tratada pronta para o início das análises 


#################### Perguntas de interesse #####################################

#################################################################################
# 1. Análise das colunas TYPE e OFFENSE_DISTRICT:
#a) Qual é o principal tipo de delito relacionado com o maior número de apreensões?
tipo_delito<- M %>% 
  group_by(TYPE) %>% 
  summarise("Numero_De_Delitos" = n())
tipo_delito %>%select(Numero_De_Delitos) %>% max()
paste0("O maior tipo de delito de apreensão é posse de maconha, com:  ", tipo_delito$Numero_De_Delitos[4])

#b) Qual o distrito que ocorre as principais apreensões desse tipo de delito? 
'''
sugestão: em vez de usar a coluna ADDRESS usar offense_district. 
A coluna address tem mais de 4 mil endereços diferentes com uma ou duas ocorrências 
Usando as colunas offense_district temos os distritos onde ocorrem mais apreensões e com
defendant_district de qual distrito vem o maior número de pessoas detidas
'''
local_apreensoes_posse<- M %>% group_by(OFFENSE_DISTRICT,TYPE) %>% 
  summarise("Numero_De_apreensões" = n()) %>%
  filter(str_detect(TYPE,pattern = "Possession$"))
local_apreensoes_posse<- local_apreensoes_posse %>% select(OFFENSE_DISTRICT, Numero_De_apreensões)%>% arrange(Numero_De_apreensões)
paste0("O distrito em que ocorre o maior número de apreensões por posse é: ", local_apreensoes_posse$OFFENSE_DISTRICT[8], ", com: ", local_apreensoes_posse$Numero_De_apreensões[8])

#c) Gráfico dos crimes de posse e posse com intenção de distribuição (tráfico de maconha) por distritos:
dist_posse_traf<- M %>% group_by(OFFENSE_DISTRICT,TYPE) %>% 
  summarise("Numero_De_apreensões" = n()) %>%
  filter(str_detect(TYPE,pattern = "Possession"))
gf_Posse_traf_dist<- dist_posse_traf %>% ggplot(aes(x=OFFENSE_DISTRICT, y=Numero_De_apreensões, color=TYPE)) + geom_point()+
  labs(title = "Gráfico de Posse e Tráfico de Maconha por Distritos", x="Distritos", y="Número de Apreensões", color="Tipo de Delito")+
  theme_solarized_2()
gf_Posse_traf_dist 
#vou mexer mais no gráfico depois
####################################################################################
# 2. Qual o distrito que ocorre mais apreensões (coluna OFFENSE_DISTRICT)? qual o distrito com o maior número de detentos(coluna DEFENDANT_DISTRICT)?



####################################################################################
# 3. Quantas apreensões foram registradas por consumo próprio? Desse número, quais são as porcentagens entre jovens e adultos? 
M %>% 
  group_by(TYPE) %>% 
  summarise("Numero_De_Delitos" = n())
# Como não existe exatamente um tipo "consumo próprio" foi considerado que porte (possession) sem intenção de distribuição é posse de consumo próprio




#####################################################################################
# 4. A maior parte dos delitos ocorreram em qual período do dia (manhã, tarde, noite)? 
# 5. Qual mês do ano ocorre mais delitos? 
# 6. Qual a idade média de pessoas envolvidas com crimes de tráfego de maconha?
# 7...











