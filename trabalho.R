#trabalho R

#################### Importação das bibliotecas ############################
library(readr)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(ggthemes) # pacote de temas para os gráficos ggplot2
library(tibble)
library(gridExtra)
library(grid)
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

#c) Gráfico dos crimes de posse e posse com intenção de distribuição (tráfico de maconha) por distritos dentro do estado:
dist_posse_traf<- M %>% group_by(OFFENSE_DISTRICT,TYPE) %>% 
  summarise("Numero_De_apreensões" = n()) %>%
  filter(str_detect(TYPE,pattern = "Possession"))

dist_posse_traf<- rename(dist_posse_traf, c(Distritos= OFFENSE_DISTRICT, Tipo_de_Delito=TYPE))#alterando o nome de algumas colunas para o português

dist_posse_traf<- dist_posse_traf%>% filter(str_detect(Distritos,pattern = ".D$"))#tirando os delitos comentidos fora do Estado

gf_Posse_traf_dist<- dist_posse_traf %>% ggplot(aes(x=Distritos, y=Numero_De_apreensões, color=Tipo_de_Delito)) + geom_point(size=4, shape=120)+
  labs(title = "Posse e Tráfico de Maconha por Distritos Dentro do Estado",
       x="Distritos", y="Número de Apreensões", color="Tipo")
gf_Posse_traf_dist+ scale_fill_manual(name="Tipo", values=c("red", "blue"),
                                      label=c("Posse", "Tráfico"))+ theme(plot.title = element_text(size = 12,hjust=0.5),
                                                                           panel.background = element_rect(fill = "grey98"),legend.position = "bottom")


#scale_alpha_discrete(name="tipo de delito", labels=c("Posse","Tráfico")) #problema em utilizar a funçao scale_alpha_discrete para alterar as labels da legenda para o português





####################################################################################
# 2.
# a)Qual o distrito que ocorre mais apreensões (coluna OFFENSE_DISTRICT)? 
n_apDist<-M %>% group_by(OFFENSE_DISTRICT) %>% 
  summarise("Numero_De_apreensões" = n())%>% arrange()
paste0("7D: ", max(n_apDist$Numero_De_apreensões))

# b)Qual o distrito com o maior número de detentos(coluna DEFENDANT_DISTRICT)?
n_dtDist<- M %>% group_by(DEFENDANT_DISTRICT) %>% 
  summarise("Numero_De_Detentos" = n())
paste0("7D: ", max(max(n_dtDist$Numero_De_Detentos)), " detentos")










####################################################################################
# 3. Quantas apreensões foram registradas por consumo próprio? Desse número, quais são as porcentagens entre jovens e adultos? 



####### Tratamento de dados para pergunta 3
# Juntando as colunas por tipo e somando o número total de cada tipo de delito 
Interest_Data <- Interest_Data %>% mutate(TYPE = str_to_title(Interest_Data$TYPE))
adult_juvenile_rate <- Interest_Data %>% 
  group_by(TYPE) %>% 
  summarise("Numero_De_Delitos" = n())
adult_juvenile_rate 

# Adicionando a coluna com número de detidos maiores de idade por tipo de crime 
num_maiores <- Interest_Data %>% 
  group_by(TYPE) %>% 
  filter(ADULT_JUVENILE == "Adult") %>%
  summarise("Maiores De Idade" = n()) %>%
  select("Maiores De Idade")
adult_juvenile_rate <- adult_juvenile_rate %>% mutate(num_maiores)

# Adicionando a coluna com número de detidos menores de idade por tipo de crime 
num_menores <- Interest_Data %>% 
  group_by(TYPE) %>% 
  filter(ADULT_JUVENILE == "Juvenile") %>%
  summarise("Menores De Idade" = n()) %>%
  select("Menores De Idade") 
num_menores <- num_menores %>% add_row("Menores De Idade" = 0, .before = 1) 
num_menores <- num_menores %>% add_row("Menores De Idade" = 0, .before = 3) 
adult_juvenile_rate <- adult_juvenile_rate %>% mutate(num_menores)


# Adicionando a coluna com número de detidos com idade desconhecida por tipo de crime 
idade_des <- Interest_Data %>% 
  group_by(TYPE) %>% 
  filter(ADULT_JUVENILE == "Unknown") %>%
  summarise("Idade_Desconhecida" = n()) %>%
  select("Idade_Desconhecida")
idade_des  <- idade_des   %>% add_row("Idade_Desconhecida" = 0, .before = 1) 
idade_des  <- idade_des   %>% add_row("Idade_Desconhecida" = 0, .before = 3) 
adult_juvenile_rate <- adult_juvenile_rate %>% mutate(idade_des)

# Tabela Pronta
adult_juvenile_rate 



###############  graficos de porcentagem adulto/juvenil para todos os tipos de crime 
# Gráficos
names <- adult_juvenile_rate %>% names() 


# Cultivo  
values <- adult_juvenile_rate %>% slice(1) %>% unname() %>% unlist() %>% as.integer()
dados <- data.frame(Faixa_Etaria = names[-c(1,2,5)],
                    valor = values[-c(1,2,5)])
total <- sum(dados$valor)
dados$percentual <- dados$valor / total * 100
grafico31 <- ggplot(dados, aes(x = "", y = valor, fill = Faixa_Etaria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Cultivo") +
  labs(fill = "Categoria") +
  geom_text(aes(label = paste0(round(percentual),"%")), position = position_stack(vjust = 0.5)) +
  theme_void()

# Distribuição 
values <- adult_juvenile_rate %>% slice(2) %>% unname() %>% unlist() %>% as.integer()
dados <- data.frame(Faixa_Etaria = names[-c(1,2,5)],
                    valor = values[-c(1,2,5)])
total <- sum(dados$valor)
dados$percentual <- dados$valor / total * 100
grafico32 <- ggplot(dados, aes(x = "", y = valor, fill = Faixa_Etaria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribuição") +
  labs(fill = "Categoria") +
  geom_text(aes(label = paste0(round(percentual),"%")), position = position_stack(vjust = 0.5)) +
  theme_void()

# Manufatura
values <- adult_juvenile_rate %>% slice(3) %>% unname() %>% unlist() %>% as.integer()
dados <- data.frame(Faixa_Etaria = names[-c(1,2,5)],
                    valor = values[-c(1,2,5)])
total <- sum(dados$valor)
dados$percentual <- dados$valor / total * 100
grafico33 <- ggplot(dados, aes(x = "", y = valor, fill = Faixa_Etaria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Manufatura") +
  labs(fill = "Categoria") +
  geom_text(aes(label = paste0(round(percentual),"%")), position = position_stack(vjust = 0.5)) +
  theme_void()

# Posse
values <- adult_juvenile_rate %>% slice(4) %>% unname() %>% unlist() %>% as.integer()
dados <- data.frame(Faixa_Etaria = names[-c(1,2,5)],
                    valor = values[-c(1,2,5)])
total <- sum(dados$valor)
dados$percentual <- dados$valor / total * 100
grafico34 <- ggplot(dados, aes(x = "", y = valor, fill = Faixa_Etaria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Posse") +
  labs(fill = "Categoria") +
  geom_text(aes(label = paste0(round(percentual),"%")), position = position_stack(vjust = 0.5)) +
  theme_void()

# Posse com intenção de distribuição
values <- adult_juvenile_rate %>% slice(5) %>% unname() %>% unlist() %>% as.integer()
dados <- data.frame(Faixa_Etaria = names[-c(1,2,5)],
                    valor = values[-c(1,2,5)])
total <- sum(dados$valor)
dados$percentual <- dados$valor / total * 100
grafico35 <- ggplot(dados, aes(x = "", y = valor, fill = Faixa_Etaria), title = "Cultivo") +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Posse com intenção de distribuição") +
  labs(fill = "Categoria") +
  geom_text(aes(label = paste0(round(percentual),"%")), position = position_stack(vjust = 0.5)) +
  theme_void()

# Consumo Público 
values <- adult_juvenile_rate %>% slice(6) %>% unname() %>% unlist() %>% as.integer()
dados <- data.frame(Faixa_Etaria = names[-c(1,2,5)],
                    valor = values[-c(1,2,5)])
total <- sum(dados$valor)
dados$percentual <- dados$valor / total * 100
grafico36 <- grafico <- ggplot(dados, aes(x = "", y = valor, fill = Faixa_Etaria), title = "Cultivo") +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Consumo Público") +
  labs(fill = "Categoria") +
  geom_text(aes(label = paste0(round(percentual),"%")), position = position_stack(vjust = 0.5)) +
  theme_void()


# imprime o grafico numa única imagem
grid.arrange(grafico31, grafico32, grafico33, grafico34, grafico35, grafico36, ncol = 3)

#essa função salva o grafico como png
ggsave(filename = "crimes_adulto_juvenil.png", plot = grid.arrange(grafico31, grafico32, grafico33, grafico34, grafico35, grafico36, ncol = 3), dpi = 500)



##### Considerando consumo próprio as categorias "posse" (sem intenção de distribuição) e "consumo publico" como 
# posse para consumo próprio temos
values1 <- adult_juvenile_rate %>% slice(4) %>% unname() %>% unlist() %>% as.integer()
values2 <- adult_juvenile_rate %>% slice(6) %>% unname() %>% unlist() %>% as.integer()
values1

# valor total de detidos por consumo próprio
valor_total_por_consumo_proprio <- sum(values1[-c(1,2)]) + sum(values2[-c(1,2)])
valor_total_por_consumo_proprio

# porcentagem adulto/juvenil de detidos por consumo próprio
dados <- data.frame(Faixa_Etaria = names[-c(1,2,5)],
                    valor = values1[-c(1,2,5)] + values2[-c(1,2,5)])
total <- sum(dados$valor)
dados$percentual <- dados$valor / total * 100
grafico37 <- grafico <- ggplot(dados, aes(x = "", y = valor, fill = Faixa_Etaria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Porcentagem Audulto/Jovem de Consumo Próprio") +
  labs(fill = "Categoria") +
  geom_text(aes(label = paste0(round(percentual),"%")), position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5)) +
  theme_void()


grid.arrange(grafico37, ncol=1)
ggsave(filename = "crimes_adulto_juvenil_consumo.png", plot = grid.arrange(grafico37, ncol=1), dpi = 500)






#####################################################################################
# 4. A maior parte dos delitos ocorreram em qual período do dia (manhã, tarde, noite)? 
# 5. Qual mês do ano ocorre mais delitos? 





#####################################################################################################
# 6. Qual a idade média de pessoas envolvidas com crimes de tráfego de maconha?
apidade <- M %>% select(TYPE, AGE)
apidade %>% group_by(TYPE) %>% summarise()

max(apidade$AGE)
min(apidade$AGE)
mean(apidade$AGE)

g61 <- apidade %>% 
    ggplot(aes(x = AGE)) +
    geom_histogram(binwidth=1,  
                  color = 1, 
                  fill = 4) +
    labs(x = "Idade", y = "Número de detidos",
    title = "Detidos envolvidos em qualquer tipo de crime") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5))

g62 <- filter(apidade, TYPE == "Cultivation") %>%
ggplot(aes(x = AGE)) +
    geom_histogram(binwidth=1,  
                  color = 1, 
                  fill = 4) +
    labs(x = "Idade", y = "Número de detidos",
    title = "Detidos por cultivo") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5))

g63 <- filter(apidade, TYPE == "Distribution") %>%
ggplot(aes(x = AGE)) +
    geom_histogram(binwidth=1,  
                  color = 1, 
                  fill = 4) +
    labs(x = "Idade", y = "Número de detidos",
    title = "Detidos por distribuição") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5))

g64 <- filter(apidade, TYPE == "Manufacture") %>%
ggplot(aes(x = AGE)) +
    geom_histogram(binwidth=1,  
                  color = 1, 
                  fill = 4) +
    labs(x = "Idade", y = "Número de detidos",
    title = "Detidos por manufatura") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5))

g65 <- filter(apidade, TYPE == "Possession") %>%
ggplot(aes(x = AGE)) +
    geom_histogram(binwidth=1,  
                  color = 1, 
                  fill = 4) +
    labs(x = "Idade", y = "Número de detidos",
    title = "Detidos por posse") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5))

g66 <- filter(apidade, TYPE == "Possession With Intent To Distribute") %>%
ggplot(aes(x = AGE)) +
    geom_histogram(binwidth=1,  
                  color = 1, 
                  fill = 4) +
    labs(x = "Idade", y = "Número de detidos",
    title = "Detidos por posse com intensão de distribuição") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5))

g67 <- filter(apidade, TYPE == "Public Consumption") %>%
ggplot(aes(x = AGE)) +
    geom_histogram(binwidth=1,  
                  color = 1, 
                  fill = 4) +
    labs(x = "Idade", y = "Número de detidos",
    title = "Detidos por uso em público") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle =element_text(hjust = 0.5)) +
    guides(fill = guide_legend(title = "Média"))


grid.arrange(g62, g63, g64, g65, g66, g67, ncol = 3)
ggsave(filename = "distribuicao_idade_por_tipo.png", plot = grid.arrange(g62, g63, g64, g65, g66, g67, ncol = 3), dpi = 500)


M %>% group_by(ANO) %>% summarise(n())




# 7...























