
#read.me->
#Aqui serão realizadas duas etapas sendo elas:
#1- manipuçaão e tratamento da base SCR.data; 
#2- concatenação de todas as bases utilizadas (scr.data, taxa de juros selic realizada, e ibcrBR) 


###### INICIANDO A ETAPA 1######

#limpando a instancia
rm(list = ls())
gc()
#importando pacotes
library(readr)
library(dplyr)
library(data.table)
library(dplyr)
library(readxl)
library(psych)
library(bit64)
library(survey)
library(srvyr)
library(tidyverse)

### Indica diretório que vai trabalhar da base de dados
setwd('D:/dissertacao/dados/descompactados')
dir()
memory.limit (9999999999) #aumentando espaço da memória utilizável

## SERÁ FEITA NA SEGUINTE SEQUÊNCIA: 1- IMPORTAÇÃO,2- CONCAT DAS BASES, 3- REMOVER O QUE NÃO SERÁ MAIS UTILIZADO,
#4- TRANFORMAÇÃO DA VARIÁVEL RESULTADO EM NUMÉRICA

#ETAPA 1
df_201206 <- data.table::fread("planilha_201206.csv", dec=',', encoding = 'UTF-8')
df_201207 <- data.table::fread("planilha_201207.csv", dec=',', encoding = 'UTF-8')
df_201208 <- data.table::fread("planilha_201208.csv", dec=',', encoding = 'UTF-8')
df_201209 <- data.table::fread("planilha_201209.csv", dec=',', encoding = 'UTF-8')
df_201210 <- data.table::fread("planilha_201210.csv", dec=',', encoding = 'UTF-8')
df_201211 <- data.table::fread("planilha_201211.csv", dec=',', encoding = 'UTF-8')
df_201212 <- data.table::fread("planilha_201212.csv", dec=',', encoding = 'UTF-8')

##########merge da base########
#ETAPA 2 
df_2012 = rbind(df_201206,df_201207,df_201208,df_201209,df_201210,df_201211,df_201212)
#ETAPA 3
#removendo as bases que ja foram agrupadas para otimizar o espaço
remove(df_201206,df_201207,df_201208,df_201209,df_201210,df_201211,df_201212)
#ETAPA4
#convertendo a classe de algumas colunas
df_2012$carteira_ativa  <- as.numeric(as.character(df_2012$carteira_ativa))

# CASO QUEIRA VERIFICAR SE A BASE ESTÁ CORRETA, SEGUE UMA VERIFICAÇÃO, APÓS ISSO BASTA OBSERVAR SE OS VALORES CONDIZEM COM
# OS DO SITE do SCR. data, lembrando sempre de fazer os filtros iguais ao da verificação abaixo.
#segue o link: https://www.bcb.gov.br/estabilidadefinanceira/scrdata?dataIn=2012-05-31&dataFim=2021-04-30&uf_filtro=Todas&cnaeocup_filtro=Todos&porte_filtro=Todos&modalidade_filtro=Todas&origem_filtro=Todas&indexador_filtro=Todos&cliente_filtro=Todos&numSeries=1
{
  #dt_2012 <- df_2012 %>%
  #  group_by(data_base,modalidade,origem,porte,cliente) %>%
  # summarise(carteira_ativa = sum(carteira_ativa, na.rm = TRUE))
  
  #class(dt_2012)
  #summary(dt_2012, exclude =NULL)
  #head(dt_2012)
  
  #describeBy(dt_2012$carteira_ativa, group = dt_2012$coluna_2012_pf_semdetinacao)
  
  
  
  #table(dt_2012$modalidade,dt_2012$carteira_ativa,exclude = NULL)
  
  
  #verificando um valor especifico da base
  #dt_2012$coluna_2012_pf_cartaodecredito_semdetinacao = NA
  #dt_2012$coluna_2012_pf_cartaodecredito_semdetinacao = ifelse(dt_2012$modalidade =="PF - Cartão de crédito" & dt_2012$porte =="PF - Sem rendimento" & dt_2012$origem =="Sem destinação específica", 1, 0)
  
  #df de teste com os valores desejados
  #dt_teste = dt_2012 %>%
  #group_by(coluna_2012_pf_cartaodecredito_semdetinacao, porte, data_base) %>%
  # summarise(Total_carteira = sum(carteira_ativa))
  #(dt_teste)
  #verificando se o gráfico condiz com os do site
  # dados que quero verificar
  #dados<-dt_teste[dt_teste$`coluna_2012_pf_cartaodecredito_semdetinacao == 1`==TRUE,]
  #head(dados)
  
  #plot(x= dados$data_base,y= dados$Total_carteira, type='l')
  #points(x= dados$data_base,y= dados$Total_carteira)
  
  
  #dados %>% ggplot(aes(x= dados$data_base,y= dados$Total_carteira))+
  # geom_line()
  
  # ok, a base tem o aval pois os valores e os gráficos batem com os do site, agora vamos avançare fazer os concats de todos os anos e depois verificar novamente
}

## 2013##
{
  df_201301 <- data.table::fread("planilha_201301.csv", dec=',', encoding = 'UTF-8')
  df_201302 <- data.table::fread("planilha_201302.csv", dec=',', encoding = 'UTF-8')
  df_201303 <- data.table::fread("planilha_201303.csv", dec=',', encoding = 'UTF-8')
  df_201304 <- data.table::fread("planilha_201304.csv", dec=',', encoding = 'UTF-8')
  df_201305 <- data.table::fread("planilha_201305.csv", dec=',', encoding = 'UTF-8')
  df_201306 <- data.table::fread("planilha_201306.csv", dec=',', encoding = 'UTF-8')
  df_201307 <- data.table::fread("planilha_201307.csv", dec=',', encoding = 'UTF-8')
  df_201308 <- data.table::fread("planilha_201308.csv", dec=',', encoding = 'UTF-8')
  df_201309 <- data.table::fread("planilha_201309.csv", dec=',', encoding = 'UTF-8')
  df_201310 <- data.table::fread("planilha_201310.csv", dec=',', encoding = 'UTF-8')
  df_201311 <- data.table::fread("planilha_201311.csv", dec=',', encoding = 'UTF-8')
  df_201312 <- data.table::fread("planilha_201312.csv", dec=',', encoding = 'UTF-8')
  
  
  df_2013 = rbind(df_201301,df_201302,df_201303,df_201304,df_201305,df_201306,df_201307,df_201308,df_201309,df_201310,df_201311,df_201312)
  
  remove(df_201301,df_201302,df_201303,df_201304,df_201305,df_201306,df_201307,df_201308,df_201309,df_201310,df_201311,df_201312)
  #convertendo a classe de algumas colunas
  df_2013$carteira_ativa  <- as.numeric(as.character(df_2013$carteira_ativa))
}

## 2014##
{
  df_201401 <- data.table::fread("planilha_201401.csv", dec=',', encoding = 'UTF-8')
  df_201402 <- data.table::fread("planilha_201402.csv", dec=',', encoding = 'UTF-8')
  df_201403 <- data.table::fread("planilha_201403.csv", dec=',', encoding = 'UTF-8')
  df_201404 <- data.table::fread("planilha_201404.csv", dec=',', encoding = 'UTF-8')
  df_201405 <- data.table::fread("planilha_201405.csv", dec=',', encoding = 'UTF-8')
  df_201406 <- data.table::fread("planilha_201406.csv", dec=',', encoding = 'UTF-8')
  df_201407 <- data.table::fread("planilha_201407.csv", dec=',', encoding = 'UTF-8')
  df_201408 <- data.table::fread("planilha_201408.csv", dec=',', encoding = 'UTF-8')
  df_201409 <- data.table::fread("planilha_201409.csv", dec=',', encoding = 'UTF-8')
  df_201410 <- data.table::fread("planilha_201410.csv", dec=',', encoding = 'UTF-8')
  df_201411 <- data.table::fread("planilha_201411.csv", dec=',', encoding = 'UTF-8')
  df_201412 <- data.table::fread("planilha_201412.csv", dec=',', encoding = 'UTF-8')
  
  df_2014 = rbind(df_201401,df_201402,df_201403,df_201404,df_201405,df_201406,df_201407,df_201408,df_201409,df_201410,df_201411,df_201412)
 
  remove(df_201401,df_201402,df_201403,df_201404,df_201405,df_201406,df_201407,df_201408,df_201409,df_201410,df_201411,df_201412)
  
   #convertendo a classe de algumas colunas
  df_2014$carteira_ativa  <- as.numeric(as.character(df_2014$carteira_ativa))
}

## 2015##
{
  df_201501 <- data.table::fread("planilha_201501.csv", dec=',', encoding = 'UTF-8')
  df_201502 <- data.table::fread("planilha_201502.csv", dec=',', encoding = 'UTF-8')
  df_201503 <- data.table::fread("planilha_201503.csv", dec=',', encoding = 'UTF-8')
  df_201504 <- data.table::fread("planilha_201504.csv", dec=',', encoding = 'UTF-8')
  df_201505 <- data.table::fread("planilha_201505.csv", dec=',', encoding = 'UTF-8')
  df_201506 <- data.table::fread("planilha_201506.csv", dec=',', encoding = 'UTF-8')
  df_201507 <- data.table::fread("planilha_201507.csv", dec=',', encoding = 'UTF-8')
  df_201508 <- data.table::fread("planilha_201508.csv", dec=',', encoding = 'UTF-8')
  df_201509 <- data.table::fread("planilha_201509.csv", dec=',', encoding = 'UTF-8')
  df_201510 <- data.table::fread("planilha_201510.csv", dec=',', encoding = 'UTF-8')
  df_201511 <- data.table::fread("planilha_201511.csv", dec=',', encoding = 'UTF-8')
  df_201512 <- data.table::fread("planilha_201512.csv", dec=',', encoding = 'UTF-8')
  
  df_2015 = rbind(df_201501,df_201502,df_201503,df_201504,df_201505,df_201506,df_201507,df_201508,df_201509,df_201510,df_201511,df_201512)
  
  remove(df_201501,df_201502,df_201503,df_201504,df_201505,df_201506,df_201507,df_201508,df_201509,df_201510,df_201511,df_201512)
  
  #convertendo a classe de algumas colunas
  df_2015$carteira_ativa  <- as.numeric(as.character(df_2015$carteira_ativa))
}

## 2016##
{
  df_201601 <- data.table::fread("planilha_201601.csv", dec=',', encoding = 'UTF-8')
  df_201602 <- data.table::fread("planilha_201602.csv", dec=',', encoding = 'UTF-8')
  df_201603 <- data.table::fread("planilha_201603.csv", dec=',', encoding = 'UTF-8')
  df_201604 <- data.table::fread("planilha_201604.csv", dec=',', encoding = 'UTF-8')
  df_201605 <- data.table::fread("planilha_201605.csv", dec=',', encoding = 'UTF-8')
  df_201606 <- data.table::fread("planilha_201606.csv", dec=',', encoding = 'UTF-8')
  df_201607 <- data.table::fread("planilha_201607.csv", dec=',', encoding = 'UTF-8')
  df_201608 <- data.table::fread("planilha_201608.csv", dec=',', encoding = 'UTF-8')
  df_201609 <- data.table::fread("planilha_201609.csv", dec=',', encoding = 'UTF-8')
  df_201610 <- data.table::fread("planilha_201610.csv", dec=',', encoding = 'UTF-8')
  df_201611 <- data.table::fread("planilha_201611.csv", dec=',', encoding = 'UTF-8')
  df_201612 <- data.table::fread("planilha_201612.csv", dec=',', encoding = 'UTF-8')
  
  df_2016 = rbind(df_201601,df_201602,df_201603,df_201604,df_201605,df_201606,df_201607,df_201608,df_201609,df_201610,df_201611,df_201612)
  
  remove(df_201601,df_201602,df_201603,df_201604,df_201605,df_201606,df_201607,df_201608,df_201609,df_201610,df_201611,df_201612)
  #convertendo a classe de algumas colunas
  df_2016$carteira_ativa  <- as.numeric(as.character(df_2016$carteira_ativa))
}

## 2017##
{
  df_201701 <- data.table::fread("planilha_201701.csv", dec=',', encoding = 'UTF-8')
  df_201702 <- data.table::fread("planilha_201702.csv", dec=',', encoding = 'UTF-8')
  df_201703 <- data.table::fread("planilha_201703.csv", dec=',', encoding = 'UTF-8')
  df_201704 <- data.table::fread("planilha_201704.csv", dec=',', encoding = 'UTF-8')
  df_201705 <- data.table::fread("planilha_201705.csv", dec=',', encoding = 'UTF-8')
  df_201706 <- data.table::fread("planilha_201706.csv", dec=',', encoding = 'UTF-8')
  df_201707 <- data.table::fread("planilha_201707.csv", dec=',', encoding = 'UTF-8')
  df_201708 <- data.table::fread("planilha_201708.csv", dec=',', encoding = 'UTF-8')
  df_201709 <- data.table::fread("planilha_201709.csv", dec=',', encoding = 'UTF-8')
  df_201710 <- data.table::fread("planilha_201710.csv", dec=',', encoding = 'UTF-8')
  df_201711 <- data.table::fread("planilha_201711.csv", dec=',', encoding = 'UTF-8')
  df_201712 <- data.table::fread("planilha_201712.csv", dec=',', encoding = 'UTF-8')
  
  df_2017 = rbind(df_201701,df_201702,df_201703,df_201704,df_201705,df_201706,df_201707,df_201708,df_201709,df_201710,df_201711,df_201712)
  
  remove(df_201701,df_201702,df_201703,df_201704,df_201705,df_201706,df_201707,df_201708,df_201709,df_201710,df_201711,df_201712)
  #convertendo a classe de algumas colunas
  df_2017$carteira_ativa  <- as.numeric(as.character(df_2017$carteira_ativa))
}

## 2018##
{
  df_201801 <- data.table::fread("planilha_201801.csv", dec=',', encoding = 'UTF-8')
  df_201802 <- data.table::fread("planilha_201802.csv", dec=',', encoding = 'UTF-8')
  df_201803 <- data.table::fread("planilha_201803.csv", dec=',', encoding = 'UTF-8')
  df_201804 <- data.table::fread("planilha_201804.csv", dec=',', encoding = 'UTF-8')
  df_201805 <- data.table::fread("planilha_201805.csv", dec=',', encoding = 'UTF-8')
  df_201806 <- data.table::fread("planilha_201806.csv", dec=',', encoding = 'UTF-8')
  df_201807 <- data.table::fread("planilha_201807.csv", dec=',', encoding = 'UTF-8')
  df_201808 <- data.table::fread("planilha_201808.csv", dec=',', encoding = 'UTF-8')
  df_201809 <- data.table::fread("planilha_201809.csv", dec=',', encoding = 'UTF-8')
  df_201810 <- data.table::fread("planilha_201810.csv", dec=',', encoding = 'UTF-8')
  df_201811 <- data.table::fread("planilha_201811.csv", dec=',', encoding = 'UTF-8')
  df_201812 <- data.table::fread("planilha_201812.csv", dec=',', encoding = 'UTF-8')
  
  df_2018 = rbind(df_201801,df_201802,df_201803,df_201804,df_201805,df_201806,df_201807,df_201808,df_201809,df_201810,df_201811,df_201812)
  
  remove(df_201801,df_201802,df_201803,df_201804,df_201805,df_201806,df_201807,df_201808,df_201809,df_201810,df_201811,df_201812)
  #convertendo a classe de algumas colunas
  df_2018$carteira_ativa  <- as.numeric(as.character(df_2018$carteira_ativa))
}

## 2019##
{
  df_201901 <- data.table::fread("planilha_201901.csv", dec=',', encoding = 'UTF-8')
  df_201902 <- data.table::fread("planilha_201902.csv", dec=',', encoding = 'UTF-8')
  df_201903 <- data.table::fread("planilha_201903.csv", dec=',', encoding = 'UTF-8')
  df_201904 <- data.table::fread("planilha_201904.csv", dec=',', encoding = 'UTF-8')
  df_201905 <- data.table::fread("planilha_201905.csv", dec=',', encoding = 'UTF-8')
  df_201906 <- data.table::fread("planilha_201906.csv", dec=',', encoding = 'UTF-8')
  df_201907 <- data.table::fread("planilha_201907.csv", dec=',', encoding = 'UTF-8')
  df_201908 <- data.table::fread("planilha_201908.csv", dec=',', encoding = 'UTF-8')
  df_201909 <- data.table::fread("planilha_201909.csv", dec=',', encoding = 'UTF-8')
  df_201910 <- data.table::fread("planilha_201910.csv", dec=',', encoding = 'UTF-8')
  df_201911 <- data.table::fread("planilha_201911.csv", dec=',', encoding = 'UTF-8')
  df_201912 <- data.table::fread("planilha_201912.csv", dec=',', encoding = 'UTF-8')
  
  df_2019 = rbind(df_201901,df_201902,df_201903,df_201904,df_201905,df_201906,df_201907,df_201908,df_201909,df_201910,df_201911,df_201912)
  
  remove(df_201901,df_201902,df_201903,df_201904,df_201905,df_201906,df_201907,df_201908,df_201909,df_201910,df_201911,df_201912)
  #convertendo a classe de algumas colunas
  df_2019$carteira_ativa  <- as.numeric(as.character(df_2019$carteira_ativa))
}

## 2020##
{
  df_202001 <- data.table::fread("planilha_202001.csv", dec=',', encoding = 'UTF-8')
  df_202002 <- data.table::fread("planilha_202002.csv", dec=',', encoding = 'UTF-8')
  df_202003 <- data.table::fread("planilha_202003.csv", dec=',', encoding = 'UTF-8')
  df_202004 <- data.table::fread("planilha_202004.csv", dec=',', encoding = 'UTF-8')
  df_202005 <- data.table::fread("planilha_202005.csv", dec=',', encoding = 'UTF-8')
  df_202006 <- data.table::fread("planilha_202006.csv", dec=',', encoding = 'UTF-8')
  df_202007 <- data.table::fread("planilha_202007.csv", dec=',', encoding = 'UTF-8')
  df_202008 <- data.table::fread("planilha_202008.csv", dec=',', encoding = 'UTF-8')
  df_202009 <- data.table::fread("planilha_202009.csv", dec=',', encoding = 'UTF-8')
  df_202010 <- data.table::fread("planilha_202010.csv", dec=',', encoding = 'UTF-8')
  df_202011 <- data.table::fread("planilha_202011.csv", dec=',', encoding = 'UTF-8')
  df_202012 <- data.table::fread("planilha_202012.csv", dec=',', encoding = 'UTF-8')
  
  df_2020 = rbind(df_202001,df_202002,df_202003,df_202004,df_202005,df_202006,df_202007,df_202008,df_202009,df_202010,df_202011,df_202012)
  
  remove(df_202001,df_202002,df_202003,df_202004,df_202005,df_202006,df_202007,df_202008,df_202009,df_202010,df_202011,df_202012)
  #convertendo a classe de algumas colunas
  df_2020$carteira_ativa  <- as.numeric(as.character(df_2020$carteira_ativa))
}


## 2021##
{
  df_202101 <- data.table::fread("planilha_202101.csv", dec=',', encoding = 'UTF-8')
  df_202102 <- data.table::fread("planilha_202102.csv", dec=',', encoding = 'UTF-8')
  df_202103 <- data.table::fread("planilha_202103.csv", dec=',', encoding = 'UTF-8')
  df_202104 <- data.table::fread("planilha_202104.csv", dec=',', encoding = 'UTF-8')
  df_202105 <- data.table::fread("planilha_202105.csv", dec=',', encoding = 'UTF-8')
  df_202106 <- data.table::fread("planilha_202106.csv", dec=',', encoding = 'UTF-8')
  
  df_2021 = rbind(df_202101,df_202102,df_202103,df_202104,df_202105,df_202106)
  
  remove(df_202101,df_202102,df_202103,df_202104,df_202105,df_202106)
  #convertendo a classe de algumas colunas
  df_2021$carteira_ativa  <- as.numeric(as.character(df_2021$carteira_ativa))
}  

#concatenando os anos para ober a base final do scr.data


df_scr = rbind(df_2012,df_2013,df_2014,df_2015,df_2016,df_2017,df_2018,df_2019,df_2020,df_2021)

remove(df_2012,df_2013,df_2014,df_2015,df_2016,df_2017,df_2018,df_2019,df_2020,df_2021)

#criando as variáveis que serão utilizadas na análise

dt_scr_agrupado <- df_scr %>%
  group_by(data_base,modalidade,origem,porte,cliente) %>% #agrupando de acordo com as keys das empresas
  summarise(carteira_ativa = sum(carteira_ativa, na.rm = TRUE), #dentro do summarise() está todas as variáveis principais que utilizaremos inicialmente 
            carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada,na.rm = TRUE),
            ativo_problematico=sum(ativo_problematico,na.rm=TRUE),
            a_vencer_ate_90_dias=sum(a_vencer_ate_90_dias,na.rm = TRUE),
            a_vencer_de_91_ate_360_dias=sum(a_vencer_de_91_ate_360_dias,na.rm = TRUE),
            a_vencer_de_361_ate_1080_dias=sum(a_vencer_de_361_ate_1080_dias,na.rm = TRUE),
            a_vencer_de_1081_ate_1800_dias=sum(a_vencer_de_1081_ate_1800_dias,na.rm = TRUE),
            a_vencer_de_1801_ate_5400_dias=sum(a_vencer_de_1801_ate_5400_dias,na.rm = TRUE),
            a_vencer_acima_de_5400_dias=sum(a_vencer_acima_de_5400_dias,na.rm = TRUE)
            )

#salvando a base
# write.table(dt_scr_agrupado, file = "D:/dissertacao/dados/df_scr_agrupado_2.csv")

#verificando se a base bate com o site mais uma vez, com o objetivo de mitigar possíveis erros no decorrer da criação da base #

{

  #base filtrada
  dt_teste <- dt_scr_agrupado %>%
    group_by(porte,data_base) %>%
    filter(modalidade =="PF - Cartão de crédito" ,porte =="PF - Sem rendimento",origem =="Sem destinação específica") %>%
    summarise(Total_carteira = sum(carteira_ativa)/1000000000) #olharemos em bilhão apra facilitar a visualização
  
  head(dt_teste)
  
  #gerando gráfiso para verificar
  
  
  plot(x= dt_teste$data_base,y= dt_teste$Total_carteira, type='l', xlab='Anos',ylab = 'Valor da carteira',main ='Pf-cartao de credito, sem rendimento e sem destinação específica')
  points(x= dt_teste$data_base,y= dt_teste$Total_carteira)
  
  
  dt_teste %>% ggplot(aes(x= dt_teste$data_base,y= dt_teste$Total_carteira))+
    geom_line()
  
  # ok, a base tem o aval pois os valores e os gráficos batem com os do site, logo podemos avançar para a próxima etapa
  }


##### ETAPA 2: JUNÇÃO DE TODAS AS  BASES QUE SERÃO UTILIZADAS NA ANÁLISE#######


#limpando a instancia mais uma vez

rm(list = ls())
gc()

#importando pacotes
library(readr)
library(dplyr)
library(data.table)
library(dplyr)
library(readxl)
library(psych)
library(bit64)
library(survey)
library(srvyr)
library(tidyverse)
library(lubridate)

### Indica diretório que vai trabalhar da base de dados
setwd('D:/dissertacao/dados/descompactados')

dir() #verificando se o que utilizaremso está no diretório

df_scr <- data.table::fread("D:/dissertacao/dados/df_scr_agrupado_2.csv", dec='.', encoding = 'Latin-1')

############# MERGE COM OUTRAS BASES #####

#adicionando a base da selic realizada( QUE É A QUE SERÁ UTILIZADA), que é a selic acumulada ao mes
selic_realizada<-read.csv2("selic_realizada.csv")

lubridate::as_date(selic_realizada$data_base)
selic_realizada$data_base = dmy(selic_realizada$data_base)

df_scr <- merge(df_scr,selic_realizada,by='data_base' ,all.x=TRUE)

#adicionando a selic COPOM na base para testes futuros de robustez

# selic_copom
selic_copom<-read.csv2("selic_copom.csv")

selic_copom <- rename(selic_copom, data_base = ï..data_base)

lubridate::as_date(selic_copom$data_base)

selic_copom$data_base =dmy(selic_copom$data_base)

## ADICIONANDO a série de atividade econômica,IBC-M##

# a key será entre selic e ibc-m e depois colocar com o scr

ibc_br <- read.csv2("ibc_br.csv")

ibc_br <- rename(ibc_br, data_base = Data)

lubridate::as_date(ibc_br$data_base)

ibc_br$data_base = dmy(ibc_br$data_base)

#JUNTANDO AS BASES em 2 estágios, para poder verificar depois se está batendo os valores

SELIC_IBC_BR <- merge(selic_copom,ibc_br,by='data_base' ,all.x=TRUE)

### juntando com a base SCR##
df_scr <- merge(df_scr,SELIC_IBC_BR,by='data_base' ,all.x=TRUE)

#salvando a base

# write.table(df_scr, file = "D:/dissertacao/dados/df_scr_final.csv", row.names = FALSE)




