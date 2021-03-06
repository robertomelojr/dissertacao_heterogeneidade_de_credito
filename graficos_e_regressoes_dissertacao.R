## READ.ME:
#Nesse script ser�o realizados os filtros, cria��o de gr�ficos, tabelas e regress�es que ser�o utilizadas na an�lise


# limpando instancia 
rm(list = ls())
gc()
#importando pacotes
library(readr)
library(dplyr)
library(data.table)
library(psych)
library(bit64)
library(survey)
library(srvyr)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(hrbrthemes)
library(ggplot2)
library(plotly)
library(forecast)
library(tsibble)


#indicando o diret�rio
setwd('D:/roberto_jr_pasta/dissertacao_heterogeneidade_de_credito-main')
dir() #vendo o que tem no diret�rio, para conferir se a base est� l� 

df_scr<- data.table::fread("df_scr_final.csv") #base utilizada na presente an�lise

### Filtros e vari�veis criadas
{
## como a an�lise � para empresas, n�o faz sentido termos pessoa f�sica na base, ent�o iremos retira-las da nossa base

df_scr_sem_pf<- df_scr%>%
  filter(cliente !="PF")
#verificando se apenas os clientes pessoa jur�dica est�o na base

table(df_scr$cliente,exclude = NULL)
# certo, temos apenas pessoa jur�dicam vamos prosseguir
# criando uma dummie para Mar�o de 2020
# 1 para depois e 0 para antes 
df_scr_sem_pf$crise_marco=NA
df_scr_sem_pf$crise_marco = ifelse(df_scr_sem_pf$data_base >= "2020-02-29", 1, 0)

#Agora criaremos as primeira dummies, sendo elas para identificar a modalidade da empresa, porte, tipo de origem e etc,pois trabalhar com vari�veis str �
#muito suscet�vel ao erro

### dummies para o mercado de cr�dito####


## por linha de cr�dito

df_scr_sem_pf$tipos_modalidade = NA
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Capital de giro" ] = 1 
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Capital de giro rotativo" ] = 2
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Com�rcio exterior" ] = 3
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Financiamento de infraestrutura/desenvolvimento/projeto e outros cr�ditos" ] = 4
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Habitacional" ] = 5
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Investimento" ] = 6
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Opera��es com receb�veis" ] = 7
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Outros cr�ditos" ] = 8
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Rural e agroindustrial" ] = 9

#verificando se est� correta a distribui��o
table(df_scr_sem_pf$tipos_modalidade,df_scr_sem_pf$modalidade,exclude = NULL) #ok, vamos  prosseguir

#tipos de origem dos financiamento#  # 1 para com destina��o e 0 sem
df_scr_sem_pf$tipo_origem=NA
df_scr_sem_pf$tipo_origem = ifelse(df_scr_sem_pf$origem =="Com destina��o espec�fica", 1, 0)

table(df_scr_sem_pf$tipo_origem,df_scr_sem_pf$origem,exclude = NULL) #verificando

#por porte de maneira multinomial (1,2,3...)

df_scr_sem_pf$tipo_porte =NA
df_scr_sem_pf$tipo_porte[df_scr_sem_pf$porte=="PJ - Grande" ] = 1 
df_scr_sem_pf$tipo_porte[df_scr_sem_pf$porte=="PJ - Indispon�vel" ] = 2
df_scr_sem_pf$tipo_porte[df_scr_sem_pf$porte=="PJ - M�dio" ] = 3
df_scr_sem_pf$tipo_porte[df_scr_sem_pf$porte=="PJ - Micro" ] = 4
df_scr_sem_pf$tipo_porte[df_scr_sem_pf$porte=="PJ - Pequeno" ] = 5


table(df_scr_sem_pf$tipo_porte,df_scr_sem_pf$porte,exclude = NULL) #verificando

#tirando as pjs indispon�veis, pois elas n�o interessam apra a an�lise 
df_scr_sem_pf<-df_scr_sem_pf%>%
  filter(tipo_porte!=2)


###CRIANDO AS AGREGA��ES de tempo do emprestimo##
## O SOMAT�RIO DOS TEMPOS DOS EMPR�STIMO � IGUAL A CARTEIRA ATIVA !!!!!!!!!!!!!
## fazendo uma vari�vel do valor e outra da propor��o de quanto cada um desses pesa na carteira ativa total

#curto prazo at� um ano

df_scr_sem_pf$curto_prazo <- df_scr_sem_pf$a_vencer_ate_90_dias + df_scr_sem_pf$a_vencer_de_91_ate_360_dias

df_scr_sem_pf$prop_curto_prazo <- df_scr_sem_pf$curto_prazo/(df_scr_sem_pf$carteira_ativa)

#medio prazo 1 a 3 anos
df_scr_sem_pf$medio_prazo <- df_scr_sem_pf$a_vencer_de_361_ate_1080_dias

df_scr_sem_pf$prop_medio_prazo <- df_scr_sem_pf$medio_prazo/(df_scr_sem_pf$carteira_ativa)

#longo prazo acima de 3 anos

df_scr_sem_pf$longo_prazo <- df_scr_sem_pf$a_vencer_de_1081_ate_1800_dias + df_scr_sem_pf$a_vencer_de_1801_ate_5400_dias + df_scr_sem_pf$a_vencer_acima_de_5400_dias

df_scr_sem_pf$prop_longo_prazo <- df_scr_sem_pf$longo_prazo/(df_scr_sem_pf$carteira_ativa)

####Dummies para pj #######
#micro e pequena

df_scr_sem_pf$grande_x_micro_e_pequeno<-ifelse(df_scr_sem_pf$tipo_porte>=4, 1,ifelse(df_scr_sem_pf$tipo_porte == 1, 0, NA))
table(df_scr_sem_pf$porte,df_scr_sem_pf$grande_x_micro_e_pequeno,exclude = NULL) #verificando
#grande x micro
df_scr_sem_pf$grande_x_micro<-ifelse(df_scr_sem_pf$tipo_porte==4, 1,ifelse(df_scr_sem_pf$tipo_porte == 1, 0, NA))
table(df_scr_sem_pf$porte,df_scr_sem_pf$grande_x_micro,exclude = NULL) #verificando

#medioxgrande

df_scr_sem_pf$grande_x_medio<-ifelse(df_scr_sem_pf$tipo_porte==3, 1,ifelse(df_scr_sem_pf$tipo_porte == 1, 0, NA))
table(df_scr_sem_pf$porte,df_scr_sem_pf$grande_x_medio,exclude = NULL) #verificando

#### LOG DA CARTEIRA ATIVA, ATIVO PROBLEMATICO E CARTEIRA ATIVA######## 

df_scr<- mutate(df_scr, ln_carteira_ativa = log(carteira_ativa))

df_scr_sem_pf$ln_carteira_ativa <- log(df_scr_sem_pf$carteira_ativa)
df_scr_sem_pf$ln_ativo_problematico <-log(df_scr_sem_pf$ativo_problematico)
df_scr_sem_pf$ln_carteira_inadimplida_arrastada <-log(df_scr_sem_pf$carteira_inadimplida_arrastada)

## LOG DO IBCBR##
df_scr_sem_pf$ln_ibc_br <- log(df_scr_sem_pf$IBC_Br_indice)

#CRIANDO OS FILTROS QUE UTILIZAREI NOS GR�FICOS#####
                                                          ####  HABITACIONAL COM DESTINA��O ###
df_habitacional_com_destinacao <- df_scr_sem_pf %>%
  group_by(porte,data_base,tipo_porte) %>%
  filter(tipos_modalidade==5,tipo_origem==1,tipo_porte!=2) %>%
  summarise(Total_carteira = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))


#                                                           ### RURAL COM DESTINA��O##


df_rural_com_destinacao <- df_scr_sem_pf %>%
  group_by(porte,data_base) %>%
  filter(tipos_modalidade==9,tipo_origem==1,tipo_porte!=2) %>%
  summarise(Total_carteira = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))


                                                                      ##CAPITAL DE GIRO
df_capital_sem_destinacao <- df_scr_sem_pf %>%
  group_by(porte,data_base) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte!=2) %>%
  summarise(Total_carteira = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))
}
###### GR�FICOS#######
{
  
  ####gr�fico da propor��o da distribui��o dos cr�ditos ao longo da distribui��o das empresas - Um gr�fico de barras agregado ao longo dos anos POR ANO ####
  
  # rela��o entre o porte e a carteira ativa
  
  # Rural x habitacional destina��o espec�fica 
  seq_datas <- seq.Date(as.Date('2012-06-30'),
                        as.Date('2021-06-30'),
                        by = '2 years')
  ####  HABITACIONAL COM DESTINA��O #####
  
  
  #plotando O gr�fico de carteira ativa em habitacional com destina��o 
  
  g_habitacional <- ggplot(df_habitacional_com_destinacao)
  
  # Adicionar pontos (geom_point) mapeando vari�veis a elementos est�ticos dos pontos
  # Size = 3 define o tamanho de todos os pontos
  g_habitacional <- g_habitacional +
    geom_line(aes(x = data_base,
                  y = (Total_carteira/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa para cr�dito habitacional com destina��o espec�fica',
         y = 'Carteira Ativa em bilh�es',
         x = 'Per�odo')
  
  g_habitacional
  #plotando O gr�fico de inadimplecia em habitacional com destina��o 
  
  g_habitacional_inadimplencia <- ggplot(df_habitacional_com_destinacao)
  
  # Adicionar pontos (geom_point) mapeando vari�veis a elementos est�ticos dos pontos
  # Size = 3 define o tamanho de todos os pontos
  g_habitacional_inadimplencia <- g_habitacional_inadimplencia +
    geom_line(aes(x = data_base,
                  y = (total_carteira_inadimplida_arrastada/1000000000),
                  color = porte),
              size = 0.8)+
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a inadimpl�ncia arrastada para cr�dito habitacional com destina��o espec�fica',
         y = 'Inadimpl�ncia Arrastada em bilh�es',
         x = 'Per�odo')
  
  g_habitacional_inadimplencia
  # plotando os prazos###
  
  ## CURTO PRAZO##
  
  g_habitacional_curto_prazo <-  ggplot(df_habitacional_com_destinacao) +
    geom_line(aes(x = data_base,
                  y = (total_curto_prazo/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa de curto prazo para cr�dito habitacional com destina��o espec�fica',
         y = 'Carteira ativa de Curto prazo em bilh�es',
         x = 'Per�odo')
  
  g_habitacional_curto_prazo
  
  
  ## MEDIO PRAZO###
  g_habitacional_medio_prazo <-  ggplot(df_habitacional_com_destinacao) +
    geom_line(aes(x = data_base,
                  y = (total_medio_prazo/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa de medio prazo para cr�dito habitacional com destina��o espec�fica',
         y = 'Carteira ativa de M�dio prazo em bilh�es',
         x = 'Per�odo')
  
  g_habitacional_medio_prazo
  
  ## LONGO PRAZO###
  g_habitacional_longo_prazo <-  ggplot(df_habitacional_com_destinacao) +
    geom_line(aes(x = data_base,
                  y = (total_longo_prazo/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa de longo prazo para cr�dito habitacional com destina��o espec�fica',
         y = 'Carteira ativa de Longo prazo em bilh�es',
         x = 'Per�odo')
  
  g_habitacional_longo_prazo
  
  #prazos em um �nico gr�fico
  
  fig_habitacional_prazo <-subplot(style(g_habitacional_curto_prazo,showlegend = FALSE),style(g_habitacional_medio_prazo,showlegend = FALSE),
                                   style(g_habitacional_longo_prazo,showlegend = FALSE), nrows = 3,titleY = TRUE)%>%
    layout(title= list(text = "Rela��o entre o porte e a carteira ativa para cr�dito habitacional a vencer ao longo dos anos"))
  
  fig_habitacional_prazo
  
  
  # plotando ambos para o comparativo em subplots
  
  
  
  #### RURAL COM DESTINA��O####
  
  
  
  #plotando O gr�fico PPARA CARTEIRA ATIVA NO RURAL COM DESTINA��O
  g_rural <-  ggplot(df_rural_com_destinacao)+
    geom_line(aes(x = data_base,
                  y = (Total_carteira/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa para cr�dito rural com destina��o espec�fica',
         y = 'Carteira Ativa em bilh�es',
         x = 'Per�odo')
  
  
  g_rural
  #plotando O gr�fico PARA CARTEIRA INACIMPLINDA NO RURAL COM DESTINA��O
  
  g_rural_inadimplencia <- ggplot(df_rural_com_destinacao)
  
  # Adicionar pontos (geom_point) mapeando vari�veis a elementos est�ticos dos pontos
  # Size = 3 define o tamanho de todos os pontos
  g_rural_inadimplencia <- g_rural_inadimplencia +
    geom_line(aes(x = data_base,
                  y = (total_carteira_inadimplida_arrastada/1000000000),
                  color = porte),
              size = 0.8)  +
    scale_x_date(breaks = seq_datas, date_labels = "%Y") +
    labs(title = 'Rela��o entre o porte e a inadimpl�ncia arrastada para cr�dito rural com destina��o espec�fica',
         y = 'Inadimpl�ncia Arrastada em bilh�es',
         x = 'Per�odo')
  
  g_rural_inadimplencia
  
  
  # plotando os prazos###
  
  ## CURTO PRAZO##
  
  g_rural_curto_prazo <-  ggplot(df_rural_com_destinacao) +
    geom_line(aes(x = data_base,
                  y = (total_curto_prazo/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa de curto prazo para cr�dito rural com destina��o espec�fica',
         y = 'Carteira ativa de Curto prazo em bilh�es',
         x = 'Per�odo')
  
  g_rural_curto_prazo
  
  
  ## MEDIO PRAZO###
  g_rural_medio_prazo <-  ggplot(df_rural_com_destinacao) +
    geom_line(aes(x = data_base,
                  y = (total_medio_prazo/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa de medio prazo para cr�dito rural com destina��o espec�fica',
         y = 'Carteira ativa de M�dio prazo em bilh�es',
         x = 'Per�odo')
  
  g_rural_medio_prazo
  
  ## LONGO PRAZO###
  g_rural_longo_prazo <-  ggplot(df_rural_com_destinacao) +
    geom_line(aes(x = data_base,
                  y = (total_longo_prazo/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa de longo prazo para cr�dito rural com destina��o espec�fica',
         y = 'Carteira ativa de Longo prazo em bilh�es',
         x = 'Per�odo')
  
  g_rural_longo_prazo
  #prazos em um �nico gr�fico
  
  fig_rural_prazo <-subplot(style(g_rural_curto_prazo,showlegend = FALSE),style(g_rural_medio_prazo,showlegend = FALSE),
                            style(g_rural_longo_prazo,showlegend = FALSE), nrows = 3,titleY = TRUE)%>%
    layout(title= list(text = "Rela��o entre o porte e a carteira ativa para cr�dito rural a vencer ao longo dos anos"))
  
  
  fig_rural_prazo
  
  ##### Capital de  Giro#### 
  
  
  #plotando O gr�fico para a carteira ativa no capital de giro
  
  g_capital_de_giro <- ggplot(df_capital_sem_destinacao)
  
  # Adicionar pontos (geom_point) mapeando vari�veis a elementos est�ticos dos pontos
  # Size = 3 define o tamanho de todos os pontos
  g_capital_de_giro <- g_capital_de_giro +
    geom_line(aes(x = data_base,
                  y = (Total_carteira/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y") +
    labs(title = 'Rela��o entre o porte e a carteira ativa para Capital de Giro sem destina��o espec�fica',
         y = 'Carteira Ativa eme bilh�es',
         x = 'Per�odo')
  
  
  g_capital_de_giro
  
  
  #plotando O gr�fico para a inadimplencia no capital de giro
  
  g_capital_de_giro_inadimplencia <- ggplot(df_capital_sem_destinacao)
  
  # Adicionar pontos (geom_point) mapeando vari�veis a elementos est�ticos dos pontos
  # Size = 3 define o tamanho de todos os pontos
  
  g_capital_de_giro_inadimplencia <- ggplot(df_capital_sem_destinacao) +
    geom_line(aes(x = data_base,
                  y = (total_carteira_inadimplida_arrastada/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a Inadimpl�ncia arrastada para Capital de Giro sem destina��o espec�fica',
         y = 'Inadimpl�ncia arrastada em bilh�es',
         x = 'Per�odo')
  
  
  g_capital_de_giro_inadimplencia
  
  
  # plotando os prazos###
  
  ## CURTO PRAZO##
  
  g_capital_curto_prazo <-  ggplot(df_capital_sem_destinacao) +
    geom_line(aes(x = data_base,
                  y = (total_curto_prazo/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa de curto prazo para capital de giro',
         y = 'Curto prazo em bilh�es',
         x = 'Per�odo',color='Portes')
  
  g_capital_curto_prazo
  
  
  ## MEDIO PRAZO###
  g_capital_medio_prazo <-  ggplot(df_capital_sem_destinacao) +
    geom_line(aes(x = data_base,
                  y = (total_medio_prazo/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa de medio prazo para capital de giro',
         y = 'M�dio prazo em bilh�es',
         x = 'Per�odo',color='Portes')
  
  g_capital_medio_prazo
  
  ## LONGO PRAZO###
  g_capital_longo_prazo <-  ggplot(df_capital_sem_destinacao) +
    geom_line(aes(x = data_base,
                  y = (total_longo_prazo/1000000000),
                  color = porte),
              size = 0.8) +
    scale_x_date(breaks = seq_datas, date_labels = "%Y")+
    labs(title = 'Rela��o entre o porte e a carteira ativa de longo prazo para capital de giro',
         y = 'Longo prazo em bilh�es',
         x = 'Per�odo',color='Portes')
  g_capital_longo_prazo
  #prazos em um �nico gr�fico
  
  fig_capital_de_giro_prazo <-subplot(style(g_capital_curto_prazo,showlegend = FALSE),style(g_capital_medio_prazo,showlegend = FALSE),
                                      style(g_capital_longo_prazo,showlegend = FALSE), nrows = 3,titleY = TRUE,shareX = TRUE)%>%
    layout(title= "Rela��o entre o porte e a carteira ativa para capital de giro a vencer ao longo dos anos")
  
  annotations = list( 
    list( 
      x = 0.5,  
      y = 1.0,  
      text = "Rela��o entre o porte e a carteira ativa de curto prazo para capital de giro",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),list( 
      x = 0.5,  
      y = 0.646,  
      text = "Rela��o entre o porte e a carteira ativa de m�dio prazo para capital de giro",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE),list( 
        x = 0.5,  
        y = 0.312,  
        text = "Rela��o entre o porte e a carteira ativa de longo prazo para capital de giro",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE))
  
  fig_capital_de_giro_prazo<- fig_capital_de_giro_prazo %>% layout(annotations = annotations)
  
  
  fig_capital_de_giro_prazo
  
  #gr�fico com habitacional X rural x capital de giro
  
  fig_creditos <-subplot(style(g_habitacional,showlegend = FALSE),style(g_rural,showlegend = TRUE),
                         style(g_capital_de_giro,showlegend = TRUE), nrows = 3,titleY = TRUE,shareX = TRUE, shareY = TRUE)%>%
    layout(title= list(text = "Rela��o entre o porte e a carteira ativa para habitacional X rural x capital de giro a vencer ao longo dos anos"))
  fig_creditos
  
  
}

### Tabelas e estat�sticas descritivas

### em desenvolvimento ainda!
{
  ##### PARTE INICIAL, CRIA��O DE BASES E FILTROS ####
  ## carteira ativa total ##
  
  
#  divididos pelo describeBy
  
  
  df_carteira_ativa_total<- df_scr_sem_pf %>% #OBS APENAS AP�S MARCO#
    group_by(data_base,crise_marco) %>%
    filter(tipo_porte!=2) %>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo))%>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)
  
 carteira_ativa_total_describe = describeBy(df_carteira_ativa_total$carteira_ativa_bilhao, group = df_carteira_ativa_total$crise_marco)


  
  ### Criando manualmente para checar!!! 
  
  #Carteira ativa total micro pre e pos crise
  
  df_carteira_ativa_micro <-df_scr_sem_pf %>% 
    group_by(data_base,crise_marco) %>%
    filter(tipo_porte==4) %>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo))%>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)
  
  describeBy(df_carteira_ativa_micro$carteira_ativa_bilhao, group = df_carteira_ativa_micro$crise_marco)
  
  #Carteira ativa total media pre e poscrise
  
  df_carteira_ativa_pequena <- df_scr_sem_pf %>% 
    group_by(data_base,crise_marco) %>%
    filter(tipo_porte==5) %>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo))%>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 
  
  describeBy(df_carteira_ativa_pequena$carteira_ativa_bilhao, group = df_carteira_ativa_pequena$crise_marco)
  
  #Carteira ativa total media pre e poscrise
  
  df_carteira_ativa_media <- df_scr_sem_pf %>% 
    group_by(data_base,crise_marco) %>%
    filter(tipo_porte==3) %>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo))%>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 
  
  describeBy(df_carteira_ativa_media$carteira_ativa_bilhao, group = df_carteira_ativa_media$crise_marco)
  
  
  #Carteira ativa total grande pre e pos crise
  
  df_carteira_ativa_grande <- df_scr_sem_pf %>% 
    group_by(data_base,crise_marco) %>%
    filter(tipo_porte==1) %>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo))%>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 
  
  describeBy(df_carteira_ativa_grande$carteira_ativa_bilhao, group = df_carteira_ativa_grande$crise_marco)
  
 
  
  
                                    ##CAPITAL DE GIRO
  df_capital_de_giro_total <- df_scr_sem_pf %>%
    group_by(data_base,crise_marco) %>%
    filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte!=2)%>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo)) %>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)
  
  
  capital_de_giro_describe= describeBy(df_capital_de_giro_total$carteira_ativa_bilhao, group = df_capital_de_giro_total$crise_marco)
  
  
  #carteira ativa #CAPITAL DE GIRO para micro 
  
  df_capital_de_giro_micro <- df_scr_sem_pf %>%
    group_by(data_base,crise_marco) %>%
    filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte==4)%>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo)) %>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

  describeBy(df_capital_de_giro_micro$carteira_ativa_bilhao, group = df_capital_de_giro_micro$crise_marco)
  
  
  #carteira ativa #CAPITAL DE GIRO para pequenas 
  
  df_capital_de_giro_pequena <- df_scr_sem_pf %>%
    group_by(data_base,crise_marco) %>%
    filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte==5)%>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo)) %>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)
  
  describeBy(df_capital_de_giro_pequena$carteira_ativa_bilhao, group = df_capital_de_giro_pequena$crise_marco)
  
  
  #carteira ativa para medias # CAPITAL DE GIRO 
  df_capital_de_giro_media <- df_scr_sem_pf %>%
    group_by(data_base,crise_marco) %>%
    filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte==3)%>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo)) %>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 
  
  describeBy(df_capital_de_giro_media$carteira_ativa_bilhao, group = df_capital_de_giro_media$crise_marco)
  
  
  #carteira ativa para grande # CAPITAL DE GIRO 
  df_capital_de_giro_grande <- df_scr_sem_pf %>%
    group_by(data_base,crise_marco) %>%
    filter(tipos_modalidade==1|tipos_modalidade==2,tipo_porte==1)%>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo)) %>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)  

  describeBy(df_capital_de_giro_grande$carteira_ativa_bilhao, group = df_capital_de_giro_grande$crise_marco)
  
 
  #### AGORA AS TABELAS DAS ESTAT�STICAS ####
## aqui est� o que realmente importa!!!
  
  ## Tabela 1 - Estat�sticas descritivas antes e ap�s a crise do COVID-19 dos subgrupos 
  #carteira_ativa
  
  df_carteira_ativa_subgrupos<- df_scr_sem_pf %>% #OBS APENAS AP�S MARCO#
    group_by(data_base,crise_marco,tipo_porte,porte) %>%
    filter(tipo_porte!=2) %>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo))%>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)
  
  tabela_carteira_ativa_subgrupos <-  df_carteira_ativa_subgrupos %>%
    group_by(tipo_porte,porte,crise_marco)%>%
    summarise(carteira_ativa_Media = mean(carteira_ativa_bilhao),carteira_ativa_total_Sd = sd(carteira_ativa_bilhao) )

  #estatisticas da carteira ativa total
    carteira_ativa_total_describe
  tabela_carteira_ativa_subgrupos
  
  ##capital de giro
  
  df_capital_de_giro_subgrupos <- df_scr_sem_pf %>%
    group_by(data_base,crise_marco,porte,tipo_porte) %>%
    filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte!=2)%>%
    summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
              total_longo_prazo=sum(longo_prazo)) %>%
    mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)
  
  tabela_capital_de_giro_subgrupos <-  df_capital_de_giro_subgrupos %>%
    group_by(tipo_porte,porte,crise_marco)%>%
    summarise(carteira_ativa_Media = mean(carteira_ativa_bilhao),carteira_ativa_total_Sd = sd(carteira_ativa_bilhao) )
  
  
capital_de_giro_describe  
tabela_capital_de_giro_subgrupos
  
  
  
  
  
}


## REGRESS�ES ###
{
  ############ rodando as regress�es#
  
  #diferen�a do ibc br
  #diferen�a do ibc br
  df_scr_sem_pf <- df_scr_sem_pf %>%
    as_tsibble(key = c("porte", "modalidade"), index = data_base)
  
  df_scr_sem_pf <- df_scr_sem_pf %>% mutate(diff_ln_ibc = difference(ln_ibc_br))
  
  
  df_scr_sem_pf <- df_scr_sem_pf %>% mutate(inad = carteira_inadimplida_arrastada / carteira_ativa)
  
  df_scr_sem_pf <- df_scr_sem_pf %>% mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)
  
  #testando carteira ativa em n�vel e em ln e grandeXmicro e pequena
  #micro e pequena X  grande
  
  summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_micro_e_pequeno + crise_marco * grande_x_micro_e_pequeno + Taxa_de_juros_Selic_acumulada_no_mes +ln_ibc_br, df_scr_sem_pf))
  
  summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_micro_e_pequeno + crise_marco * grande_x_micro_e_pequeno + Taxa_de_juros_Selic_acumulada_no_mes +diff_ln_ibc, df_scr_sem_pf))
  
  #pequena x grande
  summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_micro + crise_marco * grande_x_micro + Taxa_de_juros_Selic_acumulada_no_mes +ln_ibc_br, df_scr_sem_pf))
  
  summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_micro + crise_marco * grande_x_micro + Taxa_de_juros_Selic_acumulada_no_mes +diff_ln_ibc, df_scr_sem_pf))
  
  #media x grande 
  summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_medio + crise_marco * grande_x_medio + Taxa_de_juros_Selic_acumulada_no_mes +ln_ibc_br, df_scr_sem_pf))
  
  summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_medio + crise_marco * grande_x_medio + Taxa_de_juros_Selic_acumulada_no_mes +diff_ln_ibc, df_scr_sem_pf))
  
  
  
  
  ### tabela das regress�es
  
  ## Tabela 2 - Efeito da pandemia no mercado de cr�dito- Efeitos heterog�neos

  
}
























