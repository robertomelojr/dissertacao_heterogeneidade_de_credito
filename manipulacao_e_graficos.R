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
library(ggthemes)
library(hrbrthemes)
library(ggplot2)
library(plotly)
library(forecast)
library(tsibble)
### Indica diretório que vai trabalhar da base de dados
#setwd('D:/dissertacao/dados')
#dir()

#df_scr<- data.table::fread("df_scr_ibcbr_selic.csv", dec='.', encoding = 'Latin-1')

setwd('D:/dissertacao/dados/descompactados')
dir()

dt_scr_base<- data.table::fread("D:/dissertacao/dados/df_scr_agrupado_2.csv", dec='.', encoding = 'Latin-1')


############# MERGE COM OUTRAS BASES #####

#adicionando a selic na base

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

#JUNTANDO AS BASES

SELIC_IBC_BR <- merge(selic_copom,ibc_br,by='data_base' ,all.x=TRUE)


describe(SELIC_IBC_BR$IBC_Br_indice)

describe(ibc_br$IBC_Br_indice)
### juntando com a base SCR##
df_scr <- merge(dt_scr_base,SELIC_IBC_BR,by='data_base' ,all.x=TRUE)

#adicionando a base da selic realizada, que é aselic acumulada ao mes
selic_realizada<-read.csv2("selic_realizada.csv")

lubridate::as_date(selic_realizada$data_base)
selic_realizada$data_base = dmy(selic_realizada$data_base)

df_scr <- merge(df_scr,selic_realizada,by='data_base' ,all.x=TRUE)



#como utilizaremos pfs para robustez vamos iniciar dropando as pfs

############################# criando uma dummie para 1 se pf e 0 se não#################

df_scr$filtro=NA
df_scr$filtro = ifelse(df_scr$cliente =="PF", 1, 0)

# novo df apenas com PJ
df_scr_sem_pf= subset(df_scr,filtro!=1)
strptime(df_scr_sem_pf$data_base,format="%Y-%m-%d")


#######criando as dummies#########

#dummies para cada tipo de crédito de maneira multinomial (1,2,3...)
### primeiro criando por linhas de crédito##
### primeiro criando por linhas de crédito##
df_scr_sem_pf$tipos_modalidade = NA
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Capital de giro" ] = 1 
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Capital de giro rotativo" ] = 2
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Comércio exterior" ] = 3
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Financiamento de infraestrutura/desenvolvimento/projeto e outros créditos" ] = 4
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Habitacional" ] = 5
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Investimento" ] = 6
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Operações com recebíveis" ] = 7
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Outros créditos" ] = 8
df_scr_sem_pf$tipos_modalidade[df_scr_sem_pf$modalidade=="PJ - Rural e agroindustrial" ] = 9

#verificando se está certa a distribuição
table(df_scr_sem_pf$tipos_modalidade,df_scr_sem_pf$modalidade,exclude = NULL) 

#tipos de origem dos financiamento#  # 1 para com destinação e 0 sem
df_scr_sem_pf$tipo_origem=NA
df_scr_sem_pf$tipo_origem = ifelse(df_scr_sem_pf$origem =="Com destinação específica", 1, 0)

table(df_scr_sem_pf$tipo_origem,df_scr_sem_pf$origem,exclude = NULL) #verificando

#por porte de maneira multinomial (1,2,3...)

df_scr_sem_pf$tipo_porte =NA
df_scr_sem_pf$tipo_porte[df_scr_sem_pf$porte=="PJ - Grande" ] = 1 
df_scr_sem_pf$tipo_porte[df_scr_sem_pf$porte=="PJ - Indisponível" ] = 2
df_scr_sem_pf$tipo_porte[df_scr_sem_pf$porte=="PJ - Médio" ] = 3
df_scr_sem_pf$tipo_porte[df_scr_sem_pf$porte=="PJ - Micro" ] = 4
df_scr_sem_pf$tipo_porte[df_scr_sem_pf$porte=="PJ - Pequeno" ] = 5

table(df_scr_sem_pf$tipo_porte,df_scr_sem_pf$porte,exclude = NULL) #verificando

#tirando as pjs indisponíveis
df_scr_sem_pf<-df_scr_sem_pf%>%
  filter(tipo_porte!=2)

#ALGUMMAS DUMMIES PARA PJS
#micro e pequena

df_scr_sem_pf$grande_x_micro_e_pequeno<-ifelse(df_scr_sem_pf$tipo_porte>=4, 1,ifelse(df_scr_sem_pf$tipo_porte == 1, 0, NA))
table(df_scr_sem_pf$porte,df_scr_sem_pf$grande_x_micro_e_pequeno,exclude = NULL)
#grande x micro
df_scr_sem_pf$grande_x_micro<-ifelse(df_scr_sem_pf$tipo_porte==4, 1,ifelse(df_scr_sem_pf$tipo_porte == 1, 0, NA))
table(df_scr_sem_pf$porte,df_scr_sem_pf$grande_x_micro,exclude = NULL)

#medioxgrande

df_scr_sem_pf$grande_x_medio<-ifelse(df_scr_sem_pf$tipo_porte==3, 1,ifelse(df_scr_sem_pf$tipo_porte == 1, 0, NA))
table(df_scr_sem_pf$porte,df_scr_sem_pf$grande_x_medio,exclude = NULL)



#ALGUMMAS DUMMIES PARA PJS



# criando uma dummie para Março de 2020
# 1 para depois e 0 para antes 
df_scr_sem_pf$crise_marco=NA
df_scr_sem_pf$crise_marco = ifelse(df_scr_sem_pf$data_base >= "2020-02-29", 1, 0)


###CRIANDO AS AGREGAÇÕES de tempo do emprestimo######
## O SOMATÓRIO DOS TEMPOS DOS EMPRÉSTIMO É IGUAL A CARTEIRA ATIVA !!!!!!!!!!!!!
## fazendo uma variável do valor e outra da proporção de quanto cada um desses pesa na carteira ativa total


#curto prazo até um ano

df_scr_sem_pf$curto_prazo <- df_scr_sem_pf$a_vencer_ate_90_dias + df_scr_sem_pf$a_vencer_de_91_ate_360_dias

df_scr_sem_pf$prop_curto_prazo <- df_scr_sem_pf$curto_prazo/(df_scr_sem_pf$carteira_ativa)

#medio prazo 1 a 3 anos
df_scr_sem_pf$medio_prazo <- df_scr_sem_pf$a_vencer_de_361_ate_1080_dias

df_scr_sem_pf$prop_medio_prazo <- df_scr_sem_pf$medio_prazo/(df_scr_sem_pf$carteira_ativa)

#longo prazo acima de 3 anos

df_scr_sem_pf$longo_prazo <- df_scr_sem_pf$a_vencer_de_1081_ate_1800_dias + df_scr_sem_pf$a_vencer_de_1801_ate_5400_dias + df_scr_sem_pf$a_vencer_acima_de_5400_dias

df_scr_sem_pf$prop_longo_prazo <- df_scr_sem_pf$longo_prazo/(df_scr_sem_pf$carteira_ativa)

            


#### LOG DA CARTEIRA ATIVA, ATIVO PROBLEMATICO E CARTEIRA ATIVA######## 

df_scr<- mutate(df_scr, ln_carteira_ativa = log(carteira_ativa))

df_scr_sem_pf$ln_carteira_ativa <- log(df_scr_sem_pf$carteira_ativa)
df_scr_sem_pf$ln_ativo_problematico <-log(df_scr_sem_pf$ativo_problematico)
df_scr_sem_pf$ln_carteira_inadimplida_arrastada <-log(df_scr_sem_pf$carteira_inadimplida_arrastada)


## LOG DO IBCBR##
df_scr_sem_pf$ln_ibc_br <- log(df_scr_sem_pf$IBC_Br_indice)

summary(df_scr_sem_pf$ln_ibc_br)





#                                            #CRIANDO OS FILTROS QUE UTILIZAREI NOS GRÁFICOS#####
####  HABITACIONAL COM DESTINAÇÃO ###
df_habitacional_com_destinacao <- df_scr_sem_pf %>%
  group_by(porte,data_base,tipo_porte) %>%
  filter(tipos_modalidade==5,tipo_origem==1,tipo_porte!=2) %>%
  summarise(Total_carteira = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))


#                                                           ### RURAL COM DESTINAÇÃO##


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
###### GRÁFICOS#######
{

####gráfico da proporção da distribuição dos créditos ao longo da distribuição das empresas - Um gráfico de barras agregado ao longo dos anos POR ANO ####

# relação entre o porte e a carteira ativa

# Rural x habitacional destinação específica 
  seq_datas <- seq.Date(as.Date('2012-06-30'),
                        as.Date('2021-06-30'),
                        by = '2 years')
                                                      ####  HABITACIONAL COM DESTINAÇÃO #####


                                       #plotando O gráfico de carteira ativa em habitacional com destinação 

g_habitacional <- ggplot(df_habitacional_com_destinacao)

# Adicionar pontos (geom_point) mapeando variáveis a elementos estéticos dos pontos
# Size = 3 define o tamanho de todos os pontos
g_habitacional <- g_habitacional +
  geom_line(aes(x = data_base,
                y = (Total_carteira/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa para crédito habitacional com destinação específica',
       y = 'Carteira Ativa em bilhões',
       x = 'Período')

g_habitacional
                                               #plotando O gráfico de inadimplecia em habitacional com destinação 

g_habitacional_inadimplencia <- ggplot(df_habitacional_com_destinacao)

# Adicionar pontos (geom_point) mapeando variáveis a elementos estéticos dos pontos
# Size = 3 define o tamanho de todos os pontos
g_habitacional_inadimplencia <- g_habitacional_inadimplencia +
  geom_line(aes(x = data_base,
                y = (total_carteira_inadimplida_arrastada/1000000000),
                color = porte),
            size = 0.8)+
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a inadimplência arrastada para crédito habitacional com destinação específica',
       y = 'Inadimplência Arrastada em bilhões',
       x = 'Período')

g_habitacional_inadimplencia
                                                # plotando os prazos###

               ## CURTO PRAZO##

g_habitacional_curto_prazo <-  ggplot(df_habitacional_com_destinacao) +
  geom_line(aes(x = data_base,
                y = (total_curto_prazo/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa de curto prazo para crédito habitacional com destinação específica',
       y = 'Carteira ativa de Curto prazo em bilhões',
       x = 'Período')

g_habitacional_curto_prazo


             ## MEDIO PRAZO###
g_habitacional_medio_prazo <-  ggplot(df_habitacional_com_destinacao) +
  geom_line(aes(x = data_base,
                y = (total_medio_prazo/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa de medio prazo para crédito habitacional com destinação específica',
       y = 'Carteira ativa de Médio prazo em bilhões',
       x = 'Período')

g_habitacional_medio_prazo

## LONGO PRAZO###
g_habitacional_longo_prazo <-  ggplot(df_habitacional_com_destinacao) +
  geom_line(aes(x = data_base,
                y = (total_longo_prazo/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa de longo prazo para crédito habitacional com destinação específica',
       y = 'Carteira ativa de Longo prazo em bilhões',
       x = 'Período')

g_habitacional_longo_prazo

#prazos em um único gráfico

fig_habitacional_prazo <-subplot(style(g_habitacional_curto_prazo,showlegend = FALSE),style(g_habitacional_medio_prazo,showlegend = FALSE),
                                    style(g_habitacional_longo_prazo,showlegend = FALSE), nrows = 3,titleY = TRUE)%>%
  layout(title= list(text = "Relação entre o porte e a carteira ativa para crédito habitacional a vencer ao longo dos anos"))

fig_habitacional_prazo


# plotando ambos para o comparativo em subplots



                                                   #### RURAL COM DESTINAÇÃO####



                              #plotando O gráfico PPARA CARTEIRA ATIVA NO RURAL COM DESTINAÇÃO
g_rural <-  ggplot(df_rural_com_destinacao)+
  geom_line(aes(x = data_base,
                y = (Total_carteira/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa para crédito rural com destinação específica',
       y = 'Carteira Ativa em bilhões',
       x = 'Período')


g_rural
                       #plotando O gráfico PARA CARTEIRA INACIMPLINDA NO RURAL COM DESTINAÇÃO

g_rural_inadimplencia <- ggplot(df_rural_com_destinacao)

# Adicionar pontos (geom_point) mapeando variáveis a elementos estéticos dos pontos
# Size = 3 define o tamanho de todos os pontos
g_rural_inadimplencia <- g_rural_inadimplencia +
  geom_line(aes(x = data_base,
                y = (total_carteira_inadimplida_arrastada/1000000000),
                color = porte),
            size = 0.8)  +
  scale_x_date(breaks = seq_datas, date_labels = "%Y") +
  labs(title = 'Relação entre o porte e a inadimplência arrastada para crédito rural com destinação específica',
       y = 'Inadimplência Arrastada em bilhões',
       x = 'Período')

g_rural_inadimplencia


                            # plotando os prazos###

## CURTO PRAZO##

g_rural_curto_prazo <-  ggplot(df_rural_com_destinacao) +
  geom_line(aes(x = data_base,
                y = (total_curto_prazo/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa de curto prazo para crédito rural com destinação específica',
       y = 'Carteira ativa de Curto prazo em bilhões',
       x = 'Período')

g_rural_curto_prazo


## MEDIO PRAZO###
g_rural_medio_prazo <-  ggplot(df_rural_com_destinacao) +
  geom_line(aes(x = data_base,
                y = (total_medio_prazo/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa de medio prazo para crédito rural com destinação específica',
       y = 'Carteira ativa de Médio prazo em bilhões',
       x = 'Período')

g_rural_medio_prazo

## LONGO PRAZO###
g_rural_longo_prazo <-  ggplot(df_rural_com_destinacao) +
  geom_line(aes(x = data_base,
                y = (total_longo_prazo/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa de longo prazo para crédito rural com destinação específica',
       y = 'Carteira ativa de Longo prazo em bilhões',
       x = 'Período')

g_rural_longo_prazo
#prazos em um único gráfico

fig_rural_prazo <-subplot(style(g_rural_curto_prazo,showlegend = FALSE),style(g_rural_medio_prazo,showlegend = FALSE),
                                    style(g_rural_longo_prazo,showlegend = FALSE), nrows = 3,titleY = TRUE)%>%
  layout(title= list(text = "Relação entre o porte e a carteira ativa para crédito rural a vencer ao longo dos anos"))


fig_rural_prazo

                              ##### Capital de  Giro#### 


                        #plotando O gráfico para a carteira ativa no capital de giro

g_capital_de_giro <- ggplot(df_capital_sem_destinacao)

# Adicionar pontos (geom_point) mapeando variáveis a elementos estéticos dos pontos
# Size = 3 define o tamanho de todos os pontos
g_capital_de_giro <- g_capital_de_giro +
  geom_line(aes(x = data_base,
                y = (Total_carteira/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y") +
  labs(title = 'Relação entre o porte e a carteira ativa para Capital de Giro sem destinação específica',
       y = 'Carteira Ativa eme bilhões',
       x = 'Período')


g_capital_de_giro


                     #plotando O gráfico para a inadimplencia no capital de giro

g_capital_de_giro_inadimplencia <- ggplot(df_capital_sem_destinacao)

# Adicionar pontos (geom_point) mapeando variáveis a elementos estéticos dos pontos
# Size = 3 define o tamanho de todos os pontos

g_capital_de_giro_inadimplencia <- ggplot(df_capital_sem_destinacao) +
  geom_line(aes(x = data_base,
                y = (total_carteira_inadimplida_arrastada/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a Inadimplência arrastada para Capital de Giro sem destinação específica',
       y = 'Inadimplência arrastada em bilhões',
       x = 'Período')


g_capital_de_giro_inadimplencia


                      # plotando os prazos###

## CURTO PRAZO##

g_capital_curto_prazo <-  ggplot(df_capital_sem_destinacao) +
  geom_line(aes(x = data_base,
                y = (total_curto_prazo/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa de curto prazo para capital de giro',
       y = 'Curto prazo em bilhões',
       x = 'Período',color='Portes')

g_capital_curto_prazo


## MEDIO PRAZO###
g_capital_medio_prazo <-  ggplot(df_capital_sem_destinacao) +
  geom_line(aes(x = data_base,
                y = (total_medio_prazo/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa de medio prazo para capital de giro',
       y = 'Médio prazo em bilhões',
       x = 'Período',color='Portes')

g_capital_medio_prazo

## LONGO PRAZO###
g_capital_longo_prazo <-  ggplot(df_capital_sem_destinacao) +
  geom_line(aes(x = data_base,
                y = (total_longo_prazo/1000000000),
                color = porte),
            size = 0.8) +
  scale_x_date(breaks = seq_datas, date_labels = "%Y")+
  labs(title = 'Relação entre o porte e a carteira ativa de longo prazo para capital de giro',
       y = 'Longo prazo em bilhões',
       x = 'Período',color='Portes')
g_capital_longo_prazo
                                     #prazos em um único gráfico

fig_capital_de_giro_prazo <-subplot(style(g_capital_curto_prazo,showlegend = FALSE),style(g_capital_medio_prazo,showlegend = FALSE),
                                    style(g_capital_longo_prazo,showlegend = FALSE), nrows = 3,titleY = TRUE,shareX = TRUE)%>%
  layout(title= "Relação entre o porte e a carteira ativa para capital de giro a vencer ao longo dos anos")

annotations = list( 
  list( 
    x = 0.5,  
    y = 1.0,  
    text = "Relação entre o porte e a carteira ativa de curto prazo para capital de giro",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),list( 
    x = 0.5,  
    y = 0.646,  
    text = "Relação entre o porte e a carteira ativa de médio prazo para capital de giro",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE),list( 
      x = 0.5,  
      y = 0.312,  
      text = "Relação entre o porte e a carteira ativa de longo prazo para capital de giro",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE))

fig_capital_de_giro_prazo<- fig_capital_de_giro_prazo %>% layout(annotations = annotations)


fig_capital_de_giro_prazo
          
#gráfico com habitacional X rural x capital de giro

fig_creditos <-subplot(style(g_habitacional,showlegend = FALSE),style(g_rural,showlegend = TRUE),
                       style(g_capital_de_giro,showlegend = TRUE), nrows = 3,titleY = TRUE,shareX = TRUE, shareY = TRUE)%>%
  layout(title= list(text = "Relação entre o porte e a carteira ativa para habitacional X rural x capital de giro a vencer ao longo dos anos"))
fig_creditos


}
#criando controles por crédito#####

#criando uma identificação para cada um 


############ rodando as regressões########

#diferença do ibc br
#diferença do ibc br
df_scr_sem_pf <- df_scr_sem_pf %>%
  as_tsibble(key = c("porte", "modalidade"), index = data_base)

df_scr_sem_pf <- df_scr_sem_pf %>% mutate(diff_ln_ibc = difference(ln_ibc_br))


df_scr_sem_pf <- df_scr_sem_pf %>% mutate(inad = carteira_inadimplida_arrastada / carteira_ativa)

df_scr_sem_pf <- df_scr_sem_pf %>% mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

#testando carteira ativa em nível e em ln e grandeXmicro e pequena
#micro e pequena X  grande

summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_micro_e_pequeno + crise_marco * grande_x_micro_e_pequeno + Taxa_de_juros_Selic_acumulada_no_mes +ln_ibc_br, df_scr_sem_pf))

summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_micro_e_pequeno + crise_marco * grande_x_micro_e_pequeno + Taxa_de_juros_Selic_acumulada_no_mes +diff_ln_ibc, df_scr_sem_pf))

#pequena x grande
summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_micro + crise_marco * grande_x_micro + Taxa_de_juros_Selic_acumulada_no_mes +ln_ibc_br, df_scr_sem_pf))

summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_micro + crise_marco * grande_x_micro + Taxa_de_juros_Selic_acumulada_no_mes +diff_ln_ibc, df_scr_sem_pf))

#media x grande 
summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_medio + crise_marco * grande_x_medio + Taxa_de_juros_Selic_acumulada_no_mes +ln_ibc_br, df_scr_sem_pf))

summary(lm(ln_carteira_ativa ~ crise_marco + grande_x_medio + crise_marco * grande_x_medio + Taxa_de_juros_Selic_acumulada_no_mes +diff_ln_ibc, df_scr_sem_pf))


        

# #########estatísticas descritivas##


#####  proporções pre e pos crise -#### 
                                                        
                   ## carteira ativa total ##

df_pos_marco<- df_scr_sem_pf %>% #OBS APENAS APÓS MARCO#
  group_by(data_base) %>%
  filter(tipo_porte!=2,crise_marco==1) %>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

df_pre_marco<- df_scr_sem_pf %>% #OBS APENAS PRE MARCO#
  group_by(data_base) %>%
  filter(tipo_porte!=2,crise_marco==0) %>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

mean(df_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
mean(df_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(df_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(df_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)


#Carteira ativa total micro pre e pos crise

carteira_ativa_micro_pre_marco <-df_scr_sem_pf %>% 
  group_by(data_base) %>%
  filter(crise_marco==0, tipo_porte==4) %>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

carteira_ativa_micro_pos_marco <- df_scr_sem_pf %>% 
  group_by(data_base) %>%
  filter(crise_marco==1, tipo_porte==4) %>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

mean(carteira_ativa_micro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
mean(carteira_ativa_micro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(carteira_ativa_micro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(carteira_ativa_micro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)


#Carteira ativa total media pre e poscrise

carteira_ativa_pequena_pre_marco <- df_scr_sem_pf %>% 
  group_by(porte,data_base) %>%
  filter(crise_marco==0, tipo_porte==5) %>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 

carteira_ativa_pequena_pos_marco <- df_scr_sem_pf %>% 
  group_by(porte,data_base) %>%
  filter(crise_marco==1, tipo_porte==5) %>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 

mean(carteira_ativa_pequena_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
mean(carteira_ativa_pequena_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(carteira_ativa_pequena_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(carteira_ativa_pequena_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)


#Carteira ativa total media pre e poscrise

carteira_ativa_media_pre_marco <- df_scr_sem_pf %>% 
  group_by(porte,data_base) %>%
  filter(crise_marco==0, tipo_porte==3) %>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 

carteira_ativa_media_pos_marco <- df_scr_sem_pf %>% 
  group_by(porte,data_base) %>%
  filter(crise_marco==1, tipo_porte==3) %>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 

mean(carteira_ativa_media_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
mean(carteira_ativa_media_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(carteira_ativa_media_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(carteira_ativa_media_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

#Carteira ativa total grande pre e pos crise

carteira_ativa_grande_pre_marco <- df_scr_sem_pf %>% 
  group_by(porte,data_base) %>%
  filter(tipo_porte==1,crise_marco==0) %>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 

carteira_ativa_grande_pos_marco <- df_scr_sem_pf %>% 
  group_by(porte,data_base) %>%
  filter(tipo_porte==1,crise_marco==1) %>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)  

mean(carteira_ativa_grande_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
mean(carteira_ativa_grande_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(carteira_ativa_grande_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(carteira_ativa_grande_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

                                                        ##CAPITAL DE GIRO
capital_de_giro_pre_marco <- df_scr_sem_pf %>%
  group_by(data_base) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte!=2,crise_marco==0)%>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo)) %>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

capital_de_giro_pos_marco <-  df_scr_sem_pf %>%
  group_by(data_base) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte!=2,crise_marco==1)%>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo))%>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

mean(capital_de_giro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
mean(capital_de_giro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(capital_de_giro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(capital_de_giro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)


#carteira ativa #CAPITAL DE GIRO para micro 

capital_de_giro_micro_pre_marco <- df_scr_sem_pf %>%
  group_by(data_base,porte) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte==4,crise_marco==0)%>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo)) %>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

capital_de_giro_micro_pos_marco <- df_scr_sem_pf %>%
  group_by(data_base,porte) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte==4,crise_marco==1)%>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo)) %>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

mean(capital_de_giro_micro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
mean(capital_de_giro_micro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(capital_de_giro_micro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(capital_de_giro_micro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

#carteira ativa #CAPITAL DE GIRO para pequenas 

capital_de_giro_pequena_pre_marco <- df_scr_sem_pf %>%
  group_by(data_base,porte) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte==5,crise_marco==0)%>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo)) %>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

capital_de_giro_pequena_pos_marco <- df_scr_sem_pf %>%
  group_by(data_base,porte) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte==5,crise_marco==1)%>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo)) %>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)

mean(capital_de_giro_pequena_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
mean(capital_de_giro_pequena_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(capital_de_giro_pequena_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(capital_de_giro_pequena_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)


#carteira ativa para medias # CAPITAL DE GIRO 
capital_de_giro_media_pre_marco <- df_scr_sem_pf %>%
  group_by(data_base,porte) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte==3,crise_marco==0)%>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo)) %>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 

capital_de_giro_media_pos_marco <- df_scr_sem_pf %>%
  group_by(data_base,porte) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_origem==0,tipo_porte==3,crise_marco==1)%>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo)) %>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000) 

mean(capital_de_giro_media_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
mean(capital_de_giro_media_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(capital_de_giro_media_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(capital_de_giro_media_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

#carteira ativa para grande # CAPITAL DE GIRO 
capital_de_giro_grande_pre_marco <- df_scr_sem_pf %>%
  group_by(data_base) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_porte==1,crise_marco==0)%>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo)) %>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)  

capital_de_giro_grande_pos_marco <- df_scr_sem_pf %>%
  group_by(data_base) %>%
  filter(tipos_modalidade==1|tipos_modalidade==2,tipo_porte==1,crise_marco==1)%>%
  summarise(carteira_ativa = sum(carteira_ativa),total_carteira_inadimplida_arrastada=sum(carteira_inadimplida_arrastada),total_curto_prazo=sum(curto_prazo),total_medio_prazo=sum(medio_prazo),
            total_longo_prazo=sum(longo_prazo)) %>%
  mutate(carteira_ativa_bilhao = carteira_ativa/1000000000)  

mean(capital_de_giro_grande_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
mean(capital_de_giro_grande_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(capital_de_giro_grande_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(capital_de_giro_grande_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

#Carteira ativa total micro pre e poscrise

carteira_ativa_micro_pre_marco <- df_scr_sem_pf %>%
  filter(tipo_porte==4,crise_marco==0) 

carteira_ativa_micro_pos_marco <- df_scr_sem_pf %>%
  filter(tipo_porte==4,crise_marco==1) 

summary(carteira_ativa_micro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(carteira_ativa_micro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

#Carteira ativa total pequena pre e poscrise

carteira_ativa_pequena_pre_marco <- df_scr_sem_pf %>%
  filter(tipo_porte==5,crise_marco==0) 

carteira_ativa_pequena_pos_marco <- df_scr_sem_pf %>%
  filter(tipo_porte==5,crise_marco==1) 

summary(carteira_ativa_pequena_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(carteira_ativa_pequena_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)


#Carteira ativa total media pre e poscrise

carteira_ativa_media_pre_marco <- df_scr_sem_pf %>%
  filter(tipo_porte==3,crise_marco==0) 

carteira_ativa_media_pos_marco <- df_scr_sem_pf %>%
  filter(tipo_porte==3,crise_marco==1) 

summary(carteira_ativa_media_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(carteira_ativa_media_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)


#Carteira ativa total grande pre e pos crise

carteira_ativa_grande_pre_marco <- df_scr_sem_pf %>%
  filter(tipo_porte==1,crise_marco==0) 

carteira_ativa_grande_pos_marco <- df_scr_sem_pf %>%
  filter(tipo_porte==1,crise_marco==1) 

summary(carteira_ativa_grande_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(carteira_ativa_grande_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)


#ESTATÍSTICAS!!!####
##CARTEIRA ATIVA total PRE E POS CRISE
summary(df_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(df_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(df_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(df_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

# carteira ativa TOTAL micro pre e pos crise
summary(carteira_ativa_micro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(carteira_ativa_micro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(carteira_ativa_micro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(carteira_ativa_micro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

# carteira ativa TOTAL media pre e pos crise
summary(carteira_ativa_media_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(carteira_ativa_media_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(carteira_ativa_media_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(carteira_ativa_media_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

#carteira ativa total grande pre e pos crise

summary(carteira_ativa_grande_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(carteira_ativa_grande_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

summary(carteira_ativa_grande_pre_marco$inad,na.rm=TRUE)
summary(carteira_ativa_grande_pos_marco$inad,na.rm=TRUE)


sd(carteira_ativa_grande_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(carteira_ativa_grande_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)


#CAPITAL DE GIRO TOTAL PRE E POS CRISE 
summary(capital_de_giro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(capital_de_giro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(capital_de_giro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(capital_de_giro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

# CAPITAL DE GIRO MICRO PREE POS CRISE 
summary(capital_de_giro_micro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(capital_de_giro_micro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(capital_de_giro_micro_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(capital_de_giro_micro_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

# CAPITAL DE GIRO media PREE POS CRISE 
summary(capital_de_giro_media_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(capital_de_giro_media_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

sd(capital_de_giro_media_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(capital_de_giro_media_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

#CAPITAL DE GIRO PARA GRANDE PRE E POS CRISE
summary(capital_de_giro_grande_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
summary(capital_de_giro_grande_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)

summary(capital_de_giro_grande_pre_marco$inad,na.rm=TRUE)
summary(capital_de_giro_grande_pos_marco$inad,na.rm=TRUE)


sd(capital_de_giro_grande_pre_marco$carteira_ativa_bilhao,na.rm=TRUE)
sd(capital_de_giro_grande_pos_marco$carteira_ativa_bilhao,na.rm=TRUE)












