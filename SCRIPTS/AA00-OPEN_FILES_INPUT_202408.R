

### Essa rotina é o inicio do tratamento dos dados para o coinR ####

########################################################################### 
# 
#   Autor:  Nete Barreto
#   Data:   20/07/2024
#           30/08/2024
#   Informações Basicas: 
#       Versão do R:
#           "R version 4.3.2 (2023-10-31 ucrt)"
#       Pacotes Instalados: 
#           COINr_1.1.14
#           lubridate_1.9.3  
#           openxlsx_4.2.6.1
#     
###########################################################################
rm(list=ls())

library(openxlsx)
library("lubridate")
library("COINr")

### Função que Limpa o terminal #### 

f1<-function(){shell("cls")}

### 

# Path_p --> Variavel que determina a localização do Diretorio principal das Pastas e Arquivos 

Path_p <- "c:/Users/Nete/Documents/NETE_PROJETOS/AB01-INSTITUCIONAIS/INPE/ADAPTABRASIL/GIT_ADAPTA/reprodutibilidade"

### No Diretório dos Arquivos contém 4 pasta : 
#                SCRIPT      - Rotinas e Funções 
#                DATASET      - Arquivos de Entrada
#                OUTPUT      - Arquivos de Saida 
#                DESCRITORES - Arquivos com Descrições   

## Midifica o diretório de trabalho do R para o diretório principal
setwd(Path_p)

f1()
#### Leitura das Tabelas e Verificação ##### 

### Arquivo em .csv separado por (;) e decimal (.) 
    ####        No caso das colunas de string, celulas em branco devem ter NA
    ####        Nas colunas numéricas não há necessidade. 
####

iMeta_in <- read.csv("DATASET/IMeta_RH_INDBRT.csv",sep=";",dec=".",header=TRUE,fileEncoding="latin1")

iMeta_adapta <- data.frame(iMeta_in, stringsAsFactors = default.stringsAsFactors())

check_iMeta(iMeta_adapta)

###### 

iData_input <- as.data.frame(read.csv("DATASET/IData_RH_INDBRT.csv",sep=";",dec=",",header=TRUE))

UCode<-paste0(iData_input$UF,iData_input$Geocódigo)
iData_input$uCode = UCode

    ## O ideal é remover algumas colunas que não serão usadas na geração do objeto ADAPTA_coin
    ## Nesse momento são as colunas :  
    ## c("uName","Geocódigo","UF","Região","Cluster")
    
        ### Nomes das Colunas do Objeto
        iData_ncol <- colnames(iData_input) 
        ### Nomes das Colunas a serem removidas 
        col_remove <- c("uName" , "Geocódigo", "UF", "Região", "Cluster")
   
    iData_adapta <- iData_input[,!(iData_ncol %in% col_remove)]   
   
check_iData(iData_adapta)

## Modificação 23/09/2023
######  "Winsorization" ##### 

# EM DESENVOLVIMENTO 
# VERIFICAR PARAMETROS DE AJUSTE 

##### 
ADAPTA_coin <- COINr::new_coin(iData = iData_adapta, iMeta = iMeta_adapta)

coin <- COINr::qNormalise(ADAPTA_coin, dset = "Raw", f_n = "n_minmax")

coin <- COINr::Aggregate(coin, dset = "Normalised", f_ag = "a_amean")

# ####### 
