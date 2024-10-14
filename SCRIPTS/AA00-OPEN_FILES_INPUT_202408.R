

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

COINr::check_iMeta(iMeta_adapta)

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
   
    iData_bruto <- iData_input[,!(iData_ncol %in% col_remove)]   
   
COINr::check_iData(iData_bruto)


######  "Winsorization" ##### 

func_tt <- function(Y)
            {   X = as.numeric(Y)
                (bx_stat <- boxplot.stats(X))
                (L_inf <-  min(bx_stat$stats))
                (L_sup <-  max(bx_stat$stats))
                (outl =boxplot.stats(X)$out) 
                X1b = X
                X1b[which(X>L_sup)]=L_sup
                X1b[which(X<L_inf)]=L_inf
                return(X1b)
            }

iData_winsor <- iData_bruto
nn <- which(iMeta_adapta$Score_ADP == 0)+1
iData_winsor[,nn] = apply(iData_bruto[,nn], 2, func_tt)


######  "Box Cox" ##### 
 # Considerando o critério do "COIN Tool User Guide" página 23 box 4 #
 # aplica-se o boxCox quando ambas as condições abaixo são satisfeitas : 
                    # Distorção >= 2.5  e  Custose >= 3.5 

distorcao <- apply(iData_winsor[,-1],2,COINr::skew,na.rm=TRUE)                     
curtose <- apply(iData_winsor[,-1],2,COINr::kurt,na.rm=TRUE)                     


iMeta_adapta$BoxCox = c(ifelse(distorcao>=2 & curtose>=3.5, 1, 0),NA)







