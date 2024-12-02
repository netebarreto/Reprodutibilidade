

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
### 
xlsx_file <- openxlsx::loadWorkbook(file = "DATASET/FUN_VALORES_DES_INUND_20231213_R06.xlsx")
dbrutos_plan   <- openxlsx::read.xlsx(xlsx_file, sheet = "Ind_brutos")
winsorise_plan    <- openxlsx::read.xlsx(xlsx_file, sheet = "Winsorization")


#### Leitura das Tabelas e Verificação ##### 

inxlsx <- openxlsx::loadWorkbook(file = "DATASET/Base_inicial_RH_INDBRT.xlsx")
iMeta_adapta   <- openxlsx::read.xlsx(inxlsx, sheet = "Metadados")
iData_bruto <-  openxlsx::read.xlsx(inxlsx, sheet = "Dados_RH_INDBRT")
###### 

source("SCRIPTS/FUNCTION/ADPResumo.r")
resumo1 = res_bruto(x1=iData_bruto[,-c(1:4)], y1 = iMeta_adapta$Classe,z1=iData_bruto[,4],b1=colnames(iData_bruto[,-c(1:4)]))


######  "Winsorization" ##### 

source("SCRIPTS/FUNCTION/ADPwinsorise.r")
iData_winsor = ADPwinsorise(iData_bruto,iMeta_adapta)


######## Box Cox ###########

source("SCRIPTS/FUNCTION/ADPBoxCox.r")

iData_boxcox = ADPBoxCox(dados=iData_winsor$Winsorise[,5],classe = iMeta_adapta$Classe[1],cluster=iData_winsor$Winsorise[,4],nome=colnames(iData_winsor$Winsorise)[5])

res1 = mapply(ADPBoxCox,dados=iData_winsor$Winsorise[,-c(1:4)],classe = iMeta_adapta$Classe,cluster=iData_winsor$Winsorise[,4],nome=colnames(iData_winsor$Winsorise[,-c(1:4)]))

