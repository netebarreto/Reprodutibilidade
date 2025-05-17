

### Essa rotina é o inicio do tratamento dos dados para o coinR ####

########################################################################### 
# 
#   Autor:  Nete Barreto
#   Data:   20/07/2024
#           30/08/2024
#           21/02/2025
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


### 
f1<-function(){shell("cls")}

# Path_p --> Variavel que determina a localização do Diretorio principal das Pastas e Arquivos 

Path_p <- "c:/Users/Nete/Documents/NETE_PROJETOS/AB01-PROJETOS_INSTITUCIONAIS/INPE/ADAPTABRASIL/GIT_ADAPTA/reprodutibilidade"

### No Diretório dos Arquivos contém 4 pasta : 
#                SCRIPT      - Rotinas e Funções 
#                DATASET      - Arquivos de Entrada
#                OUTPUT      - Arquivos de Saida 
#                DESCRITORES - Arquivos com Descrições   

## Modifica o diretório de trabalho do R para o diretório principal
setwd(Path_p)


#### LEITURA DOS DADOS ADAPTADOS PARA A ROTINA  ##### 
# Duas planilhas de dados, a primeira com metadados e a segunda com os dados brutos 

inxlsx <- openxlsx::loadWorkbook(file = "DATASET/Base_inicial_SA_Acesso.xlsx")
iMeta_adapta   <- openxlsx::read.xlsx(inxlsx, sheet = "Metadados")
iData_bruto <-  openxlsx::read.xlsx(inxlsx, sheet = "Dados_RA_Acesso")

#### CRIAÇÃO DOS RESUMOS SEM WINSORISAÇÃO ##### 
#### Faz o Calculo das Estatísticas Descritivas, percentual de NA e Valores Unicos 

source("SCRIPTS/FUNCTION/F01_ADPResumo.r")

iMeta_N7=subset(iMeta_adapta,Nivel==7)
iData_ref = iData_bruto[,c(1:4)]
iData_N7 = iData_bruto[,-c(1:4)]

resumo_bruto <- ADPresumo(iData_N7, iMeta_N7$Classe, iData_ref[,4], colnames(iData_N7))

######  "Winsorization" ##### 
### Aplica o Winsorization, pelos critérios do AdapaTa 

source("SCRIPTS/FUNCTION/F02_ADPwinsorise.r")
iData_winsor = ADPwinsorise(iData=iData_N7,iMeta=iMeta_N7,iRef=iData_ref[,4])

## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #    
#
## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


f1()
# ### 

library(officer)
library(flextable)
library(ggplot2)
library(dplyr)

library(DiagrammeR)
library(webshot)
library(magrittr)


source("SCRIPTS/FUNCTION/F06_ADPCriar_pptx_E01.R")
source("SCRIPTS/FUNCTION/F05_ADPGraficos.R")

infile <- "TEMPLATE/ADAPTA_RESUMO.pptx"

outfile2 = create_pptx(template=infile,
            meta_dados = iMeta_adapta, 
            setor_estrategico="Segurança Alimentar",
            sigla="SA",
            subsetor="ACESSO")

####################################################
slides_result(
  caminho_shp_mun = "DATASET/SHP/BR_Municipios_2022.shp",
  caminho_shp_uf = "DATASET/SHP/BR_UF_2022.shp",
  resumo_bruto = resumo_bruto,
  iMeta_adapta = iMeta_N7,
  iData_N7 = iData_N7,
  iData_ref = iData_ref,
  RES=TRUE,
  WIN=TRUE,
  BXC=NULL,
  iDataWIN = iData_winsor,
  local_output = outfile2)


######## Box Cox ###########

source("SCRIPTS/FUNCTION/F03_ADPBoxCox.r")

classe = iMeta_N7$Classe[1]
cluster=iData_ref[,4]
nome=colnames(iData_N7)[1]

dados=iData_winsor$iData[,1]
dados1=iData_N7[,1]

        meta_bxcx <- data.frame(Nome=nome,
                          Classe="Numérico",
                          BoxCox = NA, 
                          Distorcao = COINr::skew(dados,na.rm=TRUE),
                          Curtose = COINr::kurt(dados,na.rm=TRUE)) 
        meta_bxcx$BoxCox = ifelse(as.numeric(meta_bxcx$Distorcao)>=2 & as.numeric(meta_bxcx$Curtose)>=3.5, 1, 0)

       meta_bxcx1 <- data.frame(Nome=nome,
                          Classe="Numérico",
                          BoxCox = NA, 
                          Distorcao = COINr::skew(dados1,na.rm=TRUE),
                          Curtose = COINr::kurt(dados1,na.rm=TRUE)) 
        meta_bxcx1$BoxCox = ifelse(as.numeric(meta_bxcx1$Distorcao)>=2 & as.numeric(meta_bxcx1$Curtose)>=3.5, 1, 0)

data_bcx <-data.frame(Teste_CW=sfunc_bxcx(dados))
data_bcx1 <-data.frame(Teste_SW=sfunc_bxcx(dados1))

source("SCRIPTS/FUNCTION/F04_ADPNormalise.r")
data_Nbcx <-data.frame(Teste_CW=sfunc_norm(dados))
data_Nbcx1 <-data.frame(Teste_SW=sfunc_norm(dados1))




