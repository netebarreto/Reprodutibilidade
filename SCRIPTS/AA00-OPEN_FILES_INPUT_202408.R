

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
resumo_brutos = ADPResumo(dbruto=iData_bruto, 
                           classe = iMeta_adapta$Classe,
                           m_cluster=iData_bruto[,4],
                           nome_col=colnames(iData_bruto))

######  "Winsorization" ##### 

source("SCRIPTS/FUNCTION/ADPwinsorise.r")


(iData_winsor = ADPwinsorise(iData_bruto,iMeta_adapta))


######## Box Cox ###########

source("SCRIPTS/FUNCTION/ADPBoxCox.r")

iData_boxcox = ADPBoxCox(dados=iData_winsor$Winsorise[,5],classe = iMeta_adapta$Classe[5],cluster=iData_winsor$Winsorise[,4],nome=colnames(iData_winsor$Winsorise)[5])

res1$iMeta = NULL
res1$iData = NULL 

for(i in 5:NCOL(iData_winsor$Winsorise)) 
{
iData_boxcox = ADPBoxCox(dados=iData_winsor$Winsorise[,i],classe = iMeta_adapta$Classe[i-4],cluster=iData_winsor$Winsorise[,4],nome=colnames(iData_winsor$Winsorise)[i])

res1$iMeta = rbind(res1$iMeta,iData_boxcox$iMeta) 
res1$iData = cbind(res1$iData,iData_boxcox$iData) 

print(i)
}

res_bruto<-function(x1,y1,z1,b1)
                {
                   res1 = mapply(ADPBoxCox,dados=as.data.frame(x1), classe = y1,cluster=as.data.frame(z1),nome=b1)
                   res2 = do.call(rbind,lapply(res1, function(x) x))

                   return(res2)
                }


                
res1 = res_bruto(x1=iData_winsor$Winsorise[,5:6],y1 = iMeta_adapta$Classe[1:2],z1=iData_winsor$Winsorise[,4],b1=colnames(iData_winsor$Winsorise[,c(5:6)]))


