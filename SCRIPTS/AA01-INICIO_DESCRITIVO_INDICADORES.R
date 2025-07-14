
### Essa rotina é o inicio do tratamento dos dados para o coinR ####

########################################################################### 
# 
#   Autor:  Nete Barreto
#   Data:   20/07/2024
#           30/08/2024
#           21/02/2025
#           13/07/2025
#   Informações Basicas: 
#       Versão do R:
#           "R version 4.3.2 (2023-10-31 ucrt)"
#       Pacotes Instalados: 
#           COINr_1.1.14
#           lubridate_1.9.3  
#           openxlsx_4.2.6.1
#           bestNormalize_1.9.1 (2025-05-28)     
###########################################################################


# ## No Diretório dos Arquivos contém 4 pasta : 
#                SCRIPT      - Rotinas e Funções 
#                DATASET      - Arquivos de Entrada
#                OUTPUT      - Arquivos de Saida 
#                DESCRITORES - Arquivos com Descrições   

# # Modifica o diretório de trabalho do R para o diretório principal

# ### LEITURA DOS DADOS ADAPTADOS PARA A ROTINA  ##### 
# Duas planilhas de dados, a primeira com metadados e a segunda com os dados brutos 

# ### CRIAÇÃO DOS RESUMOS SEM WINSORISAÇÃO ##### 
# ### Faz o Calculo das Estatísticas Descritivas, percentual de NA e Valores Unicos 

rm(list=ls())
### Função que Limpa o terminal #### 
### 
f1<-function(){shell("cls")}

# Path_p --> Variavel que determina a localização do Diretorio principal das Pastas e Arquivos 

Path_p <- "c:/Users/Nete/Documents/NETE_PROJETOS/AB01-PROJETOS_INSTITUCIONAIS/INPE/ADAPTABRASIL/GIT_ADAPTA/reprodutibilidade"

setwd(Path_p)

# Carregando as funções para a Analise inicial 

source("SCRIPTS/FUNCTION/F01_ADPResumo.r")
source("SCRIPTS/FUNCTION/F02_ADPwinsorise.r")
source("SCRIPTS/FUNCTION/F03_ADPBoxCox.r")
source("SCRIPTS/FUNCTION/F04_ADPNormalise.r")
source("SCRIPTS/FUNCTION/F07_ADP_GeraExcell.r")

# Excutando a função que aplica as métricas, 
# Cria uma lista chamado result, onde estão os resultados 
#      das metricas aplicadas 
# Salva dois (2) arquivos .xlsx no diretório OUTPUT
# O primeiro ANALISE_DESCRITIVA_<<SETOR>>_<<DATA>>_<<HORA>>.xlsx
# O segundo 
# O primeiro DADOS_TRATADOS_<<SETOR>>_<<DATA>>_<<HORA>>.xlsx

result = Tratamento(input="DATASET/Base_inicial_SA_Acesso.xlsx",
           iMeta = "Metadados",
           iData = "Dados_RA_Acesso",
           method_boxcox="forecast")


# ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #    
# #
# ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  

f1()
# ### 
### Carrega as livrarias necessarias para executar 
# a apresentação

library(officer)
library(flextable)
library(ggplot2)
library(dplyr)

library(DiagrammeR)
library(webshot)
library(magrittr)

# Carrega as funções para gerar os mapas e graficos 
# E criar a apresentação 

source("SCRIPTS/FUNCTION/F06_ADPCriar_pptx_E01.R")
source("SCRIPTS/FUNCTION/F05_ADPGraficos.R")

f1()
#################################################### 

slides_resultT(
  template          = "TEMPLATE/ADAPTA_RESUMO.pptx",
  setor_estrategico = "Segurança Alimentar",
  sigla             = "SA",
  subsetor          = "ACESSO",
  caminho_shp_mun   = "DATASET/SHP/BR_Municipios_2022.shp",
  caminho_shp_uf    = "DATASET/SHP/BR_UF_2022.shp",
  ind               = NULL,#NCOL(result$DadosB),
  RESU              = TRUE,
  WINZ              = TRUE,
  BXCX              = TRUE,
  NORM              = TRUE,
  resumo            = result$Resumo,
  meta_adapta       = result$iMeta,
  reference         = result$Ref,
  databruto         = result$DadosB,
  datawinz          = result$Data_Win,
  databxcx          = result$Data_Bxc,
  datanorm          = result$Data_Normal$iData)

#################################################### ########## 

### Gera Slides da apresentação com os dados processados ### 
