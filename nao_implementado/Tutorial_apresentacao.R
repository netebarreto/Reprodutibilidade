
### Essa rotina é o inicio do tratamento dos dados para o  ####

########################################################################### 
# 
#   Autor:  Nete Barreto
#   Data:   20/07/2024
#           30/08/2024
#           21/02/2025
#           13/07/2025



rm(list=ls())
### Função que Limpa o terminal #### 
### 
f1<-function(){shell("cls")}

# Path_p --> Variavel que determina a localização do Diretorio principal das Pastas e Arquivos 

Path_p <- "SEULOCAL/AQUI/"
# # Modifica o diretório de trabalho do R para o diretório principal
setwd(Path_p)

#############
# instalando o pacote desenvolvido 

devtools::install_github("AdaptaBrasil/reprodutibilidade")
require(reprodutibilidade)

# Excutando a função que aplica as métricas, 
# Cria uma lista chamado result, onde estão os resultados 
#      das metricas aplicadas 
# Salva dois (2) arquivos .xlsx no diretório OUTPUT
# O primeiro ANALISE_DESCRITIVA_<<SETOR>>_<<DATA>>_<<HORA>>.xlsx
# O segundo 
# O primeiro DADOS_TRATADOS_<<SETOR>>_<<DATA>>_<<HORA>>.xlsx

caminho_arquivo <- system.file("dataset", 
                               "Base_inicial_SA_Acesso.xlsx",
                                package = "reprodutibilidade")

result = Tratamento(input=caminho_arquivo,
                    metadados = "metadados",
                    dataset = "dados_SA_Acesso",
                    nivel=7,
                    method_boxcox="forecast",
                    sigla = "SE",
                    subsetor= NULL)

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


source("function/cria_pptx_E01.R")

source("function/gera_diagrama_setor.R")
source("function/criar_grafico.R")
source("function/Map_result.R")
source("function/mapnorma_result.R")
source("function/grafico_final.R")

f1()
#################################################### 
### Gera Slides da apresentação com os dados processados ###

slides_resultT(
  template          = "TEMPLATE/ADAPTA_RESUMO.pptx",
  setor_estrategico = "Segurança Alimentar",
  sigla             = "SA",
  subsetor          = "ACESSO",
  caminho_shp_mun   = "DATASET/SHP/BR_Municipios_2022_gr.shp",
  caminho_shp_uf    = "DATASET/SHP/BR_UF_2022_gr.shp",
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
 

