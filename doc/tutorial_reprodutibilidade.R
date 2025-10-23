## ----eval = FALSE-------------------------------------------------------------
#  devtools::install_github("AdaptaBrasil/reprodutibilidade")

## ----eval = FALSE-------------------------------------------------------------
#  require(reprodutibilidade)

## ----eval = FALSE-------------------------------------------------------------
#  library("reprodutibilidade")
#  inxlsx <- openxlsx::loadWorkbook(file = "DATASET/Base_inicial_SA_Acesso.xlsx")
#    name_meta = "Metadados"
#    name_data = "Dados_RA_Acesso"

## ----eval = FALSE-------------------------------------------------------------
#    metadados <- openxlsx::read.xlsx(inxlsx, sheet = name_meta)
#    dataset  <- openxlsx::read.xlsx(inxlsx, sheet = name_data)
#  
#    # Recortando apenas os dados do Nivel 7
#    metadadosN7 = subset(metadados,metadados$Nivel==7)
#  
#    # Selecionando apenas as referencias
#    data_ref = dataset[,c(1:4)]
#  
#    # Selecionando todos os dados
#    datasetN7 = round(dateset[,-c(1:4)],2)
#  
#    #Atribuindo nome as colunas
#    colnames(datasetN7) <- colnames(dataset[,-c(1:4)])

## ----echo=TRUE, eval=TRUE, results='markup'-----------------------------------

library("reprodutibilidade")
# Carregando a base de dados de exemplo
data("iData_N7", package = "reprodutibilidade")

idx_MMPD = iData_N7$MMPD
nome = "MMPD"

res1 = criar_resumo(idx_MMPD,"Numerico" , cluster = NULL, "MMPD")

t(as.data.frame(res1))


## ----eval = FALSE-------------------------------------------------------------
#  
#  library("reprodutibilidade")
#  # Carregando a base de dados de exemplo
#  data("iMeta_N7", package = "reprodutibilidade")
#  data("iData_N7", package = "reprodutibilidade")
#  
#  # Selecionando apenas as referencias
#  data_ref = iData_N7[,c(1:4)]
#  
#  resumo <- ADPresumo(iData_N7,
#                      iMeta_N7$Classe,
#                      data_ref[,4],
#                      colnames(iData_N7))

## ----eval = FALSE-------------------------------------------------------------
#  
#  library("reprodutibilidade")
#  # Carregando a base de dados de exemplo
#  data("iMeta_N7", package = "reprodutibilidade")
#  data("iData_N7", package = "reprodutibilidade")
#  
#  
#  winsorize_data(dataset=iData_N7,
#                 metadados=iMeta_N7,
#                 cluster_ref=data_ref[,4])
#  
#  data_winsor <- winsorize_apply(dataset=iData_N7,
#                                metadados=iMeta_N7,
#                                cluster_ref=data_ref[,4])
#  

## ----eval = FALSE-------------------------------------------------------------
#  
#  library("reprodutibilidade")
#  
#  # Carregando a base de dados de exemplo
#  data("iMeta_N7", package = "reprodutibilidade")
#  data("iData_N7", package = "reprodutibilidade")
#  
#  dados <- iMeta_N7[,7]
#  
#  # Aplicando Box-Cox com pacote forecast
#  bxcx.fcs <- boxcox_transform(dados, metodo = "forecast")
#  
#  # Aplicando Box-Cox com o COINr
#  bxcx.coin <- boxcox_transform(dados,metodo = "COINr")
#  
#  # Aplicando Yeo-Johnson
#  bxcx.yjn <- boxcox_transform(dados, metodo = "yeojohnson")
#  
#  par(mfrow=c(1,3)) ; par(mar=c(5,5,6,1))
#  
#   plot(c(1:length(dados)),bxcx.fcs,
#        main = "Transformacao BoxCox \n via pacote Forecast",
#        cex.axis=1.5,las=1,ylab="Transformada BoxCox",
#        xlab="Reference", cex.lab=1.8,cex.main=2)
#  
#   plot(c(1:length(dados)),bxcx.coin,
#        main = "Transformacao BoxCox \n via pacote COINr",
#        cex.axis=1.5,las=1,ylab="Transformada BoxCox",
#        xlab="Reference",cex.lab=1.8,cex.main=2)
#  
#   plot(c(1:length(dados)),bxcx.yjn,
#        main = "Transformacao BoxCox \n via pacote bestNormalize::Yeojohnson",
#        cex.axis=1.5,las=1,ylab="Transformada BoxCox",
#        xlab="Reference",cex.lab=1.8,cex.main=2)

## ----eval = FALSE-------------------------------------------------------------
#  data_bxcx <- ADPBoxCox(dadoswin=data_winsor$dataset,
#                        dados=iData_N7,
#                        classe=iMeta_N7$Classe,
#                        cluster=data_ref[,4],
#                        nome=colnames(idata_N7),
#                        metodo=method_boxcox)
#  
#  
#  data_normal <- ADPNormalise(data_bxcx$data)
#  

## ----eval = FALSE-------------------------------------------------------------
#  result = Tratamento(input="../../DATASET/Base_inicial_SA_Acesso.xlsx",
#             iMeta = "Metadados",
#             iData = "Dados_RA_Acesso",
#             method_boxcox="forecast")

