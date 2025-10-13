## ----eval = FALSE-------------------------------------------------------------
#  devtools::install_github("AdaptaBrasil/reprodutibilidade")

## ----eval = FALSE-------------------------------------------------------------
#  require(reprodutibilidade)

## ----eval = FALSE-------------------------------------------------------------
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

## ----eval = FALSE-------------------------------------------------------------
#  resumo <- ADPresumo(idata_N7,
#                      imeta_N7$Classe,
#                      data_ref[,4],
#                      colnames(idata_N7))

## ----eval = FALSE-------------------------------------------------------------
#  
#  winsorize_data(dataset=idata_N7,
#                 metadados=imeta_N7,
#                 cluster_ref=data_ref[,4])
#  
#  data_winsor <- winsorize_apply(dataset=idata_N7,
#                                metadados=imeta_N7,
#                                cluster_ref=data_ref[,4])
#  
#  data_bxcx <- ADPBoxCox(dadoswin=data_winsor$dataset,
#                        dados=idata_N7,
#                        classe=imeta_N7$Classe,
#                        cluster=data_ref[,4],
#                        nome=colnames(idata_N7),
#                        metodo=method_boxcox)
#  

## ----eval = FALSE-------------------------------------------------------------
#  result = Tratamento(input="../../DATASET/Base_inicial_SA_Acesso.xlsx",
#             iMeta = "Metadados",
#             iData = "Dados_RA_Acesso",
#             method_boxcox="forecast")

## ----eval = FALSE-------------------------------------------------------------
#  NA

