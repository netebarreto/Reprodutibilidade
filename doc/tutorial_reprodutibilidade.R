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

## ----echo=TRUE, eval=TRUE, results='markup'-----------------------------------

library("reprodutibilidade")
# Carregando a base de dados de exemplo
data("iData_N7", package = "reprodutibilidade")

idx_MMPD = iData_N7$MMPD
nome = "MMPD"

res1 = criar_resumo(idx_MMPD,"Numerico" , cluster = NULL, "MMPD")

t(as.data.frame(res1))


## ----eval = TRUE, echo = TRUE-------------------------------------------------

library("reprodutibilidade")
# Carregando a base de dados de exemplo
data("iMeta_N7", package = "reprodutibilidade")
data("iData_N7", package = "reprodutibilidade")

# Selecionando apenas as referencias 
data_ref = iData_N7[,c(1:4)]

resumo <- ADPresumo(iData_N7, 
                    iMeta_N7$Classe, 
                    data_ref[,4], 
                    colnames(iData_N7))

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

