## ----eval = FALSE-------------------------------------------------------------
#  devtools::install_github("AdaptaBrasil/reprodutibilidade")

## ----eval = FALSE-------------------------------------------------------------
#  require(reprodutibilidade)

## ----eval = FALSE-------------------------------------------------------------
#  
#  library("reprodutibilidade")
#  
#  inxlsx <- openxlsx::loadWorkbook(file = "DATASET/Base_inicial_SA_Acesso.xlsx")
#  

## ----eval = TRUE--------------------------------------------------------------
  
  library("reprodutibilidade")

  input <- read_exemplo_xlsx()

  metadados <- input$metadados
  dataset  <- input$dataset

  # Recortando apenas os dados do Nivel 7
  metadadosN7 = subset(metadados,metadados$Nivel==7)

  # Selecionando apenas as referencias 
  data_ref = dataset[,c(1:3)]

  # Selecionando todos os dados   
  datasetN7 = round(dataset[,-c(1:3)],2)

  #Atribuindo nome as colunas
  colnames(datasetN7) <- colnames(dataset[,-c(1:3)])


## ----echo=TRUE, eval=TRUE, results='markup',fig.width=6.8, fig.height=4, dpi=100----
library("reprodutibilidade")

# Carregando a base de dados de exemplo
data("datasetN7", package = "reprodutibilidade")

idx_MMPD = datasetN7$MMPD
nome = "MMPD"

res1 = criar_resumo(idx_MMPD,"Numerico" , "MMPD")

print_summary(res1)

criar_grafico(idx_MMPD, plot=TRUE, nome_arquivo = "grafico_combinado.png", 
              largura = 10, altura = 5, dpi = 25, 
              nvalores="MMPD",fsize=6)


## ----eval = TRUE--------------------------------------------------------------

library("reprodutibilidade")
# Carregando a base de dados de exemplo

# Metadados
data("metadadosN7", package = "reprodutibilidade")

# Dataset 
data("datasetN7", package = "reprodutibilidade")

# Dataset referencia
data("data_ref", package = "reprodutibilidade")

resumo <- ADPresumo(datasetN7, 
                    metadadosN7$CLASSE,  
                    colnames(datasetN7))

print(resumo$resumo_total)                    

## ----eval = TRUE--------------------------------------------------------------
library("reprodutibilidade")

# Carregando a base de dados de exemplo
data("metadadosN7", package = "reprodutibilidade")

data("datasetN7", package = "reprodutibilidade")

idx_MMPD = datasetN7$MMPD
nome = "MMPD"

res1 = winsorize_info(idx_MMPD,"Numerico" , "MMPD")

print_summary(res1)



## ----eval = TRUE--------------------------------------------------------------

winsorize_data(dataset = datasetN7, 
               metadados = metadadosN7)

## ----eval = TRUE, fig.width=6.8, fig.height=4, dpi=100------------------------

data_winsor <- winsorize_apply(dataset=datasetN7,
                              metadados=metadadosN7)

plot(idx_MMPD,data_winsor$dataset$MMPD,
      main = paste0("Comparação dados sem Winzorise X Dados com Winzorise  \n Indicador: ",nome),
      cex.axis=1,las=1,ylab="Dados Transformados",
      xlab="Reference", cex.lab=0.8,cex.main=1) 


## ----eval = TRUE,fig.width=6.8, fig.height=4, dpi=100-------------------------

library("reprodutibilidade")

# Carregando a base de dados de exemplo
data("metadadosN7", package = "reprodutibilidade")
data("datasetN7", package = "reprodutibilidade")

dados <- datasetN7[,19]

# Aplicando Box-Cox com pacote forecast
bxcx.fcs <- boxcox_transform(dados, metodo = "forecast")
 
# Aplicando Box-Cox com o COINr
bxcx.coin <- boxcox_transform(dados,metodo = "COINr")
 
# Aplicando Yeo-Johnson
bxcx.yjn <- boxcox_transform(dados, metodo = "yeojohnson")

par(mfrow=c(1,3)) ; par(mar=c(5,4,6,1))
 
 plot(sort(dados),sort(bxcx.fcs),
      main = "Transformacao BoxCox \n via pacote Forecast",
      cex.axis=1,las=1,ylab="Transformada BoxCox",
      xlab="Reference", cex.lab=0.8,cex.main=1) 
 
 plot(sort(dados),sort(bxcx.coin),
      main = "Transformacao BoxCox \n via pacote COINr",
      cex.axis=1,las=1,ylab="Transformada BoxCox",
      xlab="Reference",cex.lab=0.8,cex.main=1) 
 
 plot(sort(dados),sort(bxcx.yjn),
      main = "Transformacao BoxCox \n via pacote bestNormalize::Yeojohnson",
      cex.axis=1,las=1,ylab="Transformada BoxCox",
      xlab="Reference",cex.lab=0.8,cex.main=1)

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

