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

  resultado <- read_exemplo_xlsx()

  metadados <- resultado$metadados
  dataset  <- resultado$dataset

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

## ----eval = TRUE--------------------------------------------------------------
method_boxcox = "forecast"

data_bxcx <- ADPBoxCox(dadoswin=data_winsor$dataset,
                      dados=datasetN7,
                      classe=metadadosN7$CLASSE,
                      nome=colnames(datasetN7),
                      metodo=method_boxcox)

## ----eval = TRUE--------------------------------------------------------------
data_normal <- ADPNormalise(data_bxcx$data)

## ----eval = TRUE--------------------------------------------------------------
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

## ----eval = TRUE,fig.width=7.2, fig.height=5----------------------------------
library("reprodutibilidade")

map_result("MMPD", datasetN7, data_ref, fs = 10, titulo = "MMPD")

## ----eval = TRUE,fig.width=7.2, fig.height=5----------------------------------
map_result_normal("MMPD", result$Data_Normal$dataset, data_ref, 
                  fs = 10, 
                  titulo = "MMPD")

## ----eval = FALSE-------------------------------------------------------------
#  
#  # definindo o caminho da base de dados no formato .xlsx
#    caminho_arquivo <- system.file("dataset",
#                                 "Base_inicial_SA_Acesso.xlsx",
#                                  package = "reprodutibilidade")
#  
#  # gerando os resultados e salvando em .xlsx
#    result = Tratamento(input=caminho_arquivo,
#                        metadados = "metadados",
#                        dataset = "dados_SA_Acesso",
#                        nivel=7,
#                        method_boxcox="forecast",
#                        sigla = "SE",
#                        subsetor= NULL)
#  
#  # Determinado o indicador que será apresentado
#    icode <- tab.descricao$CODE
#  
#  # Criando os gráficos de resumo
#    criar_grafico(result$DadosB[[icode]],
#                  plot=FALSE,
#                  nome_arquivo = "grafico_combinado.png",
#                  largura = 10, altura = 5, dpi = 100,
#                  nvalores="MMPD", fsize=18)
#  
#  # Gerando a figura do mapa
#  
#    map_result(icode,
#               result$DadosB,
#               result$Ref,
#               fs = 30,
#               Titulo = icode,
#               salvar=TRUE)
#  
#    slides_descricao(result, icode,
#                     titulo=NULL,
#                     caminho_arquivo = "saida_apresentacao.pptx",
#                     caminho_imagem  = "grafico_combinado.png",
#                     caminho_map     = "map_N7.png")
#  

## ----eval = FALSE-------------------------------------------------------------
#  slides_winsorize(
#    result$Data_Win$resumo[1,],
#    titulo          = "Tabela Gerada no R",
#    caminho_arquivo = paste0("saida_apresentacao_Win_",i,".pptx"),
#    caminho_map     = "MMPD_result$Data_Win$dataset.png"
#  )

## ----eval = TRUE--------------------------------------------------------------
total.na(datasetN7$MMPD)

## ----eval = TRUE--------------------------------------------------------------
mat <- cor(mtcars)

get_max_cor("mpg", mat)

## ----eval = TRUE--------------------------------------------------------------
res <- ADPcorrel(result$Data_Normal$dataset)

head(res$cor_summary)

## ----eval = TRUE,fig.width=7.2, fig.height=5----------------------------------
result_cor <- correl_ind(result$Data_Normal$dataset)

head(result_cor$Resumo)

FigContNA(result_cor$Contagem_NA,
          nfile="Contagem_NA_ISimples.png",
          visivel=TRUE)

## ----eval = TRUE,fig.width=7.2, fig.height=5----------------------------------
 FigCorrelPlot(result_cor$Correl,
               tipo="Total",
               save=FALSE,
               nfile="FIGs/Correlação_Total.png")

