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
              icode="MMPD",fsize=6)


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
#    icode <- "MMPD"
#  
#  # Criando os gráficos de resumo
#    criar_grafico(result$DadosB[[icode]],
#                  plot=FALSE,
#                  nome_arquivo = "grafico_combinado.png",
#                  largura = 10, altura = 5, dpi = 100,
#                  icode="MMPD", fsize=18)
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
#  
#    map_result(icode,
#               result$Data_Win$dataset[1,],
#               result$Ref,
#               fs = 30,
#               Titulo = icode,
#               salvar=TRUE)
#  
#    slides_process(
#      result$Data_Win$resumo[1,],
#      process         = "winsorize"
#      titulo          = "Tabela Gerada no R",
#      caminho_arquivo = "slide_process_winsorize.pptx",
#      caminho_map     = "arquivo_mapa.png")

## ----eval = FALSE-------------------------------------------------------------
#    map_result("FPAAPCT", result$Data_Bxc$data, result$Ref,
#                fs     = 26,
#                titulo = "FPAAPCT",
#                salvar = TRUE)
#  
#   slides_process(result$Data_Bxc$meta[16,],
#                  process         = "BoxCox"
#                  titulo          = "Titulo do slide",
#                  caminho_arquivo = "saida_apresentacao_bxcx.pptx",
#                  caminho_map     = "FPAAPCT_result$Data_Bxc$data.png")

## ----eval = FALSE-------------------------------------------------------------
#  
#    grafico_final(result$Data_Bxc$data, result$Data_Normal$dataset,
#              nome_arquivo = "grafico_final.png",
#              largura = 20,
#              altura = 10,
#              dpi = 100,
#              icode = "MMPD",
#              fsize = 32)
#  
#  
#    map_result_normal("MMPD", result$Data_Normal$dataset, result$Ref,
#                    fs = 10,
#                    titulo = "MMPD",
#                    salvar = TRUE,
#                    nome_arquivo = "mapa_normalizado.png")
#  
#    slides_normal(datain = result$Data_Bxc$data[[icode]],
#      titulo          = "Titulo Slide",
#      title_tab       = "Testes Estatísticos",
#      caminho_arquivo = paste0("saida_apresentacao_normal.pptx"),
#      caminho_map     = "mapa_normalizado.png" ,
#      caminho_grafico = "grafico_final.png" )
#  

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
               tipo    = "Total",
               save    = FALSE,
               visivel = TRUE,
               nfile   = "Correlação_Total.png")

## ----eval = FALSE-------------------------------------------------------------
#  
#  CODES = result$metadados$CODE
#  
#  for(i in CODES)
#  {
#   icode <- i
#   nome <- result$metadados$NOME[result$metadados$CODE == icode]
#  
#  # Criando os gráficos de resumo
#    criar_grafico(result$DadosB[[icode]],
#                  plot=FALSE,
#                  nome_arquivo = paste0("FIG/grafico_combinado_",icode,".png"),
#                  largura = 10, altura = 5, dpi = 100,
#                  fsize=18)
#  
#  # Gerando a figura do mapa
#    map_result(icode,
#               result$DadosB,
#               result$Ref,
#               fs = 30,
#               nome_arquivo = paste0("FIG/mapa_bruto_",icode,".png"),
#               salvar=TRUE)
#  
#  # Gerando o arquivo
#    slides_descricao(result, icode,
#                     titulo=paste0(icode,"-",nome),
#                     caminho_arquivo = "Apresentacao_Resumo_Total.pptx",
#                     caminho_imagem  = paste0("FIG/grafico_combinado_",icode,".png"),
#                     caminho_map     = paste0("FIG/mapa_bruto_",icode,".png"))
#  print(icode)
#  }
#  
#  

## ----eval = FALSE-------------------------------------------------------------
#  CODES = result$metadados$CODE
#  
#  for(i in CODES)
#  {
#   icode <- i
#   nn <- which(result$Data_Bxc$meta$Nome == icode)
#   nome <- result$metadados$NOME[result$metadados$CODE == icode]
#  
#  # Gerando a figura do mapa
#    map_result(icode, result$Data_Win$data, result$Ref,
#                fs = 26, titulo = icode,
#                nome_arquivo = paste0("FIG/mapa_win_",icode,".png"),
#                salvar=TRUE)
#  
#  # Gerando arquivo
#    slides_process( result$Data_Win$resumo[nn,],
#            process = "winsorize",
#            titulo = paste0(icode,"-",nome),
#            caminho_arquivo = paste0("Apresentacao_Winzorization_Total.pptx"),
#            caminho_map = paste0("FIG/mapa_win_",icode,".png"))
#  
#  print(icode)
#  }
#  

## ----eval = FALSE-------------------------------------------------------------
#  
#  CODES = result$metadados$CODE
#  
#  for(i in CODES)
#  {
#   icode <- i
#   nn <- which(result$Data_Bxc$meta$Nome == icode)
#   nome <- result$metadados$NOME[result$metadados$CODE == icode]
#  
#    map_result(icode, result$Data_Bxc$data, result$Ref,
#                fs = 26, titulo = icode,
#                nome_arquivo = paste0("FIG/mapa_bxcx_",icode,".png"),
#                salvar=TRUE)
#  
#  
#   slides_process( result$Data_Bxc$meta[nn,],
#                  process = "BoxCox",
#                  titulo = paste0(icode,"-",nome),
#                  caminho_arquivo = "Apresentacao_Boxcox_Total.pptx",
#                  caminho_map = paste0("FIG/mapa_bxcx_",icode,".png"))
#  
#  print(icode)
#  }
#  

## ----eval = FALSE-------------------------------------------------------------
#  
#  CODES = result$metadados$CODE[-3]
#  
#  for(i in CODES)
#  {
#   icode <- i
#   nn <- which(result$Data_Bxc$meta$Nome == icode)
#   nome <- result$metadados$NOME[result$metadados$CODE == icode]
#  
#  map_result_normal(icode, result$Data_Normal$dataset, result$Ref,
#                    fs = 10,
#                    titulo = icode,
#                    salvar=TRUE,
#                    nome_arquivo=paste0("FIG/mapa_norm_",icode,".png"))
#  
#  
#  grafico_final(dtset_proc = result$DadosB,
#                dtset_norm = result$Data_Normal$dataset,
#                nome_arquivo = paste0("FIG/grafico_final_",icode,".png"),
#                largura = 20,
#                altura = 10,
#                dpi = 100,
#                icode=icode,
#                fsize=32)
#  
#  slides_normal(datain=result$DadosB[[icode]],
#    titulo          = paste0(icode,"-",nome),
#    title_tab       = "Testes Estatísticos",
#    caminho_arquivo = "Apresentacao_Norm_Total.pptx",
#    caminho_map     = paste0("FIG/mapa_norm_",icode,".png") ,
#  caminho_grafico = paste0("FIG/grafico_final_",icode,".png"))
#  
#  print(icode)
#  }
#  

## ----eval = FALSE-------------------------------------------------------------
#  
#  caminho_arquivo <- system.file("rotinas_extras",
#                                 "rotinas_extras.r",
#                                  package = "reprodutibilidade")
#  source(caminho_arquivo)

## ----eval = FALSE-------------------------------------------------------------
#  
#  vif_df <- ADPvif(result$Data_Normal$dataset)
#  
#  FigVIF(vif_df,nfile=NULL,visivel=TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  
#  alpha_df<- ADPAlphaCron(result$Data_Normal$dataset)
#  
#  plotAlphaCronbach(alpha_df$alpha_df,
#                    alpha_df$alpha_total,
#                    visivel = TRUE,
#                    nfile=NULL)

## ----eval = FALSE-------------------------------------------------------------
#  result_metricas <- resumo_metricas(result$Data_Normal$dataset)
#  
#  # FigCorrelPlot(result_metricas$pcorrel,tipo="Parcial",nfile="nome_arquivo.png")
#  
#  # FigCorrelPlot(result_metricas$pcorrel,tipo="Total",nfile="nome_arquivo.png")
#  
#  # FigVIF(result_metricas$VIF,nfile="nome_arquivo.png",visivel=FALSE)
#  
#  # plotAlphaCronbach(result_metricas$AlphaCronbach$alpha_df,
#                    result_metricas$AlphaCronbach$alpha_total,
#                    visivel=FALSE,
#                    nfile="nome_arquivo.png")
#  
#  result_metricas$Res_visivel
#  

