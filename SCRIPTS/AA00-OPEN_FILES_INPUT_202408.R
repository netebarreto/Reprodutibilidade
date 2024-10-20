

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

# library(openxlsx)
# library("lubridate")
# library("COINr")

### Função que Limpa o terminal #### 

f1<-function(){shell("cls")}

### Função que aplica o Winsorization ###
#
ADPwinsorise <- function(Z) {
                yy <- function(Y) {   
                      X <- as.numeric(Y)
                      bx_stat <- boxplot.stats(X)
                      L_inf <-  min(bx_stat$stats)
                      L_sup <-  max(bx_stat$stats)
                      outl <- boxplot.stats(X)$out 
                      X1b <- X
                      X1b[which(X>L_sup)] <- L_sup
                      X1b[which(X<L_inf)] <- L_inf
                      return(X1b)
                    }
                zz=apply(Z,2,yy)
                return(zz)
                }

### Função que Gera os boxplot de compração entre os dados brutos e os dados winsorise 
### verifica se o diretório que será salvo as figuras já existe, 
### caso negativo cria o local 

bxplotcompare <- function(info) { 
  # Criar diretório, se não existir
  if (!dir.exists(info$local)) { dir.create(info$local, recursive = TRUE) }
  
  # Definir parâmetros fixos fora do loop
  RES <- 300 ;  SIZE <- 450 ;  psize <- 16 
  plot_params <- list(width = 9 * SIZE, height = 5.5 * SIZE, type = 'windows', res = RES, pointsize = psize)
  par_params <- list(mar = c(5, 4, 1, 0), mai = c(1.5, 1.2, 1.0, 0.2), xpd = TRUE)
  
  # Loop pelos dados
  for (i in 2:(NROW(info$iMeta) - 1)) {
    # Nome do arquivo
    namefile <- paste0(info$local, formatC((i - 1), width = 2, flag = 0), "-", colnames(info$bruto)[i], ".png")
    
    # Ajustar o título do gráfico
    titulo <- strwrap(paste0(
                      formatC((i - 1), width = 2, flag = 0), " - ", info$iMeta$iCode[(i - 1)], ": ", info$iMeta$iName[(i - 1)]), width = 50)
    
    # criar o arquivo .png
    do.call(png, c(list(filename = namefile), plot_params))
    
    # Ajustar parâmetros 
    do.call(par, par_params)
    
    # Gerando os Graficos 
    if (i %in% nn) {
      bx_dados <- data.frame(Bruto = as.numeric(info$bruto[, i]), 
                             Winsorization = info$winsorise[, i])
      outp <- (length(boxplot.stats(bx_dados[, 1])$out) / length(bx_dados[, 1])) * 100
      
      boxplot(bx_dados, main = titulo, las = 1, ylim = c(0.95 * min(bx_dados$Bruto, na.rm = TRUE), 1.05 * max(bx_dados$Bruto, na.rm = TRUE)), cex.main = 1.5)
      mtext(paste0("Percentual de Outliers = ", format(outp, digits = 2, nsmall = 2), "%"), 3, line = -1.2, at = 0.85, col = 2, cex = 1.3, font = 2)
    } else {
      bx_dados <- data.frame(Bruto = as.numeric(info$bruto[, i]))
      boxplot(bx_dados, main = titulo, las = 1, ylim = c(0.95 * min(bx_dados$Bruto, na.rm = TRUE), 1.05 * max(bx_dados$Bruto, na.rm = TRUE)), cex.main = 1.5)
      mtext("Indicador tipo Score ou Cluster (Winsorization - Não se aplica)", 3, line = -1.2, at = 0.85, col = 2, cex = 1.3, font = 2)
    }
    
    # Fechando o Grafico
    dev.off()
  }
  
  print("Finalizou")
}


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

### Arquivo em .csv separado por (;) e decimal (.) 
    ####        No caso das colunas de string, celulas em branco devem ter NA
    ####        Nas colunas numéricas não há necessidade. 
####

iMeta_in <- read.csv("DATASET/IMeta_RH_INDBRT.csv",sep=";",dec=".",header=TRUE,fileEncoding="latin1")

iMeta_adapta <- data.frame(iMeta_in, stringsAsFactors = default.stringsAsFactors())

COINr::check_iMeta(iMeta_adapta)

###### 

iData_input <- as.data.frame(read.csv("DATASET/IData_RH_INDBRT.csv",sep=";",dec=",",header=TRUE))

UCode<-paste0(iData_input$UF,iData_input$Geocódigo)
iData_input$uCode = UCode

    ## O ideal é remover algumas colunas que não serão usadas na geração do objeto ADAPTA_coin
    ## Nesse momento são as colunas :  
    ## c("uName","Geocódigo","UF","Região","Cluster")
    
        ### Nomes das Colunas do Objeto
        iData_ncol <- colnames(iData_input) 
        ### Nomes das Colunas a serem removidas 
        col_remove <- c("uName" , "Geocódigo", "UF", "Região", "Cluster")
   
    iData_bruto <- iData_input[,!(iData_ncol %in% col_remove)]   
   
COINr::check_iData(iData_bruto)


######  "Winsorization" ##### 

iData_winsor <- iData_bruto
nn <- which(iMeta_adapta$Score_ADP == 0)+1
iData_winsor[,nn] = ADPwinsorise(iData_bruto[,nn])

bxplot_info <- list(local = "RESPOSTAS/FIGURAS/IS/",
              iMeta = iMeta_adapta,  
              brutos = iData_bruto,
              winsorise = iData_winsor,
              nn = nn)
### Gerando as figuras de comparação 

bxplotcompare(bxplot_info)

winsorise_plan <- as.data.frame(winsorise_plan[6:5575,-c(34:37)])
colnames(winsorise_plan) <- c(colnames(iData_input)[c(3:1)][-2],colnames(iData_input)[-c(1:3)]) 

winsorise_plan[,-c(1:5)] <- lapply(winsorise_plan[,-c(1:5)], as.numeric) 

compare_winsor<-data.frame(mapply("-",iData_winsor[,nn],winsorise_plan[,-c(2:5)][,nn]))


RES<-300 ; SIZE=450 ; ARG2 = 1.2 ; psize = 16
namefile = "RESPOSTAS/FIGURAS/COMPARE_WINSORISE.png"
png(namefile,width=6.5*SIZE,height=9*SIZE,type='windows',res=RES,pointsize=psize)
par(mar=c(1,1,1,0), mai=c(1,1.5,1,0.2),xpd=TRUE)
titulo = strwrap("Diferença entre a funçao winsorization e o winsorization aplicado no excell",50)
boxplot(compare_winsor,horizontal=TRUE,las=1,main=titulo)
dev.off()

######### Gerando Mapas de Comparação da Diferença entre os calculos 

shp_mun<-sf::st_transform(sf::st_read('DATASET/SHP/BR_Municipios_2022.shp'),crs=4326)
shp_uf<-sf::st_transform(sf::st_read('DATASET/SHP/BR_UF_2022.shp'),crs=4326)

SHP_0 <- base::merge(shp_mun,cbind(Geocódigo=winsorise_plan[,1],compare_winsor) , by.x = "CD_MUN", by.y = "Geocódigo")

nome_col = "IFDM"
nome_ind = iMeta_adapta$iName[which(iMeta_adapta$iCode==nome_col)]
maps_compare = ggplot2::ggplot(data = SHP_0) +
  ggplot2::geom_sf(ggplot2::aes(fill = !!rlang::sym(nome_col)),color=NA)+
  ggplot2::scale_fill_distiller(palette ="Reds", direction = -1,n.breaks = 10,
  guide = ggplot2::guide_colourbar(barwidth = 0.4, barheight = 25,title.position = "top"))+
  ggplot2::geom_sf(data = shp_uf,fill = NA, colour = "#7f7f7f", size=0.5)+
  ggplot2::ggtitle(paste0("Comparação entre os calculos de Winsorization (Função - Excel)) \n Indicador: ",nome_ind," - ",nome_col))+
  ggplot2::theme(legend.title=ggplot2::element_text(size=12), 
        legend.text=ggplot2::element_text(size=10))

maps_compare   
  

######  "Box Cox" ##### 
 # Considerando o critério do "COIN Tool User Guide" página 23 box 4 #
 # aplica-se o boxCox quando ambas as condições abaixo são satisfeitas : 
                    # Distorção >= 2  e  Custose >= 3.5 
### Em Andamento                     

distorcao <- apply(iData_winsor[,-1],2,COINr::skew,na.rm=TRUE)                     
curtose <- apply(iData_winsor[,-1],2,COINr::kurt,na.rm=TRUE)                     

iMeta_adapta$BoxCox_ADP = c(ifelse(distorcao>=2 & curtose>=3.5, 1, 0),NA)

df1 =  "IFGF" 
df2 = iData_winsor[df1]
nna<- which(is.na(df2))
df2_sna = df2[-nna]
df2_sna[df2_sna==0] = df2_sna[df2_sna==0] +1E-5 
lambda <- forecast::BoxCox.lambda(df2_sna[,1]) 
boxcox_ind = COINr::boxcox(df2_sna[,1],lambda=lambda)
boxcox_ind[nna] = NA

#### Não Finaliozado Ainda em Teste #### 

