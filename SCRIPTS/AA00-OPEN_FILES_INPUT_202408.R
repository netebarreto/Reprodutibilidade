

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

### Arquivo em .csv separado por (;) e decimal (.) 
    ####        No caso das colunas de string, celulas em branco devem ter NA
    ####        Nas colunas numéricas não há necessidade. 
####

iMeta_in <- read.csv("DATASET/IMeta_RH_INDBRT.csv",sep=";",dec=".",header=TRUE,fileEncoding="latin1")

iMeta_adapta <- data.frame(iMeta_in, stringsAsFactors = default.stringsAsFactors())

###### 

iData_bruto <- as.data.frame(read.csv("DATASET/IData_RH_INDBRT.csv",sep=";",dec=",",header=TRUE))

UCode<-paste0(iData_bruto$UF,iData_bruto$Geocódigo)
iData_bruto$uCode = UCode


######  "Winsorization" ##### 

source("SCRIPTS/FUNCTION/ADPwinsorise.r")
iData_winsor = ADPwinsorise(iData_bruto,iMeta_adapta)


######## Box Cox ###########

source("SCRIPTS/FUNCTION/ADPBoxCox.r")
iData_boxcox = ADPBoxCox(iData_winsor$Winsorise)

iMeta_adapta$BoxCox=c(iData_boxcox$iMeta$BoxCox,NA)

######## Normalização ###########
source("SCRIPTS/FUNCTION/ADPNormalise.r")
iData_boxcox = ADPNormalise(iData_boxcox$iData)



### Gerando as figuras de comparação 

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

nome_col = "DAR"
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
  