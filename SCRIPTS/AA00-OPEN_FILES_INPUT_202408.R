

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

library(openxlsx)
library("lubridate")
library("COINr")

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

func_tt <- function(Y)
            {   X = as.numeric(Y)
                (bx_stat <- boxplot.stats(X))
                (L_inf <-  min(bx_stat$stats))
                (L_sup <-  max(bx_stat$stats))
                (outl =boxplot.stats(X)$out) 
                X1b = X
                X1b[which(X>L_sup)]=L_sup
                X1b[which(X<L_inf)]=L_inf
                return(X1b)
            }

iData_winsor <- iData_bruto
nn <- which(iMeta_adapta$Score_ADP == 0)+1
iData_winsor[,nn] = apply(iData_bruto[,nn], 2, func_tt)

### Verifica se o diretório que será salvo as figuras já existe, 
### caso negativo cria o local 

local1 = "RESPOSTAS/FIGURAS/IS/"
ifelse(dir.exists(local1),print("Diretório já existe"),dir.create(local1,recursive=TRUE))

RES<-300 ; SIZE=450 ; ARG2 = 1.2
for(i in 2:(nrow(iMeta_adapta)-1)) { 
    if(i %in% nn) {
		png(paste0(local1,formatC((i-1),width=2,flag=0),"-",colnames(iData_bruto)[i],".png"),width=9*SIZE,height=5.5*SIZE,type='windows',res=RES,pointsize=16)
		par(mar=c(5,4,1,0), mai=c(1.5,1.2,1.0,0.2),xpd=TRUE)
        bx_dados=data.frame(Bruto=as.numeric(iData_bruto[,i]),Winsorization=iData_winsor[,i])
        outp = (length(boxplot.stats(bx_dados[,1])$out)/length(bx_dados[,1]))*100
		boxplot(bx_dados,main=strwrap(paste0(formatC((i-1),width=2,flag=0)," - ",iMeta_adapta$iCode[(i-1)],": ",iMeta_adapta$iName[(i-1)]), width = 50),las=1,ylim=c(0.95*min(bx_dados$Bruto,na.rm=T),1.05*max(bx_dados$Bruto,na.rm=T)),cex.main=1.5)
        mtext(paste0("Percentual de Outliers = ",format(outp,digits=2,nsmall=2),"%"),3,line=-1.2,at=0.85,col=2,cex=1.3,font=2)
		dev.off()
    }
    else {
		png(paste0(local1,formatC((i-1),width=2,flag=0),"-",colnames(iData_bruto)[i],".png"),width=9*SIZE,height=5.5*SIZE,type='windows',res=RES,pointsize=16)
		par(mar=c(5,4,1,0), mai=c(1.5,1.2,1.0,0.2),xpd=TRUE)
        bx_dados=data.frame(Bruto=as.numeric(iData_bruto[,i]))
		boxplot(bx_dados,main=strwrap(paste0(formatC((i-1),width=2,flag=0)," - ",iMeta_adapta$iCode[(i-1)],": ",iMeta_adapta$iName[(i-1)]), width = 50),las=1,ylim=c(0.95*min(bx_dados$Bruto,na.rm=T),1.05*max(bx_dados$Bruto,na.rm=T)),cex.main=1.5)
        mtext("Indicador tipo Score ou Cluster ",3,line=-1.2,at=0.85,col=2,cex=1.3,font=2)
		dev.off()
    }
}


