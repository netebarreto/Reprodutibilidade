#########

    #' @title Gerar resumo sobre os limites do Winsorize de unico indicador.
    #',
    #' @description Esta funcao gera os limites inferiores e superiores para a aplicacao do winzorise num indicador especifico,
    #' com suporte para indicadores do tipo cluster.
    #',
    #' @param indice Um vetor contendo os dados .
    #' @param classe classe do indicador (ex: "Numerico", "Cluster", "Score")."
    #' @param nome Vetor com os nomes descritivos de cada coluna de `data`.
#' 
#' @export
wins_par <- function(indice,classe,nome) 
              {   
              # Funcao calcula os limites de Winsorization
              # Y = iData[,i] 
              # a = iMeta$Classe[i]
              # b = colnames(iData)[i] 
              # Retornando uma lista com os limites de Winsorization
              # o numero de valores NA e o numero de outliers
              
                X <- as.numeric(indice)
                OLinf <-  min(X,na.rm=T)
                OLsup <-  max(X,na.rm=T)

                # Calculando os limites teoricos
                TLinf <- stats::quantile(X, 0.25,na.rm=T) - 1.5 * (stats::quantile(X, 0.75,na.rm=T) - stats::quantile(X, 0.25,na.rm=T))
                TLsup <- stats::quantile(X, 0.75,na.rm=T) + 1.5 * (stats::quantile(X, 0.75,na.rm=T) - stats::quantile(X, 0.25,na.rm=T))

                L_inf=max(c(OLinf,TLinf))                
                L_sup=ifelse(TLsup==0,OLsup,min(c(OLsup,TLsup)))

                Awin = ifelse(L_inf!=OLinf & L_sup != OLsup,"A",
                            ifelse(L_inf==OLinf & L_sup != OLsup,"S",
                                   ifelse(L_inf!=TLinf & L_sup == OLsup,"I","N")))

                n.na <- length(which(is.na(X)))

                resumo<-data.frame(iName=nome,
                              Classe=classe,
                              Aplicacao=Awin,
                              LINF=round(L_inf,2),
                              LSUP=round(L_sup,2),
                              T.outlier = length(which(X>L_sup | X<L_inf)),
                              T.NA=n.na)
                return(resumo)
                }

#' @title Aplicar Winsorization a variaveis numericas e de cluster
#'
#' @description
#' Aplica o processo de Winsorization a colunas numericas e, opcionalmente, a colunas de cluster
#' considerando grupos de referencia.
#'
#' @param iData \code{data.frame} com os dados originais.
#' @param iMeta \code{data.frame} com metadados das variaveis. Deve conter colunas:
#'   \itemize{
#'     \item \code{Classe} - Tipo da variavel ("Numerico" ou "Cluster").
#'     \item \code{Code} - Nome da coluna correspondente em \code{iData}.
#'   }
#' @param iRef Vetor ou coluna de referencia para identificar grupos (usado no caso de variaveis "Cluster").
#'
#' @details
#' Esta funcao utiliza a funcao auxiliar \code{wins_par} para calcular os limites de Winsorization
#' e retorna um resumo para cada variavel processada.
#'
#' - Se houver variaveis do tipo "Cluster", o processo e aplicado separadamente para cada grupo identificado em \code{iRef}.
#' - As variaveis sao retornadas na ordem definida por \code{iMeta$Code}.
#'
#' @return Um \code{data.frame} com o resumo da Winsorization de cada variavel.
#'
#' @examples
#' # Exemplo ficticio (assumindo que wins_par ja esteja implementada)
#' iData <- data.frame(
#'   var1 = c(1, 2, 3, 100),
#'   var2 = c(10, 20, 30, 40),
#'   cluster = c(1, 1, 2, 2)
#' )
#'
#' iMeta <- data.frame(
#'   Classe = c("Numerico", "Numerico", "Cluster"),
#'   Code = c("var1", "var2", "cluster")
#' )
#'
#' iRef <- iData$cluster
#'
#' winsorise1(iData, iMeta, iRef)
#'
#' @export
winsorise1 <- function(iData,iMeta,iRef) {
  ##### Funcao que aplica o Winsorization
  # iData = iData_N7 
  # iMeta = iMeta_N7
  # utiliza a funcao wins_par para calcular os limites de Winsorization
  # retornado  o resumo de cada variavel e os dados com os limites de Winsorization
  
  
    ##### Aplicando para as colunas padrao 

      ncsc =  which(iMeta$Classe == "Numerico")   
      lcolz <- which(colnames(iData) %in% iMeta$Code[ncsc])
      list_winsor <-t(mapply(wins_par,iData[, lcolz], a = "Numerico", b = colnames(iData)[lcolz]))

      if(any(iMeta$Classe == "Cluster")) {
      ## Aplicando para as colunas de Cluster
      # iRef = iData_ref[,4]    
      ncsc1 =  which(iMeta$Classe == "Cluster") 
      lcolzg1 <- which(colnames(iData) %in% iMeta$Code[ncsc])
      nrowzg1 <- iData[which(iRef == 1),lcolzg1]
      list_winsor1 <-t(mapply(wins_par,nrowzg1, a = "Grupo 1", b = colnames(iData)[lcolzg1]))
      
      nrowzg2 <- iData[which(iRef == 2),lcolzg1]
      list_winsor2 <-t(mapply(wins_par,nrowzg2, a = "Grupo 2", b = colnames(iData)[lcolzg1]))

      list_winsor <- rbind(list_winsor,list_winsor1,list_winsor2)
      }

      Resumo <- as.data.frame(list_winsor)
      Resumo <- Resumo[order(match(Resumo$iName,iMeta$Code)),] 
      rownames(Resumo)<-c(1:NROW(Resumo))
      message("")
      return(Resumo)
    }

#' @title Aplicar Winsorization a um conjunto de dados
#'
#' @description
#' Aplica a funcao \code{winsorise1} para calcular limites de Winsorization
#' e depois substitui os valores dos indicadores de acordo com esses limites,
#' considerando variaveis numericas e de cluster.
#'
#' @param iData \code{data.frame} contendo os dados originais.
#' @param iMeta \code{data.frame} de metadados, conforme exigido por \code{winsorise1}.
#' @param iRef Vetor ou coluna de referencia para identificar grupos (necessario para variaveis de tipo "Cluster").
#'
#' @details
#' - A funcao espera que a saida de \code{winsorise1} contenha:
#'   \itemize{
#'     \item Coluna 1: nome ou indice da variavel.
#'     \item Coluna 2: tipo ("Numerico", "Descricao", "Score", "Grupo 1", "Grupo 2").
#'     \item Colunas \code{LSUP} e \code{LINF}: limites superiores e inferiores.
#'   }
#' - Para variaveis do tipo "Cluster", o Winsorization e aplicado separadamente para cada grupo.
#'
#' @return Lista com dois elementos:
#' \describe{
#'   \item{Resumo}{\code{data.frame} com os limites e informacoes do Winsorization.}
#'   \item{iData}{\code{data.frame} com os dados ajustados.}
#' }
#'
#' @examples
#' # Exemplo ficticio (assumindo que winsorise1 e wins_par ja existam)
#' dados <- data.frame(
#'   var1 = c(1, 2, 3, 100),
#'   var2 = c(10, 20, 30, 40),
#'   CLUSTER = c(1, 1, 2, 2)
#' )
#' meta <- data.frame(
#'   Classe = c("Numerico", "Numerico", "Cluster"),
#'   Code = c("var1", "var2", "CLUSTER")
#' )
#' ref <- dados$CLUSTER
#'
#' ADPwinsorise(dados, meta, ref)
#'
#' @export
ADPwinsorise <- function(iData=NULL,iMeta=NULL,iRef=NULL) 
{ # Funcao que aplica o Winsorization
  # iData = iData_N7
  # iMeta = iMeta_N7
  # iRef = iData_ref[,4] 

   resumo1 = winsorise1(iData,iMeta,iRef)
   dados_out = iData
   dados_out[,1:NCOL(iData)] <- NA
   for(i in 1:NROW(resumo1))
   {
    if(resumo1[i,2]=="Descricao" | resumo1[i,2]=="Score") 
    {
     dados_out[unlist(resumo1[i,1])] <- iData[unlist(resumo1[i,1])]
    }
    if(resumo1[i,2]=="Numerico") 
    {
      X1 <- iData[unlist(resumo1[i,1])]
      X1[which(iData[unlist(resumo1[i,1])]>as.numeric(resumo1$LSUP[i])),1] <- as.numeric(resumo1$LSUP[i])

      X1[which(iData[unlist(resumo1[i,1])]<as.numeric(resumo1$LINF[i])),1] <- as.numeric(resumo1$LINF[i])
      dados_out[unlist(resumo1[i,1])] <- X1[,1]
    }

    if(resumo1[i,2]=="Grupo 1") 
    {
      X1a <- iData[unlist(resumo1[i,1])]
      X1=X1a[iData$CLUSTER==1,1]
      X1[which(X1>as.numeric(resumo1$LSUP[i]))] <- as.numeric(resumo1$LSUP[i])
      X1[which(X1<as.numeric(resumo1$LINF[i]))] <- as.numeric(resumo1$LINF[i])
      X1a[iData$CLUSTER==1,1]<-X1
      dados_out[unlist(resumo1[i,1])] <- X1a

    }

    
    if(resumo1[i,2]=="Grupo 2") 
    {
      X1a <- iData[unlist(resumo1[i,1])]
      X1=X1a[iData$CLUSTER==2,1]
      X1[which(X1>as.numeric(resumo1$LSUP[i]))] <- as.numeric(resumo1$LSUP[i])
      X1[which(X1<as.numeric(resumo1$LINF[i]))] <- as.numeric(resumo1$LINF[i])
      X1a[iData$CLUSTER==2,1]<-X1
      dados_out[unlist(resumo1[i,1])] <- X1a

    }

   }

   result <- list(Resumo=resumo1,iData=dados_out)
   message("\n Winzorizacao aplicada \n ")
return(result)
}
