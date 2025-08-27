#########

    #' @title Gerar resumo sobre os limites do Winsorize de unico indicador.
    #',
    #' @description Esta função gera os limites inferiores e superiores para a aplicação do winzorise num indicador especifico,
    #' com suporte para indicadores do tipo cluster.
    #',
    #' @param indice Um vetor contendo os dados .
    #' @param classe classe do indicador (ex: "Numérico", "Cluster", "Score")."
    #' @param nome Vetor com os nomes descritivos de cada coluna de `data`.
#' 
#' @export
wins_par <- function(indice,classe,nome) 
              {   
              # Funçao calcula os limites de Winsorization
              # Y = iData[,i] 
              # a = iMeta$Classe[i]
              # b = colnames(iData)[i] 
              # Retornando uma lista com os limites de Winsorization
              # o número de valores NA e o número de outliers
              
                X <- as.numeric(indice)
                OLinf <-  min(X,na.rm=T)
                OLsup <-  max(X,na.rm=T)

                # Calculando os limites teoricos
                TLinf <- quantile(Y, 0.25,na.rm=T) - 1.5 * (quantile(Y, 0.75,na.rm=T) - quantile(Y, 0.25,na.rm=T))
                TLsup <- quantile(Y, 0.75,na.rm=T) + 1.5 * (quantile(Y, 0.75,na.rm=T) - quantile(Y, 0.25,na.rm=T))

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

#' @title Aplicar Winsorization a variáveis numéricas e de cluster
#'
#' @description
#' Aplica o processo de Winsorization a colunas numéricas e, opcionalmente, a colunas de cluster
#' considerando grupos de referência.
#'
#' @param iData \code{data.frame} com os dados originais.
#' @param iMeta \code{data.frame} com metadados das variáveis. Deve conter colunas:
#'   \itemize{
#'     \item \code{Classe} — Tipo da variável ("Numérico" ou "Cluster").
#'     \item \code{Code} — Nome da coluna correspondente em \code{iData}.
#'   }
#' @param iRef Vetor ou coluna de referência para identificar grupos (usado no caso de variáveis "Cluster").
#'
#' @details
#' Esta função utiliza a função auxiliar \code{wins_par} para calcular os limites de Winsorization
#' e retorna um resumo para cada variável processada.
#'
#' - Se houver variáveis do tipo "Cluster", o processo é aplicado separadamente para cada grupo identificado em \code{iRef}.
#' - As variáveis são retornadas na ordem definida por \code{iMeta$Code}.
#'
#' @return Um \code{data.frame} com o resumo da Winsorization de cada variável.
#'
#' @examples
#' # Exemplo fictício (assumindo que wins_par já esteja implementada)
#' iData <- data.frame(
#'   var1 = c(1, 2, 3, 100),
#'   var2 = c(10, 20, 30, 40),
#'   cluster = c(1, 1, 2, 2)
#' )
#'
#' iMeta <- data.frame(
#'   Classe = c("Numérico", "Numérico", "Cluster"),
#'   Code = c("var1", "var2", "cluster")
#' )
#'
#' iRef <- iData$cluster
#'
#' winsorise1(iData, iMeta, iRef)
#'
#' @export
winsorise1 <- function(iData,iMeta,iRef) {
  ##### Função que aplica o Winsorization
  # iData = iData_N7 
  # iMeta = iMeta_N7
  # utiliza a função wins_par para calcular os limites de Winsorization
  # retornado  o resumo de cada variável e os dados com os limites de Winsorization
  
  
    ##### Aplicando para as colunas padrão 

      ncsc =  which(iMeta$Classe == "Numérico")   
      lcolz <- which(colnames(iData) %in% iMeta$Code[ncsc])
      list_winsor <-t(mapply(wins_par,iData[, lcolz], a = "Numérico", b = colnames(iData)[lcolz]))

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
#' Aplica a função \code{winsorise1} para calcular limites de Winsorization
#' e depois substitui os valores dos indicadores de acordo com esses limites,
#' considerando variáveis numéricas e de cluster.
#'
#' @param iData \code{data.frame} contendo os dados originais.
#' @param iMeta \code{data.frame} de metadados, conforme exigido por \code{winsorise1}.
#' @param iRef Vetor ou coluna de referência para identificar grupos (necessário para variáveis de tipo "Cluster").
#'
#' @details
#' - A função espera que a saída de \code{winsorise1} contenha:
#'   \itemize{
#'     \item Coluna 1: nome ou índice da variável.
#'     \item Coluna 2: tipo ("Numérico", "Descricao", "Score", "Grupo 1", "Grupo 2").
#'     \item Colunas \code{LSUP} e \code{LINF}: limites superiores e inferiores.
#'   }
#' - Para variáveis do tipo "Cluster", o Winsorization é aplicado separadamente para cada grupo.
#'
#' @return Lista com dois elementos:
#' \describe{
#'   \item{Resumo}{\code{data.frame} com os limites e informações do Winsorization.}
#'   \item{iData}{\code{data.frame} com os dados ajustados.}
#' }
#'
#' @examples
#' # Exemplo fictício (assumindo que winsorise1 e wins_par já existam)
#' dados <- data.frame(
#'   var1 = c(1, 2, 3, 100),
#'   var2 = c(10, 20, 30, 40),
#'   CLUSTER = c(1, 1, 2, 2)
#' )
#' meta <- data.frame(
#'   Classe = c("Numérico", "Numérico", "Cluster"),
#'   Code = c("var1", "var2", "CLUSTER")
#' )
#' ref <- dados$CLUSTER
#'
#' ADPwinsorise(dados, meta, ref)
#'
#' @export
ADPwinsorise <- function(iData=NULL,iMeta=NULL,iRef=NULL) 
{ # Função que aplica o Winsorization
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
    if(resumo1[i,2]=="Numérico") 
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
   message("\n Winzorização aplicada \n ")
return(result)
}
