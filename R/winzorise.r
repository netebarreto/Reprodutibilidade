#########

    #' @title Gerar resumo sobre os limites do Winsorize de unico indicador.
    #',
    #' @description Esta funcao gera os limites inferiores e superiores para a aplicacao do winzorise num indicador especifico,
    #' com suporte para indicadores do tipo cluster.
    #',
    #' @param indice Um vetor contendo os dados de entrada .
    #' @param classe_label classe do indice (ex: "Numerico", "Cluster", "Score")."
    #' @param var_name nome da variavel.
#' 
#' @export
winsorize_info <- function(indice,classe_label=NULL,var_name=NULL) 
              {              
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

                resumo<-data.frame(nome=var_name,
                              classe=classe_label,
                              winsorize=Awin,
                              linf=round(L_inf,2),
                              lsup=round(L_sup,2),
                              n_outlier = length(which(X>L_sup | X<L_inf)),
                              n_na=n.na)
                return(resumo)
                }

#' @title Aplicar Winsorization a variaveis numericas
#'
#' @description
#' Aplica o processo de Winsorization a colunas numericas 
#'
#' @param dataset \code{data.frame} com os dados originais.
#' @param metadados \code{data.frame} com metadados das variaveis. Deve conter colunas:
#'   \itemize{
#'     \item \code{Classe} - Tipo da variavel ("Numerico").
#'     \item \code{Code} - Nome da coluna correspondente em \code{dataset}.
#'   }
#' @param cluster_ref Vetor ou coluna de cluster_ref para identificar grupos (usado no caso de variaveis "Cluster").
#'
#' @details
#' Esta funcao utiliza a funcao auxiliar \code{wins_par} para calcular os limites de Winsorization
#' e retorna um resumo para cada variavel processada.
#'
#' - Se houver variaveis do tipo "Cluster", o processo e aplicado separadamente para cada grupo identificado em \code{cluster_ref}.
#' - As variaveis sao retornadas na ordem definida por \code{metadados$Code}.
#'
#' @return Um \code{data.frame} com o resumo da Winsorization de cada variavel.
#'
#' @examples
#' # Exemplo ficticio (assumindo que wins_par ja esteja implementada)
#' dataset <- data.frame(
#'   var1 = c(1, 2, 3, 100),
#'   var2 = c(10, 20, 30, 40),
#'   cluster = c(1, 1, 2, 2)
#' )
#'
#' metadados <- data.frame(
#'   Classe = c("Numerico", "Numerico", "Cluster"),
#'   Code = c("var1", "var2", "cluster")
#' )
#'
#' cluster_ref <- dataset$cluster
#'
#' winsorize_data(dataset, metadados, cluster_ref)
#'
#' @export
winsorize_data <- function(dataset=NULL,metadados=NULL,cluster_ref=NULL) {
  
    ##### Aplicando para as colunas padrao 

      ncsc =  which(as.data.frame(metadados$Classe) == "Numerico")   
      lcolz <- which(colnames(dataset) %in% metadados$Code[ncsc])
      list_winsor <-t(mapply(winsorize_info,dataset[, lcolz], classe_label = "Numerico", var_name = colnames(dataset)[lcolz]))

      resumo_df <- as.data.frame(list_winsor)
      resumo_df <- resumo_df[order(match(resumo_df$nome,metadados$Code)),] 
      rownames(resumo_df)<-c(1:nrow(resumo_df))
      message("\n Resumo Winsorize Criado\n")
      return(resumo_df)
    }

#' @title Aplicar Winsorization a um conjunto de dados
#'
#' @description
#' Aplica a funcao \code{winsorize_data} para calcular limites de Winsorization
#' e depois substitui os valores dos indicadores de acordo com esses limites,
#' considerando variaveis numericas e de cluster.
#'
#' @param dataset \code{data.frame} contendo os dados originais.
#' @param metadados \code{data.frame} de metadados, conforme exigido por \code{winsorize_data}.
#' @param cluster_ref Vetor ou coluna de referencia para identificar grupos (necessario para variaveis de tipo "Cluster").
#'
#' @details
#' - A funcao espera que a saida de \code{winsorize_data} contenha:
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
#'   \item{dataset}{\code{data.frame} com os dados ajustados.}
#' }
#'
#' @examples
#' # Exemplo ficticio (assumindo que winsorize_data e winsorize_info ja existam)
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
#' winsorize_apply(dataset=dados, metadados=meta,cluster_ref= ref)
#'
#' @export
winsorize_apply <- function(dataset=NULL,metadados=NULL,cluster_ref=NULL) 
{ 
   resumo_df = winsorize_data(dataset,metadados,cluster_ref)

   dados_out = dataset
   dados_out[,1:ncol(dataset)] <- NA
   for(i in 1:nrow(resumo_df))
   {
    if(resumo_df[i,2]=="Descricao" | resumo_df[i,2]=="Score") 
    {
     dados_out[unlist(resumo_df[i,1])] <- dataset[unlist(resumo_df[i,1])]
    }
    if(resumo_df[i,2]=="Numerico") 
    {
      X1 <- dataset[unlist(resumo_df[i,1])]
      X1[which(dataset[unlist(resumo_df[i,1])]>as.numeric(resumo_df$LSUP[i])),1] <- as.numeric(resumo_df$LSUP[i])

      X1[which(dataset[unlist(resumo_df[i,1])]<as.numeric(resumo_df$LINF[i])),1] <- as.numeric(resumo_df$LINF[i])
      dados_out[unlist(resumo_df[i,1])] <- X1[,1]
    }



   }

   result <- list(resumo=resumo_df,dataset=dados_out)
   message("\n Winzorize aplicado no dataframe \n\n ")
return(result)
}
