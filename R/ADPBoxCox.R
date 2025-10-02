#########
#' @title Aplicar transformada Box-Cox (ou variacoes) a um unico indicador
#'
#' @description
#' Aplica a transformada Box-Cox, Yeo-Johnson ou metodo equivalente a um unico vetor numerico. 
#' 
#' Com suporte para diferentes metodos de transformacao.
#'   
#' Remove valores ausentes antes do calculo. 
#' 
#' Ajustando valores iguais a zero 
#' para evitar problemas de logaritmo.  
#'
#' @param value Vetor numerico contendo os valores do indicador ou indice.
#' 
#' 
#' @param metodo Definicao do metodo de transformacao a ser utilizado:
#' 
#'   \itemize{
#'     \item `"forecast"`: Usa \code{forecast::BoxCox} com lambda estimado por \code{forecast::BoxCox.lambda}.
#'     \item `"COINr"`: Usa \code{COINr::boxcox} com lambda estimado por \code{forecast::BoxCox.lambda}.
#'     \item `"yeojohnson"`: Usa \code{bestNormalize::yeojohnson}.
#'   }
#' 
#' @details
#' - Valores \code{NA} sao ignorados na aplicacao da transformada e preservados na saida.
#' - Nos valores iguais a zero sao adicionado +0.001 para evitar erros numericos.
#' @return Vetor numerico com os valores transformados, mantendo a localizacao dos \code{NA} originais.
#' @examples
#' #Vetor de exemplo
#' dados <- c(25, 30, 28, 35, 40, 28, NA, 40,25,14,NA,33,56,23,NA,27,20, 35,71)
#' # Aplicando Box-Cox com pacote forecast
#' bxcx.fcs <- boxcox_transform(dados, metodo = "forecast")
#' 
#' # Aplicando Box-Cox com o COINr
#' bxcx.coin <- boxcox_transform(dados,metodo = "COINr")
#' 
#' # Aplicando Yeo-Johnson
#' bxcx.yjn <- boxcox_transform(dados, metodo = "yeojohnson")
#' 
#' par(mfrow=c(1,3)) ; par(mar=c(5,5,6,1))
#' 
#' plot(c(1:length(dados)),bxcx.fcs,
#'      main = "Transformacao BoxCox \n via pacote Forecast",
#'      cex.axis=1.5,las=1,ylab="Transformada BoxCox",
#'      xlab="Reference", cex.lab=1.8,cex.main=2) 
#' 
#' plot(c(1:length(dados)),bxcx.coin,
#'      main = "Transformacao BoxCox \n via pacote COINr",
#'      cex.axis=1.5,las=1,ylab="Transformada BoxCox",
#'      xlab="Reference",cex.lab=1.8,cex.main=2) 
#' 
#' plot(c(1:length(dados)),bxcx.yjn,
#'      main = "Transformacao BoxCox \n via pacote bestNormalize::Yeojohnson",
#'      cex.axis=1.5,las=1,ylab="Transformada BoxCox",
#'      xlab="Reference",cex.lab=1.8,cex.main=2)
#' @export
boxcox_transform<- function(value,metodo=c("forecast", "COINr", "yeojohnson")) 
                {
    Ysna = value[-which(is.na(value))]
    
    Ysna[Ysna==0] = Ysna[Ysna==0] +0.001 
    
    lambda <- forecast::BoxCox.lambda(Ysna)
                      
    Ybcx = switch(metodo,
    "forecast"    = forecast::BoxCox(Ysna, lambda = lambda),
    "COINr"       = COINr::boxcox(Ysna, lambda = lambda),
    "yeojohnson"  = bestNormalize::yeojohnson(Ysna)$x.t,
    stop("Metodo invalido. Use 'forecast', 'COINr' ou 'yeojohnson'."))
    
    Yres = value
    
    Yres[which(is.na(value))] = NA
    
    Yres[-which(is.na(value))] = Ybcx
    message(sprintf("Box-Cox aplicado com metodo: %s", metodo))
    return(Yres)
                }

#' @title Aplicar transformacao Box-Cox condicionalmente
#'
#' @description
#' Aplica a transformacao Box-Cox (ou variacoes) a variaveis numericas de acordo com criterios de distorcao (skewness) e curtose.
#' Apenas variaveis com distorcao >= 2 e curtose >= 3.5 sao transformadas.
#'
#' @param dadoswin \code{data.frame} com os dados ja winsorizados.
#' @param dados \code{data.frame} com os dados originais.
#' @param classe Vetor de caracteres indicando a classe de cada variavel.
#' @param cluster Vetor ou coluna de clusters (atualmente nao utilizado nesta funcao, mas mantido para compatibilidade).
#' @param nome Vetor com os nomes das variaveis.
#' @param metodo String indicando o metodo de transformacao:
#'   \itemize{
#'     \item `"forecast"` - usa \code{forecast::BoxCox}.
#'     \item `"COINr"` - usa \code{COINr::boxcox}.
#'     \item `"yeojohnson"` - usa \code{bestNormalize::yeojohnson}.
#'   }
#'
#' @details
#' A decisao de aplicar Box-Cox e baseada nas seguintes regras:
#' - Se \code{skewness} (distorcao) >= 2 **e**
#' - \code{kurtosis} (curtose) >= 3.5  
#' entao a transformacao e aplicada.
#'
#' @return Lista com:
#' \describe{
#'   \item{meta}{\code{data.frame} com informacoes sobre a decisao de aplicar Box-Cox para cada variavel.}
#'   \item{data}{\code{data.frame} com os dados transformados (ou originais, caso nao se aplique Box-Cox).}
#' }
#'
#' @examples
#' # Exemplo ficticio (assumindo que boxcox_transform ja exista)
#' set.seed(123)
#' dados <- data.frame(
#'   x1 = c(rnorm(10), 100),# variavel distorcida
#'   x2 = rnorm(11))        # variavel normal
#'
#' classe <- c("Numerico", "Numerico")
#' 
#' nomes <- c("x1", "x2")
#' 
#' ADPBoxCox(dadoswin = dados, 
#'           dados = dados, 
#'           classe = classe,
#'           cluster = NULL,
#'           nome = nomes,
#'           metodo = "forecast")
#'
#' @export
ADPBoxCox <- function(dadoswin=NULL,dados=NULL,classe=NULL,cluster=NULL,nome=NULL,metodo=NULL)
{
   prec_boxcox<-function(dadoswin,dados,classe,cluster,nome,metodo) 
   { 
       if(classe == "Numerico") 
        {
        meta_cx <- data.frame(Nome=nome,
                          Classe="Numerico",
                          BoxCox = NA, 
                          Distorcao = COINr::skew(dadoswin,na.rm=TRUE),
                          Curtose = COINr::kurt(dadoswin,na.rm=TRUE),
                          Metodo = metodo) 
        meta_cx$BoxCox = ifelse(is.na(meta_cx$Distorcao) | is.na(meta_cx$Curtose),0,ifelse(as.numeric(meta_cx$Distorcao)>=2 & as.numeric(meta_cx$Curtose)>=3.5, 1, 0))
        {
        if (meta_cx$BoxCox == 1 ) 
        data_bx <-boxcox_transform(dados,metodo) 
        else 
         {
          if (meta_cx$BoxCox == 0 )
           data_bx <-dadoswin
         }
        }}
         res1 <- list(meta = meta_cx, data = data_bx)
 return(res1)
        }
data_bcx =NULL
meta_bcx =NULL

for(i in 1:NCOL(dados))
{
res = prec_boxcox(dadoswin[,i],dados[,i],classe[i],cluster,nome[i],metodo)
meta_bcx <- rbind(meta_bcx,res$meta)
data_bcx <- cbind(data_bcx,res$data)
colnames(data_bcx)[i]<-nome[i]
}

 result <- list(meta = meta_bcx, data = as.data.frame(data_bcx))
  message("\n BoxCox aplicado no dataframe \n\n ")
 return(result)
}
