#########
#' @title Aplicar transformada Box-Cox (ou variações) a um único indicador
#'
#' @description
#' Aplica a transformada Box-Cox, Yeo-Johnson ou método equivalente a um único vetor numérico. 
#' 
#' Com suporte para diferentes métodos de transformação.
#'   
#' Remove valores ausentes antes do cálculo. 
#' 
#' Ajustando valores iguais a zero 
#' para evitar problemas de logaritmo.  
#'
#' @param Y Vetor numérico contendo os valores do indicador ou índice.
#' 
#' 
#' @param metodo Definição do método de transformação a ser utilizado:
#' 
#'   \itemize{
#'     \item `"forecast"`: Usa \code{forecast::BoxCox} com lambda estimado por \code{forecast::BoxCox.lambda}.
#'     \item `"COINr"`: Usa \code{COINr::boxcox} com lambda estimado por \code{forecast::BoxCox.lambda}.
#'     \item `"yeojohnson"`: Usa \code{bestNormalize::yeojohnson}.
#'   }
#' 
#' @details
#' - Valores \code{NA} são ignorados na aplicação da transformada e preservados na saída.
#' - Nos valores iguais a zero são adicionado +0.001 para evitar erros numéricos.
#' @return Vetor numérico com os valores transformados, mantendo a localização dos \code{NA} originais.
#' @examples
#' Vetor de exemplo
#' dados <- c(25, 30, 28, 35, 40, 28, NA, 40,25,14,NA,33,56,23,NA,27,20, 35,71)
#' # Aplicando Box-Cox com pacote forecast
#' bxcx.fcs <- sfunc_bxcx(dados, "forecast")
#' 
#' # Aplicando Box-Cox com o COINr
#' bxcx.coin <- sfunc_bxcx(dados, "COINr")
#' 
#' # Aplicando Yeo-Johnson
#' bxcx.yjn <- sfunc_bxcx(dados, "yeojohnson")
#' 
#' par(mfrow=c(1,3)) ; par(mar=c(5,5,6,1))
#' 
#' plot(index,bxcx.fcs,
#'      main = "Transformação BoxCox \n via pacote Forecast",
#'      cex.axis=1.5,las=1,ylab="Transformada BoxCox",
#'      xlab="Reference", cex.lab=1.8,cex.main=2) 
#' 
#' plot(index,bxcx.coin,
#'      main = "Transformação BoxCox \n via pacote COINr",
#'      cex.axis=1.5,las=1,ylab="Transformada BoxCox",
#'      xlab="Reference",cex.lab=1.8,cex.main=2) 
#' 
#' plot(index,bxcx.yjn,
#'      main = "Transformação BoxCox \n via pacote bestNormalize::Yeojohnson",
#'      cex.axis=1.5,las=1,ylab="Transformada BoxCox",
#'      xlab="Reference",cex.lab=1.8,cex.main=2)
#' @export
sfunc_bxcx <- function(Y,metodo) 
                {
    Ysna = Y[-which(is.na(Y))]
    
    Ysna[Ysna==0] = Ysna[Ysna==0] +0.001 
    
    lambda <- forecast::BoxCox.lambda(Ysna)
                      
    Ybcx = switch(metodo,
    "forecast"    = forecast::BoxCox(Ysna, lambda = lambda),
    "COINr"       = COINr::boxcox(Ysna, lambda = lambda),
    "yeojohnson"  = bestNormalize::yeojohnson(Ysna)$x.t,
    stop("Método inválido. Use 'forecast', 'COINr' ou 'yeojohnson'."))
    
    Yres = Y
    
    Yres[which(is.na(Y))] = NA
    
    Yres[-which(is.na(Y))] = Ybcx
    message(paste(" "," Box Cox Aplicado metodo:",metodo," ", collapse = "\n"))
    return(Yres)
                }

#' @title Aplicar transformação Box-Cox condicionalmente
#'
#' @description
#' Aplica a transformação Box-Cox (ou variações) a variáveis numéricas de acordo com critérios de distorção (skewness) e curtose.
#' Apenas variáveis com distorção >= 2 e curtose >= 3.5 são transformadas.
#'
#' @param dadoswin \code{data.frame} com os dados já winsorizados.
#' @param dados \code{data.frame} com os dados originais.
#' @param classe Vetor de caracteres indicando a classe de cada variável.
#' @param cluster Vetor ou coluna de clusters (atualmente não utilizado nesta função, mas mantido para compatibilidade).
#' @param nome Vetor com os nomes das variáveis.
#' @param metodo String indicando o método de transformação:
#'   \itemize{
#'     \item `"forecast"` — usa \code{forecast::BoxCox}.
#'     \item `"COINr"` — usa \code{COINr::boxcox}.
#'     \item `"yeojohnson"` — usa \code{bestNormalize::yeojohnson}.
#'   }
#'
#' @details
#' A decisão de aplicar Box-Cox é baseada nas seguintes regras:
#' - Se \code{skewness} (distorção) >= 2 **e**
#' - \code{kurtosis} (curtose) >= 3.5  
#' então a transformação é aplicada.
#'
#' @return Lista com:
#' \describe{
#'   \item{meta}{\code{data.frame} com informações sobre a decisão de aplicar Box-Cox para cada variável.}
#'   \item{data}{\code{data.frame} com os dados transformados (ou originais, caso não se aplique Box-Cox).}
#' }
#'
#' @examples
#' # Exemplo fictício (assumindo que sfunc_bxcx já exista)
#' set.seed(123)
#' dados <- data.frame(
#'   x1 = c(rnorm(10), 100),  # variável distorcida
#'   x2 = rnorm(11)           # variável normal
#' )
#' classe <- c("Numérico", "Numérico")
#' nomes <- c("x1", "x2")
#' ADPBoxCox(dadoswin = dados, dados = dados, classe = classe, cluster = NULL, nome = nomes, metodo = "forecast")
#'
#' @export
ADPBoxCox <- function(dadoswin,dados,classe,cluster,nome,metodo)
{
   prec_boxcox<-function(dadoswin,dados,classe,cluster,nome,metodo) 
   { 
       if(classe == "Numérico") 
        {
        meta_cx <- data.frame(Nome=nome,
                          Classe="Numérico",
                          BoxCox = NA, 
                          Distorcao = COINr::skew(dadoswin,na.rm=TRUE),
                          Curtose = COINr::kurt(dadoswin,na.rm=TRUE),
                          Metodo = metodo) 
        meta_cx$BoxCox = ifelse(is.na(meta_cx$Distorcao) | is.na(meta_cx$Curtose),0,ifelse(as.numeric(meta_cx$Distorcao)>=2 & as.numeric(meta_cx$Curtose)>=3.5, 1, 0))
        {
        if (meta_cx$BoxCox == 1 ) 
        data_bx <-sfunc_bxcx(dados,metodo) 
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
 return(result)
}