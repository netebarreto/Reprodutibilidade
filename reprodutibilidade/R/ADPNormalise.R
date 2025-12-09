#' @title Normalizacao Min-Max
#'
#' @description
#' Normaliza um vetor numerico usando o metodo Min-Max, 
#' transformando seus valores para o intervalo \[0, 1\].
#'
#' @param Y Vetor numerico a ser normalizado.
#'
#' @details
#' - Valores \code{NA} sao ignorados nos calculos de minimo e maximo, mas preservados na saida.
#' - Formula utilizada:
#'   \deqn{Y_{norm} = \frac{Y - \min(Y)}{\max(Y) - \min(Y)}}
#'
#' @return Vetor numerico normalizado no intervalo \[0, 1\], preservando as posicoes originais de \code{NA}.
#'
#' @examples
#' # Vetor de exemplo
#' dados <- c(10, 20, 30, NA, 40)
#'
#' # Normalizacao Min-Max
#' sfunc_norm(dados)
#'
#' @export
sfunc_norm <- function(Y) {
  X <- as.numeric(Y)
  X_max <- max(X, na.rm = TRUE)
  X_min <- min(X, na.rm = TRUE)
  
  Y_res <- (X - X_min) / (X_max - X_min)
  message("\n Normalizacao Aplicada \n")
  return(Y_res)
}

#' @title Normalizar dados para o intervalo \code{[0 , 1]}
#'
#' @description
#' Aplica a normalizacao min-max para cada coluna numerica de um data.frame ou matriz.
#' 
#' @param iData \code{data.frame} ou \code{matrix} contendo os dados.
#' Valores nao numericos serao ignorados e retornados sem modificacao.
#'
#' @details
#' A normalizacao min-max e dada por:
#' \deqn{ X' = (X - \min(X)) / (\max(X) - \min(X)) }
#' 
#' Colunas constantes (mesmo valor para todos) sao retornadas como 0.
#'
#' @return Lista com:
#' \describe{
#'   \item{iData}{\code{data.frame} com colunas normalizadas (numericas) ou originais (nao numericas).}
#' }
#'
#' @examples
#' df <- data.frame(a = 1:5, b = c(2,2,2,2,2), c = letters[1:5])
#' ADPNormalise(df)
#'
#' @export
ADPNormalise <- function(iData)
{

data_value <- iData
data_norm <- iData
data_norm <- suppressMessages(apply(data_value,2,sfunc_norm)) 
result <- list(dataset = data_norm)
message("\n Normalizacao Aplicada no data.frame \n")
return(result)}
