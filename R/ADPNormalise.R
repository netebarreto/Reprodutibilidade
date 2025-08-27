#' @title Normalização Min-Max
#'
#' @description
#' Normaliza um vetor numérico usando o método Min-Max, 
#' transformando seus valores para o intervalo \[0, 1\].
#'
#' @param Y Vetor numérico a ser normalizado.
#'
#' @details
#' - Valores \code{NA} são ignorados nos cálculos de mínimo e máximo, mas preservados na saída.
#' - Fórmula utilizada:
#'   \deqn{Y_{norm} = \frac{Y - \min(Y)}{\max(Y) - \min(Y)}}
#'
#' @return Vetor numérico normalizado no intervalo \[0, 1\], preservando as posições originais de \code{NA}.
#'
#' @examples
#' # Vetor de exemplo
#' dados <- c(10, 20, 30, NA, 40)
#'
#' # Normalização Min-Max
#' sfunc_norm(dados)
#'
#' @export
sfunc_norm <- function(Y) {
  X <- as.numeric(Y)
  X_max <- max(X, na.rm = TRUE)
  X_min <- min(X, na.rm = TRUE)
  
  Y_res <- (X - X_min) / (X_max - X_min)
  message("\n Normalização Aplicada \n")
  return(Y_res)
}

#' @title Normalizar dados para o intervalo [0, 1]
#'
#' @description
#' Aplica a normalização min-max para cada coluna numérica de um data.frame ou matriz.
#' 
#' @param iData \code{data.frame} ou \code{matrix} contendo os dados.
#' Valores não numéricos serão ignorados e retornados sem modificação.
#'
#' @details
#' A normalização min-max é dada por:
#' \deqn{ X' = (X - \min(X)) / (\max(X) - \min(X)) }
#' 
#' Colunas constantes (mesmo valor para todos) são retornadas como 0.
#'
#' @return Lista com:
#' \describe{
#'   \item{iData}{\code{data.frame} com colunas normalizadas (numéricas) ou originais (não numéricas).}
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
data_norm <-apply(data_value,2,sfunc_norm) 
result <- list(iData = data_norm)
return(result)}