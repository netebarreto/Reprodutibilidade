#' Dados de exemplo datasetN7
#'
#' Um tibble de exemplo com dados do N7.
#'
#' @format Um tibble com X linhas e Y colunas.
#' @source Fonte dos dados Base_inicial_SA_Acesso.xlsx
"datasetN7"

#' Metadados metadadosN7
#'
#' Metadados do dataset datasetN7.
#'
#' @format Um tibble com X linhas e Y colunas.
#' @source Fonte dos dados Base_inicial_SA_Acesso.xlsx
"metadadosN7"

#' Metadados data_ref
#'
#' Colunas de referencia para indexacao do dataset.
#'
#' @format Um tibble com X linhas e Y colunas.
#' @source Fonte dos dados Base_inicial_SA_Acesso.xlsx
"data_ref"


#' Shape de municípios
#'
#' Objeto sf com municípios brasileiros, incluindo a coluna \code{CD_MUN}.
#'
#' @format Objeto \code{sf} com colunas de atributos.
#' @source IBGE
#' @examples
#' head(mun_shp)
"mun_shp"

#' Shape de unidades federativas
#'
#' Objeto sf com unidades da federação.
#'
#' @format Objeto \code{sf}.
#' @source IBGE
#' @examples
#' head(uf_shp)
"uf_shp"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("mun_shp", "uf_shp"))
}
