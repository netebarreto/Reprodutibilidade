#' Gera um slide em PowerPoint com tabelas automáticas
#'
#' Esta funcao integra as etapas de criacao dos flextables de metadados,
#' resumo estatistico e informacoes de valores ausentes, e em seguida
#' monta um slide PowerPoint contendo esses elementos.
#'
#' Ela funciona como um encapsulamento das funcoes
#' \code{cria_flextables_descricao()} e \code{monta_ppt_descricao()},
#' evitando que o usuario precise chamar ambas manualmente.
#'
#' @param result Objeto de resultados contendo os elementos
#'   \code{metadados} e \code{Resumo}, utilizados para montar as tabelas.
#' @param icode Codigo da variavel de interesse. Deve existir na coluna
#'   \code{CODE} de \code{result$metadados}.
#' @param titulo Titulo opcional para o slide. Se for \code{NULL},
#'   a funcao \code{monta_ppt_descricao} monta o titulo automaticamente.
#'   Valor padrao: \code{"Tabela Gerada no R"}.
#' @param caminho_arquivo Caminho do arquivo PowerPoint (\code{.pptx})
#'   onde o slide sera criado ou atualizado.
#' @param caminho_imagem Caminho de uma imagem opcional a ser incluida
#'   no slide (por exemplo, um grafico).
#' @param caminho_map Caminho de um mapa opcional a ser inserido no slide.
#'
#' @return Nenhum valor util e retornado. A funcao grava o slide no arquivo
#'   indicado em \code{caminho_arquivo}.
#'
#' @examples
#' # Exemplo hipotetico:
#' # slides_descricao(result, icode = "VAR001",
#' #   caminho_arquivo = "saida_apresentacao.pptx")
#'
#' @seealso
#'   \code{\link{cria_flextables_descricao}} para a geracao das tabelas,
#'   \code{\link{monta_ppt_descricao}} para a montagem do slide.
#' @export 

slides_descricao <- function(
  result,
  icode,
  titulo          = "Tabela Gerada no R",
  caminho_arquivo = "saida_apresentacao.pptx",
  caminho_imagem  = NULL,
  caminho_map     = NULL
) {
  fts <- cria_flextables_descricao(result, icode)

  monta_ppt_descricao(
    icode           = icode,
    ft              = fts$ft,
    ft2             = fts$ft2,
    ft3             = fts$ft3,
    tabela2         = fts$tabela2,
    tabela1         = fts$tabela1,
    titulo          = titulo,
    caminho_arquivo = caminho_arquivo,
    caminho_imagem  = caminho_imagem,
    caminho_map     = caminho_map
  )
}
