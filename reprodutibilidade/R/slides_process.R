#' Gera um slide PowerPoint contendo uma tabela genérica e opcionalmente um mapa
#'
#' Função de alto nível que cria o flextable e monta o slide em um único passo.
#'
#' @param tabela1 Data frame a ser exibido.
#' @param titulo Título do slide.
#' @param process processo do qual sera gerado o slide, pode ser "winsorize" ou "BoxCox"
#' @param caminho_arquivo Caminho de saída do arquivo `.pptx`.
#' @param caminho_map Caminho opcional para imagem de mapa.
#'
#' @return Um arquivo PowerPoint salvo no caminho especificado.
#'
#' @examples
#' \dontrun{
#' slides_generico(mtcars)
#' }
#'
#' @export
slides_process <- function(
  tabela1,
  process         = NULL, 
  titulo          = "Tabela Gerada no R",
  caminho_arquivo = "saida_apresentacao.pptx",
  caminho_map     = NULL
) {
 if(process == "winsorize") 
  {
  ft_list <- cria_flextable_winsorize(tabela1)
  tem_tabela <- !is.null(ft_list)}
 else if (process == "BoxCox") 
 {ft_list <- cria_flextable_boxcox(tabela1)
  tem_tabela <- !is.null(ft_list)}

  monta_ppt_process(
    ft              = if (tem_tabela) ft_list$ft else NULL,
    tem_tabela      = tem_tabela,
    titulo          = titulo, 
    caminho_arquivo = caminho_arquivo,
    caminho_map     = caminho_map
  )
}