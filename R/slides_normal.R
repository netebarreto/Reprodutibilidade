#' Gera um slide PowerPoint contendo uma tabela genérica e opcionalmente um mapa e um gtrafico
#'
#' Função de alto nível que cria o flextable e monta o slide em um único passo.
#'
#' @param datain Data frame a ser aplicado os textes.
#' @param titulo Titulo do slide.
#' @param title_tab Titulo da tabela 
#' @param caminho_arquivo Caminho de saida do arquivo `.pptx`.
#' @param caminho_map Caminho opcional para imagem de mapa.
#' @param caminho_grafico Caminho opcional para o grafico.
#'
#' @return Um arquivo PowerPoint salvo no caminho especificado.
#'
#' @examples
#' \dontrun{
#' slides_normal(datain=result$Data_Bxc$data[[icode]],
#'  titulo          = "Titulo Slide",
#'  title_tab       = "Testes Estatísticos",
#'  caminho_arquivo = "saida_apresentacao.pptx"),
#'  caminho_map     = "mapa_normalizado_T1.png" ,
#'  caminho_grafico  ="grafico_final_T1.png" )
#' }
#'
#' @export
slides_normal <- function(datain=datain,
  titulo          = "Titulo Slide",
  title_tab       = "Titulo Tabela",
  caminho_arquivo = "saida_apresentacao.pptx",
  caminho_map     = NULL ,
  caminho_grafico  = NULL ) {

    res_tests <- cria_flextable_normalidade(datain)
    ft_tests <- res_tests$ft
    tem_test <- !is.null(ft_tests)
   
  monta_ppt_normal(ft_tests,
                tem_test,
                titulo = titulo,
                title_tab=title_tab,
                caminho_arquivo = caminho_arquivo,
                caminho_grafico = caminho_grafico,
                caminho_map=caminho_map)
}
