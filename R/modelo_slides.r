#' Abrir modelo de apresentacao do pacote
#'
#' Esta funcao carrega o arquivo .pptx incluído no pacote
#' e retorna um objeto pptx pronto para ser manipulado com o pacote officer.
#'
#' @return Objeto `pptx` (classe do pacote officer).
#' @export
#' @examples
#' ppt <- modelo_pptx()
#' print(ppt)
modelo_pptx <- function() {
  # Localiza o arquivo dentro do pacote
  arquivo <- system.file("templates", "template_slide.pptx", package = "reprodutibilidade")

  if (arquivo == "") {
    stop("O arquivo modelo.pptx nao foi encontrado no pacote.")
  }

  # Le o pptx com officer
  ppt <- officer::read_pptx(arquivo)

  return(ppt)
}
