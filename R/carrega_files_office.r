#' Abrir modelo de apresentação do pacote
#'
#' Esta função carrega o arquivo .pptx incluído no pacote
#' e retorna um objeto pptx pronto para ser manipulado com o pacote officer.
#'
#' @return Objeto `pptx` (classe do pacote officer).
#' @export
#' @examples
#' ppt <- modelo_pptx()
#' print(ppt)
carregar_pptx <- function() {
  # Localiza o arquivo dentro do pacote
  arquivo <- system.file("templates", "modelo.pptx", package = "reprodutibilidade")

  if (arquivo == "") {
    stop("O arquivo modelo.pptx nao foi encontrado no pacote.")
  }

  # Lê o pptx com officer
  ppt <- officer::read_pptx(arquivo)

  return(ppt)
}


carregar_xlsx <- function() {
  arq <- system.file("dataset", "Base_inicial_SA_Acesso.xlsx", package = "reprodutibilidade")
  if (arq == "") stop("Arquivo XLSX nao encontrado.")
  openxlsx::read.xlsx(arq)
}
