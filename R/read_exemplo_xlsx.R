#' @title Ler arquivo de exemplo incluído no pacote
#'
#' @description
#' Carrega o arquivo Excel de exemplo \code{Base_inicial_SA_Acesso.xlsx} que acompanha o pacote,
#' contendo as planilhas de metadados e dados utilizados para demonstração das funções.
#'
#' @details
#' Esta função é útil para testar e exemplificar o fluxo completo de análise
#' (leitura, resumo, winsorização, normalização e Box-Cox) sem necessidade de
#' carregar dados externos.  
#' O arquivo de exemplo está localizado internamente em:
#' \code{inst/dataset/Base_inicial_SA_Acesso.xlsx}
#'
#' O arquivo contém duas planilhas:
#' \itemize{
#'   \item \strong{"metadados"} — informações descritivas sobre as variáveis.
#'   \item \strong{"dados_SA_Acesso"} — base de dados com indicadores de acesso e atributos numéricos.
#' }
#'
#' @return Uma lista contendo:
#' \describe{
#'   \item{File}{Objeto \code{Workbook} carregado com \code{openxlsx}.}
#'   \item{metadados}{\code{data.frame} com a planilha de metadados.}
#'   \item{dataset}{\code{data.frame} com a planilha de dados.}
#' }
#'
#' @examples
#' # Carregar o arquivo de exemplo do pacote
#' dados_exemplo <- read_exemplo_xlsx()
#'
#' # Visualizar os metadados
#' head(dados_exemplo$metadados)
#'
#' # Visualizar o dataset
#' head(dados_exemplo$dataset)
#'
#' @export
read_exemplo_xlsx <- function() {
  caminho_arquivo <- system.file("dataset", "Base_inicial_SA_Acesso.xlsx", package = "reprodutibilidade")
  inxlsx <- openxlsx::loadWorkbook(file = caminho_arquivo)
  metadados <- openxlsx::read.xlsx(inxlsx, sheet = "metadados")
  dataset <- openxlsx::read.xlsx(inxlsx, sheet = "dados_SA_Acesso")
  
  message("\n✔ Arquivo de exemplo 'Base_inicial_SA_Acesso.xlsx' carregado com sucesso!")
  message("📂 O arquivo foi lido a partir do pacote 'reprodutibilidade'.")
  message("📑 As planilhas disponíveis são: 'metadados' e 'dados_SA_Acesso'.")
  message("💡 Acesse-as com: resultado$metadados ou resultado$dataset\n")
  result <- list(
    File = inxlsx,
    metadados = metadados,
    dataset = dataset
  )
  
  return(result)
}
