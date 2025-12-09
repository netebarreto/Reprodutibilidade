#' Monta um slide em PowerPoint com tabelas e imagens
#'
#' Esta funcao adiciona um slide a uma apresentacao PowerPoint existente
#' (ou cria um novo arquivo, caso nao exista) e insere:
#' \itemize{
#'   \item um titulo do slide;
#'   \item uma tabela de metadados (\code{ft});
#'   \item uma tabela de resumo estatistico (\code{ft2});
#'   \item uma tabela de informacoes sobre valores ausentes (\code{ft3});
#'   \item opcionalmente, uma imagem adicional e/ou um mapa.
#' }
#'
#' Se o argumento \code{titulo} for \code{NULL}, o texto do titulo e
#' construido a partir de \code{tabela1$NOME} e do objeto \code{icode}
#' presente no ambiente (por exemplo, \code{"<NOME> - <icode>"}).
#' @param icode sigla do indicador que será calculado.
#' @param ft Objeto \code{flextable} com informacoes de metadados a ser
#'   inserido no slide (tipicamente derivado de uma tabela de descricao
#'   de variavel).
#' @param ft2 Objeto \code{flextable} contendo o resumo estatistico da
#'   variavel, inserido na regiao central do slide.
#' @param ft3 Objeto \code{flextable} com o resumo de valores ausentes
#'   (NA) e valores unicos, inserido na parte inferior do slide.
#' @param tabela2 Data.frame utilizado para controlar a logica de exibicao
#'   do bloco "Resumo Descritivo". Espera-se que possua a coluna
#'   \code{Descritivo}, cuja primeira linha e usada para testar se o
#'   resumo deve ser mostrado (por exemplo, diferente de "Grupo 1" ou
#'   "Grupo 2").
#' @param tabela1 Data.frame de metadados da variavel, usado para
#'   construir o titulo quando \code{titulo} e \code{NULL}. Espera-se
#'   que possua a coluna \code{NOME}.
#' @param titulo Texto do titulo do slide. Se \code{NULL}, o titulo e
#'   gerado automaticamente a partir de \code{tabela1$NOME} e do objeto
#' @param caminho_arquivo Caminho do arquivo PowerPoint (\code{.pptx})
#'   onde o slide sera adicionado. Se o arquivo existir, e carregado;
#'   caso contrario, um novo documento e criado. Valor padrao:
#'   \code{"saida_apresentacao.pptx"}.
#' @param caminho_imagem Caminho para um arquivo de imagem opcional a ser
#'   inserido no slide (por exemplo, um grafico). Se \code{NULL} ou se o
#'   arquivo nao existir, nenhuma imagem adicional e inserida.
#' @param caminho_map Caminho para um arquivo de imagem opcional contendo
#'   um mapa. Se \code{NULL} ou se o arquivo nao existir, o mapa nao e
#'   inserido.
#'
#' @return Nao retorna um objeto R util; a funcao tem efeito colateral
#'   de gravar o arquivo PowerPoint especificado em \code{caminho_arquivo}.
#'
#' @importFrom officer read_pptx add_slide ph_with ph_location
#'   ph_location_type fpar ftext fp_text external_img
#'
#' @seealso \code{\link{cria_flextables_descricao}} para a geracao dos
#'   objetos \code{ft}, \code{ft2} e \code{ft3}.
#'
#' @examples
#' # Exemplo hipotetico:
#' # res <- cria_flextables_descricao(result, icode = "VAR001")
#' # monta_ppt_descricao(
#' #   ft      = res$ft,
#' #   ft2     = res$ft2,
#' #   ft3     = res$ft3,
#' #   tabela2 = res$tabela2,
#' #   tabela1 = res$tabela1,
#' #   caminho_arquivo = "saida_apresentacao.pptx"
#' # )
#' @export
monta_ppt_descricao <- function(
  icode,  
  ft,
  ft2,
  ft3,
  tabela2,
  tabela1,
  titulo          = NULL,
  caminho_arquivo = "saida_apresentacao.pptx",
  caminho_imagem  = NULL,
  caminho_map     = NULL
) {

  if (file.exists(caminho_arquivo)) {
    ppt <- officer::read_pptx(caminho_arquivo)
  } else {
    ppt <- officer::read_pptx()
  }
  if(!is.null(titulo)) {
  ppt <- ppt |>
    officer::add_slide(layout = "Title and Content", master = "Office Theme") |>
    officer::ph_with(
      value    = officer::fpar(officer::ftext(titulo, prop = officer::fp_text(font.size = 22))),
      location = officer::ph_location_type(type = "title")
    )} 
    else 
    { 
    titulo = paste0(tabela1$NOME, " - ", icode)
    ppt <- ppt |>
    officer::add_slide(layout = "Title and Content", master = "Office Theme") |>
    officer::ph_with(
      value    = officer::fpar(officer::ftext(titulo, prop = officer::fp_text(font.size = 22))),
      location = officer::ph_location_type(type = "title")
    )

    }

    ppt <- ppt |>
      officer::ph_with(
        ft,
        location = officer::ph_location(left = 0.5, top = 1.5)
      ) |>
      officer::ph_with(
        value = officer::fpar(officer::ftext(
          "Resumo Descritivo",
          prop = officer::fp_text(font.size = 14, color = "black", bold = TRUE)
        )),
        location = officer::ph_location(left = 0.5, top = -0.2)
      )

  ppt <- ppt |>
    officer::ph_with(
      ft2,
      location = officer::ph_location(left = 0.5, top = 2.5)
    ) |>
    officer::ph_with(
      ft3,
      location = officer::ph_location(left = 0.5, top = 5.9)
    ) |>
    officer::ph_with(
      value = officer::fpar(officer::ftext(
        "Avaliacao de NA e Valores Unicos",
        prop = officer::fp_text(font.size = 13, color = "black", bold = TRUE)
      )),
      location = officer::ph_location(left = 0.5, top = 4.2)
    )

  if (!is.null(caminho_imagem) &&
      nzchar(caminho_imagem) &&
      file.exists(caminho_imagem)) {

    ppt <- ppt |>
      officer::ph_with(
        officer::external_img(caminho_imagem, width = 12, height = 6, unit = "cm"),
        location     = officer::ph_location(left = 2.4, top = 3),
        use_loc_size = FALSE
      )
  }

  if (!is.null(caminho_map) &&
      nzchar(caminho_map) &&
      file.exists(caminho_map)) {

    ppt <- ppt |>
      officer::ph_with(
        officer::external_img(caminho_map, width = 6.3, height = 7.56, unit = "cm"),
        location     = officer::ph_location(left = 7.1, top = 2.6),
        use_loc_size = FALSE
      )
  }

  print(ppt, target = caminho_arquivo)
}
