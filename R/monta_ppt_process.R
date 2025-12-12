#' Monta um slide PowerPoint simples contendo uma tabela e opcionalmente um mapa
#'
#' @param ft Flextable a ser inserido no slide.
#' @param tem_tabela Lógico indicando se tabela deve ser exibida.
#' @param process processo do qual sera gerado o slide, pode ser "winsorize" ou "BoxCox" 
#' @param titulo Título do slide.
#' @param caminho_arquivo Local de salvamento do arquivo `.pptx`.
#' @param caminho_map Caminho opcional para mapa.
#'
#' @return Arquivo `.pptx` salvo no caminho informado.
#'
#' @export
monta_ppt_process <- function(
  ft,
  tem_tabela,
  process = NULL,
  titulo          = "Tabela Gerada no R",
  caminho_arquivo = "saida_apresentacao.pptx",
  caminho_map     = NULL
) {

  if (file.exists(caminho_arquivo)) {
    ppt <- officer::read_pptx(caminho_arquivo)
  } else {
    ppt <- officer::read_pptx()
  }

  ppt <- ppt |>
    officer::add_slide(layout = "Title and Content", master = "Office Theme") |>
    officer::ph_with(
      value    = officer::fpar(officer::ftext(titulo, prop = officer::fp_text(font.size = 18))),
      location = officer::ph_location_type(type = "title")
    )

     if(!is.null(process)) {
     title_tab = paste0("Resumo ",process) }
     else
     title_tab = "Resumo" 

  if (tem_tabela) {
    ppt <- ppt |>
            officer::ph_with(ft, location = officer::ph_location(left = 0.5, top = 1.5)) |>
            officer::ph_with(value = officer::fpar(officer::ftext(title_tab, prop = officer::fp_text(font.size = 14, color = "black", bold = TRUE))),location = officer::ph_location(left = 0.5, top = -0.2))
  }

  if (!is.null(caminho_map) && file.exists(caminho_map)) {
    ppt <- ppt |>
      officer::ph_with(
        officer::external_img(caminho_map, width = 11.48, height = 14, unit = "cm"),
        location     = officer::ph_location(left = 2.55, top = 2.1),
        use_loc_size = FALSE
      )
  }

  print(ppt, target = caminho_arquivo)
}
