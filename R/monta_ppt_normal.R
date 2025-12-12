#' Monta um slide PowerPoint simples contendo uma tabela e opcionalmente um mapa e um grafico
#'
#' @param ft_tests Flextable a ser inserido no slide.
#' @param tem_test Lógico indicando se tabela deve ser exibida.
#' @param title_tab Titulo da tabela 
#' @param titulo Titulo do slide.
#' @param caminho_arquivo Local de salvamento do arquivo `.pptx`.
#' @param caminho_grafico Caminho opcional para grafico.
#' @param caminho_map Caminho opcional para mapa.
#'
#' @return Arquivo `.pptx` salvo no caminho informado.
#'
#' @export
monta_ppt_normal <- function(ft_tests,
                tem_test,
                title_tab = "Tabela",
                titulo = "Titulo slide",
                caminho_arquivo = "saida_apresentacao.pptx",
                caminho_grafico = NULL,
                caminho_map=NULL) 
{
  if (file.exists(caminho_arquivo)) {
    ppt <- officer::read_pptx(caminho_arquivo)
  } else {
    ppt <- officer::read_pptx()
  }

  # Criar o documento PowerPoint
  ppt <- ppt |>
    officer::add_slide(layout = "Title and Content", master = "Office Theme") |>
    officer::ph_with(
      value    = officer::fpar(officer::ftext(titulo, prop = officer::fp_text(font.size = 22))),
      location = officer::ph_location_type(type = "title")
    )


  if (tem_test) {
    ppt <- ppt |>
            officer::ph_with(ft_tests, location = officer::ph_location(left = 0.3, top = 1.5)) |>
            officer::ph_with(value = officer::fpar(officer::ftext(title_tab, prop = officer::fp_text(font.size = 14, color = "black", bold = TRUE))),location = officer::ph_location(left = 0.3, top = -0.2))
  }

  if (!is.null(caminho_map) && file.exists(caminho_map)) {
    ppt <- ppt |>
      officer::ph_with(
        officer::external_img(caminho_map, width = 10.45, height = 12.75, unit = "cm"),
        location     = officer::ph_location(left = 5.5, top = 2.5),
        use_loc_size = FALSE
      )
  }
  
  
    if (!is.null(caminho_grafico) && file.exists(caminho_grafico)) 
    {
      ppt <- ppt |>
        officer::ph_with(
          officer::external_img(caminho_grafico, width =13, height = 6.5,unit="cm"), location = officer::ph_location(left = 0.3, top = 3.4),use_loc_size = FALSE )  
    }

  # Salvar o arquivo no caminho especificado
  print(ppt, target = caminho_arquivo)
}

