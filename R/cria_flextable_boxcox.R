#' Cria um flextable boxcox a partir de uma tabela
#'
#' Calcula larguras das colunas automaticamente e retorna o flextable
#' configurado.
#'
#' @param tab_input Data frame a ser convertido para flextable.
#'
#' @return Lista contendo:
#' \describe{
#'   \item{ft}{flextable gerado}
#'   \item{largura_colunas}{vetor com larguras utilizadas}
#' }
#'
#' @examples
#' \dontrun{
#' cria_flextable_winsorize(mtcars)
#' }
#'
#' @export
cria_flextable_boxcox <- function(tab_input) 
{ 
    if (is.null(tab_input)) { return(NULL) } 
    stopifnot(is.data.frame(tab_input)) 
    tabela1 <- tibble::as_tibble(purrr::map_dfc(tab_input, ~ .x[[1]])) 
    # Cálculo de larguras baseado no conteúdo 

    
    tabela1$BoxCox <- ifelse(tabela1$BoxCox == 0, "Nao foi aplicado BoxCox (Sem Mapa)","BoxCox aplicado"  ) 

    largura_colunas <- sapply( tabela1, function(col) max(nchar(as.character(col)), na.rm = TRUE) ) 
    
    largura_colunas <- ifelse(largura_colunas <= 10, 1, 2) 

    ft <- flextable::flextable(tabela1) |> 
    flextable::width(j = 1:ncol(tabela1), width = largura_colunas) |> flextable::bold(i = 1, j = 1:ncol(tabela1), part = "header") |> flextable::height(i = 1:nrow(tabela1), height = 0.5) 
    list( ft = ft, largura_colunas = largura_colunas ) 
}
