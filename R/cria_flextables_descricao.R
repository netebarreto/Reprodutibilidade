#' Cria objetos flextable a partir do resultado de analise
#'
#' Esta funcao constroi tres tabelas de apoio (metadados, resumo estatistico
#' e informacoes de valores ausentes) a partir do objeto 'result' e do codigo
#' de variavel 'icode', e em seguida gera os respectivos objetos 'flextable'.
#'
#' A partir de:
#' - 'result$metadados': seleciona a linha correspondente a 'icode' e remove
#'   a sétima coluna, formando 'tabela1';
#' - 'result$resumo$resumo_basico': monta 'tabela2' com o nome das linhas como
#'   "Resumo Estatistico" e a primeira coluna como "Descritivo";
#' - 'result$Resumo$resumo_na': extrai a primeira linha (exceto a primeira
#'   coluna) para formar 'tabela3'.
#'
#' Em seguida, cada uma dessas tabelas e formatada com o pacote 'flextable'
#' para uso em relatorios ou apresentacoes (por exemplo, slides em PowerPoint).
#'
#' @param result Lista contendo os componentes 'metadados' e 'Resumo',
#'   tipicamente produzida por funcoes de analise anteriores. Espera-se que
#'   'result$metadados' seja um data.frame com a coluna 'CODE' e que
#'   'result$Resumo' contenha pelo menos os elementos 'resumo_basico' e 'resumo_na'.
#' @param icode Codigo da variavel a ser filtrada na tabela de metadados
#'   ('result$metadados$CODE'). Pode ser caracter ou numerico, desde que
#'   seja compativel com a coluna 'CODE'.
#'
#' @return Uma lista com os seguintes elementos:
#' \itemize{
#'   \item \code{ft}: objeto \code{flextable} construido a partir de 'tabela1'
#'   (metadados da variavel selecionada);
#'   \item \code{ft2}: objeto \code{flextable} construido a partir de 'tabela2'
#'   (resumo estatistico);
#'   \item \code{ft3}: objeto \code{flextable} construido a partir de 'tabela3'
#'   (resumo de valores ausentes);
#'   \item \code{tabela1}: data.frame original usado na construcao de 'ft';
#'   \item \code{tabela2}: data.frame original usado na construcao de 'ft2';
#'   \item \code{tabela3}: data.frame original usado na construcao de 'ft3'.
#' }
#'
#' @importFrom flextable flextable width bold height align merge_at border
#' @importFrom officer fp_border
#' @importFrom tibble tibble
#' @importFrom utils tail
#' 
#' @examples
#' # Exemplo hipotetico:
#' # result <- lista_de_resultados
#' # out <- cria_flextables_descricao(result, icode = "VAR001")
#' # out$ft      # flextable de metadados
#' # out$ft2     # flextable de resumo estatistico
#' # out$ft3     # flextable de resumo de NA
#' @export
cria_flextables_descricao <- function(result, icode) {
  # Extrai e monta as tabelas de entrada
  meta_adapta <- result$metadados
  stopifnot(is.data.frame(meta_adapta))

  tabela1 <- meta_adapta[which(meta_adapta$CODE == icode), -7, drop = FALSE]

  resumo  <- result$Resumo

  tabela2 <- tibble::tibble(
    'Resumo Estatistico' = rownames(resumo$resumo_basico ),
    Descritivo           = resumo$resumo_basico[, 1]
  )

  tabela3 <- resumo$resumo_na[1, -1, drop = FALSE]

  stopifnot(
    is.data.frame(tabela1),
    is.data.frame(tabela2),
    is.data.frame(tabela3)
  )

  # --------- TABELA 1 (ft) ---------
  larguras_base <- c(0.65, 0.65, 1, 4.0, 0.8, 1.4)

  if (ncol(tabela1) <= length(larguras_base)) {
    largura_colunas <- larguras_base[seq_len(ncol(tabela1))]
  } else {
    largura_colunas <- c(
      larguras_base,
      rep(utils::tail(larguras_base, 1), ncol(tabela1) - length(larguras_base))
    )
  }

  ft <- flextable::flextable(tabela1) |>
    flextable::width(j = 1:ncol(tabela1), width = largura_colunas) |>
    flextable::bold(i = 1, j = 1:ncol(tabela1), part = "header") |>
    flextable::height(i = 1:nrow(tabela1), height = 0.5)

  # --------- TABELA 2 (ft2) ---------
  ft2 <- flextable::flextable(tabela2) |>
    flextable::width(j = 1:ncol(tabela2), width = .95) |>
    flextable::merge_at(i = 1, j = 1:ncol(tabela2), part = "header") |>
    flextable::bold(i = 1, j = 1, part = "header") |>
    flextable::align(i = 1, j = 1:ncol(tabela2), align = "center", part = "header") |>
    flextable::border(
      i = 1,
      j = 1:ncol(tabela2),
      border.top = officer::fp_border(color = "black", width = 1),
      part = "all"
    ) |>
    flextable::align(j = 2, align = "right", part = "all") |>
    flextable::height(i = 1, height = 0.5, part = "header") |>
    flextable::height(i = 2:nrow(tabela2), height = 0.35)

  # --------- TABELA 3 (ft3) ---------
  ft3 <- flextable::flextable(tabela3) |>
    flextable::width(j = 1:ncol(tabela3), width = 1.65) |>
    flextable::bold(i = 1, j = 1:ncol(tabela3), part = "header") |>
    flextable::align(j = 1:ncol(tabela3), align = "right", part = "all")

  # Retorna flextables e tabelas cruas
  list(
    ft      = ft,
    ft2     = ft2,
    ft3     = ft3,
    tabela1 = tabela1,
    tabela2 = tabela2,
    tabela3 = tabela3
  )
}