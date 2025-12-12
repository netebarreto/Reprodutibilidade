#' Cria um flextable boxcox a partir de uma tabela
#'
#' Calcula larguras das colunas automaticamente e retorna o flextable
#' configurado.
#'
#' @param tab_input Data frame a ser convertido para flextable.
#' @param alpha nivel de confianca
#' @return Lista contendo:
#' \describe{
#'   \item{ft}{flextable gerado}
#'   \item{largura_colunas}{vetor com larguras utilizadas}
#' }
#'
#' @examples
#' \dontrun{
#' icode = "MMPD"
#' cria_flextable_normalidade(result$Data_Bxc$data[[icode]])
#' }
#'
#' @export
cria_flextable_normalidade <- function(tab_input, alpha = 0.05) {
  x <- as.numeric(x)
  n = length(x)
  nn = sample(1:n,5000)

  x_shapiro <- if (is.null(nn)) x else x[nn]

  # auxiliar para montar linha quando o teste da certo
  monta_linha_sucesso <- function(nome_teste, res, alpha) {
    estat <- as.numeric(unname(res$statistic))
    p     <- as.numeric(res$p.value)

    # 1) p-valor como texto
    p_txt <- if (is.finite(p) && p < 2.2e-16) {
      "p-value < 2.2e-16"
    } else if (is.finite(p)) {
      sprintf("p-value = %.4g", p)
    } else {
      "p-value indisponivel"
    }

    # 3) interpretaco
    msg <- if (!is.finite(p)) {
      "Nao foi possivel calcular o p-valor."
    } else if (p >= alpha) {
      "Dados podem ser considerados consistentes com uma distribuicao normal (nao se rejeita H0)."
    } else {
      "Dados nao podem ser considerados consistentes com uma distribuicao normal (rejeita-se H0)."
    }

    tibble::tibble(
      Metodo       = nome_teste,
      Estatistica = estat,
      `p-valor`   = p_txt,
      Mensagem    = msg,
      erro        = FALSE
    )
  }

  # auxiliar para montar linha quando o teste NÃO pode ser aplicado
  monta_linha_erro <- function(nome_teste, msg_erro) {
    tibble::tibble(
      Metodo       = nome_teste,
      Estatistica = NA_real_,
      `p-valor`   = NA_character_,
      Mensagem    = msg_erro,
      erro        = TRUE
    )
  }

  ## Anderson-Darling
  ad_res <- tryCatch(
    nortest::ad.test(x),
    error = function(e) e
  )

  linha_ad <- if (inherits(ad_res, "error")) {
    monta_linha_erro(
      "Anderson-Darling Test",
      "Serie de dados incompatevel com o AD.test"
    )
  } else {
    monta_linha_sucesso("Anderson-Darling Test", ad_res, alpha)
  }

  ## Shapiro-Wilk
  sh_res <- tryCatch(
    stats::shapiro.test(x_shapiro),
    error = function(e) e
  )

  linha_sh <- if (inherits(sh_res, "error")) {
    monta_linha_erro(
      "Shapiro-Wilk Test",
      "Serie de dados incompativel com o Shapiro.test"
    )
  } else {
    monta_linha_sucesso("Shapiro-Wilk Test", sh_res, alpha)
  }

  # Tabela final
  tab <- dplyr::bind_rows(linha_ad, linha_sh)

  # remove a coluna auxiliar erro da exibico, mas guarda indice
  idx_erro <- which(tab$erro)
  tab$erro <- NULL

  # monta flextable
  ft <- flextable::flextable(tab)

  # formata Estatistica (opcional)
  ft <- flextable::colformat_num(ft, j = "Estatistica", digits = 4)

  # tirar sombreamento, deixar neutro
  ft <- ft |>
    flextable:: width(j = 1:4, width =c(1.75,1.5,1.5,4.25))|>
    flextable::bg(part = "body", bg = "white") |>
    flextable::bg(part = "header", bg = "grey90") |>
    flextable::hline(part="all") |>
    flextable::bold(part = "header") 
  list(
    tabela = tab,
    ft     = ft
  )
}


