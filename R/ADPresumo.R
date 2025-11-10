#########

    #' @title Gerar resumo estatistico para um indicador ou indice especifico,
    #',
    #' @description Esta funcao aplica resumos estatisticos a multiplas variaveis de um data frame.
    #',
    #' @param dataset Um data frame contendo os dados (colunas numericas).
    #' @param class_type Um vetor com os tipos de classe para cada coluna (ex: "Numerico")."
    #' @param name Vetor com os nomes descritivos de cada coluna de `dataset`.
    #'
    #' @return Um tibble (tabela moderna do R, semelhante a um data.frame) com os resumos estatisticos completos para cada variavel.
    #' \describe{
#'       \item{Min}{Valor minimo}
#'       \item{Quartil 1}{Primeiro Quartil}
#'       \item{Mediana}{Valor mediano}
#'       \item{Quartil 3}{Terceiro Quartil}
#'       \item{Max}{Valor maximo}
#'       \item{Outliers_Per}{Percentual de outliers do indicador/indice}
#'       \item{NAs}{Total de valores ausentes (NAs)}
#'       \item{Valores_Unicos}{Total de valores unicos}}
#' @importFrom rlang .data       
#'
#' @examples
#' # Criando um data frame de exemplo
#' dados <- data.frame(Pessoas = c(25, 30, 28, 35, 40, 28, 35, 40))
#' 
#' # Chamando a funcao com parametros ficticios
#' criar_resumo(dados$Pessoas,
#'             "Numerico",
#'              "Pessoas")
#'
#' @export
criar_resumo <- function(dataset,class_type,name) {
  
  #percentual de outlines

  # Contagem de NAs
  na_count <- sum(is.na(dataset))
  na_percent <- round(na_count / length(dataset) * 100, 2)
  
  # Contagem de valores unicos
  unique_count <- length(unique(dataset[!is.na(dataset)]))
  unique_percent <- round(unique_count / length(dataset[!is.na(dataset)]) * 100, 2)
  
    fnum <- grDevices::boxplot.stats(dataset)$stats
    per_out <- round(length(grDevices::boxplot.stats(dataset)$out)/length(dataset) * 100, 2)
    summary <- tidyr::tibble(
      Nome = name,
      Classe = "Numerico",
      Min = fnum[1],
      quartil1 = fnum[2],
      Mediana = fnum[3],
      quartil3 = fnum[4],
      Max = fnum[5],
      Outliers_Per = per_out,
      NAs = na_count,
      Percentual_NAs = na_percent,
      Valores_Unicos = unique_count,
      Percentual_Unicos = unique_percent)
   
  summary <- summary |>
    dplyr::rename(
      `Quartil 1` = .data$quartil1,
      `Quartil 3` = .data$quartil3
    )
texto <- utils::capture.output(print(summary[1:2]))
message(paste(" ",texto[c(2,4)]," ", collapse = "\n"))
  return(summary)
}

#' Imprimir resumo amigavel no console
#'
#' @description Formata e imprime um resumo (por exemplo, saida transposta de listas/data.frames)
#' com cabecalhos e alinhamento simples.
#'
#' @param result_resumo Objeto tipo matrix/data.frame (1 coluna) ou data.frame com campos e valores.
#'
#' @return Invisivelmente, retorna \code{NULL}. Efeito colateral e a impressao no console.
#' @export
#' @examples
#' result_resumo <- list(iName="MMPD", Classe="Numerico", Min="2.33")
#' print_summary(result_resumo)
print_summary <- function(result_resumo) {
  # Aceita tanto lista quanto data.frame transposto como no exemplo

  summary_data = t(as.data.frame(result_resumo))

  if (is.matrix(summary_data) || is.data.frame(summary_data)) {
    summary_data <- as.data.frame(summary_data)
  }
  
  # Se tiver apenas uma coluna (como no seu caso), simplifica a estrutura
  if (ncol(summary_data) == 1) {
    summary_data <- data.frame(
      Campo = rownames(summary_data),
      Valor = as.character(summary_data[, 1]),
      row.names = NULL
    )
  } else {
    colnames(summary_data) <- c("Campo", "Valor")
  }
  
  # Titulo amigavel
  cat("\n  Resumo Estatistico da Variavel\n")
  cat(strrep(" ", 40), "\n", sep = "")
  
  # Nome da variavel (se existir)
  if ("Nome" %in% summary_data$Campo) {
    var_name <- summary_data$Valor[summary_data$Campo == "Nome"]
    cat(" Variavel:", var_name, "\n")
  }
  if ("Classe" %in% summary_data$Campo) {
    var_class <- summary_data$Valor[summary_data$Campo == "Classe"]
    cat(" Classe:", var_class, "\n")
  }
  cat(strrep(" ", 40), "\n", sep = "")
  
  # Tabela organizada
  suppressWarnings({
    for (i in seq_len(nrow(summary_data))) {
      field <- summary_data$Campo[i]
      value <- summary_data$Valor[i]
      if (!field %in% c("Nome", "Classe")) {
        cat(sprintf("%-20s : %s\n", field, value))
      }
    }
  })
  
  cat(strrep(" ", 40), "\n\n", sep = "")
}


###############################
#' @title Gerar resumos estatisticos para multiplos indicadores
#'
#' @description
#' Aplica a funcao \code{criar_resumo} a cada coluna de um \code{data.frame},
#' gerando tabelas com os resultados organizados.
#'
#' @param dataset Um \code{data.frame} contendo os indicadores.
#' @param class_types Vetor de caracteres com a classe/tipo de cada indicador (mesma ordem das colunas de \code{dataset}).
#' @param names Vetor de nomes dos indicadores (mesma ordem das colunas de \code{dataset}).
#'
#' @details
#' - O numero de colunas de \code{dataset} deve ser igual ao comprimento de \code{class_types} e \code{names}.
#' - Valores \code{NA} nos resumos sao substituidos por "-".
#'
#' @return Uma lista com tres elementos:
#' \describe{
#'   \item{resumo_total}{\code{data.frame} com todos os resumos completos.}
#'   \item{resumo_basico}{\code{data.frame} transposto com estatisticas basicas (linhas: medidas, colunas: indicadores).}
#'   \item{resumo_na}{\code{data.frame} com informacoes sobre valores ausentes, outliers e valores unicos.}
#' }
#'
#' @examples
#' # Exemplo ficticio (assumindo que criar_resumo ja esteja implementada)
#' dados <- data.frame(
#'   Pessoas = c(25, 30, 28, 35, 40, 28, 35, 40),
#'   Vendas = c(100, 200, 150, NA, 180, 175, 190, 210)
#' )
#' class_types <- c("Numerico", "Numerico")
#' nomes <- c("Pessoas", "Vendas")
#'
#' # Gerar resumos
#' ADPresumo(dados, class_types, nomes)
#'
#' @export
ADPresumo <- function(dataset, class_types, names) {
  if (ncol(dataset) != length(class_types) || ncol(dataset) != length(names)) {
    stop("Number of columns in dataset must match the length of class_types and names.")
  }
  
  resumo <- lapply(seq_along(names), function(i) {
    suppressMessages(criar_resumo(dataset[[i]], class_types[i], names[i]))
  })
  
  resumo_combinado <- dplyr::bind_rows(resumo)
  
  # Substituir NA por -
  resumo_combinado <- resumo_combinado |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), "-", .)))
  
  
  res_basico <- as.data.frame(t(resumo_combinado)[2:8,]) 
  colnames(res_basico) <- resumo_combinado$Nome
  
  resumo_na<-resumo_combinado[,c(1,9:12)]
  
  result_resumo <- list(resumo_total = resumo_combinado,
                        resumo_basico = res_basico,
                        resumo_na = resumo_na)
  message("\n Resumo Geral Obtido \n")
  return(result_resumo)
}

