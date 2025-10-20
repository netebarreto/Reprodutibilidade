#########

    #' @title Gerar resumo estatistico para um indicador ou indice especifico,
    #',
    #' @description Esta funcao aplica resumos estatisticos a multiplas variaveis de um data frame,
    #' com suporte para indicadores do tipo cluster.
    #',
    #' @param dataset Um data frame contendo os dados (colunas numericas).
    #' @param class_type Um vetor com os tipos de classe para cada coluna (ex: "Numerico", "Cluster", "Score")."
    #' @param cluster Um vetor com os grupos de cluster (obrigatorio se usar tipo "Cluster")."
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
#' dados <- data.frame(Pessoas = c(25, 30, 28, 35, 40, 28, 35, 40),
#'                    Cluster = c("1", "2", "1", "1", "2","2","2","1"))
#' 
#' # Chamando a funcao com parametros ficticios
#' criar_resumo(dados$Pessoas,
#'             "Numerico",
#'              dados$cluster,
#'              "Pessoas")
#'
#' @export
criar_resumo <- function(dataset, class_type, cluster = NULL, name) {
  
  #percentual de outlines

  # Contagem de NAs
  na_count <- sum(is.na(dataset))
  na_percent <- round(na_count / length(dataset) * 100, 2)
  
  # Contagem de valores unicos
  unique_count <- length(unique(dataset[!is.na(dataset)]))
  unique_percent <- round(unique_count / length(dataset[!is.na(dataset)]) * 100, 2)
  
  if (class_type == "Numerico") {
    fnum <- grDevices::boxplot.stats(dataset)$stats
    per_out <- round(length(grDevices::boxplot.stats(dataset)$out)/length(dataset) * 100, 2)
    summary <- tidyr::tibble(
      iName = name,
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
      Percentual_Unicos = unique_percent
    )
  } else if (class_type == "Cluster") {
    if (is.null(cluster)) {
      stop("Cluster information must be provided for 'Cluster' class type.")
    }
    
    # Resumo para cada grupo de cluster
    cluster_summary <- lapply(unique(cluster), function(cl) {
      cluster_data <- dataset[cluster == cl & !is.na(dataset)]
      fnum <- grDevices::boxplot.stats(dataset)$stats
      per_out <- round(length(grDevices::boxplot.stats(dataset[cluster == cl])$out)/length(dataset[cluster == cl]) * 100, 2)
      tidyr::tibble(
        iName = name,
        Classe = paste("Grupo", cl),
        Min = round(fnum[1], 2),
        quartil1 = round(fnum[2], 2),
        Mediana = round(fnum[3], 2),
        quartil3 = round(fnum[4], 2),
        Max = round(fnum[5], 2),
        Outliers_Per = per_out,
        NAs = sum(is.na(dataset[cluster == cl])),
        Percentual_NAs = round(sum(is.na(dataset[cluster == cl])) / length(dataset[cluster == cl]) * 100, 2),
        Valores_Unicos = length(unique(cluster_data)),
        Percentual_Unicos = round(length(unique(cluster_data)) / length(cluster_data) * 100, 2)
      )
    }) |> dplyr::bind_rows()
    
    # Resumo para o conjunto completo
    full_summary <- tidyr::tibble(
      iName = name,
      Classe = "Conjunto Completo",
      Min = round(min(dataset, na.rm = TRUE), 2),
      quartil1 = round(stats::quantile(dataset, 0.25, na.rm = TRUE), 2),
      Mediana = round(stats::median(dataset, na.rm = TRUE), 2),
      quartil3 = round(stats::quantile(dataset, 0.75, na.rm = TRUE), 2),
      Max = round(max(dataset, na.rm = TRUE), 2),
      Outliers_Per = round(length(grDevices::boxplot.stats(dataset)$out)/length(dataset) * 100, 2),
      NAs = na_count,
      Percentual_NAs = na_percent,
      Valores_Unicos = unique_count,
      Percentual_Unicos = unique_percent
    )
    
    # Combinar resumos
    summary <- dplyr::bind_rows(full_summary, cluster_summary)
  } else {
    summary <- tidyr::tibble(
      iName = name,
      Classe = "Score",
      Min = NA,
      quartil1 = NA,
      Mediana = NA,
      quartil3 = NA,
      Max = NA,
      Outliers_Per = NA,
      NAs = NA,
      Percentual_NAs = NA,
      Valores_Unicos = NA,
      Percentual_Unicos = NA
    )
  }

  summary <- summary |>
    dplyr::mutate(
      Min = round(.data$Min, 2),
      quartil1 = round(.data$quartil1, 2),
      Mediana = round(.data$Mediana, 2),
      quartil3 = round(.data$quartil3, 2),
      Max = round(.data$Max, 2),
      Outliers_Per = round(.data$Outliers_Per, 2),
      Percentual_NAs = round(.data$Percentual_NAs, 2),
      Percentual_Unicos = round(.data$Percentual_Unicos, 2)
    )
  summary <- summary |>
    dplyr::rename(
      `Quartil 1` = .data$quartil1,
      `Quartil 3` = .data$quartil3
    )
texto <- utils::capture.output(print(summary[1:2]))
message(paste(" ",texto[c(2,4)]," ", collapse = "\n"))
  return(summary)
}

###############################
#' @title Gerar resumos estatísticos para múltiplos indicadores
#'
#' @description
#' Aplica a função \code{criar_resumo} a cada coluna de um \code{data.frame},
#' gerando tabelas com os resultados organizados.
#'
#' @param dataset Um \code{data.frame} contendo os indicadores.
#' @param class_types Vetor de caracteres com a classe/tipo de cada indicador (mesma ordem das colunas de \code{dataset}).
#' @param clusters Vetor ou coluna representando os clusters associados aos dados.
#' @param names Vetor de nomes dos indicadores (mesma ordem das colunas de \code{dataset}).
#'
#' @details
#' - O número de colunas de \code{dataset} deve ser igual ao comprimento de \code{class_types} e \code{names}.
#' - Valores \code{NA} nos resumos são substituídos por "-".
#'
#' @return Uma lista com três elementos:
#' \describe{
#'   \item{resumo_total}{\code{data.frame} com todos os resumos completos.}
#'   \item{resumo_basico}{\code{data.frame} transposto com estatísticas básicas (linhas: medidas, colunas: indicadores).}
#'   \item{resumo_na}{\code{data.frame} com informações sobre valores ausentes, outliers e valores únicos.}
#' }
#'
#' @examples
#' # Exemplo fictício (assumindo que criar_resumo já esteja implementada)
#' dados <- data.frame(
#'   Pessoas = c(25, 30, 28, 35, 40, 28, 35, 40),
#'   Vendas = c(100, 200, 150, NA, 180, 175, 190, 210)
#' )
#' class_types <- c("Numérico", "Numérico")
#' clusters <- c("1", "2", "1", "1", "2", "2", "2", "1")
#' nomes <- c("Pessoas", "Vendas")
#'
#' # Gerar resumos
#' ADPresumo(dados, class_types, clusters, nomes)
#'
#' @export
ADPresumo <- function(dataset, class_types, clusters, names) {
  if (ncol(dataset) != length(class_types) || ncol(dataset) != length(names)) {
    stop("Number of columns in dataset must match the length of class_types and names.")
  }
  
  resumo <- lapply(seq_along(names), function(i) {
    suppressMessages(criar_resumo(dataset[[i]], class_types[i], clusters, names[i]))
  })
  
  resumo_combinado <- dplyr::bind_rows(resumo)
  
  # Substituir NA por -
  resumo_combinado <- resumo_combinado |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), "-", .)))
  
  
  res_basico <- as.data.frame(t(resumo_combinado)[2:8,]) 
  colnames(res_basico) <- resumo_combinado$iName
  
  resumo_na<-resumo_combinado[,c(1,9:12)]
  
  result_resumo <- list(resumo_total = resumo_combinado,
                        resumo_basico = res_basico,
                        resumo_na = resumo_na)
  message("\n Resumo Geral Obtido \n")
  return(result_resumo)
}

