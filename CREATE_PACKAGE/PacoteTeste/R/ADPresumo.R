#########

    #' @title Gerar resumo estatístico para um indicador ou índice específico,
    #',
    #' @description Esta função aplica resumos estatísticos a múltiplas variáveis de um data frame,
    #' com suporte para indicadores do tipo cluster.
    #',
    #' @param data Um vetor contendo indicador.
    #' @param class_types classe do indicador que será avaliado (ex: "Numérico", "Cluster", "Score")."
    #' @param clusters Um vetor com os grupos de cluster (obrigatório se usar tipo "Cluster")."
    #' @param names o nome do indicador.
    #'
    #' @return Um tibble (tabela moderna do R, semelhante a um data.frame) com os resumos estatísticos completos para cada variável.
    #' \describe{
#'       \item{Min}{Valor mínimo}
#'       \item{Quartil 1}{Primeiro Quartil}
#'       \item{Mediana}{Valor mediano}
#'       \item{Quartil 3}{Terceiro Quartil}
#'       \item{Max}{Valor máximo}
#'       \item{Outliers_Per}{Percentual de outliers do indicador/indice}
#'       \item{NAs}{Total de valores ausentes (NAs)}
#'       \item{Valores_Unicos}{Total de valores únicos}}
#'
#' @examples
#' # Criando um data frame de exemplo
#' dados <- data.frame(Pessoas = c(25, 30, 28, 35, 40, 28, 35, 40),
#'                    Cluster = c("1", "2", "1", "1", "2","2","2","1"))
#' 
#' # Chamando a função com parâmetros fictícios
#' criar_resumo(dados$Pessoas,
#'             "Numérico",
#'              dados$cluster,
#'              "Pessoas")
#'
#' @export
criar_resumo <- function(data, class_type, cluster = NULL, name) {
  
  #percentual de outlines

  # Contagem de NAs
  na_count <- sum(is.na(data))
  na_percent <- round(na_count / length(data) * 100, 2)
  
  # Contagem de valores únicos
  unique_count <- length(unique(data[!is.na(data)]))
  unique_percent <- round(unique_count / length(data[!is.na(data)]) * 100, 2)
  
  if (class_type == "Numérico") {
    fnum <- boxplot.stats(data)$stats
    per_out <- round(length(boxplot.stats(data)$out)/length(data) * 100, 2)
    summary <- tidyr::tibble(
      iName = name,
      Classe = "Numérico",
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
      cluster_data <- data[cluster == cl & !is.na(data)]
      fnum <- boxplot.stats(data)$stats
      per_out <- round(length(boxplot.stats(data[cluster == cl])$out)/length(data[cluster == cl]) * 100, 2)
      tidyr::tibble(
        iName = name,
        Classe = paste("Grupo", cl),
        Min = round(fnum[1], 2),
        quartil1 = round(fnum[2], 2),
        Mediana = round(fnum[3], 2),
        quartil3 = round(fnum[4], 2),
        Max = round(fnum[5], 2),
        Outliers_Per = per_out,
        NAs = sum(is.na(data[cluster == cl])),
        Percentual_NAs = round(sum(is.na(data[cluster == cl])) / length(data[cluster == cl]) * 100, 2),
        Valores_Unicos = length(unique(cluster_data)),
        Percentual_Unicos = round(length(unique(cluster_data)) / length(cluster_data) * 100, 2)
      )
    }) |> dplyr::bind_rows()
    
    # Resumo para o conjunto completo
    full_summary <- tidyr::tibble(
      iName = name,
      Classe = "Conjunto Completo",
      Min = round(min(data, na.rm = TRUE), 2),
      quartil1 = round(quantile(data, 0.25, na.rm = TRUE), 2),
      Mediana = round(median(data, na.rm = TRUE), 2),
      quartil3 = round(quantile(data, 0.75, na.rm = TRUE), 2),
      Max = round(max(data, na.rm = TRUE), 2),
      Outliers_Per = round(length(boxplot.stats(data)$out)/length(data) * 100, 2),
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
    Min = round(Min, 2),
    quartil1 = round(quartil1, 2),
    Mediana = round(Mediana, 2),
    quartil3 = round(quartil3, 2),
    Max = round(Max, 2),
    Outliers_Per = round(Outliers_Per, 2),
    Percentual_NAs = round(Percentual_NAs, 2),
    Percentual_Unicos = round(Percentual_Unicos, 2)
  )

summary <- summary |>
  dplyr::rename(
    `Quartil 1` = quartil1,
    `Quartil 3` = quartil3
  )
texto <- capture.output(print(summary[1:2]))
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
#' @param data Um \code{data.frame} contendo os indicadores.
#' @param class_types Vetor de caracteres com a classe/tipo de cada indicador (mesma ordem das colunas de \code{data}).
#' @param clusters Vetor ou coluna representando os clusters associados aos dados.
#' @param names Vetor de nomes dos indicadores (mesma ordem das colunas de \code{data}).
#'
#' @details
#' - O número de colunas de \code{data} deve ser igual ao comprimento de \code{class_types} e \code{names}.
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
ADPresumo <- function(data, class_types, clusters, names) {
  if (ncol(data) != length(class_types) || ncol(data) != length(names)) {
    stop("Number of columns in data must match the length of class_types and names.")
  }
  
  resumo <- lapply(seq_along(names), function(i) {
    criar_resumo(data[[i]], class_types[i], clusters, names[i])
  })
  
  resumo_combinado <- dplyr::bind_rows(resumo)
  
  # Substituir NA por -
  resumo_combinado <- resumo_combinado %>%
    dplyr::mutate(across(everything(), ~ ifelse(is.na(.), "-", .)))


res_basico <- as.data.frame(t(resumo_combinado)[2:8,]) 
colnames(res_basico) <- resumo_combinado$iName

resumo_na<-resumo_combinado[,c(1,9:12)]

result_resumo <- list(resumo_total = resumo_combinado,
                      resumo_basico = res_basico,
                      resumo_na = resumo_na)
  message("\n Resumo Geral Obtido \n")
  return(result_resumo)
}

