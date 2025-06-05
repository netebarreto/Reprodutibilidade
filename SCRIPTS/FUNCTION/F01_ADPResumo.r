
library(dplyr)
library(tidyr)

# Função para criar resumos estatísticos
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
    }) %>% dplyr::bind_rows()
    
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

summary <- summary %>%
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

summary <- summary %>%
  dplyr::rename(
    `Quartil 1` = quartil1,
    `Quartil 3` = quartil3
  )
  return(summary)
}


# Função principal para gerar resumos brutos
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
  
  return(result_resumo)
}

