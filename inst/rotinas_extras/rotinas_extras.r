
######### 

  ADPcorrel_parcial <- function(X) 
  { pcorrel  <- abs(corpcor::pcor.shrink(as.data.frame(X),verbose=FALSE))
    pcor_summary <- t(sapply(colnames(X), cor_matrix = pcorrel,get_max_cor))
    colnames(pcor_summary) <- c("Pcor_Max", "Pcor_Par")
    res_pcor<-list(pcorrel = pcorrel,pcor_summary = pcor_summary)
    return(res_pcor) 
  }


  ADPvif<- function(X) 
  { lm_model <- lm(rep(1, nrow(X)) ~ ., data = as.data.frame(X))
    vif_vals <- car::vif(lm_model)
    vif_df <- data.frame(Indicador = names(vif_vals), VIF = round(vif_vals, 3))
    return(vif_df) }


ADPAlphaCron<- function(X) 
  { 
  alpha_all <-  suppressMessages(suppressWarnings(psych::alpha(X,check.keys=TRUE)))
  alpha_drop <- alpha_all$alpha.drop
  alpha_df <- data.frame(Indicador = rownames(alpha_drop), Alpha_if_Dropped = round(alpha_drop[,"raw_alpha"], 3))
  alpha_drop <- alpha_all$alpha.drop[, "raw_alpha"]
  alpha_total_val <- alpha_all$total$raw_alpha
  alpha_result <- list(alpha_all = alpha_all,alpha_df=alpha_df,alpha_drop =alpha_drop, alpha_total = alpha_total_val )
  return(alpha_result)
  }

##################

FigVIF = function(Y,visivel=FALSE,nfile = NULL)
{
# Preparar dados
if(!file.exists("FIGs")) dir.create("FIGs")
vif_df <- data.frame(Variavel=Y[,1],VIF=Y[,2])

plot = ggplot2::ggplot(vif_df, ggplot2::aes(x = Variavel, y = VIF)) +
  ggplot2::geom_segment(ggplot2::aes(x = Variavel, xend = Variavel, y = 0, yend = VIF), linewidth = 1.5) +  # Barras verticais
  ggplot2::geom_point(color = "blue", size = 4) +
  ggplot2::labs(title = "Fatores de Inflação de Variância - VIF",
       x = "Indicadores Simples",
       y = "VIF") +    # Pontos no topo
  ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "blue", linewidth = 1) +
  ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1.2) +
  ggplot2::scale_y_continuous(limits = c(0, 15), expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, face = "bold", size = 10),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 14, face = "bold"),
    plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1))

    if (!is.null(nfile)) 
    {
    ggplot2::ggsave(filename =nfile, plot = plot, width = 10, height = 6, dpi = 600,bg="white")
    }


  if(isTRUE(visivel)) { print(plot) }

    cat("\n    Figura VIF \n ",nfile,"\n ") 
  }


plotAlphaCronbach <- function(alpha_df, alpha_total,visivel=FALSE,nfile = NULL) {
  if(!file.exists("FIGs")) dir.create("FIGs")
  # Detectar quais foram invertidos
  alpha_df$Invertido <- grepl("-", alpha_df$Indicador)
  alpha_df$Indicador <- gsub("-", "", alpha_df$Indicador)

  # Adicionar * para invertidos
  alpha_df$Indicador_label <- ifelse(alpha_df$Invertido,
                                     paste0(alpha_df$Indicador, "*"),
                                     alpha_df$Indicador)

  # Classificar grupo
  alpha_df <- alpha_df |>
    dplyr::mutate(
      Grupo = dplyr::case_when(
        Alpha_if_Dropped > alpha_total ~ "Remover",
        TRUE ~ "Manter"
      ),
      GrupoExibicao = dplyr::case_when(
        Grupo == "Remover" & Invertido ~ "Remover (invertido)",
        Grupo == "Remover" ~ "Remover",
        Grupo == "Manter" & Invertido ~ "Manter (invertido)",
        Grupo == "Manter" ~ "Manter"
      )
    )

  # Cores de preenchimento e contorno
  fill_colors <- c(
    "Remover" = "#bd0a0aff",
    "Remover (invertido)" = "#F8BDBD",
    "Manter" = "#0f09ccff",
    "Manter (invertido)" = "#919ee8ff"
  )

  # Gráfico com legendas
 plot <- ggplot2::ggplot(alpha_df, ggplot2::aes(x = Indicador_label,
                       y = Alpha_if_Dropped,
                       fill = GrupoExibicao)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7) +
    ggplot2::scale_fill_manual(values = fill_colors, name = "Sugestão de ação") +
    ggplot2::scale_y_continuous(limits = c(0, 1.2*round(alpha_total,1)), expand = ggplot2::expansion(mult = c(0, 0.05)))+
    ggplot2::geom_hline(yintercept = alpha_total, linetype = "dashed", color = "black") +
    ggplot2::labs(
      title = paste0("Impacto no Alpha de Cronbach (α = ", round(alpha_total, 3), ")"),
      x = "Indicadores (* indica inversão automática)",
      y = "Alpha se removido"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1))

    if(isTRUE(visivel)) {print(plot)}

    if (!is.null(nfile)) 
    { ggplot2::ggsave(nfile, plot = plot, width = 10, height = 6, dpi = 600,bg="white") }

cat("\n    Figura Alpha de Cronbach \n ",nfile,"\n ") 

}



resumo_metricas <- function(x) {

  cont.NA = apply(x,2,total.na)
  col_select <- names(subset(cont.NA,cont.NA<=55))
  indicadores_removidas_NAs <- names(cont.NA)[-match(col_select,names(cont.NA))]
  dados <- na.exclude(x[,col_select])

  cor_summary <- ADPcorrel(dados)
  pcor_summary <- ADPcorrel_parcial(dados)
  vif_df <- ADPvif(dados)
  alpha_df<- ADPAlphaCron(dados)


# Construção da tabela final
resumo_df <- data.frame(
  Indicador = colnames(dados),
  Cor_Max = round(as.numeric(cor_summary$cor_summary[, "Cor_Max"]), 3),
  Cor_Par_Indicador = cor_summary$cor_summary[, "Cor_Par"],
  Pcor_Shrink_Max = round(as.numeric(pcor_summary$pcor_summary[, "Pcor_Max"]), 3),
  Pcor_Par_Indicador = pcor_summary$pcor_summary[, "Pcor_Par"],
  VIF = vif_df$VIF,
  Alpha_Drop = round(alpha_df$alpha_drop, 3),
  row.names = NULL
)
  ft <- flextable::flextable(resumo_df)
  ft <- ft |>
    flextable:: width(j = 1:4, width =c(1.75,1.5,1.5,4.25))|>
    flextable::bg(part = "body", bg = "white") |>
    flextable::bg(part = "header", bg = "grey90") |>
    flextable::hline(part="all") |>
    flextable::bold(part = "header") 

result = list(correl_total = cor_summary,
              pcorrel = pcor_summary, 
              VIF =vif_df,
               AlphaCronbach=alpha_df,
               Resumo_geral = resumo_df,
               Res_visivel = ft)

return(result)

}
