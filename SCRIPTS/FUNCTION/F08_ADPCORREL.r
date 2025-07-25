
######### 


 total.na <- function(x){length(x[is.na(x)]) }

  get_max_cor <- function(var_name,cor_matrix=NULL) 
    {
      idx <- which(colnames(cor_matrix) == var_name)
      cor_vals <- cor_matrix[-idx, idx]
      max_val <- max(cor_vals)
      max_var <- names(cor_vals)[which.max(cor_vals)]
      return(c(max_pcor = max_val, var_max = max_var))
    }

  # Correlação Parcial
  ADPcorrel <- function(X) 
  { res <- Hmisc::rcorr(as.matrix(X), type = "spearman")
    cor_mat <- abs(res$r)
    p_mat <- res$P
    cor_summary <- t(sapply(colnames(X), cor_matrix = cor_mat,get_max_cor))
    colnames(cor_summary) <- c("Cor_Max", "Cor_Par") 
    res_cor<-list(cor_mat = cor_mat,p_mat = p_mat,cor_summary=cor_summary)
    return(res_cor)
  }

  # correlação parcial
  ADPcorrel_parcial <- function(X) 
  { pcorrel  <- abs(corpcor::pcor.shrink(as.data.frame(X),verbose=FALSE))
    pcor_summary <- t(sapply(colnames(X), cor_matrix = pcorrel,get_max_cor))
    colnames(pcor_summary) <- c("Pcor_Max", "Pcor_Par")
    res_pcor<-list(pcorrel = pcorrel,pcor_summary = pcor_summary)
    return(res_pcor) 
  }

# VIF Values

  ADPvif<- function(X) 
  { lm_model <- lm(rep(1, nrow(X)) ~ ., data = as.data.frame(X))
    vif_vals <- car::vif(lm_model)
    vif_df <- data.frame(Indicador = names(vif_vals), VIF = round(vif_vals, 3))
    return(vif_df) }

# alpha Values

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


correl_ind <- function(x) {

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

# Sugestão de remoção com base em critérios combinados
resumo_df <- resumo_df %>%
  mutate(Sugestao_Remocao = case_when(
    Cor_Max >= 0.6 ~ "Remover (Correl alta)",
    VIF >= 5 ~ "Remover (VIF alto)",
    Pcor_Shrink_Max >= 0.6 ~ "Remover (Pcor alta)",
    Alpha_Drop > alpha_df$alpha_total ~ "Remover (melhora alpha)",
    TRUE ~ "Manter"
  ))

indicadores_invertidos<- alpha_df$alpha_all$keys[[1]][grep("-",unlist(alpha_df$alpha_all$keys))]

result_cor<- list(Contagem_NA = cont.NA,
Indicadores_NA =indicadores_removidas_NAs,
Correl = cor_summary$cor_mat,
p.valueCor = cor_summary$p_mat,
pcorrel = pcor_summary$pcorrel,
VIF = vif_df,
AlphaCrobach = alpha_df,
Indicadores_Invertidos = indicadores_invertidos, 
Resumo = resumo_df)

cat("\n   Métricas de Avaliação Calculadas.\n")
return(result_cor)
}

FigContNA = function(Y,nfile = "TESTE.png")
{
   if(!file.exists("FIGs")) dir.create("FIGs")
data.fr <- data.frame(Categorias =names(Y)  , 
                      Value = Y)

# Barplot using ggplot2
plot = ggplot2::ggplot(data.fr, aes(x = Categorias, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total de NA's por Indicador Simples",
       x = "Indicadores Simples",
       y = "Número de NA's") +  scale_y_continuous(
    limits = c(0, 6000),  # Começa em 0 e vai até um valor maior que o máximo
    breaks = seq(0, 6000, by = 1000)  # Mais labels no eixo Y
  )+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),   # Labels do eixo X
    axis.text.y = element_text(size = 12),                          # Labels do eixo Y
    axis.title = element_text(size = 14, face = "bold"),            # Títulos dos eixos
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Título principal
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Borda ao redor
  )

ggsave(nfile, plot = plot, width = 10, height = 6, dpi = 600,bg="white")

cat("\n       Figura de Contagem de NA's Gerada\n")
}

# cor_pos <- result_cor$Correl
# cor_pos[cor_pos < 0] <- 0

FigCorrelPlot <- function(Y,tipo="Total",nfile = "output.png")
{ 
  if(!file.exists("FIGs")) dir.create("FIGs")
  png(nfile,width = 2000, height = 2000,res = 300,bg="white")

lado = ifelse(tipo=="Total", "lower","upper")
corrplot::corrplot(Y,
         method = "circle",
         type = lado,
         col = colorRampPalette(c("beige","beige","sienna1", "firebrick"))(100),
         tl.cex = 0.8,
         tl.col = "black",
         order = "original",
         col.lim=c(0,1),
         mar=c(0,0,1,0))
mtext(paste0("Correlação ",tipo," entre Indicadores Simples"),side=3,cex=1.2,line=2.5)         
mtext("Indicadores Simples",side=2,cex=1,line=2.5)
dev.off()

cat("\n       Figura de Correlação (",tipo,") Gerada.\n")
} 


FigVIF = function(Y,nfile = "TESTE.png")
{
# Preparar dados
if(!file.exists("FIGs")) dir.create("FIGs")
vif_df <- data.frame(Variavel=Y[,1],VIF=Y[,2])

plot = ggplot2::ggplot(vif_df, aes(x = Variavel, y = VIF)) +
  geom_segment(aes(x = Variavel, xend = Variavel, y = 0, yend = VIF), linewidth = 1.5) +  # Barras verticais
  geom_point(color = "blue", size = 4) +
  labs(title = "Fatores de Inflação de Variância - VIF",
       x = "Indicadores Simples",
       y = "VIF") +    # Pontos no topo
  geom_hline(yintercept = 5, linetype = "dashed", color = "blue", linewidth = 1) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1.2) +
  scale_y_continuous(limits = c(0, 15), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

  
ggplot2::ggsave(nfile, plot = plot, width = 10, height = 6, dpi = 600,bg="white")

cat("\n    Figura VIF \n ",nfile,"\n ") 

}



plotAlphaCronbach <- function(alpha_df, alpha_total,nfile = "output.png") {
  if(!file.exists("FIGs")) dir.create("FIGs")
  # Detectar quais foram invertidos
  alpha_df$Invertido <- grepl("-", alpha_df$Indicador)
  alpha_df$Indicador <- gsub("-", "", alpha_df$Indicador)

  # Adicionar * para invertidos
  alpha_df$Indicador_label <- ifelse(alpha_df$Invertido,
                                     paste0(alpha_df$Indicador, "*"),
                                     alpha_df$Indicador)

  # Classificar grupo
  alpha_df <- alpha_df %>%
    mutate(
      Grupo = case_when(
        Alpha_if_Dropped > alpha_total ~ "Remover",
        TRUE ~ "Manter"
      ),
      GrupoExibicao = case_when(
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
 plot <- ggplot(alpha_df, aes(x = Indicador_label,
                       y = Alpha_if_Dropped,
                       fill = GrupoExibicao)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = fill_colors, name = "Sugestão de ação") +
  scale_y_continuous(limits = c(0, 1.2*round(alpha_total,1)), expand = expansion(mult = c(0, 0.05)))+
    geom_hline(yintercept = alpha_total, linetype = "dashed", color = "black") +
    labs(
      title = paste0("Impacto no Alpha de Cronbach (α = ", round(alpha_total, 3), ")"),
      x = "Indicadores (* indica inversão automática)",
      y = "Alpha se removido"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

    
ggplot2::ggsave(nfile, plot = plot, width = 10, height = 6, dpi = 600,bg="white")

cat("\n    Figura Alpha de Cronbach \n ",nfile,"\n ") 

}
