
######### 


 total.na <- function(x){length(x[is.na(x)]) }

 get_max_cor <- function(var_name,cor_matrix=NULL) {
  idx <- which(colnames(cor_matrix) == var_name)
  cor_vals <- cor_matrix[-idx, idx]
  max_val <- max(cor_vals)
  max_var <- names(cor_vals)[which.max(cor_vals)]
  return(c(max_pcor = max_val, var_max = max_var))
}

  cont.NA = apply(normal,2,total.na)
  col_select <- names(subset(cont.NA,cont.NA<=55))
  indicadores_removidas_NAs <- names(cont.NA)[-match(col_select,names(cont.NA))]
  dados <- na.exclude(normal[,col_select])

  res <- Hmisc::rcorr(as.matrix(dados), type = "spearman")
  cor_mat <- abs(res$r)
  p_mat <- res$P
  cor_summary <- t(sapply(colnames(dados), cor_matrix = cor_mat,get_max_cor))
  colnames(cor_summary) <- c("Cor_Max", "Cor_Par")

# correlação parcial

  pcorrel  <- abs(corpcor::pcor.shrink(as.data.frame(dados),verbose=FALSE))
  pcor_summary <- t(sapply(colnames(dados), cor_matrix = pcorrel,get_max_cor))
  colnames(pcor_summary) <- c("Pcor_Max", "Pcor_Par")


# VIF Values

  lm_model <- lm(rep(1, nrow(dados)) ~ ., data = as.data.frame(dados))
  vif_vals <- car::vif(lm_model)
  vif_df <- data.frame(Indicador = names(vif_vals), VIF = round(vif_vals, 3))

# alpha Values

  alpha_all <- psych::alpha(dados,check.keys=TRUE)
  alpha_drop <- alpha_all$alpha.drop
  alpha_df <- data.frame(Indicador = rownames(alpha_drop), Alpha_if_Dropped = round(alpha_drop[,"raw_alpha"], 3))


alpha_drop <- alpha_all$alpha.drop[, "raw_alpha"]

# Construção da tabela final
resumo_df <- data.frame(
  Indicador = colnames(dados),
  Cor_Max = round(as.numeric(cor_summary[, "Cor_Max"]), 3),
  Cor_Par_Indicador = cor_summary[, "Cor_Par"],
  Pcor_Shrink_Max = round(as.numeric(pcor_summary[, "Pcor_Max"]), 3),
  Pcor_Par_Indicador = pcor_summary[, "Pcor_Par"],
  VIF = round(vif_vals, 3),
  Alpha_Drop = round(alpha_drop, 3),
  row.names = NULL

)

# Alpha total (reutilizando da análise anterior)
alpha_total_val <- alpha_all$total$raw_alpha

# Sugestão de remoção com base em critérios combinados
resumo_df <- resumo_df %>%
  mutate(Sugestao_Remocao = case_when(
    Cor_Max >= 0.6 ~ "Remover (Correl alta)",
    VIF >= 5 ~ "Remover (VIF alto)",
    Pcor_Shrink_Max >= 0.6 ~ "Remover (Pcor alta)",
    Alpha_Drop > alpha_total_val ~ "Remover (melhora alpha)",
    TRUE ~ "Manter"
  ))

indicadores_invertidos<- alpha_all$keys[[1]][grep("-",unlist(alpha_all$keys))]

result_cor<- list(Indicadores_NA =indicadores_removidas_NAs,
Correl = cor_mat,
p.valueCor = p_mat,
pcorrel = pcorrel,
VIF = vif_df,
Alpha_Drop = alpha_drop,
Indicadores_Invertidos = indicadores_invertidos, 
Resumo = resumo_df)

