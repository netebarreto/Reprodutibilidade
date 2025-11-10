######### 
#' Contagem de Valores Ausentes
#'
#' Conta a quantidade de valores `NA` em um vetor numerico ou dataframe.
#'
#' @param x Vetor, matriz ou coluna de dataframe a ser avaliada.
#'
#' @return Um numero inteiro indicando a quantidade de elementos ausentes (`NA`).
#'
#' @examples
#' total.na(c(1, NA, 3, NA, 5))
#' # Retorna: 2
#'
#' @export
 total.na <- function(x){length(x[is.na(x)]) }


#' Maior Correlacao de uma Variavel
#'
#' Dada uma matriz de correlacao, identifica a variavel que apresenta
#' a maior correlacao com a variavel de interesse.
#'
#' @param var_name String. Nome da variavel de interesse (deve estar entre as
#'                 colunas/linhas da matriz de correlacao).
#' @param cor_matrix Matriz de correlacao (quadrada e simetrica), com nomes
#'                   de colunas/linhas correspondendo as variaveis.
#'
#' @return Um vetor nomeado com:
#' \itemize{
#'   \item \code{max_pcor}: valor maximo de correlacao encontrado.
#'   \item \code{var_max}: nome da variavel mais correlacionada.
#' }
#'
#' @examples
#' mat <- cor(mtcars)
#' get_max_cor("mpg", mat)
#'
#' @export
  get_max_cor <- function(var_name,cor_matrix=NULL) 
    {
      idx <- which(colnames(cor_matrix) == var_name)
      cor_vals <- cor_matrix[-idx, idx]
      max_val <- max(cor_vals)
      max_var <- names(cor_vals)[which.max(cor_vals)]
      return(c(max_pcor = max_val, var_max = max_var))
    }

# Correlacao Total
#' Correlacao de Spearman entre Variaveis
#'
#' Calcula a matriz de correlacao de Spearman e identifica, para cada variavel,
#' qual e a outra variavel mais correlacionada.
#'
#' @param X Data frame ou matriz numerica com variaveis continuas.
#'
#' @return Uma lista contendo:
#' \itemize{
#'   \item \code{cor_mat}: matriz de correlacao absoluta.
#'   \item \code{p_mat}: matriz de valores-p da correlacao.
#'   \item \code{cor_summary}: resumo indicando a correlacao maxima de cada variavel
#'         e o respectivo par.
#' }
#'
#' @examples
#' res <- ADPcorrel(mtcars)
#' head(res$cor_summary)
#'
#' @export
  ADPcorrel <- function(X) 
  { res <- Hmisc::rcorr(as.matrix(X), type = "spearman")
    cor_mat <- abs(res$r)
    p_mat <- res$P
    cor_summary <- t(sapply(colnames(X), cor_matrix = cor_mat,get_max_cor))
    colnames(cor_summary) <- c("Cor_Max", "Cor_Par") 
    res_cor<-list(cor_mat = cor_mat,p_mat = p_mat,cor_summary=cor_summary)
    return(res_cor)
  }


#' Avaliacao de Indicadores: NA, Correlacao
#'
#' Funcao que integra diferentes metricas de qualidade dos indicadores:
#' \itemize{
#'   \item Contagem de valores ausentes (NA).
#'   \item Correlacao de Spearman.
#' }
#'
#' @param x Data frame ou matriz numerica com variaveis continuas.
#'
#' @return Lista contendo:
#' \itemize{
#'   \item \code{Contagem_NA}: numero de NAs por variavel.
#'   \item \code{Indicadores_NA}: variaveis removidas por excesso de NA.
#'   \item \code{Correl}: matriz de correlacao absoluta.
#'   \item \code{p.valueCor}: matriz de p-values da correlacao.
#'   \item \code{Resumo}: tabela consolidada com todas as metricas e sugestao de remocao.
#' }
#' @importFrom stats na.exclude
#' @examples
#' res <- correl_ind(mtcars)
#' head(res$Resumo)
#'
#' @export
correl_ind <- function(x) {

  cont.NA = apply(x,2,total.na)
  col_select <- names(subset(cont.NA,cont.NA<=55))
  indicadores_removidas_NAs <- names(cont.NA)[-match(col_select,names(cont.NA))]
  dados <- na.exclude(x[,col_select])

  cor_summary <- ADPcorrel(dados)


# Construcao da tabela final
resumo_df <- data.frame(
  Indicador = colnames(dados),
  Cor_Max = round(as.numeric(cor_summary$cor_summary[, "Cor_Max"]), 3),
  Cor_Par_Indicador = cor_summary$cor_summary[, "Cor_Par"],
  row.names = NULL
)

# Sugestao de remocao com base em criterios combinados
resumo_df <- resumo_df |>
  dplyr::mutate(Sugestao_Remocao = dplyr::case_when(
    Cor_Max >= 0.6 ~ "Remover (Correl alta)",
    TRUE ~ "Manter"
  ))


result_cor<- list(Contagem_NA = cont.NA,
Indicadores_NA =indicadores_removidas_NAs,
Correl = cor_summary$cor_mat,
p.valueCor = cor_summary$p_mat,
Resumo = resumo_df)

message("\n   Metricas de Avaliacao Calculadas.\n")
return(result_cor)
}
##################
#' Figura da Contagem de Valores Ausentes (NA)
#'
#' Gera grafico de barras mostrando a quantidade de valores ausentes por variável.
#'
#' @param Y Vetor nomeado com contagem de NAs por variável.
#' @param nfile Nome do arquivo de saída (PNG).
#' @param visivel logical, se TRUE o grafico e exibido no prompt.
#'
#' @return Nenhum objeto retornado. Salva a figura em arquivo PNG.
#'
#' @examples
#' cont_na <- apply(mtcars, 2, function(x) sum(is.na(x)))
#' FigContNA(cont_na, "ContNA.png",visivel=TRUE)
#'
#' @export
FigContNA = function(Y,nfile = "FigContNA.png",visivel=TRUE)
{
 
data.fr <- data.frame(Categorias =names(Y)  , 
                      Value = Y)

# Barplot using ggplot2
plot = ggplot2::ggplot(data.fr, ggplot2::aes(x = Categorias, y = Value)) +
  ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
  ggplot2::labs(title = "Total de NA's por Indicador Simples",
       x = "Indicadores Simples",
       y = "Numero de NA's") +  ggplot2::scale_y_continuous(
    limits = c(0, 6000),  # Começa em 0 e vai até um valor maior que o máximo
    breaks = seq(0, 6000, by = 1000)  # Mais labels no eixo Y
  )+
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),   # Labels do eixo X
    axis.text.y = ggplot2::element_text(size = 12),                          # Labels do eixo Y
    axis.title = ggplot2::element_text(size = 14, face = "bold"),            # Títulos dos eixos
    plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5), # Título principal
    panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)  # Borda ao redor
  )

ggplot2::ggsave(nfile, plot = plot, width = 10, height = 6, dpi = 600,bg="white")
if(visivel) { print(plot)}
message("\n       Figura de Contagem de NA's Gerada\n")
}

#' Mapa de Correlacao
#'
#' Gera figura de correlacao (com \code{corrplot}) para todas as variaveis.
#'
#' @param Y Matriz de correlacao.
#' @param tipo Tipo de correlacao a ser mostrada: "Total" (parte inferior) ou "Parcial" (parte superior).
#' @param save logical, se TRUE o grafico sera salvo.
#' @param nfile Nome do arquivo de saida (PNG).
#' @param visivel logical, se TRUE o grafico e exibido no prompt.
#'
#' @return Nenhum objeto retornado. Salva a figura em arquivo PNG.
#'
#' @examples
#' res <- correl_ind(mtcars)
#' FigCorrelPlot(res$Correl, tipo = "Total", save=TRUE,nfile = "Correl.png",visivel=TRUE)
#'
#' @importFrom grDevices colorRampPalette dev.off png 
#' @importFrom graphics mtext
#' @export
FigCorrelPlot <- function(Y,tipo="Total",save=TRUE,nfile = "output.png",visivel=TRUE)
{ 
lado = ifelse(tipo=="Total", "lower","upper")

if(save==TRUE) { 
png(nfile,width = 2000, height = 2000,res = 300,bg="white")

corrplot::corrplot(Y,
         method = "circle",
         type = lado,
         col = colorRampPalette(c("beige","beige","sienna1", "firebrick"))(100),
         tl.cex = 0.8,
         tl.col = "black",
         order = "original",
         col.lim=c(0,1),
         mar=c(0,0,1,0))
mtext(paste0("Correlacao ",tipo," entre Indicadores Simples"),side=3,cex=1.2,line=2.5)         
mtext("Indicadores Simples",side=2,cex=1,line=2.5)
dev.off()
}

if(visivel)
{
corrplot::corrplot(Y,
         method = "circle",
         type = lado,
         col = colorRampPalette(c("beige","beige","sienna1", "firebrick"))(100),
         tl.cex = 0.7,
         tl.col = "black",
         order = "original",
         col.lim=c(0,1),
         mar=c(0,0,1,0))
mtext(paste0("Correlacao ",tipo," entre Indicadores Simples"),side=3,cex=1,line=2.5)         
mtext("Indicadores Simples",side=2,cex=0.8,line=2.5)
}
message("\n       Figura de Correlacao (",tipo,") Gerada.\n")
} 
