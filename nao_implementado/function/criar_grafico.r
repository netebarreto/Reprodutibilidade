# Carregar a biblioteca ggplot2
library(ggplot2)
library(gridExtra)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)


criar_grafico <- function(dados, nome_arquivo = "grafico_combinado.png", largura = 10, altura = 5, dpi = 25,nvalores="DD1",fsize=16) {
  # Carregar as bibliotecas necessárias

  
  # Criar um data frame com os dados
  df <- data.frame(valores = na.exclude(dados))
  colnames(df) <- nvalores

  # Criar o boxplot
  boxplot <- ggplot(df, aes(y = !!rlang::sym(nvalores))) +
    geom_boxplot(fill = "lightblue", color = "blue") +
    theme_minimal() +
    labs(x = nvalores, y = "Valores",title = paste0("Boxplot ")) +
    theme(axis.title.x = element_text(size = fsize,vjust=-0.75),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = fsize),  # Aumentar o tamanho da letra do eixo Y
          axis.text.y = element_text(size = 0.8*fsize),
          plot.title = element_text(size = fsize, face = "bold"),
          panel.border = element_rect(color = "black", fill = NA, 
                          linewidth = 2),
      plot.margin = margin(t = 10, r = 10, b = 50, l = 10))  # Aumentar o tamanho da letra dos valores do eixo Y
  
  
  # Criar o histograma com a linha de distribuição normal
  hist_base <- hist(df[,1], plot = FALSE)
  histograma <- ggplot(df, aes(x =  !!rlang::sym(nvalores))) +
    geom_histogram(aes(y = after_stat(density)),  breaks = hist_base$breaks, fill = "lightblue", color = "blue") +
    stat_function(fun = dnorm, args = list(mean = mean(df[[rlang::sym(nvalores)]]), sd = sd(df[[rlang::sym(nvalores)]])), 
                  color = "red", linewidth = 1.5) +
    theme_minimal() +
    labs(x = nvalores, y = "Densidade", title = "Histograma com Distribuição Normal")+
    theme(axis.title.x = element_text(size = fsize),  # Aumentar o tamanho da letra do eixo X
          axis.text.x = element_text(size = 0.8*fsize),  # Aumentar o tamanho da letra dos valores do eixo X
          axis.title.y = element_text(size = fsize),  # Aumentar o tamanho da letra do eixo Y
          axis.text.y = element_text(size = 0.8*fsize),
          plot.title = element_text(size = fsize, face = "bold"),
          panel.border = element_rect(color = "black", fill = NA, 
                          size = 2),
      plot.margin = margin(t = 10, r = 10, b = 20, l = 10))   
  # Combinar os dois gráficos em um único layout
  grafico_combinado <- arrangeGrob(histograma, boxplot, ncol = 2, widths = c(2,1.2))
  
  # Salvar o gráfico combinado em um arquivo PNG
  ggsave(filename = nome_arquivo, plot = grafico_combinado, width = largura, height = altura, dpi = dpi)
  }
 