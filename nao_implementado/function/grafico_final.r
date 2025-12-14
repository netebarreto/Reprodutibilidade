# Carregar a biblioteca ggplot2
library(ggplot2)
library(gridExtra)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

grafico_final <- function(df1 = df1,df = df, 
nome_arquivo = "grafico_combinado.png", 
largura = 10, 
altura = 5, 
dpi = 25,
nvalores=nvalores,
fsize=16) { 

# Crie o gráfico
scatplot  <- ggplot(df1, aes(x = Normalizado, y = N_Normalizado)) +
            geom_point(color="blue") +
            labs(title = paste("Gráfico de Dispersão -",nvalores), x = "Dados Normalizados", y = "Dados Brutos")+
            theme_minimal() +
            theme(axis.title.x = element_text(size = fsize,vjust=-0.75),
              axis.text.x = element_text(size = 0.85*fsize),
              axis.title.y = element_text(size = fsize),  # Aumentar o tamanho da letra do eixo Y
              axis.text.y = element_text(size = 0.85*fsize),
              plot.title = element_text(size = fsize, face = "bold"),
              panel.border = element_rect(color = "black", fill = NA, 
                          linewidth = 2),
            plot.margin = margin(t = 20, r = 20, b = 50, l = 20))  
  # Criar o histograma com a linha de distribuição normal

# Tenta gerar o histograma normalmente
histograma <- tryCatch({
  hist_base  <- hist(df[,1], plot = FALSE)
  
  ggplot(df, aes(x = !!rlang::sym(nvalores))) +
    geom_histogram(aes(y = after_stat(density)),  breaks = hist_base$breaks, fill = "lightblue", color = "blue") +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(df[[rlang::sym(nvalores)]]), sd = sd(df[[rlang::sym(nvalores)]])), 
                  color = "red", linewidth = 1.5) +
    theme_minimal() +
    labs(x = nvalores, y = "Densidade", title = paste("Histograma dados Normalizados -",nvalores)) +
    theme(
      axis.title.x = element_text(size = fsize),  
      axis.text.x = element_text(size = 0.85*fsize),  
      axis.title.y = element_text(size = fsize),  
      axis.text.y = element_text(size = 0.85*fsize),
      plot.title = element_text(size = fsize, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
      plot.margin = margin(t = 20, r = 20, b = 50, l = 20)
    )
}, error = function(e) {
  # Em caso de erro, cria gráfico com mensagem
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = paste("Erro ao gerar gráfico:\n", e$message),
             size = 6, hjust = 0.5, vjust = 0.5, color = "red") +
    theme_void() +
    theme(
      plot.margin = margin(t = 20, r = 20, b = 50, l = 20),
      plot.title = element_text(size = fsize, face = "bold", hjust = 0.5)
    ) +
    labs(title = paste("Histograma dados Normalizados -", nvalores))
})
  graf_combinado <- arrangeGrob(histograma, scatplot, ncol = 2, widths = c(2,2))
  
  # Salvar o gráfico combinado em um arquivo PNG
  ggsave(filename = nome_arquivo, plot = graf_combinado,width = largura, height = altura, dpi = dpi)
}
