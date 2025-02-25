

# Carregar a biblioteca ggplot2
library(ggplot2)
library(gridExtra)


criar_grafico <- function(dados, nome_arquivo = "grafico_combinado.png", largura = 10, altura = 5, dpi = 300,nvalores="DD1",fsize=16) {
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
  grafico_combinado <- arrangeGrob(boxplot, histograma, ncol = 2, widths = c(1.2, 2))
  
  # Salvar o gráfico combinado em um arquivo PNG
  ggsave(filename = nome_arquivo, plot = grafico_combinado, width = largura, height = altura, dpi = dpi)
  }
 
##################################################################
Map_result <- function(iNome,DADOS,REFERENCE,
                Titulo = "Titulo",
                shp_mun=mun_shp,
                shp_uf=uf_shp,
                nome_arquivo = "map_N7.png", 
                largura = 20, altura = 24, dpi = 300,fsize=16)
{


nome_col = iNome
SHP_0 <- base::merge(shp_mun,cbind(REFERENCE,DADOS) , by.x = "CD_MUN", by.y = "GEOCOD")

nbreaks = length(hist(unlist(DADOS[nome_col]), plot = FALSE)$breaks)

mapa_iBruto = ggplot2::ggplot(data = SHP_0) +
  ggplot2::geom_sf(ggplot2::aes(fill = !!rlang::sym(nome_col)),color=NA)+
  ggplot2::scale_fill_distiller(palette ="Spectral", direction = -1,n.breaks = nbreaks,
  guide = ggplot2::guide_colourbar(barwidth = 1, barheight = 50,title.position = "bottom"))+
  ggplot2::geom_sf(data = shp_uf,fill = NA, colour = "black", linewidth=2)+
  ggplot2::ggtitle(Titulo) +
  ggplot2::theme_minimal() +  # Usando theme_minimal para um fundo limpo
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),  # Remove as linhas de grade
    legend.title = ggplot2::element_text(size = fsize),  # Aumenta o tamanho do título da legenda
    legend.text = ggplot2::element_text(size = fsize),  # Aumenta o tamanho do texto da legenda
    plot.title = ggplot2::element_text(size = 1.5*fsize),
    axis.text.x = ggplot2::element_text(size = fsize),  # Tamanho do texto do eixo x
    axis.text.y = ggplot2::element_text(size = fsize),          
    panel.border = ggplot2::element_rect(color = "black", fill = NA, 
                          linewidth = 2)
  )+
  ggplot2::coord_sf(xlim = c(-74.1, -34.5), ylim = c(-35, 5), expand = FALSE) 


  ggplot2::ggsave(filename = nome_arquivo, plot = mapa_iBruto, width = largura, height = altura, dpi = dpi)
}

