

# Carregar a biblioteca ggplot2
library(ggplot2)
library(gridExtra)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

gerar_diagrama_setor <- function(data, setor_e, output_file = "diagrama.png", width = 1600, height = 1000) {
  
  elementos <- unique(data$Pai)
  
  nodes <- ""
  subgrafos <- ""
  
  for (i in seq_along(elementos)) {
    nomes1 <- paste(data$Code[data$Pai == elementos[i]], collapse = "\\n ")
    
    subgrafos <- paste0(subgrafos, '
    subgraph cluster_', i, ' {
      label = "', elementos[i], '"
      style = filled
      fillcolor = cyan2
      node[style = filled, fillcolor = white, fontsize = 12]
      ', paste0("node", i), ' [label = "', nomes1, '"]    
    }
    ')
    
    nodes <- paste0(nodes, '
    node0 -> ', paste0("node", i), ' [style = invis]
    ')
  }
  
  diagram <- grViz(paste0('
  digraph {
    graph[layout = dot, rankdir = TB, nodesep = 0.5, ranksep = 0.3]
    
    node[shape = box, style = filled, fontsize = 20, width = ', round(9.14 / length(elementos), 2), ', height = 1.2]
    node0 [label = "', setor_e, '"]
    
    ', subgrafos, '
    ', nodes, '
  }
  '))
  
  svg <- export_svg(diagram) %>% charToRaw()
  rsvg_png(svg, file = output_file, width = width, height = height)
  invisible(NULL) 
  }



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
 
##################################################################
Map_result <- function(iNome,DADOS,REFERENCE,
                Titulo = "Titulo",
                shp_mun=mun_shp,
                shp_uf=uf_shp,
                nome_arquivo = "map_N7.png", 
                largura = 20, altura = 24, dpi = 25,fsize=16)
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

mapnorma_result <- function(iNome,DADOS,REFERENCE,
                Titulo = "Titulo",
                shp_mun=mun_shp,
                shp_uf=uf_shp,
                nome_arquivo = "map_N7.png", 
                largura = 20, altura = 24, dpi = 25,fsize=16)
{


nome_col = iNome

SHP_0 <- base::merge(shp_mun,cbind(REFERENCE,DADOS) , by.x = "CD_MUN", by.y = "GEOCOD")

SHP_0$classe <- cut(
  SHP_0[[nome_col]],
  breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  include.lowest = TRUE,
  right = TRUE,
  labels = c("0–0.2", "0.2–0.4", "0.4–0.6", "0.6–0.8", "0.8–1")
)

cores <- c("#02c650","#a9de00","#ffcd00","#ff8300","#f40000")

mapa_iBruto <- ggplot2::ggplot(data = SHP_0) +
  ggplot2::geom_sf(ggplot2::aes(fill = classe), color = NA) +
  ggplot2::scale_fill_manual(
    values = cores,
    drop = FALSE,
    guide = ggplot2::guide_legend(
      title.position = "bottom",
      title.hjust = 0.5,
      barwidth = 1,
      barheight = 50
    )) +
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
  # Em caso de erro, cria gráfico com mensagem
  
  ggplot2::ggsave(filename = nome_arquivo, plot = mapa_iBruto, width = largura, height = altura, dpi = dpi)
}




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



