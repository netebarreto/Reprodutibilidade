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
