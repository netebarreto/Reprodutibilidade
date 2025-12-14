# Carregar a biblioteca ggplot2
library(ggplot2)
library(gridExtra)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

 
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
