# Carregar a biblioteca ggplot2
library(ggplot2)
library(gridExtra)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

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

