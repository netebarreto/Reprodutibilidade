#' Mapa tematico para indicadores municipais
#'
#' Gera um mapa temático usando shapefiles internos do pacote ou fornecidos pelo usuario.
#'
#' @param iNome Nome da coluna de DADOS a ser mapeada.
#' @param DADOS Data frame com valores.
#' @param REFERENCE Data frame com GEOCOD.
#' @param titulo Título do mapa.
#' @param shp_mun Shapefile municipal (sf). Se NULL, usa mun_shp interno.
#' @param shp_uf Shapefile estadual (sf). Se NULL, usa uf_shp interno.
#' @param salvar Se TRUE, salva o mapa.
#' @param dir diretorio em que o arquivo sera salvo
#' @param nome_arquivo Nome do arquivo salvo.
#' @param largura Largura do grafico salvo.
#' @param altura Altura do grafico salvo.
#' @param dpi Resolução.
#' @param fsize Tamanho da fonte.
#'
#' @return Objeto ggplot2.
#'
#' @examples
#' \dontrun{
#' # Exemplo que usa os shapefiles internos
#' # map_result_normal("MMPD", dados_N7, ref_N7)
#' }
#' @import sf
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_distiller guide_colourbar
#' @importFrom ggplot2 ggtitle theme_minimal theme element_blank element_text
#' @importFrom ggplot2 element_rect coord_sf ggsave
#' @importFrom rlang sym
#' @importFrom graphics hist
#' @importFrom dplyr left_join
#' @importFrom rlang .data
#'
#' @export
map_result_normal <- function(iNome,
                       DADOS,
                       REFERENCE,
                       titulo = "Titulo",
                       shp_mun = NULL,
                       shp_uf  = NULL,
                       salvar = FALSE,
                       dir = NULL, 
                       nome_arquivo = NULL,
                       largura = 20,
                       altura = 24,
                       dpi = 300,
                       fsize = 16) {

  # usa shapefiles internos se o usuario nao passar nada
  if (is.null(shp_mun)) {
    # carrega do pacote, se for o caso
    utils::data("mun_shp", package = "reprodutibilidade", envir = environment())
    shp_mun <- mun_shp
  }

  if (is.null(shp_uf)) {
    utils::data("uf_shp", package = "reprodutibilidade", envir = environment())
    shp_uf <- uf_shp
  }

  nome_col <- iNome

  # junta REFERENCE + DADOS por GEOCOD
  dados_juntos <- base::cbind(REFERENCE, DADOS)
  #colnames(dados_juntos) <- c(colnames(REFERENCE),)
SHP_0 <- merge(
  shp_mun,
  dados_juntos,
  by.x  = "CD_MUN",
  by.y  = "GEOCOD",
  all.x = TRUE,
  sort  = FALSE
) 


SHP_0$classe <- cut(
  SHP_0[[nome_col]],
  breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  include.lowest = TRUE,
  right = TRUE,
  labels = c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1")
)

cores <- c("#02c650","#a9de00","#ffcd00","#ff8300","#f40000")

nome_dataset <- deparse(substitute(DADOS))
Titulo <- paste0(titulo," - ",nome_dataset)

mapa_iBruto <- ggplot2::ggplot(data = SHP_0) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$classe), color = NA) +
  ggplot2::scale_fill_manual(
    values = cores,
    drop = FALSE,
    guide = ggplot2::guide_legend(
      title.position = "bottom",
      title.hjust = 0.5,
      barwidth = 0.1*fsize,
      barheight = 0.25*fsize
    )) +
  ggplot2::geom_sf(data = shp_uf,fill = NA, colour = "black", linewidth=0.1*fsize)+
  ggplot2::ggtitle(Titulo) +
  ggplot2::theme_minimal() +  # Usando theme_minimal para um fundo limpo
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),  # Remove as linhas de grade
    legend.title = ggplot2::element_text(size = fsize),  # Aumenta o tamanho do titulo da legenda
    legend.text = ggplot2::element_text(size = fsize),  # Aumenta o tamanho do texto da legenda
    plot.title = ggplot2::element_text(size = 1.5*fsize),
    axis.text.x = ggplot2::element_text(size = fsize),  # Tamanho do texto do eixo x
    axis.text.y = ggplot2::element_text(size = fsize),          
    panel.border = ggplot2::element_rect(color = "black", fill = NA, 
                          linewidth = 0.1*fsize)
  )+
  ggplot2::coord_sf(xlim = c(-74.1, -34.5), ylim = c(-35, 5), expand = FALSE)
  # Em caso de erro, cria grafico com mensagem

  if (isTRUE(salvar)) 
  {
    if(!is.null(dir) & is.null(nome_arquivo)) 
    { nome_file <- paste0(dir,"/",iNome,"_",nome_dataset,".png")
      ggplot2::ggsave(
        filename = nome_file,
        plot     = mapa_iBruto,
        width    = largura,
        height   = altura,
        dpi      = dpi)
    }
    else 
      { if(!is.null(dir) & !is.null(nome_arquivo)) 
         {
          nome_file <- paste0(dir,"/",nome_arquivo)
          ggplot2::ggsave(
           filename = nome_file,
           plot     = mapa_iBruto,
           width    = largura,
           height   = altura,
           dpi      = dpi)
         }
          else 
            { if (is.null(dir) & is.null(nome_arquivo)) 
              { nome_file <- paste0(iNome,"_",nome_dataset,".png")
              ggplot2::ggsave(
                filename = nome_file,
                plot     = mapa_iBruto,
                width    = largura,
                height   = altura,
                dpi      = dpi)
               }
              else 
                if(is.null(dir) & !is.null(nome_arquivo)) 
                  { nome_file <- nome_arquivo 
                    ggplot2::ggsave(
                    filename = nome_file,
                    plot     = mapa_iBruto,
                    width    = largura,
                    height   = altura,
                    dpi      = dpi)
                  }
            }
      }     
  }  

  mapa_iBruto

}
