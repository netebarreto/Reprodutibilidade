#' Gerar Diagrama Hierarquico de Setores
#'
#' Esta funcao cria um diagrama hierarquico em formato de setores (subgrafos),
#' agrupando elementos de acordo com um "pai" definido no conjunto de dados.
#' O diagrama e construido em sintaxe Graphviz e exportado como arquivo de imagem.
#'
#' @param data Data frame contendo pelo menos duas colunas: 
#'   - \code{Pai}: identifica o agrupador ou setor principal.
#'   - \code{Code}: identifica os elementos pertencentes ao setor.
#' @param setor_e (opcional) Nome de um setor especifico a ser destacado. 
#'   Caso \code{NULL}, todos os setores serao incluidos.
#' @param output_file Caminho e nome do arquivo de saida (default: "diagrama.png").
#' @param width Largura do diagrama em pixels (default: 1600).
#' @param height Altura do diagrama em pixels (default: 1000).
#'
#' @return Arquivo de imagem (\code{.png}) com o diagrama hierarquico gerado.
#'   Nao retorna objeto em R.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Pai = c("Setor A", "Setor A", "Setor B"),
#'   Code = c("Item 1", "Item 2", "Item 3")
#' )
#' gerar_diagrama_setor(df, output_file = "setores.png")
#' }
#'
#' @seealso \code{\link[DiagrammeR]{grViz}}
#'
#' @export
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
  
  diagram <- DiagrammeR::grViz(paste0('
  digraph {
    graph[layout = dot, rankdir = TB, nodesep = 0.5, ranksep = 0.3]
    
    node[shape = box, style = filled, fontsize = 20, width = ', round(9.14 / length(elementos), 2), ', height = 1.2]
    node0 [label = "', setor_e, '"]
    
    ', subgrafos, '
    ', nodes, '
  }
  '))
  
  svg <- DiagrammeRsvg::export_svg(diagram) |> charToRaw()
  rsvg::rsvg_png(svg, file = output_file, width = width, height = height)
  invisible(NULL) 
  }

#' Criar Grafico Combinado (Histograma + Boxplot)
#'
#' Esta funcao gera e salva em arquivo um grafico composto por:
#' (i) um histograma com curva de distribuicao normal ajustada; e
#' (ii) um boxplot dos mesmos dados.  
#' Os graficos sao apresentados lado a lado em um unico layout.
#'
#' @param dados Vetor numerico com os valores a serem analisados.
#' @param nome_arquivo Nome do arquivo de saida (formato PNG). 
#'   Default: "grafico_combinado.png".
#' @param largura Largura da imagem em polegadas. Default: 10.
#' @param altura Altura da imagem em polegadas. Default: 5.
#' @param dpi Resolucao (pontos por polegada). Default: 25.
#' @param nvalores Nome da variavel (string) a ser usada no grafico 
#'   e nos rotulos. Default: "DD1".
#' @param fsize Tamanho base da fonte nos eixos e titulos. Default: 16.
#'
#' @return Nenhum objeto e retornado. Um arquivo PNG e salvo no diretorio de trabalho.
#'
#' @details
#' - O histograma inclui uma linha correspondente a distribuicao normal
#'   ajustada aos dados (media e desvio-padrao).
#' - O boxplot apresenta a dispersao dos mesmos valores, sem rotulos no eixo X.
#'
#' @examples
#' \dontrun{
#' # Exemplo com dados simulados
#' set.seed(123)
#' x <- rnorm(100, mean = 50, sd = 10)
#' criar_grafico(x, nome_arquivo = "meu_grafico.png")
#' }
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[gridExtra]{arrangeGrob}}
#'
#' @export
criar_grafico <- function(dados, nome_arquivo = "grafico_combinado.png", largura = 10, altura = 5, dpi = 25,nvalores="DD1",fsize=16) {
  # Carregar as bibliotecas necessarias

  
  # Criar um data frame com os dados
  df <- data.frame(valores = stats::na.exclude(dados))
  colnames(df) <- nvalores

  # Criar o boxplot
  boxplot <- ggplot2::ggplot(df, ggplot2::aes(y = !!rlang::sym(nvalores))) +
    ggplot2::geom_boxplot(fill = "lightblue", color = "blue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = nvalores, y = "Valores",title = paste0("Boxplot ")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = fsize,vjust=-0.75),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_text(size = fsize),  # Aumentar o tamanho da letra do eixo Y
          axis.text.y = ggplot2::element_text(size = 0.8*fsize),
          plot.title = ggplot2::element_text(size = fsize, face = "bold"),
          panel.border = ggplot2::element_rect(color = "black", fill = NA, 
                          linewidth = 2),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 50, l = 10))  # Aumentar o tamanho da letra dos valores do eixo Y
  
  
  # Criar o histograma com a linha de distribuicao normal
  hist_base <- graphics::hist(df[,1], plot = FALSE)
  histograma <- ggplot2::ggplot(df, ggplot2::aes(x =  !!rlang::sym(nvalores))) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(stats::density)),  breaks = hist_base$breaks, fill = "lightblue", color = "blue") +
    ggplot2::stat_function(fun = stats::dnorm, args = list(mean = mean(df[[rlang::sym(nvalores)]]), sd = stats::sd(df[[rlang::sym(nvalores)]])), 
                  color = "red", linewidth = 1.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = nvalores, y = "Densidade", title = "Histograma com Distribuicao Normal")+
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = fsize),  # Aumentar o tamanho da letra do eixo X
          axis.text.x = ggplot2::element_text(size = 0.8*fsize),  # Aumentar o tamanho da letra dos valores do eixo X
          axis.title.y = ggplot2::element_text(size = fsize),  # Aumentar o tamanho da letra do eixo Y
          axis.text.y = ggplot2::element_text(size = 0.8*fsize),
          plot.title = ggplot2::element_text(size = fsize, face = "bold"),
          panel.border = ggplot2::element_rect(color = "black", fill = NA, 
                          size = 2),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 20, l = 10))   
  # Combinar os dois graficos em um unico layout
  grafico_combinado <- gridExtra::arrangeGrob(histograma, boxplot, ncol = 2, widths = c(2,1.2))
  
  # Salvar o grafico combinado em um arquivo PNG
  ggplot2::ggsave(filename = nome_arquivo, plot = grafico_combinado, width = largura, height = altura, dpi = dpi)
  }


  
##################################################################
#' Gerar Mapa Tematico por Municipio
#'
#' Esta funcao gera um mapa tematico (choropleth) a partir de dados associados
#' a municipios, unindo um shapefile municipal a um conjunto de dados de referencia.
#' O resultado e exportado como arquivo de imagem (\code{.png}).
#'
#' @param iNome String. Nome da coluna em \code{DADOS} que sera usada como variavel de preenchimento do mapa.
#' @param DADOS Data frame com os valores a serem mapeados, incluindo a coluna de codigos de municipio (\code{GEOCOD}).
#' @param REFERENCE Data frame de referencia contendo identificadores (\code{GEOCOD}) para associacao com \code{shp_mun}.
#' @param Titulo Titulo do mapa. Default: "Titulo".
#' @param nome_arquivo Nome do arquivo de saida (\code{.png}). Default: "map_N7.png".
#' @param largura Largura da imagem em polegadas. Default: 20.
#' @param altura Altura da imagem em polegadas. Default: 24.
#' @param dpi Resolucao (pontos por polegada). Default: 25.
#' @param fsize Tamanho da fonte para titulos, legendas e eixos. Default: 16.
#'
#' @return Nenhum objeto e retornado. Um arquivo PNG contendo o mapa e salvo no diretorio de trabalho.
#'
#' @details 
#' - O shapefile municipal e combinado com os dados fornecidos via \code{merge}, 
#'   utilizando a chave \code{CD_MUN} (shapefile) e \code{GEOCOD} (dados).
#' - A paleta usada e \code{Spectral}, invertida (\code{direction = -1}).
#' - O numero de intervalos da legenda e definido automaticamente com base no histograma dos dados.
#' - Os limites de visualizacao estao fixos para o Brasil (\code{xlim = c(-74.1, -34.5)}, 
#'   \code{ylim = c(-35, 5)}).
#'
#' @examples
#' \dontrun{
#' # Exemplo hipotetico
#' Map_result(
#'   iNome = "variavel_exemplo",
#'   DADOS = df_dados,
#'   REFERENCE = df_ref,
#'   Titulo = "Mapa de Exemplo",
#'   nome_arquivo = "meu_mapa.png"
#' )
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_sf}}, \code{\link[ggplot2]{scale_fill_distiller}}
#'
#' @export
Map_result <- function(iNome,DADOS,REFERENCE,
                Titulo = "Titulo",
                nome_arquivo = "map_N7.png", 
                largura = 20, altura = 24, dpi = 25,fsize=16)
{

  shp_mun = utils::data("mun_shp", package = "PacoteTeste", envir = environment())
  shp_uf = utils::data("uf_shp", package = "PacoteTeste", envir = environment())
  
nome_col = iNome
SHP_0 <- base::merge(shp_mun,cbind(REFERENCE,DADOS) , by.x = "CD_MUN", by.y = "GEOCOD")

nbreaks = length(graphics::hist(unlist(DADOS[nome_col]), plot = FALSE)$breaks)

mapa_iBruto = ggplot2::ggplot(data = SHP_0) +
  ggplot2::geom_sf(ggplot2::aes(fill = !!rlang::sym(nome_col)),color=NA)+
  ggplot2::scale_fill_distiller(palette ="Spectral", direction = -1,n.breaks = nbreaks,
  guide = ggplot2::guide_colourbar(barwidth = 1, barheight = 50,title.position = "bottom"))+
  ggplot2::geom_sf(data = shp_uf,fill = NA, colour = "black", linewidth=2)+
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
                          linewidth = 2)
  )+
  ggplot2::coord_sf(xlim = c(-74.1, -34.5), ylim = c(-35, 5), expand = FALSE) 


  ggplot2::ggsave(filename = nome_arquivo, plot = mapa_iBruto, width = largura, height = altura, dpi = dpi)
}

###################################
#' Gerar Mapa Normalizado por Classes
#'
#' Esta funcao gera um mapa tematico (choropleth) de municipios, 
#' classificando os valores de uma variavel em faixas fixas 
#' (0-0.2, 0.2-0.4, 0.4-0.6, 0.6-0.8, 0.8-1) e aplicando uma 
#' paleta de cores predefinida. O resultado e exportado em 
#' formato de imagem (\code{.png}).
#'
#' @param iNome String. Nome da coluna em \code{DADOS} que sera usada 
#'   como variavel de preenchimento do mapa.
#' @param DADOS Data frame com os valores a serem mapeados, incluindo 
#'   a coluna de codigos de municipio (\code{GEOCOD}).
#' @param REFERENCE Data frame de referencia contendo identificadores 
#'   (\code{GEOCOD}) para associacao com \code{shp_mun}.
#' @param Titulo Titulo do mapa. Default: "Titulo".
#' @param nome_arquivo Nome do arquivo de saida (\code{.png}). 
#'   Default: "map_N7.png".
#' @param largura Largura da imagem em polegadas. Default: 20.
#' @param altura Altura da imagem em polegadas. Default: 24.
#' @param dpi Resolucao (pontos por polegada). Default: 25.
#' @param fsize Tamanho da fonte para titulos, legendas e eixos. Default: 16.
#'
#' @return Nenhum objeto e retornado. Um arquivo PNG contendo o mapa e salvo no diretorio de trabalho.
#'
#' @details 
#' - Os valores da variavel informada sao classificados em cinco intervalos fixos.
#' - As classes sao representadas por uma paleta de cores sequencial 
#'   do verde ao vermelho:  
#'   \code{#02c650, #a9de00, #ffcd00, #ff8300, #f40000}.
#' - Os limites de visualizacao estao fixos para o Brasil 
#'   (\code{xlim = c(-74.1, -34.5)}, \code{ylim = c(-35, 5)}).
#'
#' @examples
#' \dontrun{
#' # Exemplo hipotetico
#' mapnorma_result(
#'   iNome = "indice_norm",
#'   DADOS = df_dados,
#'   REFERENCE = df_ref,
#'   Titulo = "Indice Normalizado",
#'   shp_mun = shp_municipios,
#'   shp_uf = shp_estados,
#'   nome_arquivo = "mapa_norma.png"
#' )
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_sf}}, 
#'   \code{\link[ggplot2]{scale_fill_manual}}
#'
#' @export
mapnorma_result <- function(iNome,DADOS,REFERENCE,
                Titulo = "Titulo",
                nome_arquivo = "map_N7.png", 
                largura = 20, altura = 24, dpi = 25,fsize=16)
{

  
  shp_mun = utils::data("mun_shp", package = "PacoteTeste", envir = environment())
  shp_uf = utils::data("uf_shp", package = "PacoteTeste", envir = environment())
  
nome_col = iNome

SHP_0 <- base::merge(shp_mun,cbind(REFERENCE,DADOS) , by.x = "CD_MUN", by.y = "GEOCOD")

SHP_0$classe <- cut(
  SHP_0[[nome_col]],
  breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  include.lowest = TRUE,
  right = TRUE,
  labels = c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1")
)

cores <- c("#02c650","#a9de00","#ffcd00","#ff8300","#f40000")

mapa_iBruto <- ggplot2::ggplot(data = SHP_0) +
  ggplot2::geom_sf(ggplot2::aes(fill = SHP_0$classe), color = NA) +
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
    legend.title = ggplot2::element_text(size = fsize),  # Aumenta o tamanho do titulo da legenda
    legend.text = ggplot2::element_text(size = fsize),  # Aumenta o tamanho do texto da legenda
    plot.title = ggplot2::element_text(size = 1.5*fsize),
    axis.text.x = ggplot2::element_text(size = fsize),  # Tamanho do texto do eixo x
    axis.text.y = ggplot2::element_text(size = fsize),          
    panel.border = ggplot2::element_rect(color = "black", fill = NA, 
                          linewidth = 2)
  )+
  ggplot2::coord_sf(xlim = c(-74.1, -34.5), ylim = c(-35, 5), expand = FALSE)
  # Em caso de erro, cria grafico com mensagem
  
  ggplot2::ggsave(filename = nome_arquivo, plot = mapa_iBruto, width = largura, height = altura, dpi = dpi)
}
#############################


#########################
#' Gerar Grafico Combinado (Dispersao + Histograma)
#'
#' Esta funcao cria um grafico composto por:
#' (i) um grafico de dispersao entre valores normalizados e brutos; e  
#' (ii) um histograma dos dados normalizados com curva de distribuicao normal ajustada.  
#' Os dois graficos sao apresentados lado a lado em um unico layout e salvos em arquivo PNG.
#'
#' @param df1 Data frame contendo as colunas \code{Normalizado} e \code{N_Normalizado}, 
#'   usadas para o grafico de dispersao.
#' @param df Data frame contendo a variavel numerica a ser usada no histograma 
#'   (coluna indicada em \code{nvalores}).
#' @param nome_arquivo Nome do arquivo de saida (\code{.png}). Default: "grafico_combinado.png".
#' @param largura Largura da imagem em polegadas. Default: 10.
#' @param altura Altura da imagem em polegadas. Default: 5.
#' @param dpi Resolucao (pontos por polegada). Default: 25.
#' @param nvalores String com o nome da variavel em \code{df} que sera utilizada no histograma. 
#'   Default: \code{nvalores}.
#' @param fsize Tamanho da fonte para titulos, legendas e eixos. Default: 16.
#'
#' @return Nenhum objeto e retornado. Um arquivo PNG contendo o grafico combinado e salvo no diretorio de trabalho.
#'
#' @details 
#' - O grafico de dispersao usa os eixos:  
#'   \code{x = Normalizado}, \code{y = N_Normalizado}.
#' - O histograma inclui uma linha da distribuicao normal ajustada (media e desvio-padrao dos dados).
#' - Em caso de erro na geracao do histograma (ex.: coluna nao encontrada), e exibido 
#'   um grafico de mensagem de erro no lugar.
#'
#' @examples
#' \dontrun{
#' # Exemplo com dados simulados
#' set.seed(123)
#' df1 <- data.frame(Normalizado = runif(100), N_Normalizado = rnorm(100))
#' df <- data.frame(DD1 = rnorm(100, mean = 50, sd = 10))
#' grafico_final(df1 = df1, df = df, nvalores = "DD1", nome_arquivo = "meu_grafico.png")
#' }
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, 
#'   \code{\link[gridExtra]{arrangeGrob}}, 
#'   \code{\link[ggplot2]{stat_function}}
#'
#' @export

grafico_final <- function(df1 = df1,df = df, 
nome_arquivo = "grafico_combinado.png", 
largura = 10, 
altura = 5, 
dpi = 25,
nvalores=nvalores,
fsize=16) { 

# Crie o grafico
scatplot  <- ggplot2::ggplot(df1, ggplot2::aes(x = Normalizado, y = N_Normalizado)) +
  ggplot2::geom_point(color="blue") +
  ggplot2::labs(title = paste("Grafico de Dispersao -",nvalores), x = "Dados Normalizados", y = "Dados Brutos")+
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = fsize,vjust=-0.75),
              axis.text.x = ggplot2::element_text(size = 0.85*fsize),
              axis.title.y = ggplot2::element_text(size = fsize),  # Aumentar o tamanho da letra do eixo Y
              axis.text.y = ggplot2::element_text(size = 0.85*fsize),
              plot.title = ggplot2::element_text(size = fsize, face = "bold"),
              panel.border = ggplot2::element_rect(color = "black", fill = NA, 
                          linewidth = 2),
            plot.margin = ggplot2::margin(t = 20, r = 20, b = 50, l = 20))  
  # Criar o histograma com a linha de distribuicao normal

# Tenta gerar o histograma normalmente
histograma <- tryCatch({
  hist_base  <- graphics::hist(df[,1], plot = FALSE)
  
  ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(nvalores))) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(stats::density)),  breaks = hist_base$breaks, fill = "lightblue", color = "blue") +
  ggplot2::stat_function(fun = stats::dnorm, 
                  args = list(mean = mean(df[[rlang::sym(nvalores)]]), sd = stats::sd(df[[rlang::sym(nvalores)]])), 
                  color = "red", linewidth = 1.5) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = nvalores, y = "Densidade", title = paste("Histograma dados Normalizados -",nvalores)) +
  ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = fsize),  
      axis.text.x = ggplot2::element_text(size = 0.85*fsize),  
      axis.title.y = ggplot2::element_text(size = fsize),  
      axis.text.y = ggplot2::element_text(size = 0.85*fsize),
      plot.title = ggplot2::element_text(size = fsize, face = "bold"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 2),
      plot.margin = ggplot2::margin(t = 20, r = 20, b = 50, l = 20)
    )
}, error = function(e) {
  # Em caso de erro, cria grafico com mensagem
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Erro ao gerar grafico:\n", e$message),
             size = 6, hjust = 0.5, vjust = 0.5, color = "red") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 20, r = 20, b = 50, l = 20),
      plot.title = ggplot2::element_text(size = fsize, face = "bold", hjust = 0.5)
    ) +
    ggplot2::labs(title = paste("Histograma dados Normalizados -", nvalores))
})
  graf_combinado <- gridExtra::arrangeGrob(histograma, scatplot, ncol = 2, widths = c(2,2))
  
  # Salvar o grafico combinado em um arquivo PNG
  ggplot2::ggsave(filename = nome_arquivo, plot = graf_combinado,width = largura, height = altura, dpi = dpi)
}


