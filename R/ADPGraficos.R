#' Gerar Diagrama Hierárquico de Setores
#'
#' Esta função cria um diagrama hierárquico em formato de setores (subgrafos),
#' agrupando elementos de acordo com um "pai" definido no conjunto de dados.
#' O diagrama é construído em sintaxe Graphviz e exportado como arquivo de imagem.
#'
#' @param data Data frame contendo pelo menos duas colunas: 
#'   - \code{Pai}: identifica o agrupador ou setor principal.
#'   - \code{Code}: identifica os elementos pertencentes ao setor.
#' @param setor_e (opcional) Nome de um setor específico a ser destacado. 
#'   Caso \code{NULL}, todos os setores serão incluídos.
#' @param output_file Caminho e nome do arquivo de saída (default: "diagrama.png").
#' @param width Largura do diagrama em pixels (default: 1600).
#' @param height Altura do diagrama em pixels (default: 1000).
#'
#' @return Arquivo de imagem (\code{.png}) com o diagrama hierárquico gerado.
#'   Não retorna objeto em R.
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

#' Criar Gráfico Combinado (Histograma + Boxplot)
#'
#' Esta função gera e salva em arquivo um gráfico composto por:
#' (i) um histograma com curva de distribuição normal ajustada; e
#' (ii) um boxplot dos mesmos dados.  
#' Os gráficos são apresentados lado a lado em um único layout.
#'
#' @param dados Vetor numérico com os valores a serem analisados.
#' @param nome_arquivo Nome do arquivo de saída (formato PNG). 
#'   Default: "grafico_combinado.png".
#' @param largura Largura da imagem em polegadas. Default: 10.
#' @param altura Altura da imagem em polegadas. Default: 5.
#' @param dpi Resolução (pontos por polegada). Default: 25.
#' @param nvalores Nome da variável (string) a ser usada no gráfico 
#'   e nos rótulos. Default: "DD1".
#' @param fsize Tamanho base da fonte nos eixos e títulos. Default: 16.
#'
#' @return Nenhum objeto é retornado. Um arquivo PNG é salvo no diretório de trabalho.
#'
#' @details
#' - O histograma inclui uma linha correspondente à distribuição normal
#'   ajustada aos dados (média e desvio-padrão).
#' - O boxplot apresenta a dispersão dos mesmos valores, sem rótulos no eixo X.
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
#' Gerar Mapa Temático por Município
#'
#' Esta função gera um mapa temático (choropleth) a partir de dados associados
#' a municípios, unindo um shapefile municipal a um conjunto de dados de referência.
#' O resultado é exportado como arquivo de imagem (\code{.png}).
#'
#' @param iNome String. Nome da coluna em \code{DADOS} que será usada como variável de preenchimento do mapa.
#' @param DADOS Data frame com os valores a serem mapeados, incluindo a coluna de códigos de município (\code{GEOCOD}).
#' @param REFERENCE Data frame de referência contendo identificadores (\code{GEOCOD}) para associação com \code{shp_mun}.
#' @param Titulo Título do mapa. Default: "Titulo".
#' @param shp_mun Objeto \code{sf} com polígonos municipais (default: \code{mun_shp}).
#' @param shp_uf Objeto \code{sf} com polígonos estaduais (default: \code{uf_shp}).
#' @param nome_arquivo Nome do arquivo de saída (\code{.png}). Default: "map_N7.png".
#' @param largura Largura da imagem em polegadas. Default: 20.
#' @param altura Altura da imagem em polegadas. Default: 24.
#' @param dpi Resolução (pontos por polegada). Default: 25.
#' @param fsize Tamanho da fonte para títulos, legendas e eixos. Default: 16.
#'
#' @return Nenhum objeto é retornado. Um arquivo PNG contendo o mapa é salvo no diretório de trabalho.
#'
#' @details 
#' - O shapefile municipal é combinado com os dados fornecidos via \code{merge}, 
#'   utilizando a chave \code{CD_MUN} (shapefile) e \code{GEOCOD} (dados).
#' - A paleta usada é \code{Spectral}, invertida (\code{direction = -1}).
#' - O número de intervalos da legenda é definido automaticamente com base no histograma dos dados.
#' - Os limites de visualização estão fixos para o Brasil (\code{xlim = c(-74.1, -34.5)}, 
#'   \code{ylim = c(-35, 5)}).
#'
#' @examples
#' \dontrun{
#' # Exemplo hipotético
#' Map_result(
#'   iNome = "variavel_exemplo",
#'   DADOS = df_dados,
#'   REFERENCE = df_ref,
#'   Titulo = "Mapa de Exemplo",
#'   shp_mun = shp_municipios,
#'   shp_uf = shp_estados,
#'   nome_arquivo = "meu_mapa.png"
#' )
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_sf}}, \code{\link[ggplot2]{scale_fill_distiller}}
#'
#' @export
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

###################################
#' Gerar Mapa Normalizado por Classes
#'
#' Esta função gera um mapa temático (choropleth) de municípios, 
#' classificando os valores de uma variável em faixas fixas 
#' (0–0.2, 0.2–0.4, 0.4–0.6, 0.6–0.8, 0.8–1) e aplicando uma 
#' paleta de cores predefinida. O resultado é exportado em 
#' formato de imagem (\code{.png}).
#'
#' @param iNome String. Nome da coluna em \code{DADOS} que será usada 
#'   como variável de preenchimento do mapa.
#' @param DADOS Data frame com os valores a serem mapeados, incluindo 
#'   a coluna de códigos de município (\code{GEOCOD}).
#' @param REFERENCE Data frame de referência contendo identificadores 
#'   (\code{GEOCOD}) para associação com \code{shp_mun}.
#' @param Titulo Título do mapa. Default: "Titulo".
#' @param shp_mun Objeto \code{sf} com polígonos municipais 
#'   (default: \code{mun_shp}).
#' @param shp_uf Objeto \code{sf} com polígonos estaduais 
#'   (default: \code{uf_shp}).
#' @param nome_arquivo Nome do arquivo de saída (\code{.png}). 
#'   Default: "map_N7.png".
#' @param largura Largura da imagem em polegadas. Default: 20.
#' @param altura Altura da imagem em polegadas. Default: 24.
#' @param dpi Resolução (pontos por polegada). Default: 25.
#' @param fsize Tamanho da fonte para títulos, legendas e eixos. Default: 16.
#'
#' @return Nenhum objeto é retornado. Um arquivo PNG contendo o mapa é salvo no diretório de trabalho.
#'
#' @details 
#' - Os valores da variável informada são classificados em cinco intervalos fixos.
#' - As classes são representadas por uma paleta de cores sequencial 
#'   do verde ao vermelho:  
#'   \code{#02c650, #a9de00, #ffcd00, #ff8300, #f40000}.
#' - Os limites de visualização estão fixos para o Brasil 
#'   (\code{xlim = c(-74.1, -34.5)}, \code{ylim = c(-35, 5)}).
#'
#' @examples
#' \dontrun{
#' # Exemplo hipotético
#' mapnorma_result(
#'   iNome = "indice_norm",
#'   DADOS = df_dados,
#'   REFERENCE = df_ref,
#'   Titulo = "Índice Normalizado",
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
#############################


#########################
#' Gerar Gráfico Combinado (Dispersão + Histograma)
#'
#' Esta função cria um gráfico composto por:
#' (i) um gráfico de dispersão entre valores normalizados e brutos; e  
#' (ii) um histograma dos dados normalizados com curva de distribuição normal ajustada.  
#' Os dois gráficos são apresentados lado a lado em um único layout e salvos em arquivo PNG.
#'
#' @param df1 Data frame contendo as colunas \code{Normalizado} e \code{N_Normalizado}, 
#'   usadas para o gráfico de dispersão.
#' @param df Data frame contendo a variável numérica a ser usada no histograma 
#'   (coluna indicada em \code{nvalores}).
#' @param nome_arquivo Nome do arquivo de saída (\code{.png}). Default: "grafico_combinado.png".
#' @param largura Largura da imagem em polegadas. Default: 10.
#' @param altura Altura da imagem em polegadas. Default: 5.
#' @param dpi Resolução (pontos por polegada). Default: 25.
#' @param nvalores String com o nome da variável em \code{df} que será utilizada no histograma. 
#'   Default: \code{nvalores}.
#' @param fsize Tamanho da fonte para títulos, legendas e eixos. Default: 16.
#'
#' @return Nenhum objeto é retornado. Um arquivo PNG contendo o gráfico combinado é salvo no diretório de trabalho.
#'
#' @details 
#' - O gráfico de dispersão usa os eixos:  
#'   \code{x = Normalizado}, \code{y = N_Normalizado}.
#' - O histograma inclui uma linha da distribuição normal ajustada (média e desvio-padrão dos dados).
#' - Em caso de erro na geração do histograma (ex.: coluna não encontrada), é exibido 
#'   um gráfico de mensagem de erro no lugar.
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


