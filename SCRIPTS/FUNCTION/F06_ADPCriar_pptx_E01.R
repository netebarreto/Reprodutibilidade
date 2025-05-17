###################################################################
#
#
###################################################################

# ppt <- read_pptx(outfile)
# ppt <- on_slide(ppt, index = 1)

create_pptx <- function(template="infile.pptx",
                        meta_dados = metadados,  
                        setor_estrategico="Setor Estrategico",
                        sigla="SE",
                        subsetor=NULL) 
                        {
                          if(!is.null(subsetor)) {
                            outfile <- paste0("OUTPUT/REL_",sigla,subsetor,"_",format(Sys.time(),"%d-%m-%Y_%Hh%Mm"),".pptx")
                            sub_title=paste0(setor_estrategico,"-",subsetor)}
                          else {
                            outfile <- paste0("OUTPUT/REL_",sigla,"_",format(Sys.time(),"%d-%m-%Y_%Hh%Mm"),".pptx")
                            sub_title=setor_estrategico}

                        file.copy(template,outfile)
                        #slide_01(Subtitulo = subtitulo, caminho_arquivo = outfile)
#                        print(ppt, target = caminho_arquivo) 
                        
                      stopifnot(file.exists(outfile))
                      ppt <- officer::read_pptx(outfile)
                      ppt <- officer::on_slide(ppt, index = 1)

                      res_slide <- officer::slide_summary(ppt)
                      caixa_ref <- res_slide[res_slide$id == 6, ]
                      slide_01(caixa = caixa_ref,
                               subtitulo = sub_title,
                               ppt_in=ppt)
                      slide_02(meta_dados, 
                               subtitulo = sub_title, 
                               ppt_in = ppt) 
                      print(ppt, target = outfile) 
                      return(outfile)         
                      } 


slide_01 <- function(subtitulo = "Tabela Gerada no R",
                      caixa = caixa,
                      ppt_in = ppt) 
                     {  estilo <- officer::fp_text(font.size = 44, 
                        color = "red", 
                        bold = TRUE,
                        italic=TRUE,
                        font.family="Arial",
                        shading.color="0.8")

                      texto_formatado <- officer::fpar(officer::ftext(subtitulo, estilo))

                      ppt_in <- officer::ph_with(ppt_in,
                            value = texto_formatado,
                            location = officer::ph_location(
                            left = caixa$offx,
                            top = caixa$offy+0.8,
                            width = caixa$cx,
                            height = caixa$cy,
                            alignment = "l" )) }

# # Função para gerar uma apresentação PowerPoint


# ppt <- officer::read_pptx(outfile2)  # Lê o arquivo existente

# ppt <- ppt %>%
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with("Indicadores e Indices", location = ph_location_type("title")) %>%
#   ph_with(
#     external_img("diagrama.png", width =31.6 , height = 14,unit="cm"),  # Tamanho exibido no slide
#   location = ph_location(left = 0.5, top = 0.75),use_loc_size = FALSE )

# # 4. Salvar o PowerPoint
# print(ppt, target = outfile2)


slide_02 <- function(meta_dados, 
                     subtitulo = sub_title, 
                     ppt_in = ppt) 
                     {
  gerar_diagrama_setor(
        data = meta_dados,
        setor_e = subtitulo,
        output_file = "diagrama.png",
        width = 1600,
        height = 1000
        )
  # Criar o documento PowerPoint
  ppt_in <- ppt_in %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with("Indicadores e Indices", location = ph_location_type("title")) %>%
    ph_with(
      external_img("diagrama.png", width =31.6 , height = 14,unit="cm"),  # Tamanho exibido no slide
      location = ph_location(left = 0.5, top = 0.75),use_loc_size = FALSE )
  # Salvar o arquivo no caminho especificado

}


# Validação das tabelas
# Função para gerar uma apresentação PowerPoint
slides_descricao <- function(tabela1, tabela2,tabela3,titulo = "Tabela Gerada no R", caminho_arquivo = "saida_apresentacao.pptx",caminho_imagem = NULL,caminho_map=NULL) {
  # Transformar tabela em um objeto flextable
  largura_colunas <- sapply(tabela1, function(col) max(nchar(as.character(col)), na.rm = TRUE))
  largura_colunas[1] <- 0.75 
  largura_colunas[2] <- 0.75
  largura_colunas[3] <- 1 
  largura_colunas[4] <- 5.0 
  largura_colunas[5] <- 0.8
  largura_colunas[6] <- 1.4

  ft <- flextable(tabela1) %>%
    width(j = 1:ncol(tabela1), width = largura_colunas) %>%
    bold(i =1, j =1:ncol(tabela1), part="header" )%>% 
    height(i = 1:nrow(tabela1),height=0.5) # Ajuste automático de largura

  ft2 <- flextable(tabela2) %>%
    width(j = 1:ncol(tabela2), width = 1.45 ) %>%  
    merge_at(i = 1, j = 1:ncol(tabela2), part = "header") %>%
    bold(i =1, j =1, part="header" )%>%  
    align(i = 1,j = 1:ncol(tabela2), align = "center", part = "header")%>%  # Centralizar a primeira linha mesclada
    border(i = 1, j = 1:ncol(tabela2), border.top = fp_border(color = "black", width = 1), part = "all") %>%  # Adicionar borda inferior  
    align(j = 2, align = "right", part = "all")%>% 
    height(i = 1,height=0.5,part="header") %>% 
    height(i = 2:nrow(tabela2),height=0.35)# 

  ft3 <- flextable(tabela3) %>%
    width(j = 1:ncol(tabela3), width = 1.65 )%>%
    bold(i =1, j =1:ncol(tabela3), part="header") %>%  # Adicionar borda inferior  
    align(j = 1:ncol(tabela3), align = "right", part = "all")  # Ajuste automático de largura



  if (file.exists(caminho_arquivo)) {
    ppt <- read_pptx(caminho_arquivo)  # Lê o arquivo existente
  } else {
    ppt <- read_pptx()  # Cria um novo arquivo se não existir
  }

  # Criar o documento PowerPoint
  ppt <- ppt %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = fpar(ftext(titulo, prop = fp_text(font.size = 22))), location = ph_location_type(type = "title"))
  
  if((tabela2$Descritivo[1] != "Grupo 1") & (tabela2$Descritivo[1] != "Grupo 2"))
  {
    ppt <- ppt %>%
      ph_with(ft,location = ph_location(left = 1, top = 1.5)) %>%
      ph_with(value = fpar(ftext("Resumo Descritivo", prop = fp_text(font.size = 14, color = "black",bold=TRUE))),location = ph_location(left = 1, top =-0.2 ))
  }

  ppt <- ppt %>%
    ph_with(ft2,location = ph_location(left = 1, top = 2.5)) %>%
    ph_with(ft3,location = ph_location(left = 1, top = 5.9))%>%
    ph_with(value = fpar(ftext("Avaliação de NA e Valores Únicos", prop = fp_text(font.size = 13, color = "black",bold=TRUE))),location = ph_location(left = 1.8, top =4.2 )) 

      if (!is.null(caminho_imagem) && file.exists(caminho_imagem)) {
    ppt <- ppt %>%
      ph_with(external_img(caminho_imagem, width =12.5 , height = 7.40,unit="cm"),  # Ajuste largura e altura
              location = ph_location(left = 4.2, top = 2.6),use_loc_size = FALSE )  # Ajuste posição
  }

  if (!is.null(caminho_map) && file.exists(caminho_map)) 
  {
    ppt <- ppt %>%
      ph_with(external_img(caminho_map, width =9.04 , height = 10.85,unit="cm"),  # Ajuste largura e altura
              location = ph_location(left = 9.5, top = 2.06),use_loc_size = FALSE )  # Ajuste posição
  }

  # Salvar o arquivo no caminho especificado
  print(ppt, target = caminho_arquivo)
  
  #cat("Apresentação criada com sucesso no arquivo:", caminho_arquivo, "\n")
}


# Validação das tabelas
# Função para gerar uma apresentação PowerPoint
slides_winsor <- function(tabela1,
 titulo = "Tabela Gerada no R",
  caminho_arquivo = "saida_apresentacao.pptx",
  caminho_map=NULL) {

  ft <- flextable(tabela1) %>%
    width(j = 1:ncol(tabela1), width = 1.4) %>%
    bold(i =1, j =1:ncol(tabela1), part="header" )%>% 
    height(i = 1:nrow(tabela1),height=0.5) # Ajuste automático de largura

 
  if (file.exists(caminho_arquivo)) {
    ppt <- read_pptx(caminho_arquivo)  # Lê o arquivo existente
  } else {
    ppt <- read_pptx()  # Cria um novo arquivo se não existir
  }

  # Criar o documento PowerPoint
  ppt <- ppt %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = fpar(ftext(titulo, prop = fp_text(font.size = 22))), location = ph_location_type(type = "title"))
  
  ppt <- ppt %>%
      ph_with(ft,location = ph_location(left = 1, top = 1.5)) %>%
      ph_with(value = fpar(ftext("Resumo Winsorization", prop = fp_text(font.size = 14, color = "black",bold=TRUE))),location = ph_location(left = 1, top =-0.2 ))
  
  if (!is.null(caminho_map) && file.exists(caminho_map)) 
  {
    ppt <- ppt %>%
      ph_with(external_img(caminho_map, width =11.08 , height = 13.5,unit="cm"),  # Ajuste largura e altura
              location = ph_location(left = 4.55, top = 2.1),use_loc_size = FALSE )  # Ajuste posição
  }
  
  # Salvar o arquivo no caminho especificado
  print(ppt, target = caminho_arquivo)
  
  #cat("Apresentação criada com sucesso no arquivo:", caminho_arquivo, "\n")
}

slides_result <- function(caminho_shp_mun, 
                             caminho_shp_uf, 
                             resumo_bruto,
                             iMeta_adapta,
                             iData_N7, 
                             iData_ref,
                             iDataWIN,
                             local_output, RES=NULL, WIN=NULL, BXC=NULL)
                    {
  # Carrega os shapefiles e projeta para WGS 84
  shp_mun1 <- sf::st_transform(sf::st_read(caminho_shp_mun), crs = 4326)
  shp_uf1  <- sf::st_transform(sf::st_read(caminho_shp_uf), crs = 4326)
  
  res_basico <- resumo_bruto$resumo_basico
  
  for (i in 1:ncol(res_basico)) {
    tab.descricao <- iMeta_adapta[which(iMeta_adapta$Code == colnames(res_basico)[i]), -7]
    
    tab_stats <- tibble::tibble(
      Resumo = rownames(resumo_bruto[[2]]),
      Descritivo = resumo_bruto[[2]][, i]
    )
    colnames(tab_stats)[1] <- "Resumo Estatístico"
    
    tab_NAs <- resumo_bruto$resumo_na[i, -1]
    
    icode <- tab.descricao$Code
    iclasse <- tab_stats$Descritivo[1]
    
    title_slide <- ifelse(iclasse == "Grupo 1",
                          paste0(tab.descricao$Nome, " - ", icode, " (GRUPO 1)"),
                          ifelse(iclasse == "Grupo 2",
                                 paste0(tab.descricao$Nome, " - ", icode, " (GRUPO 2)"),
                                 ifelse(iclasse == "Conjunto Completo",
                                        paste0(tab.descricao$Nome, " - ", icode, " (GERAL)"),
                                        paste0(tab.descricao$Nome, " - ", tab.descricao$Code))))
    
    idata <- as.data.frame(iData_N7[[icode]])
    colnames(idata) <- icode
    
    if (iclasse == "Grupo 1") {
      idata[iData_ref$CLUSTER != 1, ] <- NA
    }
    if (iclasse == "Grupo 2") {
      idata[iData_ref$CLUSTER != 2, ] <- NA
    }
    
    Titulo_map <- paste0(c("Indicador Bruto: ",
                           "Indicador Winsorization: ",
                           "Reference Winsorization: ",
                           "Diferença Winsorization: "), icode)
    criar_grafico(idata, 
                  nome_arquivo = "gra_descricao.png", 
                  largura = 24, altura = 12, 
                  nvalores = icode,
                  fsize = 50)
    
    Map_result(icode, idata, iData_ref["GEOCOD"],
               Titulo = Titulo_map[1],
               shp_mun = shp_mun1,
               shp_uf = shp_uf1,
               nome_arquivo = "map_bruto.png", 
               largura = 20, altura = 24, fsize = 36)

    if(!is.null(RES)) {

    slides_descricao(tabela1 = tab.descricao,
                     tabela2 = tab_stats,
                     tabela3 = tab_NAs,
                     titulo = title_slide,
                     caminho_arquivo = local_output,
                     caminho_imagem = "gra_descricao.png",
                     caminho_map = "map_bruto.png") }
    if(!is.null(WIN)) {

      iwinso <- tibble::as_tibble(purrr::map_dfc(iDataWIN$Resumo[i, ], ~ .x[[1]]))
      if(iwinso$Aplicacao == "N")
      {
         
      slides_winsor(tabela1 = iwinso,
                       titulo = title_slide,
                       caminho_arquivo = local_output,
                       caminho_map = NULL) 
    }
    else
    { 
      iwinso_data <- as.data.frame(iDataWIN$iData[[icode]])
      colnames(iwinso_data) <- icode
      Map_result(icode, iwinso_data, iData_ref["GEOCOD"],
                 Titulo = Titulo_map[2],
                 shp_mun = shp_mun1,
                 shp_uf = shp_uf1,
                 nome_arquivo = "map_Wins.png", 
                 largura = 20, altura = 24, fsize = 36)
      
      slides_winsor(tabela1 = iwinso,
                    titulo = title_slide,
                    caminho_arquivo = local_output,
                    caminho_map = "map_Wins.png") 
    
    }}

    if(!is.null(BXC)) {
      slides_descricao(tabela1 = tab.descricao,
                       tabela2 = tab_stats,
                       tabela3 = tab_NAs,
                       titulo = title_slide,
                       caminho_arquivo = local_output,
                       caminho_imagem = "gra_descricao.png",
                       caminho_map = "map_bruto.png") 
    }
    print(paste0(i, "  de  ", ncol(res_basico)))
          
  }
}
