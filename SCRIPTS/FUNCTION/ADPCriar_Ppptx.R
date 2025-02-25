
# Função para gerar uma apresentação PowerPoint
criar_slide_01 <- function(tabela, titulo = "Tabela Gerada no R", caminho_arquivo = "saida_apresentacao.pptx") {
  # Transformar tabela em um objeto flextable
  largura_colunas <- sapply(tabela, function(col) max(nchar(as.character(col)), na.rm = TRUE))
  largura_colunas <- cbind(largura_colunas,nchar(colnames(tabela)))
  largura_colunas <- apply(largura_colunas,1,max)/8 
  largura_colunas <- ifelse(largura_colunas<1,1,largura_colunas)
  largura_colunas <- ifelse(largura_colunas>2,2.2,largura_colunas)

   ft <- flextable(tabela) %>%
    width(j = 1:ncol(tabela), width = largura_colunas )  # Ajuste automático de largura


  if (file.exists(caminho_arquivo)) {
    ppt <- read_pptx(caminho_arquivo)  # Lê o arquivo existente
  } else {
    ppt <- read_pptx() 
  }

  horaa = format(Sys.time(), "%A %d de %B de %Y %H:%M") 
  texto1 = paste0("Criado em: ",horaa)

  # Criar o documento PowerPoint
  ppt <- ppt %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = fpar(ftext(titulo, prop = fp_text(font.size = 22))), location = ph_location_type(type = "title")) %>%
    ph_with(ft,location = ph_location(left = 2.81, top = 2.992))%>%
    ph_with(value = fpar(ftext(texto1, prop = fp_text(font.size = 20,bold=TRUE))), location = ph_location(left = 1.8, top =0.5,width = 14))

  # Salvar o arquivo no caminho especificado
  print(ppt, target = caminho_arquivo)
  
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

slides_compare <- function(titulo = "Tabela Gerada no R", caminho_arquivo = "saida_apresentacao.pptx",caminho_map1=NULL,caminho_map2=NULL,caminho_map3=NULL) {
  # Transformar tabela em um objeto flextable
  # largura_colunas <- sapply(tabela1, function(col) max(nchar(as.character(col)), na.rm = TRUE))
  # largura_colunas[1] <- 0.75 
  # largura_colunas[2] <- 0.75
  # largura_colunas[3] <- 1 
  # largura_colunas[4] <- 5.0 
  # largura_colunas[5] <- 0.8
  # largura_colunas[6] <- 1.4

  # ft <- flextable(tabela1) %>%
  #   width(j = 1:ncol(tabela1), width = largura_colunas) %>%
  #   bold(i =1, j =1:ncol(tabela1), part="header" )%>% 
  #   height(i = 1:nrow(tabela1),height=0.5) # Ajuste automático de largura

  if (file.exists(caminho_arquivo)) {
    ppt <- read_pptx(caminho_arquivo)  # Lê o arquivo existente
  } else {
    ppt <- read_pptx()  # Cria um novo arquivo se não existir
  }

  # Criar o documento PowerPoint
  ppt <- ppt %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = fpar(ftext(titulo, prop = fp_text(font.size = 22))), location = ph_location_type(type = "title"))
  
  # if((tabela2$Descritivo[1] != "Grupo 1") & (tabela2$Descritivo[1] != "Grupo 2"))
  # {
  #   ppt <- ppt %>%
  #     ph_with(ft,location = ph_location(left = 1, top = 1.5)) %>%
  #     ph_with(value = fpar(ftext("Resumo Descritivo", prop = fp_text(font.size = 14, color = "black",bold=TRUE))),location = ph_location(left = 1, top =-0.2 ))
  # }

  # ppt <- ppt %>%
  #   ph_with(ft2,location = ph_location(left = 1, top = 2.5)) %>%
  #   ph_with(ft3,location = ph_location(left = 1, top = 5.9))%>%
  #   ph_with(value = fpar(ftext("Avaliação de NA e Valores Únicos", prop = fp_text(font.size = 13, color = "black",bold=TRUE))),location = ph_location(left = 1.8, top =4.2 )) 

  #     if (!is.null(caminho_imagem) && file.exists(caminho_imagem)) {
  #   ppt <- ppt %>%
  #     ph_with(external_img(caminho_imagem, width =12.5 , height = 7.40,unit="cm"),  # Ajuste largura e altura
  #             location = ph_location(left = 4.2, top = 2.6),use_loc_size = FALSE )  # Ajuste posição
  # }

  if (!is.null(caminho_map1) && file.exists(caminho_map1)) 
  {
    ppt <- ppt %>%
      ph_with(external_img(caminho_map1, width =10.8 , height = 12.96,unit="cm"),  # Ajuste largura e altura
              location = ph_location(left = 0.53/2.5, top = 4.12/2.5),use_loc_size = FALSE )  # Ajuste posição
  }

  if (!is.null(caminho_map2) && file.exists(caminho_map2)) 
  {
    ppt <- ppt %>%
      ph_with(external_img(caminho_map2, width =10.8 , height = 12.96,unit="cm"),  # Ajuste largura e altura
              location = ph_location(left = 11.53/2.5, top = 4.12/2.5,unit="cm"),use_loc_size = FALSE )  # Ajuste posição
  }

  if (!is.null(caminho_map3) && file.exists(caminho_map3)) 
  {
    ppt <- ppt %>%
      ph_with(external_img(caminho_map3, width =10.8 , height = 12.96,unit="cm"),  # Ajuste largura e altura
              location = ph_location(left = 22.53/2.5, top = 4.12/2.5,unit="cm"),use_loc_size = FALSE )  # Ajuste posição
  }

  # Salvar o arquivo no caminho especificado
  print(ppt, target = caminho_arquivo)
  
  #cat("Apresentação criada com sucesso no arquivo:", caminho_arquivo, "\n")
}