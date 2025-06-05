

### Essa rotina Gera uma apresnetação com os princiapais resultados de comparação  ####


f1()
### 

library(officer)
library(flextable)
library(ggplot2)
library(dplyr)

source("SCRIPTS/FUNCTION/ADPCriar_Ppptx.R")
source("SCRIPTS/FUNCTION/ADPGraficos.R")

resumo_ref <- subset(iMeta_adapta,Nivel==0)

# Criando uma apresentação com a tabela exemplo
criar_slide_01(
  tabela = resumo_ref,
  titulo = "METADADOS INICIAIS",
  caminho_arquivo = "ADAPTA_RESUMO_V01.pptx"
)

caminho_shp_mun='DATASET/SHP/BR_Municipios_2022.shp'
caminho_shp_uf='DATASET/SHP/BR_UF_2022.shp'

shp_mun1<-sf::st_transform(sf::st_read(caminho_shp_mun),crs=4326)
shp_uf1<-sf::st_transform(sf::st_read(caminho_shp_uf),crs=4326)

res_basico <- resumo_bruto$resumo_basico


 for(i in 1)#:ncol(res_basico)) 
 {
  tab.descricao <- iMeta_adapta[which(iMeta_adapta$Code==colnames(res_basico)[i]),-7]

  tab_stats <- tibble(
    Resumo = rownames(resumo_bruto[[2]]),
    Descritivo = resumo_bruto[[2]][,i]) 
  colnames(tab_stats)[1] <- c("Resumo Estatítico")

  tab_NAs = resumo_bruto$resumo_na[i,-1]
  
  icode = tab.descricao$Code
  iclasse = tab_stats$Descritivo[1]
  title_slide = ifelse(iclasse == "Grupo 1",
                  paste0(tab.descricao$Nome," - ",icode," (GRUPO 1)"),
                    ifelse(iclasse == "Grupo 2",
                    paste0(tab.descricao$Nome," - ",icode," (GRUPO 2)"),
                    ifelse(iclasse == "Conjunto Completo",
                  paste0(tab.descricao$Nome," - ",icode," (GERAL)"),paste0(tab.descricao$Nome," - ",tab.descricao$Code))))

  idata=as.data.frame(iData_N7[[icode]])
  colnames(idata)=icode

  if(iclasse == "Grupo 1") {idata[iData_ref$CLUSTER!=1,] = NA}

  if(iclasse == "Grupo 2") {idata[iData_ref$CLUSTER!=2,] = NA}
  Titulo_map=paste0(c("Indicador Bruto: ",
                      "Indicador Winsorization: ",
                      "Reference Winsorization: ",
                      "Diferença Winsorization: "),icode)


  criar_grafico(idata, 
                nome_arquivo = "gra_descricao.png", 
                largura = 24, altura = 12, 
                dpi = 300,
                nvalores=icode,
                fsize=50)

  Map_result(icode,idata,iData_ref["GEOCOD"],
              Titulo = Titulo_map[1],
              shp_mun=shp_mun1,
              shp_uf=shp_uf1,
              nome_arquivo = "map_bruto.png", 
              largura = 20, altura = 24, dpi = 300,fsize=36)


  # Criando uma apresentação com a tabela exemplo
  slides_descricao(tabela1 = tab.descricao,
                tabela2=tab_stats,
                tabela3 = tab_NAs,
                titulo = title_slide,
                caminho_arquivo =   "ADAPTA_RESUMO_V01.pptx",
                caminho_imagem = "gra_descricao.png",
                caminho_map="map_bruto.png" )

#####
# Winsorização 
#####

  iwinso=as.data.frame(iData_winsor$iData[[icode]])
  iwinsoREF =as.data.frame( winsorise_plan[[icode]]) 
  colnames(iwinso)=icode
  colnames(iwinsoREF)=icode
  Diff_winso =iwinso-iwinsoREF 

  Map_result(icode,iwinso,iData_ref["GEOCOD"],
              Titulo=Titulo_map[2],
              shp_mun=shp_mun1,
              shp_uf=shp_uf1,
              nome_arquivo = "map_Wins.png", 
              largura = 20, altura = 24, dpi = 300,fsize=36)
  
  Map_result(icode,iwinsoREF,iData_ref["GEOCOD"],
              Titulo=Titulo_map[3],
              shp_mun=shp_mun1,
              shp_uf=shp_uf1,
              nome_arquivo = "map_Wins_REF.png", 
              largura = 20, altura = 24, dpi = 300,fsize=36)
  
  Map_result(icode,Diff_winso,iData_ref["GEOCOD"],
              Titulo=Titulo_map[4],
              shp_mun=shp_mun1,
              shp_uf=shp_uf1,
              nome_arquivo = "map_DIFF.png", 
              largura = 20, altura = 24, dpi = 300,fsize=36)

slides_compare(titulo = paste0("Comparação Winsorezation: ",icode), 
              caminho_arquivo = "ADAPTA_RESUMO_V01.pptx",
              caminho_map1="map_Wins.png",
              caminho_map2="map_Wins_REF.png",
              caminho_map3="map_DIFF.png")


#####
# Winsorização 
#####
  Ttl_map_bxcx=paste0(c("Indicador BoxCox: ",
                      "Reference BoxCox: ",
                      "Diferença BoxCox: "),icode)

  iboxcox=as.data.frame(iData_boxcox$iData[[icode]])
  iboxcoxREF =as.data.frame( boxcox_plan[[icode]]) 
  colnames(iboxcox)=icode
  colnames(iboxcoxREF)=icode
  Diff_boxcox =iboxcox-iboxcoxREF 

  Map_result(icode,iboxcox,iData_ref["GEOCOD"],
              Titulo=Ttl_map_bxcx[1],
              shp_mun=shp_mun1,
              shp_uf=shp_uf1,
              nome_arquivo = "map_BoxCox.png", 
              largura = 20, altura = 24, dpi = 300,fsize=36)
  
  Map_result(icode,iboxcoxREF,iData_ref["GEOCOD"],
              Titulo=Ttl_map_bxcx[2],
              shp_mun=shp_mun1,
              shp_uf=shp_uf1,
              nome_arquivo = "map_Boxcox_REF.png", 
              largura = 20, altura = 24, dpi = 300,fsize=36)
  
  Map_result(icode,Diff_boxcox,iData_ref["GEOCOD"],
              Titulo=Ttl_map_bxcx[3],
              shp_mun=shp_mun1,
              shp_uf=shp_uf1,
              nome_arquivo = "map_DIFF_bxcx.png", 
              largura = 20, altura = 24, dpi = 300,fsize=36)


slides_compare(titulo = paste0("Comparação BoxCox: ",icode), 
              caminho_arquivo = "ADAPTA_RESUMO_V01.pptx",
              caminho_map1="map_BoxCox.png",
              caminho_map2="map_Boxcox_REF.png",
              caminho_map3="map_DIFF_bxcx.png")


 print(paste0(i,"  ",ncol(res_basico)))

}



shell("ADAPTA_RESUMO_V01.pptx")


