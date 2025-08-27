#' Tratamento de Dados com Winsorização, Box-Cox e Normalização
#'
#' Realiza o pré-processamento de dados a partir de um arquivo Excel, aplicando
#' estatísticas descritivas, Winsorização, transformação Box-Cox e Normalização.
#' Os resultados são exportados em arquivos `.xlsx` na pasta `OUTPUT/`.
#'
#' @param input String. Caminho do arquivo Excel de entrada. 
#'              Default = "INPUT.xlsx".
#' @param iMeta String. Nome da planilha que contém os metadados.
#'              Default = "Plan_Metadados".
#' @param iData String. Nome da planilha que contém os dados brutos.
#'              Default = "Plan_Dados".
#' @param method_boxcox String. Método a ser utilizado na transformação Box-Cox
#'                      (ex.: "forecast"). Default = "forecast".
#' @param sigla String. Sigla usada na composição dos nomes dos arquivos de saída.
#'              Default = "SE".
#' @param subsetor String ou NULL. Define um subtipo/setor para diferenciar
#'                 os arquivos de saída. Default = NULL.
#'
#' @details
#' O fluxo de processamento segue as etapas:
#' 1. Leitura de metadados e dados brutos.
#' 2. Seleção de registros de nível 7 e arredondamento dos valores.
#' 3. Estatísticas descritivas com `ADPresumo()`.
#' 4. Winsorização com `ADPwinsorise()`.
#' 5. Transformação Box-Cox com `ADPBoxCox()`.
#' 6. Normalização com `ADPNormalise()`.
#' 7. Geração de dois arquivos Excel:
#'    - `ANALISE_DESCRITIVA_...xlsx`: tabelas descritivas, Winsorização e Box-Cox.
#'    - `DADOS_TRATADOS_...xlsx`: dados brutos, pós-Winsor, pós-BoxCox e normalizados.
#'
#' @return Uma lista com:
#' \itemize{
#'   \item \code{Ref}: Dados de referência.
#'   \item \code{Resumo}: Estatísticas descritivas (resultado de `ADPresumo`).
#'   \item \code{iMeta}: Metadados filtrados para nível 7.
#'   \item \code{DadosB}: Dados brutos arredondados.
#'   \item \code{Data_Win}: Resultado da Winsorização.
#'   \item \code{Data_Bxc}: Resultado da transformação Box-Cox.
#'   \item \code{Data_Normal}: Dados normalizados.
#' }
#'
#' @examples
#' \dontrun{
#' # Executar o tratamento com arquivo padrão:
#' resultado <- Tratamento()
#'
#' # Executar com planilhas específicas e sigla:
#' resultado <- Tratamento(input = "meu_arquivo.xlsx",
#'                         iMeta = "Metadados",
#'                         iData = "Dados",
#'                         sigla = "NE")
#' }
#'
#' @export
Tratamento <- function(input="INPUT.xlsx",
                       iMeta = "Plan_Metadados",
                       iData  = "Plan_Dados",
                       method_boxcox = "forecast",
                       sigla="SE",
                       subsetor=NULL)
{
  inxlsx       <- openxlsx::loadWorkbook(file = input)
  iMeta_adapta <- openxlsx::read.xlsx(inxlsx, sheet = iMeta)
  iData_bruto  <- openxlsx::read.xlsx(inxlsx, sheet = iData)

  
imeta_N7 = subset(iMeta_adapta,Nivel==7)
data_ref = iData_bruto[,c(1:4)]
idata_N7 = round(iData_bruto[,-c(1:4)],2)
colnames(idata_N7) <- colnames(iData_bruto[,-c(1:4)])
resumo <- ADPresumo(idata_N7, imeta_N7$Classe, data_ref[,4], colnames(idata_N7))

data_winsor <- ADPwinsorise(iData=idata_N7,iMeta=imeta_N7,iRef=data_ref[,4])

data_bxcx <- ADPBoxCox(data_winsor$iData,idata_N7,imeta_N7$Classe,data_ref[,4],
                      colnames(idata_N7),metodo=method_boxcox)
data_normal <- ADPNormalise(data_bxcx$data)

    xlsx_res <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(xlsx_res, "Descritivo")
    openxlsx::writeData(xlsx_res,"Descritivo",resumo$resumo_total, startCol = 1,  startRow = 1)

    openxlsx::addWorksheet(xlsx_res, "Winsorization")
    openxlsx::writeData(xlsx_res,"Winsorization",data_winsor$Resumo, startCol = 1,  startRow = 1)

    openxlsx::addWorksheet(xlsx_res, "BoxCox")
    openxlsx::writeData(xlsx_res,"BoxCox",data_bxcx$meta, startCol = 1,  startRow = 1)

    
    xlsx_dados <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(xlsx_dados, "BNivel 7")
    dadosn7 = data.frame(data_ref[,-4],idata_N7)
    openxlsx::writeData(xlsx_dados,"BNivel 7",dadosn7, startCol = 1,  startRow = 1)

    openxlsx::addWorksheet(xlsx_dados, "Winsorization")
    dadoswin = data.frame(data_ref[,-4],data_winsor$iData)
    openxlsx::writeData(xlsx_dados,"Winsorization",dadoswin, startCol = 1,  startRow = 1)

    openxlsx::addWorksheet(xlsx_dados, "BoxCox")
    dadosbxc = data.frame(data_ref[,-4],data_bxcx$data)
    openxlsx::writeData(xlsx_dados,"BoxCox",dadosbxc, startCol = 1,  startRow = 1)

    openxlsx::addWorksheet(xlsx_dados, "Normalizado")
    dadosnorm = data.frame(data_ref[,-4],data_normal$iData)
    openxlsx::writeData(xlsx_dados,"Normalizado",dadosnorm, startCol = 1,  startRow = 1)
##### Gerando nome do arquivo excel #####
      if(!is.null(subsetor)) {
        outfilex1 <- paste0("OUTPUT/ANALISE_DESCRITIVA_",sigla,subsetor,"_",format(Sys.time(),"%Y-%m-%d_%Hh%Mm"),".xlsx")
        
        outfilex2 <- paste0("OUTPUT/DADOS_TRATADOS_",sigla,subsetor,"_",format(Sys.time(),"%Y-%m-%d_%Hh%Mm"),".xlsx")}
      else {
        outfilex1 <- paste0("OUTPUT/ANALISE_DESCRITIVA_",sigla,"_",format(Sys.time(),"%Y-%m-%d_%Hh%Mm"),".xlsx")
        outfilex2 <- paste0("OUTPUT/DADOS_TRATADOS_",sigla,"_",format(Sys.time(),"%Y-%m-%d_%Hh%Mm"),".xlsx")}

    openxlsx::saveWorkbook(xlsx_res,outfilex1,overwrite = TRUE)
    openxlsx::saveWorkbook(xlsx_dados,outfilex2,overwrite = TRUE)
    cat("\n Arquivos .xlsx Gerados \n ",outfilex1,"\n ",outfilex2,"\n")
    output_result <- list(Ref = data_ref,
    Resumo = resumo,
    iMeta = imeta_N7,
    DadosB = idata_N7,
    Data_Win = data_winsor,
    Data_Bxc = data_bxcx,
    Data_Normal=data_normal)
    return(output_result)
}
