
Tratamento <- function(input="INPUT.xlsx",
                       iMeta = "Plan_Metadados",
                       iData  = "Plan_Dados")
{
  inxlsx       <- openxlsx::loadWorkbook(file = input)
  iMeta_adapta <- openxlsx::read.xlsx(inxlsx, sheet = iMeta)
  iData_bruto  <- openxlsx::read.xlsx(inxlsx, sheet = iData)

  
imeta_N7 = subset(iMeta_adapta,Nivel==7)
data_ref = iData_bruto[,c(1:4)]
idata_N7 = round(iData_bruto[,-c(1:4)],2)
resumo <- ADPresumo(idata_N7, imeta_N7$Classe, data_ref[,4], colnames(idata_N7))

data_winsor <- ADPwinsorise(iData=idata_N7,iMeta=imeta_N7,iRef=data_ref[,4])

data_bxcx <- ADPBoxCox(data_winsor$iData,idata_N7,imeta_N7$Classe,data_ref[,4],
                      colnames(idata_N7),metodo="forecast")
data_normal <- ADPNormalise(data_bxcx$data,colnames(idata_N7))

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
    dadosnorm = data.frame(data_ref[,-4],data_normal)
    openxlsx::writeData(xlsx_dados,"Normalizado",dadosnorm, startCol = 1,  startRow = 1)

    openxlsx::saveWorkbook(xlsx_res,"Analise_Descritiva.xlsx",overwrite = TRUE)
    openxlsx::saveWorkbook(xlsx_dados,"Dados_Tratados.xlsx",overwrite = TRUE)

    output_result <- list(Ref = data_ref,
    Resumo = resumo,
    iMeta = imeta_N7,
    DadosB = idata_N7,
    Data_Win = data_winsor,
    Data_Bxc = data_bxcx,
    Data_Normal=data_normal)
    return(output_result)
}
