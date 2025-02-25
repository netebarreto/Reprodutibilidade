




ADPBoxCox <- function(dados,classe,cluster,nome)
{
        sfunc_bxcx <- function(Y) 
                {
                Ysna = Y[-which(is.na(Y))]
                Ysna[Ysna==0] = Ysna[Ysna==0] +1E-5 
                lambda <- forecast::BoxCox.lambda(Ysna) 
                Ybcx = COINr::boxcox(Ysna,lambda=lambda)
                Yres = Y
                Yres[which(is.na(Y))] = NA
                Yres[-which(is.na(Y))] = Ybcx
                return(Yres)
                }
   if(classe == "Numérico") 
        {
        meta_bxcx <- data.frame(Nome=nome,
                          Classe="Numérico",
                          BoxCox = NA, 
                          Distorcao = COINr::skew(dados,na.rm=TRUE),
                          Curtose = COINr::kurt(dados,na.rm=TRUE)) 
        meta_bxcx$BoxCox = ifelse(as.numeric(meta_bxcx$Distorcao)>=2 & as.numeric(meta_bxcx$Curtose)>=3.5, 1, 0)
        {if (meta_bxcx$BoxCox == 1) 
         { data_bcx <-sfunc_bxcx(dados) }
        else(meta_bxcx$BoxCox == 0) 
        {data_bcx <-dados} }
        } 
#    else  if(classe == "Cluster") 
#         { data_bcx = dados
#         meta_bxcx1 <- data.frame(Nome=nome,
#                           Classe="Grupo 1",                
#                 BoxCox = NA, 
#                           Distorcao = COINr::skew(dados[cluster==1],na.rm=TRUE),
#                           Curtose = COINr::kurt(dados[cluster==1],na.rm=TRUE))
        
#         meta_bxcx2 <- data.frame(Nome=nome,
#                           Classe="Grupo 2",
#                 BoxCox = NA, 
#                           Distorcao = COINr::skew(dados[cluster==2],na.rm=TRUE),
#                           Curtose = COINr::kurt(dados[cluster==2],na.rm=TRUE))                   
#         meta_bxcx1$BoxCox = ifelse(as.numeric(meta_bxcx1$Distorcao)>=2 & as.numeric(meta_bxcx1$Curtose)>=3.5, 1, 0)

#         meta_bxcx2$BoxCox = ifelse(as.numeric(meta_bxcx2$Distorcao)>=2 & as.numeric(meta_bxcx2$Curtose)>=3.5, 1, 0)
#         meta_bxcx = rbind(meta_bxcx1,meta_bxcx2)

#        {if (!is.na(meta_bxcx1$BoxCox) & meta_bxcx1$BoxCox == 1) 
#         {data_bcx[dados[cluster==1]] <-sfunc_bxcx(dados[dados[cluster==1,]])} 
#         else( !is.na(meta_bxcx1$BoxCox) & meta_bxcx1$BoxCox == 0) 
#         {data_bcx[dados[cluster==1]] <-dados[dados[cluster==1]]}}

#         {if (!is.na(meta_bxcx2$BoxCox) & meta_bxcx2$BoxCox == 1) 
#         {data_bcx[dados[cluster==2]] <-sfunc_bxcx(dados[dados[cluster==2]])} 
#         else(!is.na(meta_bxcx2$BoxCox) & meta_bxcx2$BoxCox == 0) 
#         {data_bcx[dados[cluster==2]] <-dados[dados[cluster==2]]} }
#         } 

        else if(classe == "Score" | classe == "Cluster") 
       {meta_bxcx <- data.frame(Nome=nome,
                          Classe="Score",
                          BoxCox = NA, 
                          Distorcao = NA,
                          Curtose = NA) 
        meta_bxcx$BoxCox = NA
        data_bcx <- dados}

result <- list(iMeta = meta_bxcx,
               iData = data_bcx)
return(result)}

