
sfunc_bxcx <- function(Y,metodo) 
                {
                Ysna = Y[-which(is.na(Y))]
                Ysna[Ysna==0] = Ysna[Ysna==0] +0.001 
                lambda <- forecast::BoxCox.lambda(Ysna)
                if(metodo=="forecast" )        
                {Ybcx = forecast::BoxCox(Ysna,lambda=lambda)}
                else if(metodo=="COINr")
                {Ybcx = COINr::boxcox(Ysna,lambda=lambda)}
                else if(metodo=="yeojohnson")
                {Ybcx =  bestNormalize::yeojohnson(Ysna)$x.t}
                Yres = Y
                Yres[which(is.na(Y))] = NA
                Yres[-which(is.na(Y))] = Ybcx
                return(Yres)
                }


ADPBoxCox <- function(dadoswin,dados,classe,cluster,nome,metodo)
{
   prec_boxcox<-function(dadoswin,dados,classe,cluster,nome,metodo) 
   { 
       if(classe == "Numérico") 
        {
        meta_cx <- data.frame(Nome=nome,
                          Classe="Numérico",
                          BoxCox = NA, 
                          Distorcao = COINr::skew(dadoswin,na.rm=TRUE),
                          Curtose = COINr::kurt(dadoswin,na.rm=TRUE),
                          Metodo = metodo) 
        meta_cx$BoxCox = ifelse(is.na(meta_cx$Distorcao) | is.na(meta_cx$Curtose),0,ifelse(as.numeric(meta_cx$Distorcao)>=2 & as.numeric(meta_cx$Curtose)>=3.5, 1, 0))
        {
        if (meta_cx$BoxCox == 1 ) 
        data_bx <-sfunc_bxcx(dados,metodo) 
        else 
         {
          if (meta_cx$BoxCox == 0 )
           data_bx <-dadoswin
         }
        }}
         res1 <- list(meta = meta_cx, data = data_bx)
 return(res1)
        }
data_bcx =NULL
meta_bcx =NULL

for(i in 1:NCOL(dados))
{
res = prec_boxcox(dadoswin[,i],dados[,i],classe[i],cluster,nome[i],metodo)
meta_bcx <- rbind(meta_bcx,res$meta)
data_bcx <- cbind(data_bcx,res$data)
colnames(data_bcx)[i]<-nome[i]
}

 result <- list(meta = meta_bcx, data = as.data.frame(data_bcx))
 return(result)
}
 
