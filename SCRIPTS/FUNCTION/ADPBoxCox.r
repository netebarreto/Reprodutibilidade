ADPBoxCox <- function(iData)
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
data_value <- iData[,-c(1:6)]
meta_bxcx <- data.frame(BoxCox = NA, 
                          Distorcao = apply(data_value,2,COINr::skew,na.rm=TRUE),
                          Curtose = apply(data_value,2,COINr::kurt,na.rm=TRUE))                     
meta_bxcx$BoxCox = ifelse(distorcao>=2 & curtose>=3.5, 1, 0)
nbcx = which(meta_bxcx$BoxCox == 1) 
data_bcx <- iData
if(length(nbcx)>0 ) { data_bcx[,nbcx]<-apply(iData[,nbcx],2,sfunc_bxcx)} 
result <- list(iMeta = meta_bxcx,
               iData = data_bcx)
return(result)}

