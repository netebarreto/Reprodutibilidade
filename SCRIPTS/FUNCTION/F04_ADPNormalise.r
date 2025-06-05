
        sfunc_norm <- function(Y) 
                {
                X=as.numeric(Y)
                Xmax=max(X,na.rm=TRUE)
                Xmin=min(X,na.rm=TRUE)
                Yres = (X-Xmin)/(Xmax-Xmin)
                return(Yres)
                } 

ADPNormalise <- function(iData)
{

data_value <- iData[,-c(1:6)]
data_norm <- iData
data_norm[,-c(1:6)]<-apply(data_value,2,sfunc_norm) 
result <- list(iData = data_norm)
return(result)}
