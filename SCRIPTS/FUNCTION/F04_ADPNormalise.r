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

data_value <- iData
data_norm <- iData
data_norm <-apply(data_value,2,sfunc_norm) 
result <- list(iData = data_norm)
return(result)}
