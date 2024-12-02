
#
### Função que aplica o Winsorization ###
#

ADPwinsorise <- function(iData,iMeta) {
                      
                yy <- function(Y,a,b) {   
                      X <- as.numeric(Y)
                      OLinf <-  min(X,na.rm=T)
                      OLsup <-  max(X,na.rm=T)
                      X2<-new.env()
                      # Calculando os limites teoricos
                      TLinf <- quantile(Y, 0.25,na.rm=T) - 1.5 * (quantile(Y, 0.75,na.rm=T) - quantile(Y, 0.25,na.rm=T))
                      TLsup <- quantile(Y, 0.75,na.rm=T) + 1.5 * (quantile(Y, 0.75,na.rm=T) - quantile(Y, 0.25,na.rm=T))
                      L_sup=min(c(OLsup,TLsup))
                      L_inf=max(c(OLinf,TLinf))
                      n.na <- length(which(is.na(X)))
                      X1 <- X
                      X1[which(X>=L_sup)] <- L_sup
                      X1[which(X<=L_inf)] <- L_inf
                      X2$resumo<-cbind(iName=b,
                                      Classe=a,
                                      T.NA=n.na,
                                      LINF=round(L_inf,2),
                                      LSUP=round(L_sup,2),
                                      T.outlier = length(which(X>=L_sup | X<=L_inf)))
                      X2$iData_winsor <- X1
                      X2 = as.list(X2)                  
                      return(X2)
                    }
    iData_out <- data.frame(matrix(ncol = ncol(iData), nrow = nrow(iData)))
    colnames(iData_out) <- colnames(iData)
    iData_out[,1:6] = iData[,1:6]
    resumo_out<-NULL    
    
    ##### Aplicando para as colunas padrão 

      ncsc =  which(iMeta$Classe == "Numérico")   
      lcolz <- which(colnames(iData) %in% iMeta$Code[ncsc])
      list_winsor <- lapply(seq_along(iData[, lcolz]), 
                      function(i) {
                      yy(iData[, lcolz][[i]], a = "Numérico", b = colnames(iData[, lcolz])[i])})
      iData_out[,lcolz] <- do.call(cbind,lapply(list_winsor, function(x) x$iData_winsor))
      resumo_out<-rbind(resumo_out,do.call(rbind,lapply(list_winsor, function(x) x$resumo)))

    # Aplicando para as colunas do tipo "cluster" Urbano e Rural
    
      ncsc =  which(iMeta$Classe == "Cluster")   
      lcolz <- which(colnames(iData) %in% iMeta$Code[ncsc])
      nrurb <- which(iData$Cluster == 1) # Cluster Urbano
      nrrur <- which(iData$Cluster == 2) # Cluster Rural
      list_winsor <- lapply(seq_along(iData[nrurb, lcolz]), 
                      function(i) {
                      yy(iData[nrurb, lcolz][[i]], a = "Grupo 1", b = colnames(iData[nrurb, lcolz])[i])})
      resumo_out<-rbind(resumo_out,do.call(rbind,lapply(list_winsor, function(x) x$resumo)))
      iData_out[nrurb,lcolz] <- do.call(cbind,lapply(list_winsor, function(x) x$iData_winsor))

      list_winsor <- lapply(seq_along(iData[nrrur, lcolz]), 
                      function(i) {
                      yy(iData[nrrur, lcolz][[i]], a = "Grupo 2", b = colnames(iData[nrrur, lcolz])[i])})
      resumo_out<-rbind(resumo_out,do.call(rbind,lapply(list_winsor, function(x) x$resumo)))

      iData_out[nrrur,lcolz] <- do.call(cbind,lapply(list_winsor, function(x) x$iData_winsor))
    
    # atualizando data.frame para as colunas do tipo "Score"
    
      ncsc =  which(iMeta_adapta$Classe == "Score")   
      lcolz <- which(colnames(iData) %in% iMeta$Code[ncsc])
      iData_out[,lcolz] <- iData[,lcolz]
      resumo_out <- rbind(resumo_out,cbind(iMeta$Code[ncsc],"Score",NA,NA,NA,NA))
      result=list(Resumo = as.data.frame(resumo_out),
                  Winsorise = iData_out )
      return(result)
    }
