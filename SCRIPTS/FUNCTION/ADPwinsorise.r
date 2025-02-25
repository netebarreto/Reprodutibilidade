wins_par <- function(Y,a,b) 
              {   
                X <- as.numeric(Y)
                OLinf <-  min(X,na.rm=T)
                OLsup <-  max(X,na.rm=T)

                # Calculando os limites teoricos
                TLinf <- quantile(Y, 0.25,na.rm=T) - 1.5 * (quantile(Y, 0.75,na.rm=T) - quantile(Y, 0.25,na.rm=T))
                TLsup <- quantile(Y, 0.75,na.rm=T) + 1.5 * (quantile(Y, 0.75,na.rm=T) - quantile(Y, 0.25,na.rm=T))

                L_inf=max(c(OLinf,TLinf))                
                L_sup=ifelse(TLsup==0,OLsup,min(c(OLsup,TLsup)))

                n.na <- length(which(is.na(X)))

                resumo<-data.frame(iName=b,
                              Classe=a,
                              T.NA=n.na,
                              LINF=round(L_inf,2),
                              LSUP=round(L_sup,2),
                              T.outlier = length(which(X>L_sup | X<L_inf)))
                return(resumo)
                }



winsorise1 <- function(iData,iMeta) {
                      
    resumo_out<-NULL    
    
    ##### Aplicando para as colunas padrão 

      ncsc =  which(iMeta$Classe == "Numérico")   
      lcolz <- which(colnames(iData) %in% iMeta$Code[ncsc])
       list_winsor <-t(mapply(wins_par,iData[, lcolz], a = "Numérico", b = colnames(iData_bruto)[lcolz]))

      # lapply(seq_along(iData[, lcolz]), 
      #                 function(i) {
      #                 wins_par(iData[, lcolz], a = "Numérico", b = colnames(iData[, lcolz])[i])})

      resumo_out<-rbind(resumo_out,list_winsor)

    # Aplicando para as colunas do tipo "cluster" Urbano e Rural
    
      ncsc =  which(iMeta$Classe == "Cluster")   
      lcolz <- which(colnames(iData) %in% iMeta$Code[ncsc])
      nrurb <- which(iData$CLUSTER == 1) # Cluster Urbano
      nrrur <- which(iData$CLUSTER == 2) # Cluster Rural
      list_winsor <- t(mapply(wins_par,iData[nrurb, lcolz], a = "Grupo 1", b = colnames(iData_bruto)[lcolz]))
      
      
      # lapply(seq_along(iData[nrurb, lcolz]), 
      #                 function(i) {
      #                 wins_par(iData[nrurb, lcolz], a = "Grupo 1", b = colnames(iData[nrurb, lcolz])[i])})
      resumo_out<-rbind(resumo_out,list_winsor)


      list_winsor <-  t(mapply(wins_par,iData[nrrur, lcolz], a = "Grupo 2", b = colnames(iData_bruto)[lcolz]))
      
      # lapply(seq_along(iData[nrrur, lcolz]), 
      #                 function(i) {
      #                 wins_par(iData[nrrur, lcolz], a = "Grupo 2", b = colnames(iData[nrrur, lcolz])[i])})
      resumo_out<-rbind(resumo_out,list_winsor)
    
    # atualizando data.frame para as colunas do tipo "Score"
    
      ncsc =  which(iMeta$Classe == "Score")   
      lcolz <- which(colnames(iData) %in% iMeta$Code[ncsc])

      resumo_out <- rbind(resumo_out,data.frame(iName=iMeta$Code[ncsc],
                                                Classe="Score",
                                                T.NA=NA,
                                                LINF=NA,
                                                LSUP=NA,
                                                T.outlier = NA))

      ncsc =  which(iMeta$Classe == "Descricao")   
      lcolz <- which(colnames(iData) %in% iMeta$Code[ncsc])

      resumo_out <- rbind(resumo_out,data.frame(iName=iMeta$Code[ncsc],
                                                Classe="Descricao",
                                                T.NA=NA,
                                                LINF=NA,
                                                LSUP=NA,
                                                T.outlier = NA))
      Resumo <- as.data.frame(resumo_out)
      Resumo <- Resumo[order(match(Resumo$iName,iMeta_adapta$Code)),] 
      rownames(Resumo)<-c(1:NROW(Resumo))
      return(Resumo)
    }



ADPwinsorise <- function(iData,iMeta) 
{
   resumo1 = winsorise1(iData,iMeta)
   dados_out = iData
   dados_out[,1:NCOL(iData)] <- NA
   for(i in 1:NROW(resumo1))
   {
    if(resumo1[i,2]=="Descricao" | resumo1[i,2]=="Score") 
    {
     dados_out[unlist(resumo1[i,1])] <- iData[unlist(resumo1[i,1])]
    }
    if(resumo1[i,2]=="Numérico") 
    {
      X1 <- iData[unlist(resumo1[i,1])]
      X1[which(iData[unlist(resumo1[i,1])]>as.numeric(resumo1$LSUP[i])),1] <- as.numeric(resumo1$LSUP[i])

      X1[which(iData[unlist(resumo1[i,1])]<as.numeric(resumo1$LINF[i])),1] <- as.numeric(resumo1$LINF[i])
      dados_out[unlist(resumo1[i,1])] <- X1[,1]
    }

    if(resumo1[i,2]=="Grupo 1") 
    {
      X1a <- iData[unlist(resumo1[i,1])]
      X1=X1a[iData$CLUSTER==1,1]
      X1[which(X1>as.numeric(resumo1$LSUP[i]))] <- as.numeric(resumo1$LSUP[i])
      X1[which(X1<as.numeric(resumo1$LINF[i]))] <- as.numeric(resumo1$LINF[i])
      X1a[iData$CLUSTER==1,1]<-X1
      dados_out[unlist(resumo1[i,1])] <- X1a

    }

    
    if(resumo1[i,2]=="Grupo 2") 
    {
      X1a <- iData[unlist(resumo1[i,1])]
      X1=X1a[iData$CLUSTER==2,1]
      X1[which(X1>as.numeric(resumo1$LSUP[i]))] <- as.numeric(resumo1$LSUP[i])
      X1[which(X1<as.numeric(resumo1$LINF[i]))] <- as.numeric(resumo1$LINF[i])
      X1a[iData$CLUSTER==2,1]<-X1
      dados_out[unlist(resumo1[i,1])] <- X1a

    }

   }

   result <- list(Resumo=resumo1,iData=dados_out)
return(result)
}