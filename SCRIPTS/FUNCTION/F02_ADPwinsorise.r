
wins_par <- function(Y,a,b) 
              {   
              # Funçao calcula os limites de Winsorization
              # Y = iData[,i] 
              # a = iMeta$Classe[i]
              # b = colnames(iData)[i] 
              # Retornando uma lista com os limites de Winsorization
              # o número de valores NA e o número de outliers
              
                X <- as.numeric(Y)
                OLinf <-  min(X,na.rm=T)
                OLsup <-  max(X,na.rm=T)

                # Calculando os limites teoricos
                TLinf <- quantile(Y, 0.25,na.rm=T) - 1.5 * (quantile(Y, 0.75,na.rm=T) - quantile(Y, 0.25,na.rm=T))
                TLsup <- quantile(Y, 0.75,na.rm=T) + 1.5 * (quantile(Y, 0.75,na.rm=T) - quantile(Y, 0.25,na.rm=T))

                L_inf=max(c(OLinf,TLinf))                
                L_sup=ifelse(TLsup==0,OLsup,min(c(OLsup,TLsup)))

                Awin = ifelse(L_inf!=OLinf & L_sup != OLsup,"A",
                            ifelse(L_inf==OLinf & L_sup != OLsup,"S",
                                   ifelse(L_inf!=TLinf & L_sup == OLsup,"I","N")))

                n.na <- length(which(is.na(X)))

                resumo<-data.frame(iName=b,
                              Classe=a,
                              Aplicacao=Awin,
                              LINF=round(L_inf,2),
                              LSUP=round(L_sup,2),
                              T.outlier = length(which(X>L_sup | X<L_inf)),
                              T.NA=n.na)
                return(resumo)
                }



winsorise1 <- function(iData,iMeta,iRef) {
  ##### Função que aplica o Winsorization
  # iData = iData_N7 
  # iMeta = iMeta_N7
  # utiliza a função wins_par para calcular os limites de Winsorization
  # retornado  o resumo de cada variável e os dados com os limites de Winsorization
  
  
    ##### Aplicando para as colunas padrão 

      ncsc =  which(iMeta$Classe == "Numérico")   
      lcolz <- which(colnames(iData) %in% iMeta$Code[ncsc])
      list_winsor <-t(mapply(wins_par,iData[, lcolz], a = "Numérico", b = colnames(iData)[lcolz]))

      if(any(iMeta$Classe == "Cluster")) {
      ## Aplicando para as colunas de Cluster
      # iRef = iData_ref[,4]    
      ncsc1 =  which(iMeta$Classe == "Cluster") 
      lcolzg1 <- which(colnames(iData) %in% iMeta$Code[ncsc])
      nrowzg1 <- iData[which(iRef == 1),lcolzg1]
      list_winsor1 <-t(mapply(wins_par,nrowzg1, a = "Grupo 1", b = colnames(iData)[lcolzg1]))
      
      nrowzg2 <- iData[which(iRef == 2),lcolzg1]
      list_winsor2 <-t(mapply(wins_par,nrowzg2, a = "Grupo 2", b = colnames(iData)[lcolzg1]))

      list_winsor <- rbind(list_winsor,list_winsor1,list_winsor2)
      }

      Resumo <- as.data.frame(list_winsor)
      Resumo <- Resumo[order(match(Resumo$iName,iMeta$Code)),] 
      rownames(Resumo)<-c(1:NROW(Resumo))
      return(Resumo)
    }



ADPwinsorise <- function(iData=NULL,iMeta=NULL,iRef=NULL) 
{ # Função que aplica o Winsorization
  # iData = iData_N7
  # iMeta = iMeta_N7
  # iRef = iData_ref[,4] 

   resumo1 = winsorise1(iData,iMeta,iRef)
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
