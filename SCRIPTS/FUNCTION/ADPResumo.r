#### CRIAÇÃO DOS RESUMOS SEM WINSORISAÇÃO ##### 
sum_start <- function(x,y=NULL,z,b) 
                { if(y == "Numérico") 
                      {fnum = fivenum(x) 
                      resumo<-cbind(iName=b,
                                      Classe="Numérico",
                                      Min=fnum[1],
                                      quartil1=fnum[2],
                                      Mediana=fnum[3],
                                      quartil3=fnum[4],
                                      Max=fnum[5])}
                    else  if(y == "Cluster")
                            {fnum1 = fivenum(x[z==1]) 
                            resumo1<-cbind(iName=b,
                                      Classe="Grupo 1",
                                      Min=round(fnum1[1],2),
                                      quartil1=round(fnum1[2],2),
                                      Mediana=round(fnum1[3],2),
                                      quartil3=round(fnum1[4],2),
                                      Max=round(fnum1[5],2))
                            fnum2 = fivenum(x[z==2]) 
                            resumo2<-cbind(iName=b,
                                      Classe="Grupo 2",
                                      Min=round(fnum2[1],2),
                                      quartil1=round(fnum2[2],2),
                                      Mediana=round(fnum2[3],2),
                                      quartil3=round(fnum2[4],2),
                                      Max=round(fnum2[5],2))
                            resumo <- rbind(resumo1,resumo2)          
                                      }
                            else
                            {resumo<-cbind(iName=b,
                                      Classe="Score",
                                      Min=NA,
                                      quartil1=NA,
                                      Mediana=NA,
                                      quartil3=NA,
                                      Max=NA)}
                return(resumo)}                             

res_bruto<-function(x1,y1,z1,b1)
                {
                   res1 = mapply(sum_start,x=as.data.frame(x1), y = y1,z=as.data.frame(z1),b=b1)
                   res2 = do.call(rbind,lapply(res1, function(x) x))

                   return(res2)
                }