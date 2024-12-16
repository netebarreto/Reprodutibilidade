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
                            else if(y == "Score")
                            {resumo<-cbind(iName=b,
                                      Classe="Score",
                                      Min=NA,
                                      quartil1=NA,
                                      Mediana=NA,
                                      quartil3=NA,
                                      Max=NA)}
                                 else if (y == "Descricao")
                                      {resumo<-cbind(iName=b,
                                      Classe="Descricao",
                                      Min=NA,
                                      quartil1=NA,
                                      Mediana=NA,
                                      quartil3=NA,
                                      Max=NA)}
                return(resumo)}                             

ADPResumo<-function(dbruto,classe,m_cluster,nome_col)
                { 
                   res1 = mapply(sum_start,x=as.data.frame(dbruto), y = classe,z=as.data.frame(m_cluster),b=nome_col)
                   res2 = do.call(rbind,lapply(res1, function(x) x))
                  res3 = tibble::as_tibble(res2)

                  res3 <- dplyr::mutate(res3,
                        Min = as.numeric(Min),
                        Quartil1 = as.numeric(quartil1),
                        Mediana = as.numeric(Mediana),
                        Quartil3 = as.numeric(quartil3),
                        Max = as.numeric(Max))
                   return(res3)
                }
