

input <- read.csv("DATASET/Exemplo_BoxCox.csv")

index = input[-which(is.na(input[,1])),1]

index[index==0] = index[index==0] +0.001 
lambda <- forecast::BoxCox.lambda(index)

data_bx1 <-forecast::BoxCox(index,lambda=lambda)
data_bx2 <-COINr::boxcox(index,lambda=lambda)   
data_bx3 <-bestNormalize::yeojohnson(index)$x.t

png("OUTPUT/Exemplo_Boxplot.png",height=400,width=900)
par(mfrow=c(1,3))
par(mar=c(5,5,6,1))
plot(index,data_bx1,main = "Transformação BoxCox \n via pacote Forecast",cex.axis=1.5,las=1,ylab="Transformada BoxCox",xlab="Reference",cex.lab=1.8,cex.main=2)
plot(index,data_bx2,main = "Transformação BoxCox \n via pacote COINr",cex.axis=1.5,las=1,ylab="Transformada BoxCox",xlab="Reference",cex.lab=1.8,cex.main=2)
plot(index,data_bx3,main = "Transformação BoxCox \n via pacote bestNormalize::Yeojohnson",cex.axis=1.5,las=1,ylab="Transformada BoxCox",xlab="Reference",cex.lab=1.8,cex.main=2)
dev.off()
