rm(list=ls())
ds <- read.csv("C:\\Users\\Anna\\Downloads\\WA_Fn-UseC_-HR-Employee-Attrition2.csv", header = TRUE)
ds2 <- ds[, 7:8]
tmp<-kmeans(ds2, centers = 5, iter.max = 1000)
centers <- tmp$centers[tmp$cluster, ]
distances <- sqrt(rowSums((ds2 - centers)^2))
outliers <- order(distances, decreasing=T)[1:10]
plot(ds2[tmp$cluster==1,], col="red", xlim=c(min(ds2[,1]),max(ds2[,1])),ylim=c(min(ds2[,2]),max(ds2[,2])))
points(ds2[tmp$cluster==2,], col="blue")
points(ds2[tmp$cluster==3,], col="seagreen")
points(ds2[tmp$cluster==4,], col="orange")
points(ds2[tmp$cluster==5,], col="deeppink")
points(tmp$centers, col="black")
points(ds2[outliers,], col="black", pch="X")
