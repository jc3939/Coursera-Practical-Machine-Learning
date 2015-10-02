library(ElemStatLearn)
data(ozone)
ozone<-ozone[order(ozone$ozone),]
head(ozone)

ll<-matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
    ss<-sample(1:dim(ozone)[1],replace=T)
    ozone0=ozone[ss,]
    ozone0=ozone[order(ozone$ozone),]
    loess0 <- loess(temperature~ozone,data=ozone0,span=0.2)
    ll[i,]<-predict(loess0,newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

#Bagging in caret, 
#bagEarth, treebag, bagFDA