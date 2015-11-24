train <- kr_trainf1
test <- kr_f1
#Fold 2
train <- kr_trainf2
test <- kr_f2
#Fold 3
train <- kr_trainf3
test <- kr_f3
#Fold 4
train <- kr_trainf4
test <- kr_f4
#Fold 5
train <- kr_trainf5
test <- kr_f5

plot(perftest6, col="blue",lwd=.1,main="ROC Curve for GLM Using Kills")
par(new=TRUE)
plot(perftest6, col="purple", new=FALSE,lwd=.1)
par(new=TRUE)
plot(perftest6, col="red", new=FALSE,lwd=.1)
par(new=TRUE)
plot(perftest6, col="green", new=FALSE,lwd=.1)
par(new=TRUE)
plot(perftest6, col="black", new=FALSE,lwd=.1)


