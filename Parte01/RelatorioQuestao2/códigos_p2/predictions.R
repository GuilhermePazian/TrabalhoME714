predz <- function(modelo1,datasize){
library(pROC)
R <- 500
mestat1 <- matrix(0,R,4)

p1 <- predict(modelo1,type="response")

for (r in 1:R)
{
  pref1<- rbinom(datasize,1,p1)

  #
  t1 <- table(v.pref,pref1)
  #
  
  return(kkk=list(
  VP1 <- t1[2,2] ,
  VN1 <- t1[1,1],
  FP1 <- t1[1,2],
  FN1 <-  t1[2,1],
  sens1 <- VP1/(VP1+FN1) ,
  RFN1 <- FN1/(VP1+FN1),
  RFP1 <- FP1/(FP1+VN1),
  espe1 <- VN1/(FP1+VN1)
  ))
  mestat1[r,] <- c(sens1,RFN1,RFP1,espe1)
}

colnames(mestat1)<-c("Sens.(M1)", "RFN(M1)","RFP(M1)","Espec.(M1)")

par(mfrow=c(1,1))
boxplot(mestat1)

par(mfrow=c(1,2))
roc.plot(v.pref,p1,xlab="1-especificidade (falso positivo)",ylab="sensibilidade",cex=1.2,cex.lab=1.2,cex.main=1.2,main="modelo 1")

}