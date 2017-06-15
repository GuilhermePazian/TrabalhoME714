# Programa extraído do site: https://www.ime.usp.br/~giapaula/textoregressao.htm
# Créditos: Prof. Dr. Gilberto Alvarenga Paula
# Adaptado por Caio L. N. Azevedo
# source("D:\\windows\\Unicamp\\Disciplinas\\1_semestre_2016\\ME 720 MLG\\Programas\\diag_nbin.r")

diagnbin<-function(fit.model){
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
fi <- fit.model$theta
w <- fi*fitted(fit.model)/(fi + fitted(fit.model))
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model,type="pearson")/sqrt(1-h)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
di <- (h/(1-h))*(ts^2)
par(mfrow=c(2,2))
a <- max(td)
b <- min(td)
plot(td,xlab="Índice", ylab="Resíduo Componente do Desvio",
ylim=c(b-1,a+1), pch=16,cex=1.1,cex.axis=1.1,cex.lab=1.1)
abline(2,0,lty=2)
abline(-2,0,lty=2)
abline(0,0,lty=2)
#
plot(fitted(fit.model),td,xlab="Valor Ajustado", 
ylab="Resíduo Componente do Desvio", ylim=c(b-1,a+1), pch=16,cex=1.1,cex.axis=1.1,cex.lab=1.1)
#
abline(2,0,lty=2)
abline(-2,0,lty=2)
abline(0,0,lty=2)
#
hist(td,xlab="Resíduo Componente do Desvio",ylab="densidade",probability=TRUE,main="",cex=1.1,cex.axis=1.1,cex.lab=1.1)
#
w <- fit.model$weights
eta <- predict(fit.model)
z <- eta + resid(fit.model, type="pearson")/sqrt(w)
plot(predict(fit.model),z,xlab="Preditor Linear", 
ylab="Variável z", pch=16,cex=1.1,cex.axis=1.1,cex.lab=1.1)
abline(0,1)
#------------------------------------------------------------#
}
