# Programa extra?do do site: https://www.ime.usp.br/~giapaula/textoregressao.htm
# Cr?ditos: Prof. Dr. Gilberto Alvarenga Paula
# Adaptado por Caio L. N. Azevedo
# source("D:\\windows\\Unicamp\\Disciplinas\\1_semestre_2016\\ME 720 MLG\\Programas\\diag_pois.r")

diagPoisson<-function(fit.model){
# fit.model: objeto com o resultado do ajuste do MLG obtido atrav?s da fun??o glm
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
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
plot(td,xlab="Índice", ylab="R.C.D.", main = "Gráfico 1",
ylim=c(b-1,a+1), pch=16,cex.axis=1,cex.lab=1,cex=1,cex.lab=1)
abline(2,0,lty=2)
abline(-2,0,lty=2)
abline(0,0,lty=2)

# identify(td, n=1)
# title(sub="(c)")
fited = fitted(fit.model)
plot(fited ,td,xlab="Valor ajustado (média)", ylab="R.C.D.",ylim=c(b-1,a+1), pch=16,
main="Gráfico 2",cex=1,cex.axis=1,cex.lab=1)
abline(2,0,lty=2)
abline(-2,0,lty=2)
abline(0,0,lty=2)

#
#hist(td,xlab="Res?duo Componente do Desvio",ylab="densidade",probability=TRUE,main="",cex=1.1,cex.axis=1.1,cex.lab=1.1)
#
eta = predict(fit.model)
z = eta + resid(fit.model, type="pearson")/sqrt(w)
plot(predict(fit.model),z,xlab="Preditor Linear",ylab="Variavel z", pch=16,main="Gráfico 3",cex=1,cex.axis=1,cex.lab=1)
lines(smooth.spline(predict(fit.model), z, df=2))
#
#---------------------------------------------------------------#
}
