#------------------------------------------------------------#
# Para rodar este programa  deixe no objeto fit.model a sa?da 
# do  ajuste  da regress?o com  erros  Poisson e liga??o log. 
# Programa extra?do do site: https://www.ime.usp.br/~giapaula/textoregressao.htm
# Cr?ditos: Prof. Dr. Gilberto Alvarenga Paula
# Adaptado por Caio L. N. Azevedo
# source("D:\\windows\\Unicamp\\Disciplinas\\1_semestre_2016\\ME 720 MLG\\Programas\\envel_Bern.r")


envelPoisson <- function(fit.model,ligacao){
# fit.model: objeto com o resultado do ajuste do MLG obtido atrav?s da fun??o glm
# ligacao: fun??o de liga??o (mesmo nome usado pela fun??o glm (colocar entre aspas)
#par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- resid(fit.model,type="deviance")/sqrt((1-h))
e <- matrix(0,n,100)
#
for(i in 1:100){
nresp <- rpois(n, fitted(fit.model))
fit <- glm(nresp ~ X, family=poisson(link=ligacao))
w <- fit$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
  eo <- sort(e[i,])
e1[i] <- (eo[2]+eo[3])/2
e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
#par(pty="s")
qqnorm(td,xlab="Percentil da N(0,1)",
ylab="R.C.D.", ylim=faixa, pch=16,main="Gráfico de envelope",cex=0.8,cex.axis=0.8,cex.lab=0.8)
#
par(new=T)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1,main="")
par(new=T)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1,main="")
par(new=T)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2,main="")
#------------------------------------------------------------#                      
}