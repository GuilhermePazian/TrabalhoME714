testeF.CBM.MLG <- function(fit.model,m.C,m.M)
{
v.beta <-  cbind(fit.model$coef)
e.q <- nrow(m.C)
m.cov.beta <- (vcov(fit.model))
e.Q <- t(m.C%*%v.beta-m.M)%*%solve(m.C%*%m.cov.beta%*%t(m.C))%*%(m.C%*%v.beta-m.M)
e.pvalor <- 1-pchisq(e.Q,e.q)
cat("Estatistica Q = ",round(e.Q,2),"\n")
cat("gl = ",e.q,"\n")
cat("pvalor = ",round(e.pvalor,4),"\n")
cat("Matriz C :","\n")
print(m.C)
cat("Matriz M :","\n")
print(m.M)
}


# fit.model 1: H_0
# fit.model 1: modelo irrestritro

TRV.MLG <- function(fit.model1,fit.model2)
{
ep <- ncol(model.matrix(fit.model2))
eq <- ep-ncol(model.matrix(fit.model1))
desv1 <- deviance(fit.model1)
desv2 <- deviance(fit.model2)
e.TRV <- (desv1-desv2)
e.pvalor <- 1-pchisq(e.TRV,eq)
cat("Estatistica F = ",round(e.TRV,2),"\n")
cat("g.l = ",eq,"\n")
cat("pvalor = ",round(e.pvalor,4),"\n")
}


# fit.model 1: H_0
# fit.model 1: modelo irrestritro

AnaDesv.MLG <- function(fit.model1,fit.model2)
{
ep <- ncol(model.matrix(fit.model2))
eq <- ep-ncol(model.matrix(fit.model1))
n <- nrow(model.matrix(fit.model2))
desv1 <- deviance(fit.model1)
desv2 <- deviance(fit.model2)
e.F <- ((desv1-desv2)/eq)/(desv2/(n-ep))
e.pvalor <- 1-pf(e.F,eq,n-ep)
cat("Estatistica F = ",round(e.F,2),"\n")
cat("pvalor = ",round(e.pvalor,4),"\n")
}
