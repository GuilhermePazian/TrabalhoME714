# LEITURA
source("códigos_p2/source_files/diag_bin.R")
source("códigos_p2/source_files/envel_bin.R")
source("códigos_p2/source_files/gml_tests.R")

sem <- read.table("códigos_p2/sementes.dat")
names(sem) = c("Temperatura","Nivel_Umidade","Temp_Umidade","N_de_sementes")
sem$Nivel_Umidade <- gsub("m\xe9dio","medio",sem$Nivel_Umidade)

for(i in 1:3) sem[,i] <- as.factor(sem[,i])
sapply(sem,class)

prop.germ <- sem$N_de_sementes/100
sem <- cbind(sem, prop.germ)

m=rep(100,18)
#--------------------
  
# MODELO COM INTERACAO TRIPLA
sem_binfit_mod1 <- glm(cbind(sem$N_de_sementes,100-sem$N_de_sementes)~ Nivel_Umidade*Temp_Umidade + Temperatura*Nivel_Umidade + Temperatura*Temp_Umidade + Temperatura*Temp_Umidade*Nivel_Umidade,sem,family = binomial())
penal_vec_mod1 <- c(AIC(sem_binfit_mod1),BIC(sem_binfit_mod1))
m.C_3ordem = matrix(c(numeric(14),1,rep(c(numeric(18),1),3)),4,18,byrow = TRUE)
v.M_3ordem = c(numeric(4))
teste1_res <- testeF.CBM.MLG(sem_binfit_mod1,m.C_3ordem,v.M_3ordem)

# ------------------------------------
# Modelo2 sem interacao tripla

sem_binfit_mod2 <- glm(cbind(sem$N_de_sementes,100-sem$N_de_sementes)~ Nivel_Umidade*Temp_Umidade + Temperatura*Nivel_Umidade + Temperatura*Temp_Umidade,sem,family = binomial())
penal_vec_mod2 <- c(AIC(sem_binfit_mod2),BIC(sem_binfit_mod2))
m.C_mod2 <- cbind(matrix(numeric(10*4),nrow = 4),diag(4))
v.M_mod2 = c(numeric(4))
teste2_res <- testeF.CBM.MLG(sem_binfit_mod2,m.C_mod2,v.M_mod2)
# diagBern(sem_binfit_mod2)
# envelBinom(sem_binfit_mod2,"logit")
# --------------------------------------
  
# Modelo3 sem interacao dos $\alpha$
sem_binfit_mod3 <- glm(cbind(sem$N_de_sementes,100-sem$N_de_sementes)~ Nivel_Umidade*Temp_Umidade + Temperatura,sem,family = binomial())
penal_vec_mod3 <- c(AIC(sem_binfit_mod3),BIC(sem_binfit_mod3))
#diagBern(sem_binfit_mod3)
#envelBinom(sem_binfit_mod3,"logit")
# -----------------------------------

# Consideracoes finais sobre os modelos


rbind(penal_vec_mod1,penal_vec_mod2,penal_vec_mod3)
desvs <- c(deviance(sem_binfit_mod2),deviance(sem_binfit_mod3))
pvals <- c(1-pchisq(desvs[1],df=18-length(sem_binfit_mod2$coefficients)),
           1-pchisq(desvs[2],df=18-length(sem_binfit_mod3$coefficients)))
desvs_pvals <- cbind(desvs,pvals)
desvs_pvals <- rbind(numeric(2),desvs_pvals)
rownames(desvs_pvals) <- c("modelo1","modelo2","modelo3")
desvs_pvals <- cbind(desvs_pvals,rbind(penal_vec_mod1,penal_vec_mod2,penal_vec_mod3))
colnames(desvs_pvals)[3:4] <- c("AIC","BIC")

#xtable::xtable(desvs_pvals)

betas_coef <- (summary(sem_binfit_mod3))$coef
betas_est <- round(cbind(betas_coef[,1:2],confint.default(sem_binfit_mod3),betas_coef[,3:4]),4)

pred <- predict(sem_binfit_mod3,type=c("response"),se.fit = TRUE) #parte da analise preditiva
                
# plot(sem$prop.germ,axes=FALSE,ylim=c(0.0,1.0),cex.lab=1,xlab="Número de valores preditos",ylab="Valores preditos de sementes germinadas")
# axis(2,cex.axis=1)
# axis(1,1:18,c(1:18),cex.axis=1)
# plotCI(pred$fit,liw=1.96*pred$se.fit,uiw=1.96*pred$se.fit,pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=4,cex=1.2)
                






