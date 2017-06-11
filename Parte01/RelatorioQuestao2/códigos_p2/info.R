library(dplyr)
library(ggplot2)
library(xtable)
library(plotrix)
library(plyr)

#Tabela dos dados de germinação

teste <- scan("~/Documentos/Discretos/discretos_2017/trabalho/sementes.dat", what = list(integer(), character(), integer(), integer()))
teste[[2]] <- gsub("m\xe9dio", "médio", teste[[2]])
temp.germ <- cbind(factor(as.factor(teste[[1]]), levels = c("11","21"), labels = c("1","2"))) #temperatura de germinação
#temp.germ <- cbind(as.integer(teste[[1]]))
nivel.um <- cbind(factor(as.factor(teste[[2]]), levels = c("baixo", "médio", "alto"), labels = c("1", "2", "3")))  #nível de umidade
                                                                                #1 = baixo, 2 = médio e 3 = alto
nivel.temp <- cbind(factor(as.factor(teste[[3]]), levels = c("21","42", "62"), labels = c("1","2", "3"))) #nível de temperatura
#nivel.temp <- cbind(as.integer(teste[[3]]))
sem.germ <- cbind(as.integer(teste[[4]])) #número de sementes germinadas depois de 5 dias
dados <- data.frame(cbind(temp.germ, nivel.um, nivel.temp, sem.germ))
colnames(dados) <- c("temp.germ", "nivel.um", "nivel.temp", "sem.germ")

dados$temp.germ <- as.factor(dados$temp.germ)
dados$nivel.um <- as.factor(dados$nivel.um)
dados$nivel.temp <- as.factor(dados$nivel.temp)
dados$sem.germ <- as.numeric(dados$sem.germ)

sementes <-matrix(c(98,96,62,94,79,3,92,41,1,94,93,65,94,71,2,91,30,1),nrow=6,byrow=TRUE)
dimnames(sementes) <- list(c("11ºC & BAIXO","11ºC & MEDIO","11ºC & ALTO","21ºC & BAIXO","21ºC & MEDIO","21ºC & ALTO"),c("21ºC","42ºC", "62ºC"))
names(dimnames(sementes)) <- c("Temp. Germinação || Nível de umidade","Nível de temperatura")
sementes

########################################################################################################################
#Note que para cada casela pode-se obter a proporção de sementes que germinaram depois de 5 dias; elaborar um gráfico de 
#perfil que avalie o comportamento da umidade em uma dada temperatura de germinação, para os diferentes níveis da temperatura

#Cada casela é uma binomial (100, p_{ijk})
prop.sem.germ <- dados$sem.germ/100
prop.sem.n.germ <- 1 - prop.sem.germ
sd.binom.caselas <- sqrt(prop.sem.germ*prop.sem.n.germ/100)
prop_sd <- cbind(prop.sem.germ, prop.sem.n.germ, sd.binom.caselas)
colnames(prop_sd) <- c("proporção de sementes germinadas", "proporção de sementes não germinadas", "sd")

dados <- cbind(dados, prop_sd)

#O sd = desvio padrão está relacionado com a estimativa da raiz quadrada do estmador de MV da proporção

#Tentativa de construir o gráfico de perfil para temp.germ = 11, para os níveis de umidade e temperatura (ok)
par(mfrow=c(1,2))
plot(dados$`proporção de sementes germinadas`[1:3],axes=FALSE,ylim=c(0.0,1.0),cex.lab=1,xlab="Nível de temperatura",ylab="Proporção de sementes germinadas", main = "prop")
axis(2,cex.axis=1)
axis(1,1:3,c("21ºC","42ºC", "62ºC"),cex.axis=1)

plotCI(dados$`proporção de sementes germinadas`[1:3],liw=1.96*dados$`proporção de sementes germinadas`[1:3]*dados$sd[1:3],uiw=1.96*dados$`proporção de sementes germinadas`[1:3]*dados$sd[1:3],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=4,cex=1.2)
lines(dados$`proporção de sementes germinadas`[1:3],lwd=2,col=4)
plotCI(dados$`proporção de sementes germinadas`[4:6],liw=1.96*dados$`proporção de sementes germinadas`[4:6]*dados$sd[4:6],uiw=1.96*dados$`proporção de sementes germinadas`[4:6]*dados$sd[4:6],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=2,cex=1.2)
lines(dados$`proporção de sementes germinadas`[4:6],lwd=2,col=2)
plotCI(dados$`proporção de sementes germinadas`[7:9],liw=1.96*dados$`proporção de sementes germinadas`[7:9]*dados$sd[7:9],uiw=1.96*dados$`proporção de sementes germinadas`[7:9]*dados$sd[7:9],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=1,cex=1.2)
lines(dados$`proporção de sementes germinadas`[7:9],lwd=2,col=1)

legend(1,0.15,col=c(4,2,1),lwd=c(2,2),pch=c(19,23),pt.bg=c(2,2),legend=c("Umidade baixa para germinação em 11ºC","Umidade média para germinação em 11ºC", "Umidade alta para germinação em 11ºC"),bty="n",cex=.4)

plot(dados$`proporção de sementes germinadas`[10:12],axes=FALSE,ylim=c(0.0,1.0),cex.lab=1.5,xlab="Nível de temperatura",ylab="Proporção de sementes germinadas")
axis(2,cex.axis=1.2)
axis(1,1:3,c("21ºC","42ºC", "62ºC"),cex.axis=1.2)
plotCI(dados$`proporção de sementes germinadas`[10:12],liw=1.96*dados$`proporção de sementes germinadas`[10:12]*dados$sd[10:12],uiw=1.96*dados$`proporção de sementes germinadas`[10:12]*dados$sd[10:12],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=4,cex=1.2)
lines(dados$`proporção de sementes germinadas`[10:12],lwd=2,col=4)
plotCI(dados$`proporção de sementes germinadas`[13:15],liw=1.96*dados$`proporção de sementes germinadas`[13:15]*dados$sd[13:15],uiw=1.96*dados$`proporção de sementes germinadas`[13:15]*dados$sd[13:15],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=2,cex=1.2)
lines(dados$`proporção de sementes germinadas`[13:15],lwd=2,col=2)
plotCI(dados$`proporção de sementes germinadas`[16:18],liw=1.96*dados$`proporção de sementes germinadas`[16:18]*dados$sd[16:18],uiw=1.96*dados$`proporção de sementes germinadas`[16:18]*dados$sd[16:18],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=1,cex=1.2)
lines(dados$`proporção de sementes germinadas`[16:18],lwd=2,col=1)

legend(1,0.15,col=c(4,2,1),lwd=c(2,2),pch=c(19,23),pt.bg=c(2,2),legend=c("Umidade baixa para germinação em 21ºC","Umidade média para germinação em 21ºC", "Umidade alta para germinação em 21ºC"),bty="n",cex=1)

#Observar o efeito crescente do nível de temperatura e decrescente da proporção de sementes germinadas para ambas as 
#temperaturas de germinação. Com isso, pode-se perceber que existe interação entre os níveis de umidade e temperatura, dentro
#de cada nível de temperatura de germinação.

par(mfrow=c(1,2))
plot(dados$`proporção de sementes germinadas`[1:3],axes=FALSE,ylim=c(0.0,1.0),cex.lab=1,xlab="Nível de temperatura",ylab="Proporção de sementes germinadas", main = "Gráfico de perfil")
axis(2,cex.axis=1)
axis(1,1:3,c("21ºC","42ºC", "62ºC"),cex.axis=1)

plotCI(dados$`proporção de sementes germinadas`[1:3],liw=1.96*dados$`proporção de sementes germinadas`[1:3]*dados$sd[1:3],uiw=1.96*dados$`proporção de sementes germinadas`[1:3]*dados$sd[1:3],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=4,cex=1.2)
lines(dados$`proporção de sementes germinadas`[1:3],lwd=2,col=4)
plotCI(dados$`proporção de sementes germinadas`[10:12],liw=1.96*dados$`proporção de sementes germinadas`[10:12]*dados$sd[10:12],uiw=1.96*dados$`proporção de sementes germinadas`[10:12]*dados$sd[10:12],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=2,cex=1.2)
lines(dados$`proporção de sementes germinadas`[10:12],lwd=2,col=2)
legend(1,0.15,col=c(4,2),lwd=c(2,2),pch=c(19,23),pt.bg=c(2,2),legend=c("Umidade baixa para germinação em 11ºC","Umidade baixa para germinação em 21ºC"),bty="n",cex=1)


plot(dados$`proporção de sementes germinadas`[4:6],axes=FALSE,ylim=c(0.0,1.0),cex.lab=1,xlab="Nível de temperatura",ylab="Proporção de sementes germinadas", main = "Gráfico de perfil")
axis(2,cex.axis=1)
axis(1,1:3,c("21ºC","42ºC", "62ºC"),cex.axis=1)
plotCI(dados$`proporção de sementes germinadas`[4:6],liw=1.96*dados$`proporção de sementes germinadas`[4:6]*dados$sd[4:6],uiw=1.96*dados$`proporção de sementes germinadas`[4:6]*dados$sd[4:6],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=4,cex=1.2)
lines(dados$`proporção de sementes germinadas`[4:6],lwd=2,col=4)
plotCI(dados$`proporção de sementes germinadas`[13:15],liw=1.96*dados$`proporção de sementes germinadas`[13:15]*dados$sd[13:15],uiw=1.96*dados$`proporção de sementes germinadas`[13:15]*dados$sd[13:15],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=2,cex=1.2)
lines(dados$`proporção de sementes germinadas`[13:15],lwd=2,col=2)
legend(1,0.15,col=c(4,2),lwd=c(2,2),pch=c(19,23),pt.bg=c(2,2),legend=c("Umidade baixa para germinação em 11ºC","Umidade baixa para germinação em 21ºC"),bty="n",cex=1)

plot(dados$`proporção de sementes germinadas`[7:9],axes=FALSE,ylim=c(0.0,1.0),cex.lab=1,xlab="Nível de temperatura",ylab="Proporção de sementes germinadas", main = "Gráfico de perfil")
axis(2,cex.axis=1)
axis(1,1:3,c("21ºC","42ºC", "62ºC"),cex.axis=1)
plotCI(dados$`proporção de sementes germinadas`[7:9],liw=1.96*dados$`proporção de sementes germinadas`[7:9]*dados$sd[7:9],uiw=1.96*dados$`proporção de sementes germinadas`[7:9]*dados$sd[7:9],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=4,cex=1.2)
lines(dados$`proporção de sementes germinadas`[7:9],lwd=2,col=4)
plotCI(dados$`proporção de sementes germinadas`[16:18],liw=1.96*dados$`proporção de sementes germinadas`[16:18]*dados$sd[16:18],uiw=1.96*dados$`proporção de sementes germinadas`[16:18]*dados$sd[16:18],pch=19,add=TRUE,cex.lab=1.5,slty=1,lwd=2,col=2,cex=1.2)
lines(dados$`proporção de sementes germinadas`[16:18],lwd=2,col=2)
legend(1,0.15,col=c(4,2),lwd=c(2,2),pch=c(19,23),pt.bg=c(2,2),legend=c("Umidade baixa para germinação em 11ºC","Umidade baixa para germinação em 21ºC"),bty="n",cex=1)





########################################################################################################################

#Medidas resumo. Observar que os valores obtidos referem-se a informações por temperatura de germinação, em um determinado
#nível de umidade, isto é, média para os valores de sementes germinadas em uma determinada temperatura de germinação,
#a um determinado nível de umidade

ajuda <- dados %>% group_by(temp.germ,nivel.um) %>% summarise(medias=mean(sem.germ), DP = sd(sem.germ))
ajuda <- data.frame(ajuda)
cvs <- 100*ajuda$DP/ajuda$medias
med.resumo <- dados %>% group_by(temp.germ,nivel.um) %>% summarise(media = mean(sem.germ), vari = var(sem.germ), DP = sd(sem.germ), MAX = max(sem.germ), MIN = min(sem.germ))
med.resumo <- data.frame(med.resumo)

med.resumo <- cbind(med.resumo, cvs)

xtable(med.resumo)

########################################################################################################################
#Descritivas

#Avaliar o valor médio de sementes que germinaram depois de 5 dias, para os diferentes níveis de temperatura. A comparação
#pode ser feita nos dois diferentes valores de temperatura de germinação

med.temp21_11 <- mean(sementes[1:3,1]) #valor médio de sementes germinadas na temp 21º, para Temp. Germinação 11ºC
med.temp21_21 <- mean(sementes[4:6,1]) #valor médio de sementes germinadas na temp 21º, para Temp. Germinação 21ºC
sd(sementes[1:3,1]); sd(sementes[4:6,1])

med.temp42_11 <- mean(sementes[1:3,2]) #valor médio de sementes germinadas na temp 42º, para Temp. Germinação 11ºC
med.temp42_21 <- mean(sementes[4:6,2]) #valor médio de sementes germinadas na temp 42º, para Temp. Germinação 21ºC
sd(sementes[1:3,2]); sd(sementes[4:6,2])

med.temp62_11 <- mean(sementes[1:3,3]) #valor médio de sementes germinadas na temp 62º, para Temp. Germinação 11ºC
med.temp62_21 <- mean(sementes[4:6,3]) #valor médio de sementes germinadas na temp 62º, para Temp. Germinação 21ºC
sd(sementes[1:3,3]); sd(sementes[4:6,3])

med._temp_germ <-matrix(c(med.temp21_11, med.temp21_21, med.temp42_11, med.temp42_21, med.temp62_11, med.temp62_21),nrow=6,byrow=TRUE)
dimnames(med._temp_germ) <- list(c("21ºC/11ºC","21ºC/21ºC","42ºC/11ºC","42ºC/21ºC","62ºC/11ºC","62ºC/21ºC"),c("Valores médios de sementes germinadas após 5 dias"))

#Pela tabela pode-se perceber que a maior diferença entre os valores médios de sementes que germinaram depois de 5 dias está
#no nível de temperatura de 42ºC.

####
#Avaliar o valor médio de sementes que germinaram depois de 5 dias, para os diferentes níveis de umidade. A comparação
#pode ser feita nos dois diferentes valores de temperatura de germinação

med.tempB_11 <- mean(sementes[1,]) #valor médio de sementes germinadas na temp 11º, para nível de germinação baixo
med.tempB_21 <- mean(sementes[4,]) #valor médio de sementes germinadas na temp 21º, para nível de germinação baixo
sd(sementes[1,]); sd(sementes[4,])

med.tempM_11 <- mean(sementes[2,]) #valor médio de sementes germinadas na temp 11º, para nível de germinação médio
med.tempM_21 <- mean(sementes[5,]) #valor médio de sementes germinadas na temp 21º, para nível de germinação médio
sd(sementes[2,]); sd(sementes[5,])

med.tempA_11 <- mean(sementes[3,]) #valor médio de sementes germinadas na temp 11º, para nível de germinação alto
med.tempA_21 <- mean(sementes[6,]) #valor médio de sementes germinadas na temp 21º, para nível de germinação alto
sd(sementes[3,]); sd(sementes[6,])

med._temp_um <-matrix(c(med.tempB_11, med.tempB_21, med.tempM_11, med.tempM_21, med.tempA_11, med.tempA_21),nrow=6,byrow=TRUE)
med._temp_um <- cbind(med._temp_um, as.matrix(c(sd(sementes[1,]), sd(sementes[4,]), sd(sementes[2,]), sd(sementes[5,]), sd(sementes[3,]), sd(sementes[6,]))))
dimnames(med._temp_um) <- list(c("Baixa/11ºC","Baixa/21ºC","Média/11ºC","Média/21ºC","Alta/11ºC","Alta/21ºC"),c("Valores médios de sementes germinadas após 5 dias", "sd"))

########################################################################################################################
fit.model<- glm(dados$`proporção de sementes germinadas` ~ dados$temp.germ + dados$nivel.um + dados$nivel.temp + dados$nivel.um*dados$nivel.temp,family=binomial(link="logit"))
