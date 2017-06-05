#Tabela dos dados de germinação

#teste <- scan("~/Documentos/Discretos/discretos_2017/trabalho/sementes.dat", what = list(0,0,0,0))
sementes <-matrix(c(98,96,62,94,79,3,92,41,1,94,93,65,94,71,2,91,30,1),nrow=6,byrow=TRUE)
dimnames(sementes) <- list(c("11ºC & BAIXO","11ºC & MEDIO","11ºC & ALTO","21ºC & BAIXO","21ºC & MEDIO","21ºC & ALTO"),c("21ºC","42ºC", "62ºC"))
names(dimnames(sementes)) <- c("Temp. Germinação || Nível de umidade","Nível de temperatura")

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



