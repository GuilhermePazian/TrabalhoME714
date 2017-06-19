```{r modelo_poisso, echo=FALSE}
#modelo com segunda ordem / 22 parâmetros
fit.model_1 <- glm(dados$`Nº de infecções` ~ dados$`Hab. de nadar`*dados$Local*dados$`Faixa etária` + dados$`Hab. de nadar`*dados$Local*dados$Sexo + dados$`Hab. de nadar`*dados$`Faixa etária`*dados$Sexo + dados$Local*dados$`Faixa etária`*dados$Sexo, family=poisson(link="log"),data=dados)
summary(fit.model_1)
diagPoisson(fit.model_1)
envelPoisson(fit.model_1, "log")
length(coef(fit.model_1))

#avaliar nulidade 18-20 -> o teste indicou que pode-se retirar algumas interações (18-20)
m.C <- rbind(c(rep(0,17), 1, rep(0,4)), c(rep(0,18), 1, rep(0,3)), c(rep(0,19), 1, rep(0,2)))
m.M <- cbind(rep(0, 3))
testeF.CBM.MLG(fit.model_1, m.C, m.M)

#avaliar nulidade 18-21 -> o teste indicou que pode-se retirar algumas interações (18-21)
m.C <- rbind(c(rep(0,17), 1, rep(0,4)), c(rep(0,18), 1, rep(0,3)), c(rep(0,19), 1, rep(0,2)), c(rep(0,20), 1, rep(0,1)))
m.M <- cbind(rep(0, 4))
testeF.CBM.MLG(fit.model_1, m.C, m.M)

#avaliar nulidade 16-22 
m.C <- rbind(c(rep(0,15), 1, rep(0,6)), c(rep(0,16), 1, rep(0,5)), c(rep(0,17), 1, rep(0,4)), c(rep(0,18), 1, rep(0,3)), c(rep(0,19), 1, rep(0,2)), c(rep(0,20), 1, rep(0,1)), c(rep(0,21), 1))
m.M <- cbind(rep(0, 7))
testeF.CBM.MLG(fit.model_1, m.C, m.M)


#Redução do modelo de segunda ordem -> retirado: Hab. de nadar*local*sexo e Hab. de nadar*faixa etária*sexo
fit.model_1 <- glm(dados$`Nº de infecções` ~ dados$`Hab. de nadar`:dados$Sexo + dados$`Hab. de nadar`*dados$Local*dados$`Faixa etária` + dados$Local*dados$`Faixa etária`*dados$Sexo, family=poisson(link="log"),data=dados)
summary(fit.model_1)
diagPoisson(fit.model_1)
envelPoisson(fit.model_1, "log")
length(coef(fit.model_1))

#avaliar nulidade 16-17 -> o teste indicou que pode-se retirar (16-17)
m.C <- rbind(c(rep(0,15), 1, rep(0,3)), c(rep(0,16), 1, rep(0,2)))
m.M <- cbind(rep(0, 2))
testeF.CBM.MLG(fit.model_1, m.C, m.M)

#Redução do modelo de segunda ordem -> retirado: Hab. de nadar*local*faixa etária
fit.model_1 <- glm(dados$`Nº de infecções` ~ dados$`Hab. de nadar` + dados$`Hab. de nadar`:dados$Sexo + dados$`Hab. de nadar`:dados$Local + dados$`Hab. de nadar`:dados$`Faixa etária` + dados$Local*dados$`Faixa etária`*dados$Sexo, family=poisson(link="log"),data=dados)
summary(fit.model_1)
diagPoisson(fit.model_1)
envelPoisson(fit.model_1, "log")
length(coef(fit.model_1))

#avaliar nulidade 7-8 -> o teste indicou que pode-se retirar (7-8)
m.C <- rbind(c(rep(0,6), 1, rep(0,10)), c(rep(0,7), 1, rep(0,9)))
m.M <- cbind(rep(0, 2))
testeF.CBM.MLG(fit.model_1, m.C, m.M)

#Redução do modelo de segunda ordem -> retirado: Hab. de nadar*local e Hab. de nadar*sexo
fit.model_1 <- glm(dados$`Nº de infecções` ~ dados$`Hab. de nadar` + dados$`Hab. de nadar`:dados$`Faixa etária` + dados$Local*dados$`Faixa etária`*dados$Sexo, family=poisson(link="log"),data=dados)
summary(fit.model_1)
diagPoisson(fit.model_1)
envelPoisson(fit.model_1, "log")
length(coef(fit.model_1))

#avaliar nulidade 12-14 -> o teste indicou que pode-se retirar (12-14)
m.C <- rbind(c(rep(0,11), 1, rep(0,3)), c(rep(0,12), 1, rep(0,2)), c(rep(0,13), 1, rep(0,1)))
m.M <- cbind(rep(0, 3))
testeF.CBM.MLG(fit.model_1, m.C, m.M)

```