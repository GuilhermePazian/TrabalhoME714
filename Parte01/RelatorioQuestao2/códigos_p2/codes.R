sem <- read.table("~/a_link_to_papers/*2017-1/Categorical/caio_2017/sementes.dat")
names(sem) = c("Temperatura","Nivel_Umidade","Temp_Umidade","N_de_sementes")
sem$Nivel_Umidade <- gsub("m\xe9dio","medio",sem$Nivel_Umidade)

sem_binfit <- glm(cbind(sem$N_de_sementes,100-sem$N_de_sementes)~ Temperatura +Nivel_Umidade + Temp_Umidade + 
                    Nivel_Umidade*Temp_Umidade,sem,family = binomial())