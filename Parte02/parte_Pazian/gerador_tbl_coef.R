gerador_tbl_coef <- function(fit.model){
  options(encoding="utf-8")
  tabela <- summary(fit.model)$coefficients
  tabela[,4] <- ifelse(tabela[,4] < 0.0001,"<0,0001",tabela[,4])
  colnames(tabela) <- c("Estimativa","Erro Padrão","Estatística Z", "P-valor")
  options(OutDec= ",")
  return(xtable(tabela,digits = 4))
}