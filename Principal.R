# Script principal
library(dplyr)
rm(list = ls())
gc()

# Chamando funcoes do calculo das notas
source("./R/NOTA_EF.R", local = T, encoding = "UTF-8")
source("./R/NOTA_RH.R", local = T, encoding = "UTF-8")
source("./R/NOTA_Serv.R", local = T, encoding = "UTF-8")

anos <- c(2019L)

baseIDCREAS <- tibble()

for (ano in anos) {
  EF <- notaEF(ano)
  RH <- notaRH(ano)
  Serv <- notaServ(ano)
  
  NotaGeral <- left_join(EF, RH, by = "NU_IDENTIFICADOR")
  NotaGeral <- left_join(NotaGeral, Serv, by = "NU_IDENTIFICADOR")
  rm(EF, RH, Serv)
  
  NotaGeral <- NotaGeral %>% 
    mutate(IDCREAS = round((notaEF + notaRH + notaServ)/3, 2L),
           Ano = ano)
  
  baseIDCREAS <- bind_rows(baseIDCREAS, NotaGeral)
  rm(NotaGeral)
}

# Cruzamento NU_IDENTIFICADOR

dNU_ID <- readxl::read_xlsx("./bases/dNU_IDENTIFICADOR.xlsx")

baseIDCREAS <- left_join(baseIDCREAS, dNU_ID, by = "NU_IDENTIFICADOR")

write.csv2(baseIDCREAS, "./saidas/IDCREAS.csv")
