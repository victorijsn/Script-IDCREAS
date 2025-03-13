# Funcao para calculo da nota de Estrutura Fisica do IDCREAS
notaEF <- function(ano){
  library(tidyverse)
  #1. Importar a base do CREAS dados gerais
  nome_arquivo <- paste0("./bases/DadosGerais_CensoCREAS_",ano,".csv")
  base <- read_delim(nome_arquivo, ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
  
  #2. filtrar os CREAS do ES e selecionar as colunas 
  base <- base %>%
    #filter(UF=="Espirito Santo") %>% 
    select(NU_IDENTIFICADOR, Porte_pop2010, q6_1, q6_2, q6_3, q6_4,
           q6_5, q6_6, q6_7, q7_1, q7_2, q7_3, q7_4, q9_1, q9_3, q9_8,
           q9_9, q10_2,q5_4, q5_5, q5_6, q5_7) %>%
    mutate(q6_6 = gsub("Sim", 1L, q6_6),
           q6_6 = gsub("Não", 0L, q6_6),
           q6_6 = as.integer(q6_6))
  
  #3. Componente Estrutura Fisica - Grande porte
  dados_GP <- base %>% 
    filter(Porte_pop2010 %in% c("Grande", "Metrópole")) %>%
    mutate(notaEF=case_when(
      # NOTA 5
      (q6_1 + q6_2 + q6_3 + q6_4) >=5 & (q6_3 + q6_4) >=1 &
        q6_7 == "Sim" &
        q6_6 >= 2 &
        q6_5 >= 1 &
        q9_1 == "Sim" & q9_3 == "Sim" & q10_2 >= 2 & (q9_8 == "Sim" | q9_9 == "Sim") &
        q7_1 != "Não possui" & q7_2 != "Não possui" & q7_3 != "Não possui" & q7_4 != "Não possui" ~ 5L,
      # NOTA 4
      (q6_1 + q6_2 + q6_3 + q6_4) >=5 &
        q6_7 == "Sim" &
        q6_6 >= 2 &
        q10_2 >= 1 &
        (q9_8 == "Sim" | q9_9 == "Sim") &
        q7_2 != "Não possui" & q7_3 != "Não possui" ~ 4L,
      #NOTA 3
      (q6_1 + q6_2 + q6_3 + q6_4 + q6_5) >=3 &
        q6_7 == "Sim" &
        q6_6 >= 1 &
        q7_2 != "Não possui" & q7_3 != "Não possui" ~ 3L,
      # NOTA 2
      (q6_1 + q6_2 + q6_3 + q6_4 + q6_5) >=3 &
        q6_6 >= 1 ~ 2L,
      # NOTA 1
      (q6_1 + q6_2 + q6_3 + q6_4 + q6_5) <3 |
        q6_6 == 0 |
        q5_4 == "Sim" | q5_5 == "Sim" | q5_6 == "Sim" | q5_7 == "Sim" ~ 1L, TRUE ~ NA_integer_))
  
  #4. Componente Estrutura Fisica - Médio, Pequeno I e II
  dados_MP <- base %>% 
    filter(Porte_pop2010 %in% c("Médio", "Pequeno I", "Pequeno II")) %>%
    mutate(notaEF=case_when(
      # NOTA 5
      (q6_1 + q6_2 + q6_3 + q6_4) >=3 & (q6_3 + q6_4) >=1 &
        q6_7 == "Sim" &
        q6_6 >= 2 &
        q6_5 >= 1 &
        q9_1 == "Sim" & q9_3 == "Sim" & q10_2 >= 2 & (q9_8 == "Sim" | q9_9 == "Sim") &
        q7_1 != "Não possui" & q7_2 != "Não possui" & q7_3 != "Não possui" & q7_4 != "Não possui" ~ 5L,
      # NOTA 4
      (q6_1 + q6_2 + q6_3 + q6_4) >=3 &
        q6_7 == "Sim" &
        q6_6 >= 2 &
        q10_2 >= 1 &
        (q9_8 == "Sim" | q9_9 == "Sim") &
        q7_2 != "Não possui" & q7_3 != "Não possui" ~ 4L,
      #NOTA 3
      (q6_1 + q6_2 + q6_3 + q6_4 + q6_5) >=3 &
        q6_7 == "Sim" &
        q6_6 >= 1 &
        q7_2 != "Não possui" & q7_3 != "Não possui" ~ 3L,
      # NOTA 2
      (q6_1 + q6_2 + q6_3 + q6_4 + q6_5) >=3 &
        q6_6 >= 1 ~ 2L,
      # NOTA 1
      (q6_1 + q6_2 + q6_3 + q6_4 + q6_5) <3 |
        q6_6 == 0 |
        q5_4 == "Sim" | q5_5 == "Sim" | q5_6 == "Sim" | q5_7 == "Sim" ~ 1L, TRUE ~ NA_integer_))
  
  #5. Juntar bases e selecionar saida de dados
  dados <- bind_rows(dados_GP, dados_MP)
  nota_EF <- dados %>% select(NU_IDENTIFICADOR, notaEF)
  rm(dados, dados_GP, dados_MP, base, nome_arquivo)
  return(nota_EF)
}
