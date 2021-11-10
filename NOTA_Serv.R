# funcao que calcula nota Servicos
notaServ <- function(ano){
  library(tidyverse)
  nome_arquivo <- paste0("./bases/DadosGerais_CensoCREAS_",ano,".csv")
  base_geral <- read_delim(nome_arquivo, ";", escape_double = FALSE, 
                           locale = locale(decimal_mark = ",", 
                                           grouping_mark = ".",
                                           encoding = "UTF-8"), 
                           trim_ws = TRUE)
  
  nome_arquivo <- paste0("./bases/DadosRH_CensoCREAS_",ano,".csv")
  base_rh <- read_delim(nome_arquivo, ";", escape_double = FALSE, 
                        locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "UTF-8"), 
                        trim_ws = TRUE)
  
  nome_arquivo <- paste0("./bases/",ano,"_RMA_CREAS.xlsx")
  base_rma <- readxl::read_xlsx(nome_arquivo, sheet = "Base Tratada")
  
  rm(nome_arquivo)
  
  # Preparacao: selecao e calculo de indicadores auxiliares - Base geral
  id_serv <- base_geral %>% 
    #filter(UF=="Espirito Santo") %>% 
    select(NU_IDENTIFICADOR, Porte_pop2010, q11_1, q11_8, q11_2, q11_5, q11_10,
           q11_6, q11_12, q15, q20, q22, q21_1, q24_1, q21_3, q24_4, q27, 
           q54_3_5, q54_3_7, q54_3_8, q54_16_5, q54_16_7, q54_16_8,
           q54_1_5, q54_1_7, q54_1_8, q2_1, q2_2, q12_1_1, q12_1_2, q12_1_4,
           q12_2_1, q12_2_2, q12_2_4, q12_3_1, q12_3_2, q12_3_4, q12_4_1, q12_4_2, 
           q12_4_4, q12_5_1, q12_5_2, q12_5_4) %>% 
    mutate(VP1 = if_else(q12_1_1 == "Sim" & q12_1_2 == "Sim" & 
                           q12_1_4 == "Sim", 1,0)) %>% 
    mutate(VP2 = if_else(q12_2_1=="Sim" & q12_2_2=="Sim"& q12_2_4=="Sim",1,0)) %>% 
    mutate(VP3 = if_else(q12_3_1=="Sim" & q12_3_2=="Sim"& q12_3_4=="Sim",1,0)) %>% 
    mutate(VP4 = if_else(q12_4_1=="Sim" & q12_4_2=="Sim"& q12_4_4=="Sim",1,0)) %>%
    mutate(VP5 = if_else(q12_5_1 =="Sim"& q12_5_2=="Sim"& q12_5_4=="Sim",1,0)) %>%
    mutate(VP= VP1 + VP2 + VP3 + VP4 + VP5) %>% 
    mutate(ART_CRAS = if_else(q54_3_5 == "Sim" | q54_3_7 == "Sim" | q54_3_8 == "Sim", 1L, 0L)) %>%  
    mutate(ART_CONS = if_else(q54_16_5 == "Sim" | q54_16_7 == "Sim" | q54_16_8 == "Sim", 1L, 0L)) %>% 
    mutate(ART_ACOL = if_else(q54_1_5 == "Sim" | q54_1_7 == "Sim" | q54_1_8 == "Sim", 1L, 0L)) %>%
    mutate(q2_1 = str_sub(q2_1, end = 1), q2_1 = as.integer(q2_1)) %>%
    mutate(q2_2 = str_replace(q2_2, " horas por dia", ""), q2_2 = as.integer(q2_2))
  
  # Preparacao: selecao e calculo de indicadores auxiliares - Base RH
  as_psico <- base_rh %>% 
    #filter(UF=="Espirito Santo") %>% 
    select(NU_IDENTIFICADOR, d_58_9bin2_sum, d_58_9bin5_sum) %>% 
    group_by(NU_IDENTIFICADOR) %>% 
    summarise_all(max) %>% 
    mutate(profissionais = d_58_9bin2_sum + d_58_9bin5_sum)
  
  # Preparacao: selecao e calculo de indicadores auxiliares - Base RMA
  rma_creas_es <- base_rma %>%
    filter(mes==8) %>%
    select(NU_IDENTIFICADOR, a1) %>% 
    mutate(NU_IDENTIFICADOR=as.numeric(gsub("'", "", NU_IDENTIFICADOR)))
  
  #Juntando as tabelas
  
  serv <- left_join(rma_creas_es, id_serv)
  id_serv <- left_join(serv, as_psico)
  id_serv <- id_serv %>% filter(!is.na(Porte_pop2010))
  
  rm(base_rma, base_rh, base_geral, as_psico, serv, rma_creas_es)
  
  #Dimensão Serviços  # Porte grande
  
  id_serv_GP <- id_serv %>% 
    filter(Porte_pop2010 %in% c("Grande", "Metrópole")) %>%
    mutate(notaServ=case_when(
      # NOTA 5
      q11_1 =="Sim" & q11_8 == "Sim" & q11_2 =="Sim" & q11_5 == "Sim" & q11_10 == "Sim" & q11_12 == "Sim" & q11_6 == "Sim" &
        (a1 / profissionais) <= 30 &
        q15 == "Sim" & q20 %in% c("Semanal", "Quinzenal") & q22 %in% c("Semanal", "Quinzenal") & q21_1 == "Sim" & q24_1 == "Sim" & q24_4 == "Sim" & q21_3 == "Sim" &
        q27 != "Não realiza, nem possui o Serviço referenciado a este CREAS" &
        (ART_CRAS+ART_CONS+ART_ACOL) == 3L &
        VP >= 1 &
        d_58_9bin2_sum >=1 & d_58_9bin5_sum >= 1 &
        q2_1 >= 5 & (q2_1*q2_2) >= 40 ~ 5L,
      # NOTA 4
      q11_1 =="Sim" & q11_8 == "Sim" & q11_2 =="Sim" & q11_5 == "Sim" & q11_10 == "Sim" & q11_6 == "Sim" &
        (a1 / profissionais) <= 50 &
        q15 == "Sim" & q21_1 == "Sim" & q24_1 == "Sim" &
        q27 != "Não realiza, nem possui o Serviço referenciado a este CREAS" &
        (ART_CRAS+ART_CONS) == 2L &
        VP >= 1 ~ 4L,
      # NOTA 3
      q11_1 =="Sim" & q11_2 =="Sim" & q11_5 == "Sim" & q11_10 == "Sim" & q11_6 == "Sim" &
        q15 == "Sim" &
        (ART_CRAS+ART_CONS) == 2L &
        VP >= 1 ~ 3L,
      # NOTA 2
      q11_1 =="Sim" & q11_2 =="Sim" & q11_10 == "Sim"& q11_6 == "Sim" &
        ART_CRAS == 1L ~ 2L,
      # NOTA 1
      q11_1 =="Não" | q11_2 =="Não" | q11_10 == "Não" | q11_6 == "Não" |
        ART_CRAS == 0L |
        (d_58_9bin2_sum == 0 & d_58_9bin5_sum == 0) ~ 1L, TRUE ~ NA_integer_))

  #Dimensão Serviços  # Porte Pequeno 
  
  id_serv_PP <- id_serv %>% 
    filter(Porte_pop2010 %in% c("Médio", "Pequeno I", "Pequeno II")) %>% 
    mutate(notaServ=case_when(
      # NOTA 5
      q11_1 =="Sim" & q11_8 == "Sim" & q11_2 =="Sim" & q11_5 == "Sim" & q11_10 == "Sim" & q11_12 == "Sim" & q11_6 == "Sim" &
        (a1 / profissionais) <= 30 &
        q15 == "Sim" & q20 %in% c("Semanal", "Quinzenal") & q22 %in% c("Semanal", "Quinzenal") & q21_1 == "Sim" & q24_1 == "Sim" & q24_4 == "Sim" & q21_3 == "Sim" &
        q27 != "Não realiza, nem possui o Serviço referenciado a este CREAS" &
        (ART_CRAS+ART_CONS) == 2L &
        VP >= 1 &
        d_58_9bin2_sum >=1 & d_58_9bin5_sum >= 1 &
        q2_1 >= 5 & (q2_1*q2_2) >= 40 ~ 5L,
      # NOTA 4
      q11_1 =="Sim" & q11_8 == "Sim" & q11_2 =="Sim" & q11_5 == "Sim" & q11_10 == "Sim" & q11_6 == "Sim" &
        (a1 / profissionais) <= 50 &
        q15 == "Sim" & q21_1 == "Sim" & q24_1 == "Sim" &
        q27 != "Não realiza, nem possui o Serviço referenciado a este CREAS" &
        (ART_CRAS+ART_CONS) == 2L &
        VP >= 1 ~ 4L,
      # NOTA 3
      q11_1 =="Sim" & q11_2 =="Sim" & q11_5 == "Sim" & q11_10 == "Sim" & q11_6 == "Sim" &
        q15 == "Sim" &
        (ART_CRAS+ART_CONS) == 2L &
        VP >= 1 ~ 3L,
      # NOTA 2
      q11_1 =="Sim" & q11_2 =="Sim" & q11_10 == "Sim" & q11_6 == "Sim" &
        ART_CRAS == 1L ~ 2L,
      # NOTA 1
      q11_1 =="Não" | q11_2 =="Não" | q11_10 == "Não" | q11_6 == "Não" |
        ART_CRAS == 0L |
        (d_58_9bin2_sum == 0 & d_58_9bin5_sum == 0) ~ 1L, TRUE ~ NA_integer_))
  
  dados <- bind_rows(id_serv_GP, id_serv_PP)
  nota_Serv <- dados %>%
    select(NU_IDENTIFICADOR, notaServ)
  
  rm(id_serv_GP, id_serv_PP, id_serv)
  
  return(nota_Serv)
}