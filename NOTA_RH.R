# funcao que calcula nota RH
notaRH <- function(ano){
  library(tidyverse)
  nome_arquivo <- paste0("./bases/DadosRH_CensoCREAS_",ano,".csv")
  base <- read_delim(nome_arquivo, ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
  
  #Selecionar e filtrar 
  id_RH <- base %>% 
    #filter(UF=="Espirito Santo") %>% 
    select(NU_IDENTIFICADOR, Porte_pop2010, d_58_8bin1_sum, d_58_8bin2_sum, d_58_8bin3_sum,
           d_58_9bin2_sum, d_58_9bin4_sum, d_58_9bin5_sum, d_58_10, d_58_8, q58_11)
  
  # indicadores auxiliares
  id_RH_ind <- id_RH %>% 
    select(NU_IDENTIFICADOR, Porte_pop2010, d_58_8, d_58_10, q58_11) %>%
    mutate(eh_coordenador_ES = if_else(q58_11=="Coordenador(a)" & d_58_8 =="Nível Superior" ,1L, 0L),
           eh_superior_est_clt= if_else(d_58_8 =="Nível Superior" & d_58_10 %in% c("Empregado Público (CLT)","Servidor Estatutário"), 1L,0L)) %>% 
    select(NU_IDENTIFICADOR, eh_coordenador_ES, eh_superior_est_clt) %>% 
    group_by(NU_IDENTIFICADOR) %>% 
    summarise_all(sum)
  
  # Componente RH - Grande porte
  dados_PG <- id_RH %>% 
    filter(Porte_pop2010 %in% c("GRANDE", "METROPOLE")) %>%
    group_by(NU_IDENTIFICADOR) %>% 
    summarise_all(max) %>% 
    select(-d_58_8,-d_58_10,-q58_11) %>%
    left_join(id_RH_ind, by = "NU_IDENTIFICADOR") %>% 
    mutate(notaRH=case_when(
      # Nota 5
      (d_58_8bin2_sum + d_58_8bin3_sum) >= 14 & d_58_8bin3_sum >= 7 &
        d_58_9bin2_sum >= 2 &
        d_58_9bin5_sum >= 2 &
        d_58_9bin4_sum >= 1 &
        eh_coordenador_ES == 1 &
        eh_superior_est_clt >= 2 ~ 5L,
      # Nota 4
      (d_58_8bin2_sum + d_58_8bin3_sum) >= 10 & d_58_8bin3_sum >= 6 &
        (d_58_9bin2_sum + d_58_9bin5_sum) >= 4 & d_58_9bin2_sum >= 1 & d_58_9bin5_sum >= 1 &
        d_58_9bin4_sum >= 1 &
        eh_coordenador_ES == 1 ~ 4L,
      # Nota 3
      (d_58_8bin2_sum + d_58_8bin3_sum) >= 10 & d_58_8bin3_sum >= 5 &
        (d_58_9bin2_sum + d_58_9bin5_sum) >= 4 & d_58_9bin2_sum >= 1 & d_58_9bin5_sum >= 1 &
        eh_coordenador_ES == 1 ~ 3L,
      # Nota 2
      (d_58_8bin2_sum + d_58_8bin3_sum) >= 6 &
        (d_58_9bin2_sum + d_58_9bin5_sum) >= 4 & d_58_9bin2_sum >= 1 & d_58_9bin5_sum >= 1 ~ 2L,
      # Nota 1
      (d_58_8bin1_sum + d_58_8bin2_sum + d_58_8bin3_sum) < 6 | (d_58_9bin2_sum + d_58_9bin5_sum) < 4 |
        d_58_9bin2_sum == 0 | d_58_9bin5_sum == 0 ~ 1L, TRUE ~ NA_integer_))
  
  # Componente RH - Pequeno e Medio portes
  
  dados_PP <- id_RH %>%
    filter(Porte_pop2010 %in% c("MEDIO","PEQUENO I","PEQUENO II")) %>%  
    group_by(NU_IDENTIFICADOR) %>% 
    summarise_all(max) %>% 
    select(-d_58_8,-d_58_10,-q58_11) %>% 
    left_join(id_RH_ind, by = "NU_IDENTIFICADOR") %>% 
    mutate(notaRH=case_when(
      # Nota 5
      (d_58_8bin2_sum + d_58_8bin3_sum) >= 7 & d_58_8bin3_sum >= 4 &
        d_58_9bin2_sum >= 1 &
        d_58_9bin5_sum >= 1 &
        d_58_9bin4_sum >= 1 &
        eh_coordenador_ES == 1 &
        eh_superior_est_clt >= 1 ~ 5L,
      # Nota 4
      (d_58_8bin2_sum + d_58_8bin3_sum) >= 5 & d_58_8bin3_sum >= 4 &
        d_58_9bin2_sum >= 1 &
        d_58_9bin5_sum >= 1 &
        d_58_9bin4_sum >= 1 &
        eh_coordenador_ES == 1 ~ 4L,
      # Nota 3
      (d_58_8bin2_sum + d_58_8bin3_sum) >= 5 & d_58_8bin3_sum >= 3 &
        d_58_9bin2_sum >= 1 &
        d_58_9bin5_sum >= 1 &
        eh_coordenador_ES == 1 ~ 3L,
      # Nota 2
      (d_58_8bin2_sum + d_58_8bin3_sum) >= 3 &
        d_58_9bin2_sum >= 1 &
        d_58_9bin5_sum >= 1 ~ 2L,
      # Nota 1
      (d_58_8bin1_sum + d_58_8bin2_sum + d_58_8bin3_sum) < 3 |
        d_58_9bin2_sum == 0 | d_58_9bin5_sum == 0 ~ 1L, TRUE ~ NA_integer_))
  
  dados <- bind_rows(dados_PG, dados_PP)
  rm(dados_PG, dados_PP)
  nota_RH <- dados %>% select(NU_IDENTIFICADOR, notaRH)
  
  return(nota_RH)
}
