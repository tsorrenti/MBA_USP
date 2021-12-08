library(readxl)
library(tidyverse)

dre_20 <- read.csv("itr_cia_aberta_DRE_con_2020.csv",sep = ";")

receita_magalu <- dre_20 %>% filter(CD_CONTA == "3.01" , 
                                 ORDEM_EXERC == "ÚLTIMO",
                                 DENOM_CIA == "MAGAZINE LUIZA S.A.") %>% 
                             mutate(VALOR = VL_CONTA / 1000 ) %>% 
                             select(everything() , -VL_CONTA)

dt_inicio <- substr(receita_magalu$DT_INI_EXERC , 6,7)
dt_fim <- substr(receita_magalu$DT_FIM_EXERC , 6,7)

receita_magalu <- receita_magalu %>% mutate(dt_inc = dt_inicio,
                                            dt_f = dt_fim)
                                      




