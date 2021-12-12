library(readxl)
library(tidyverse)
library(writexl)

dre_20 <- read.csv("itr_cia_aberta_DRE_con_2020.csv",sep = ";")

todas_cias <- dre_20 %>% filter(ORDEM_EXERC == "ÚLTIMO") %>% 
                         mutate(VALOR = VL_CONTA / 1000) %>% 
                         mutate(inicio = inic,
                                final = fim,
                                dife = final - inicio) %>% 
                         filter(dife == 2)

cias <- mutate(todas_cias ,
                 Mes = replace(final , final == 3 , "Mar"),
                 Mes = replace(Mes , final == 6 , "Jun"),
                 Mes = replace(Mes , final == 9 , "Set"),
                 Mes = replace(Mes , final == 12 , "Dez"),
                 Mes = replace(Mes , final == 5 , "Maio"),
                 Mes = replace(Mes , final == 8 , "Ago"),
                 Mes = replace(Mes , final == 11 , "Nov")) 
cias$CNPJ_CIA = NULL

write.csv(cias , "cias_abertas.csv" ,fileEncoding = "UTF-8")
write_xlsx(cias, "cias_abertas.xlsx"  )

pivot <- cias %>% filter(Mes == "Mar" | Mes == "Jun" | 
                           Mes == "Set" | Mes == "Dez") %>% 
                  group_by(Mes,DENOM_CIA,CD_CONTA,DS_CONTA) %>% 
                  summarise(saldo = sum(VALOR)) %>% 
                  ungroup() %>% droplevels(.)

pivot <- pivot %>% mutate(ano = 2020) %>% 
                  relocate(Mes , .before = ano)

  write.table(pivot , "cias_abetas_data_studio.csv" , sep = ",",
              row.names = FALSE , fileEncoding = "UTF-8")


magalu_group <- magalu %>% group_by(final,CD_CONTA,DS_CONTA) %>% 
                     summarise(Valores = sum(VALOR))




inic <- as.numeric(substr(todas_cias$DT_INI_EXERC , 6,7))
fim <- as.numeric(substr(todas_cias$DT_FIM_EXERC , 6,7))



receita_magalu <- dre_20 %>% filter(CD_CONTA == "3.01" , 
                                 ORDEM_EXERC == "?LTIMO",
                                 DENOM_CIA == "MAGAZINE LUIZA S.A.") %>% 
                             mutate(VALOR = VL_CONTA / 1000 ) %>% 
                             select(everything() , -VL_CONTA)

dt_inicio <- as.numeric(substr(receita_magalu$DT_INI_EXERC , 6,7))
dt_fim <- as.numeric(substr(receita_magalu$DT_FIM_EXERC , 6,7))

receita_magalu <- receita_magalu %>% mutate(dt_inc = dt_inicio,
                                            dt_f = dt_fim,
                                            dif = dt_f - dt_inc) %>% 
                                    filter(dif == 2)

receita_magalu$dt_f <- NULL      




