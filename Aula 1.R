library(tidyverse)
library(readxl)

table(base_covid$tipo_transmissao)



a <- base_covid[-c(1),] # excluir uma determinada linha ou conjunto de linhas pode ser -c(1:3)
a <- a[! (a$nome == "Other") , ] # Excluir de acordo com um determinado parametro


a <- mutate(a , tipo_transmissao = recode(tipo_transmissao,
                                          "Clusters of cases" = "Casos Concentrados",
                                          "Community transmission" = "Transmissão Comunitária",
                                          "No cases" = "Sem Casos",
                                          "Not applicable" = "Não Aplicável",
                                          "Pending" = "Pendente",
                                          "Sporadic cases" = "Casos Esporádicos") )

a <-  mutate(a,grupos = cut(casos_relativo,
                          c(-Inf, quantile(base_covid$casos_relativo,
                                           type = 5,
                                           probs = c(0.25, 0.50, 0.75),
                                           TRUE),Inf),
                          c("primeiro quartil",
                            "segundo quartil",
                            "terceiro quartil",
                            "quarto quartil")))

b <- transmute(a , nome , casos_total,  ) #Escolher as variáveis

b <- nova_base %>% transmute(obs, temp) %>% 
                   mutate(posicao = cut(temp,
                                        c(0,median(temp),Inf),
                                        c("menores","maiores"))) %>% 
                  mutate(teste = cut(temp,
                                     c(0,20,50,100),
                                     c("menor","maior","super"))) 
                  

filtro_covid <- base_covid[ ,c("nome", "casos_total" , "mortes_total")] %>% 
                mutate("percentual_mortes" = (mortes_total / casos_total)*100)

filtro2 <- select(base_covid,"nome", "casos_total" , "mortes_total" )
filtro3 <- select(base_covid , everything() , -"tipo_transmissao")

filtro4 <- select(base_covid , "nome" , "mortes_dia" , "mortes_total" )
filtro4 <- relocate(filtro4 ,"mortes_total" , .after ="nome"  )

excluir <- nova_base %>% select("obs","temp","N3") %>% 
           mutate(nova =  round(N3,0))

excluir <- pull(base_covid , "mortes_total") # gera um vetor
# Case_when faz condições igual formula se
excluir <- excluir %>% mutate(nova = case_when(
  c(excluir$temp) < 20 ~ "menor que 20",
  c(excluir$temp) < 40 ~ "menor que 40",
  c(excluir$temp) < Inf ~ "Maiores valores"
))


excluir2 <- select(nova_base,N1,N2)
relocmediaate(excluir, nova , .before = obs)
gimpse(excluir$nova)
view(excluir$nova)
unique(excluir$nova)
table(excluir$nova)

#parei no slice

teste <- base_covid %>% group_by(regiao) %>% 
  summarise(total = sum(mortes_total)) %>% 
  filter(regiao != "" & total > 500000) %>% 
  arrange(desc(total))

dataset_join <- left_join(dataset_inicial , dataset_merge , by = "nome" )
dataset_join2 <- right_join(dataset_inicial , dataset_merge , by = "nome" )
dataset_join3 <- inner_join(dataset_inicial , dataset_merge , by = "nome")
dataset_join4 <- full_join(dataset_inicial , dataset_merge , by = "nome")
semi_join(dataset_inicial , dataset_merge , by = "nome")
anti_join(dataset_inicial , dataset_merge , by = "nome")
anti_join(dataset_merge , dataset_inicial , by = "nome")
bind_rows(dataset_inicial , dataset_merge) # adiciona um banco no outro
view(dataset_join4)

nome <- c("thiago" , "dayane")
nota <- c(5,8)
teste1 <- data.frame(nome,nota)
sexo <- c("masc","fem")

teste2 <- data.frame(nome,sexo,nota)
teste3 <- bind_rows(teste1 ,teste2)
teste3 %>% group_by(nome) %>% 
          summarise(nota_quant = n(),
                    media_nota = median(nota)) %>% 
          arrange(desc(nota)) %>% 
  ggplot()+
    geom_col(aes(x=nome,y=media_nota),fill="orange")+
    labs(x = "Nomes dos alunos",
         y = "Notas dos alunos",
    title = "Notas das provas")
nova_base %>% group_by(periodo, perfil) %>% 
              summarise(media = median(temp),
                        min = min(temp),
                        max = max(temp))
teste3 %>%  slice(2:3)
