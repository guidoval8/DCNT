library(dplyr)
library(microdatasus)
#library(devtools)
library(summarytools)
library(lubridate)
library(tidyr)
library(foreign)
library(stringr)
library(openxlsx)
library(rio)
library(sf)
library(ggplot2)
library(rmapshaper)
library(read.dbc)
library(purrr)
library(nanoparquet)

df_sidra <- read.xlsx("C:\\R\\DCNT\\Paineis\\Violencia\\tabela3175.xlsx")

df_sidra <- df_sidra %>%
  mutate(
    Branca = as.numeric(Branca),
    Preta = as.numeric(Preta),
    Parda = as.numeric(Parda),
    Amarela = as.numeric(Amarela),
    Indigena = as.numeric(Indígena),
  ) %>%
  select(-Indígena)

pop_raca_2010 <- df_sidra %>%
  fill(cod, Município, Sexo, .direction = "down") %>%
  
  mutate(CODMUNRES = str_sub(as.character(cod), 1, 6)) %>%
  filter(str_starts(CODMUNRES, "35"), 
         Sexo != "Total", 
         Idade != "Total") %>%

  pivot_longer(
    cols = c(Branca, Preta, Amarela, Parda, Indigena),
    names_to = "CS_RACA",
    values_to = "POPULACAO"
  ) %>%

  mutate(
    POPULACAO = as.numeric(POPULACAO),
    CS_SEXO = if_else(Sexo == "Homens", "Masculino", "Feminino"),

    FX = case_when(
      Idade %in% c("5 a 9 anos", "10 a 14 anos", "15 a 19 anos") ~ "5-19 anos",
      Idade %in% c("20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos") ~ "20-39 anos",
      Idade %in% c("40 a 44 anos", "45 a 49 anos", "50 a 54 anos", "55 a 59 anos") ~ "40-59 anos",
      Idade %in% c("60 a 69 anos", "70 anos ou mais") ~ "60 anos ou mais",
      TRUE ~ NA_character_
    )
  ) %>%

  filter(!is.na(FX)) %>%
  
  group_by(CODMUNRES, CS_SEXO, FX, CS_RACA) %>%
  summarise(populacao_agrupada = sum(POPULACAO, na.rm = TRUE), .groups = "drop") %>%
  
  group_by(CODMUNRES, CS_SEXO, FX) %>%
  mutate(
    pop_total_estrato = sum(populacao_agrupada, na.rm = TRUE),
    peso_raca = if_else(pop_total_estrato > 0, populacao_agrupada / pop_total_estrato, 0)
  ) %>%
  ungroup() %>%
  
  select(CODMUNRES, CS_SEXO, FX, CS_RACA, peso_raca, populacao_agrupada, pop_total_estrato)

# Exportando
write.xlsx(pop_raca_2010, "C:/R/DCNT/Paineis/Violencia/pop_raca_2010.xlsx")

ano_atual <- as.numeric(format(Sys.Date(), "%Y"))

pop_2010 <- pop_raca_2010 %>% 
  select(CODMUNRES, CS_SEXO, FX, CS_RACA, peso_raca) %>% 
  mutate(ANO = 2010)

pop_raca_2022 <- import("https://github.com/guidoval8/DCNT/blob/86c286ea3a05f3e5b3bebb4d57a9a10cb3084df3/dados/pop_raca_2022.xlsx?raw=true")

pop_2022 <- pop_raca_2022 %>% 
  select(CODMUNRES, CS_SEXO, FX, CS_RACA, peso_raca) %>% 
  mutate(ANO = 2022)

pop_censos <- bind_rows(pop_2010, pop_2022)

#Interpolar
pesos_intercensitarios <- pop_censos %>%
  #Agrupa pela chave única de perfil demográfico
  group_by(CODMUNRES, CS_SEXO, FX, CS_RACA) %>%
  #Garante que existam os anos de 2010 e 2022. Se faltar, preenche com 0
  complete(ANO = c(2010, 2022), fill = list(peso_raca = 0)) %>%
  #Expande para a série inteira (gera os NAs para os anos intercensitários)
  complete(ANO = 2010:ano_atual) %>%
  arrange(ANO) %>%
  #Interpolação linear
  mutate(
    peso_interpolado = approx(
      x = ANO[!is.na(peso_raca)], 
      y = peso_raca[!is.na(peso_raca)], 
      xout = ANO, 
      rule = 2
    )$y
  ) %>%
  ungroup() %>%
  group_by(CODMUNRES, ANO, CS_SEXO, FX) %>%
  mutate(
    soma_pesos = sum(peso_interpolado, na.rm = TRUE),
    peso_raca_final = if_else(soma_pesos > 0, peso_interpolado / soma_pesos, 0)
  ) %>%
  ungroup() %>%
  select(CODMUNRES, ANO, CS_SEXO, FX, CS_RACA, peso_raca = peso_raca_final)

library(nanoparquet)
write.xlsx(pesos_intercensitarios, "C:/R/DCNT/Paineis/Violencia/pop_raca.xlsx")


