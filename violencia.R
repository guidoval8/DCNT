#--------------VIOLÊNCIA - SINAN------------#
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

#BANCO NACIONAL
#"ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/VIOLBR23.dbc"
#Filtros: SG_UF_OCOR == 35, "SEX_ESTUPR" == 1

#-------------------#
#----EXTRAÇÃO-------#
#-------------------#

anos <- 2015:2024

processar_dbc_ftp <- function(ano) {
  
  ano_curto <- substr(ano, 3, 4)
  nome_arquivo <- paste0("VIOLBR", ano_curto, ".dbc")
  
  if (ano >= 2024) {
    url <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/", nome_arquivo)
  } else {
    url <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/", nome_arquivo)
  }
  
  temp_file <- tempfile(fileext = ".dbc")
  
  #tryCatch para evitar que o loop quebre caso a conexão falhe em um ano específico
  message("Baixando e processando ano: ", ano, " - URL: ", url)
  
  resultado <- tryCatch({
    #Fazer o download do arquivo binário (mode = "wb" é obrigatório para .dbc)
    download.file(url, destfile = temp_file, mode = "wb", quiet = TRUE)
    
    df <- read.dbc(temp_file)
    
    df_filtrado <- df %>%
      mutate(ANO = year(DT_OCOR)) %>%
      filter(ANO >= 2015 & as.character(SG_UF_OCOR) == "35")
    
    return(df_filtrado)
    
  }, error = function(e) {
    message("Erro no ano ", ano, ": ", e$message)
    return(NULL)
  }, finally = {
    if (file.exists(temp_file)) {
      unlink(temp_file)
    }
  })
  
  return(resultado)
}

#Rodar o loop para todos os anos e empilhar em um único data.frame final
df_final <- map_dfr(anos, processar_dbc_ftp)

#-------------------------------#
#---EXTRAÇÃO ANTIGA (IGNORAR)---#
#-------------------------------#

# colunas <- c("ID_AGRAVO", "DT_OCOR","NU_IDADE_N", "CS_SEXO", "CS_RACA","SG_UF_OCOR", "ID_MN_OCOR", "HORA_OCOR", "VIOL_SEXU", 
#              "SEX_ESTUPR", "PROC_DST", "PROC_HIV", "PROC_CONTR", "CLASSI_FIN", "DT_NOTIFIC")
# 
# arquivos <- list.files(
#   path = "C:\\R\\DCNT\\VIO\\VIOBR",
#   pattern = "\\.dbc$",
#   full.names = TRUE
# )
# 
# processar_dbc <- function(arquivo) {
#   
#   df <- read.dbc(arquivo)
#   
#   df %>%
#    
#     select(any_of(colunas)) %>%
#     mutate(ANO = year(DT_OCOR)) %>%
#     filter(ANO >= 2015 & as.character(SG_UF_OCOR) == "35" & as.character(SEX_ESTUPR) == "1")
# }
# 
# df_final <- map_dfr(arquivos, processar_dbc)
# 
# df_final <- df_final %>%
#   mutate(ANO = year(DT_OCOR))

#-------------------------------#

#----------------------------------------#
#-------------VIOLÊNCIA SEXUAL-----------#
#----------------------------------------#

df_estupro <- df_final %>%
  select("ANO", "ID_AGRAVO", "DT_OCOR","NU_IDADE_N", "CS_SEXO", "CS_RACA","SG_UF_OCOR", "ID_MN_OCOR", "HORA_OCOR", "VIOL_SEXU", 
         "SEX_ESTUPR", "PROC_DST", "PROC_HIV", "PROC_CONTR", "CLASSI_FIN", "DT_NOTIFIC") %>%
  filter(as.character(SEX_ESTUPR) == "1")

#----RRAS----#
RRAS_Municipios <- import("https://github.com/guidoval8/DCNT/blob/main/dados/RRAS_Municipios.xlsx?raw=true")

#Padronização
RRAS_RS <- RRAS_Municipios %>%
  mutate(COD_6_mun = as.character(COD_6_mun)) %>%
  mutate(COD_6_mun = str_sub(COD_6_mun, start = 1L, end = 6L)) %>%
  select(COD_6_mun, MUNICIPIO, RRAS_2025, COD_RS_2025, NOME_RS_2025) %>%
  distinct(COD_6_mun, .keep_all = TRUE)

estupro <- left_join(df_estupro, RRAS_RS, by = c("ID_MN_OCOR" = "COD_6_mun"))

#----------------------------------------#
#---NÚMERO DE NOTIFICAÇÕES POR ESTUPRO---#
#----------------------------------------#

# n_sexo_total <- df_final %>%
#   mutate(CS_SEXO = "Total")
# 
# n_estupro <- rbind(n_sexo_total, df_final)

n_estupro <- estupro %>%
  group_by(ANO, MUNICIPIO) %>%
  rename(NOME = MUNICIPIO) %>%
  summarise(n = n()) %>%
  mutate(indicador = "n_estupro",
         nivel_geografico = "Município")%>%
  ungroup()

#rs
n_estupro_rs <- estupro %>%
  group_by(ANO, NOME_RS_2025) %>%
  rename(NOME = NOME_RS_2025) %>%
  summarise(n = n()) %>%
  mutate(indicador = "n_estupro",
         nivel_geografico = "RS")%>%
  ungroup()

#RRAS
n_estupro_rass <- estupro %>%
  group_by(ANO, RRAS_2025) %>%
  rename(NOME = RRAS_2025) %>%
  summarise(n = n()) %>%
  mutate(indicador = "n_estupro",
         nivel_geografico = "RRAS")%>%
  ungroup()

#estado
n_estupro_estado <- estupro %>%
  group_by(ANO) %>%
  summarise(n = n()) %>%
  mutate(indicador = "n_estupro",
         nivel_geografico = "Estado SP",
         NOME = "Estado SP") %>%
  ungroup()

n_estupro_final <- rbind(n_estupro,n_estupro_estado,n_estupro_rass,n_estupro_rs)
#----------------------------------------------#
#---Proporção de notificações estupro em 72h---#
#----------------------------------------------#
n_estupro_72 <- estupro %>%
  mutate(DT_NOTIFIC = as.Date(DT_NOTIFIC),
         DT_OCOR = as.Date(DT_OCOR),
         dias_at = as.numeric(difftime(DT_NOTIFIC, DT_OCOR, units = "days")),
         h72 = case_when(
           dias_at >= 0 & dias_at <= 3 ~ 1,
           dias_at > 3 ~ 0,
           dias_at < 0 ~ 2,
           is.na(dias_at) ~ 4
         )
        )

prop_estupro_72 <- n_estupro_72 %>%
  group_by(ANO, MUNICIPIO) %>%
  rename(NOME = MUNICIPIO) %>%
  filter(h72 %in% c(1, 0)) %>%
  summarise(
    total = n(),
    casos_72h = sum(h72 == 1),
    prop_72h = round((casos_72h / total) * 100, 2)
  )%>%
  mutate(indicador = "prop_estupro_72",
         nivel_geografico = "Município")

#rs
prop_estupro_72_rs <- n_estupro_72 %>%
  group_by(ANO, NOME_RS_2025) %>%
  rename(NOME = NOME_RS_2025) %>%
  filter(h72 %in% c(1, 0)) %>%
  summarise(
    total = n(),
    casos_72h = sum(h72 == 1),
    prop_72h = round((casos_72h / total) * 100, 2)
  )%>%
  mutate(indicador = "prop_estupro_72",
         nivel_geografico = "RS")

#rras
prop_estupro_72_rras <- n_estupro_72 %>%
  group_by(ANO, RRAS_2025) %>%
  rename(NOME = RRAS_2025) %>%
  filter(h72 %in% c(1, 0)) %>%
  summarise(
    total = n(),
    casos_72h = sum(h72 == 1),
    prop_72h = round((casos_72h / total) * 100, 2)
  )%>%
  mutate(indicador = "prop_estupro_72",
         nivel_geografico = "RRAS")

#estado
prop_estupro_72_estado <- n_estupro_72 %>%
  group_by(ANO) %>%
  filter(h72 %in% c(1, 0)) %>%
  summarise(
    total = n(),
    casos_72h = sum(h72 == 1),
    prop_72h = round((casos_72h / total) * 100, 2)
  )%>%
  mutate(indicador = "prop_estupro_72",
         nivel_geografico = "Estado SP",
         NOME = "Estado SP")

prop_estupro_72_final <- rbind(prop_estupro_72, prop_estupro_72_rs, prop_estupro_72_rras, prop_estupro_72_estado)
#--------------------------------------------------#
#---Proporção de notificações estupro em 72h PEP---#
#--------------------------------------------------#

prop_estupro_72_pep <- n_estupro_72 %>%
  filter(PROC_DST == "1" | PROC_HIV == "1") %>%
  group_by(ANO, MUNICIPIO) %>%
  rename(NOME = MUNICIPIO) %>%
  filter(h72 %in% c(1,0)) %>%
  summarise(
    total = n(),
    casos_72h_pep = sum(h72 == 1),
    prop_72h_pep = round((casos_72h_pep / total) * 100, 2)
  )%>%
  mutate(indicador = "prop_estupro_72_pep",
         nivel_geografico = "Município")

#RS
prop_estupro_72_pep_rs <- n_estupro_72 %>%
  filter(PROC_DST == "1" | PROC_HIV == "1") %>%
  group_by(ANO, NOME_RS_2025) %>%
  rename(NOME = NOME_RS_2025) %>%
  filter(h72 %in% c(1,0)) %>%
  summarise(
    total = n(),
    casos_72h_pep = sum(h72 == 1),
    prop_72h_pep = round((casos_72h_pep / total) * 100, 2)
  )%>%
  mutate(indicador = "prop_estupro_72_pep",
         nivel_geografico = "RS")

#RRAS
prop_estupro_72_pep_rras <- n_estupro_72 %>%
  filter(PROC_DST == "1" | PROC_HIV == "1") %>%
  group_by(ANO, RRAS_2025) %>%
  rename(NOME = RRAS_2025) %>%
  filter(h72 %in% c(1,0)) %>%
  summarise(
    total = n(),
    casos_72h_pep = sum(h72 == 1),
    prop_72h_pep = round((casos_72h_pep / total) * 100, 2)
  )%>%
  mutate(indicador = "prop_estupro_72_pep",
         nivel_geografico = "RRAS")

#Estado
prop_estupro_72_pep_estado <- n_estupro_72 %>%
  filter(PROC_DST == "1" | PROC_HIV == "1") %>%
  group_by(ANO) %>%
  filter(h72 %in% c(1,0)) %>%
  summarise(
    total = n(),
    casos_72h_pep = sum(h72 == 1),
    prop_72h_pep = round((casos_72h_pep / total) * 100, 2)
  )%>%
  mutate(indicador = "prop_estupro_72_pep",
         nivel_geografico = "Estado SP",
         NOME = "Estado SP")

prop_estupro_72_pep_final <- rbind(prop_estupro_72_pep, prop_estupro_72_pep_rs, prop_estupro_72_pep_rras, prop_estupro_72_pep_estado)  
#----------------------------------------------------#
#---Proporção de notificações estupro em 72h CONTR---#
#----------------------------------------------------#

prop_estupro_72_contr <- n_estupro_72 %>%
  filter(PROC_CONTR == "1" & CS_SEXO == "F" & NU_IDADE_N >= 4010 & NU_IDADE_N <= 4049) %>%
  group_by(ANO, MUNICIPIO) %>%
  rename(NOME = MUNICIPIO) %>%
  filter(h72 %in% c(1,0)) %>%
  summarise(
    total = n(),
    casos_72h_contr = sum(h72 == 1),
    prop_72h_contr = round((casos_72h_contr / total) * 100, 2)
  ) %>%
  mutate(indicador = "prop_estupro_72_contr",
         nivel_geografico = "Município")

#rs
prop_estupro_72_contr_rs <- n_estupro_72 %>%
  filter(PROC_CONTR == "1" & CS_SEXO == "F" & NU_IDADE_N >= 4010 & NU_IDADE_N <= 4049) %>%
  group_by(ANO, NOME_RS_2025) %>%
  rename(NOME = NOME_RS_2025) %>%
  filter(h72 %in% c(1,0)) %>%
  summarise(
    total = n(),
    casos_72h_contr = sum(h72 == 1),
    prop_72h_contr = round((casos_72h_contr / total) * 100, 2)
  ) %>%
  mutate(indicador = "prop_estupro_72_contr",
         nivel_geografico = "RS")

#RRAS
prop_estupro_72_contr_rras <- n_estupro_72 %>%
  filter(PROC_CONTR == "1" & CS_SEXO == "F" & NU_IDADE_N >= 4010 & NU_IDADE_N <= 4049) %>%
  group_by(ANO, RRAS_2025) %>%
  rename(NOME = RRAS_2025) %>%
  filter(h72 %in% c(1,0)) %>%
  summarise(
    total = n(),
    casos_72h_contr = sum(h72 == 1),
    prop_72h_contr = round((casos_72h_contr / total) * 100, 2)
  ) %>%
  mutate(indicador = "prop_estupro_72_contr",
         nivel_geografico = "RRAS")

#Estado
prop_estupro_72_contr_estado <- n_estupro_72 %>%
  filter(PROC_CONTR == "1" & CS_SEXO == "F" & NU_IDADE_N >= 4010 & NU_IDADE_N <= 4049) %>%
  group_by(ANO) %>%
  filter(h72 %in% c(1,0)) %>%
  summarise(
    total = n(),
    casos_72h_contr = sum(h72 == 1),
    prop_72h_contr = round((casos_72h_contr / total) * 100, 2)
  ) %>%
  mutate(indicador = "prop_estupro_72_contr",
         nivel_geografico = "Estado SP",
         NOME = "Estado SP")

prop_estupro_72_contr_final <- rbind(prop_estupro_72_contr, prop_estupro_72_contr_rs, prop_estupro_72_contr_rras, prop_estupro_72_contr_estado)  

#------JOIN------#
tab_1 <- n_estupro_final %>% 
  select(-indicador)

tab_2 <- prop_estupro_72_final %>% 
  rename(total_72h = total) %>% 
  select(-indicador)

tab_3 <- prop_estupro_72_pep_final %>% 
  rename(total_pep = total) %>% 
  select(-indicador)

tab_4 <- prop_estupro_72_contr_final %>% 
  rename(total_contr = total) %>% 
  select(-indicador)

lista_tabelas <- list(tab_1, tab_2, tab_3, tab_4)

tabela_mestra_pbi <- lista_tabelas %>%
  reduce(left_join, by = c("ANO", "NOME", "nivel_geografico")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  rename(
    `Notificacoes_Estupro` = n,
    `Oportunidade_72h` = prop_72h,
    `PEP_72h` = prop_72h_pep,
    `Contracepção_72h_MIF` = prop_72h_contr
  )

 
write.csv2(tabela_mestra_pbi, "C:\\R\\DCNT\\VIO\\VIOL_ESTUPRO.csv", row.names = FALSE)

