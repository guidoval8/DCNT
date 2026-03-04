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
#------EXTRAÇÃO-----#
#-------------------#

ano_atual <-  as.numeric(format(Sys.Date(), "%Y"))
anos <- 2015:ano_atual

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

tabela_mestra_estupro_pbi <- lista_tabelas %>%
  reduce(left_join, by = c("ANO", "NOME", "nivel_geografico")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  rename(
    `Notificacoes_Estupro` = n,
    `Oportunidade_72h` = prop_72h,
    `PEP_72h` = prop_72h_pep,
    `Contracepção_72h_MIF` = prop_72h_contr
  )
 
#write.csv2(tabela_mestra_estupro_pbi, "C:\\R\\DCNT\\VIO\\VIOL_ESTUPRO.csv", row.names = FALSE)

#----------------------------------------#
#-------------TIPOS VIOLÊNCIA------------#
#----------------------------------------#

cols_tipo <- c("DT_NOTIFIC", "ID_MN_RESI", "VIOL_FISIC", "VIOL_PSICO", 
                    "VIOL_TORT", "VIOL_SEXU", "VIOL_TRAF", "VIOL_FINAN", 
                    "VIOL_LEGAL", "VIOL_TRAB", "LES_AUTOP")

for (col in cols_tipo) {
  if (!(col %in% names(df_final))) {
    df_final[[col]] <- NA
  }
}

df_tipos_violencia <- df_final %>%
  mutate(
    CODMUNRES = str_sub(as.character(ID_MN_RESI), 1, 6),
    # MAPEAMENTO DOS TIPOS DE VIOLÊNCIA (1, S ou SIM = Sim)
    TIPO_FISICA = ifelse(str_detect(toupper(as.character(VIOL_FISIC)), "1|S|SIM"), 1, 0),
    TIPO_PSICOLOGICA = ifelse(str_detect(toupper(as.character(VIOL_PSICO)), "1|S|SIM"), 1, 0),
    TIPO_TORTURA = ifelse(str_detect(toupper(as.character(VIOL_TORT)), "1|S|SIM"), 1, 0),
    TIPO_SEXUAL = ifelse(str_detect(toupper(as.character(VIOL_SEXU)), "1|S|SIM"), 1, 0),
    TIPO_TRAFICO = ifelse(str_detect(toupper(as.character(VIOL_TRAF)), "1|S|SIM"), 1, 0),
    TIPO_FINANCEIRA = ifelse(str_detect(toupper(as.character(VIOL_FINAN)), "1|S|SIM"), 1, 0),
    TIPO_NEGLIGENCIA = ifelse(str_detect(toupper(as.character(VIOL_LEGAL)), "1|S|SIM"), 1, 0),
    TIPO_TRAB_INFANTIL = ifelse(str_detect(toupper(as.character(VIOL_TRAB)), "1|S|SIM"), 1, 0),
    TIPO_AUTOPROVOCADA = ifelse(str_detect(toupper(as.character(LES_AUTOP)), "1|S|SIM"), 1, 0)
  )

tipos_geo <- left_join(df_tipos_violencia, RRAS_RS, by = c("CODMUNRES" = "COD_6_mun"))

#----------------------------------------#
#---NÚMERO DE NOTIFICAÇÕES POR TIPO------#
#----------------------------------------#

#Município
n_tipos_municipio <- tipos_geo %>%
  group_by(ANO, MUNICIPIO) %>%
  rename(NOME = MUNICIPIO) %>%
  summarise(
    Total_Notificacoes = n(),
    Violencia_Fisica = sum(TIPO_FISICA, na.rm = TRUE),
    Violencia_Psicologica = sum(TIPO_PSICOLOGICA, na.rm = TRUE),
    Violencia_Tortura = sum(TIPO_TORTURA, na.rm = TRUE),
    Violencia_Sexual = sum(TIPO_SEXUAL, na.rm = TRUE),
    Violencia_Trafico = sum(TIPO_TRAFICO, na.rm = TRUE),
    Violencia_Financeira = sum(TIPO_FINANCEIRA, na.rm = TRUE),
    Violencia_Negligencia = sum(TIPO_NEGLIGENCIA, na.rm = TRUE),
    Violencia_Trab_Infantil = sum(TIPO_TRAB_INFANTIL, na.rm = TRUE),
    Violencia_Autoprovocada = sum(TIPO_AUTOPROVOCADA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nivel_geografico = "Município")

#RS
n_tipos_rs <- tipos_geo %>%
  group_by(ANO, NOME_RS_2025) %>%
  rename(NOME = NOME_RS_2025) %>%
  summarise(
    Total_Notificacoes = n(),
    Violencia_Fisica = sum(TIPO_FISICA, na.rm = TRUE),
    Violencia_Psicologica = sum(TIPO_PSICOLOGICA, na.rm = TRUE),
    Violencia_Tortura = sum(TIPO_TORTURA, na.rm = TRUE),
    Violencia_Sexual = sum(TIPO_SEXUAL, na.rm = TRUE),
    Violencia_Trafico = sum(TIPO_TRAFICO, na.rm = TRUE),
    Violencia_Financeira = sum(TIPO_FINANCEIRA, na.rm = TRUE),
    Violencia_Negligencia = sum(TIPO_NEGLIGENCIA, na.rm = TRUE),
    Violencia_Trab_Infantil = sum(TIPO_TRAB_INFANTIL, na.rm = TRUE),
    Violencia_Autoprovocada = sum(TIPO_AUTOPROVOCADA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nivel_geografico = "RS")

#RRAS
n_tipos_rras <- tipos_geo %>%
  group_by(ANO, RRAS_2025) %>%
  rename(NOME = RRAS_2025) %>%
  summarise(
    Total_Notificacoes = n(),
    Violencia_Fisica = sum(TIPO_FISICA, na.rm = TRUE),
    Violencia_Psicologica = sum(TIPO_PSICOLOGICA, na.rm = TRUE),
    Violencia_Tortura = sum(TIPO_TORTURA, na.rm = TRUE),
    Violencia_Sexual = sum(TIPO_SEXUAL, na.rm = TRUE),
    Violencia_Trafico = sum(TIPO_TRAFICO, na.rm = TRUE),
    Violencia_Financeira = sum(TIPO_FINANCEIRA, na.rm = TRUE),
    Violencia_Negligencia = sum(TIPO_NEGLIGENCIA, na.rm = TRUE),
    Violencia_Trab_Infantil = sum(TIPO_TRAB_INFANTIL, na.rm = TRUE),
    Violencia_Autoprovocada = sum(TIPO_AUTOPROVOCADA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nivel_geografico = "RRAS")

#Estado SP
n_tipos_estado <- tipos_geo %>%
  group_by(ANO) %>%
  summarise(
    Total_Notificacoes = n(),
    Violencia_Fisica = sum(TIPO_FISICA, na.rm = TRUE),
    Violencia_Psicologica = sum(TIPO_PSICOLOGICA, na.rm = TRUE),
    Violencia_Tortura = sum(TIPO_TORTURA, na.rm = TRUE),
    Violencia_Sexual = sum(TIPO_SEXUAL, na.rm = TRUE),
    Violencia_Trafico = sum(TIPO_TRAFICO, na.rm = TRUE),
    Violencia_Financeira = sum(TIPO_FINANCEIRA, na.rm = TRUE),
    Violencia_Negligencia = sum(TIPO_NEGLIGENCIA, na.rm = TRUE),
    Violencia_Trab_Infantil = sum(TIPO_TRAB_INFANTIL, na.rm = TRUE),
    Violencia_Autoprovocada = sum(TIPO_AUTOPROVOCADA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    nivel_geografico = "Estado SP",
    NOME = "Estado SP"
  )

#------JOIN------#
tabela_mestra_tipos_pbi <- rbind(n_tipos_municipio, n_tipos_estado, n_tipos_rras, n_tipos_rs)

#Organiza as colunas
tabela_mestra_tipos_pbi <- tabela_mestra_tipos_pbi %>%
  select(ANO, NOME, nivel_geografico, everything()) %>%
  filter(!is.na(NOME))

#----------------------------------------#
#--------------------LAP-----------------#
#----------------------------------------#

#----POPULAÇÃO----#
options(download.file.method = "libcurl")
anos_baixar <- 2015:ano_atual
lista_pop_bruta <- list()

for (ano in anos_baixar) {
  
  #Pega os dois últimos dígitos do ano
  sufixo_ano <- substr(as.character(ano), 3, 4)
  
  #Cria os nomes dos arquivos dinamicamente
  url_pop_zip  <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/POPSBR", sufixo_ano, ".zip")
  nome_pop_zip <- paste0("POPSBR", sufixo_ano, ".zip")
  nome_dbf     <- paste0("POP", sufixo_ano, ".dbf")
  
  tryCatch({
    #Baixa o arquivo
    print(paste("Baixando:", nome_pop_zip))
    download.file(url = url_pop_zip, destfile = nome_pop_zip, mode = "wb", quiet = TRUE)
    
    #Descompacta
    unzip(nome_pop_zip)
    
    #Le o arquivo .dbf
    print(paste("Lendo:", nome_dbf))
    df_ano_atual <- read.dbf(nome_dbf)
    
    names(df_ano_atual) <- toupper(names(df_ano_atual))
    
    #Adiciona o dataframe lido a lista
    lista_pop_bruta[[as.character(ano)]] <- df_ano_atual
    
    print(paste("Ano", ano, "processado com sucesso."))
    
  }, error = function(e) {
    # Se der erro, ele avisa e continua
    print(paste("ERRO ao processar o ano", ano, ":", e$message))
  })
}

pop_bruta_total <- bind_rows(lista_pop_bruta)

#CLASSIFICAÇÃO DE FAIXA ETÁRIA
DENOMINADOR <- pop_bruta_total %>%
  mutate(IDADE = as.numeric(IDADE)) %>%
  mutate(
    FAIXA_ETARIA = case_when(
      IDADE < 5  ~ "00-04 anos",
      IDADE >= 5  & IDADE <= 9  ~ "05-09 anos",
      IDADE >= 10 & IDADE <= 14 ~ "10-14 anos",
      IDADE >= 15 & IDADE <= 19 ~ "15-19 anos",
      IDADE >= 20 & IDADE <= 29 ~ "20-29 anos",
      IDADE >= 30 & IDADE <= 39 ~ "30-39 anos",
      IDADE >= 40 & IDADE <= 49 ~ "40-49 anos",
      IDADE >= 50 & IDADE <= 59 ~ "50-59 anos",
      IDADE >= 60 ~ "60 anos e mais",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FAIXA_ETARIA))

#Criando o estrato por sexo e ambos
df_pop_sexo <- DENOMINADOR
df_pop_sexo_total <- DENOMINADOR %>%
  mutate(SEXO = "Total")

DENOMINADOR <- rbind(df_pop_sexo, df_pop_sexo_total)

DENOMINADOR <- DENOMINADOR %>%
  group_by(COD_MUN, ANO, SEXO, FAIXA_ETARIA) %>%
  summarise(
    populacao = sum(POP, na.rm = TRUE)
  ) %>%
  ungroup()

#TRANSFORMAÇÃO PARA LINKAGE ENTRE NUMERADOR E DENOMINADOR
DENOMINADOR <- DENOMINADOR %>%
  #Converter tipos
  mutate(
    COD_MUN = as.character(COD_MUN),
    ANO = as.double(as.character(ANO)),
    SEXO = as.character(SEXO)
  ) %>%
  #Filtrar mun de SP
  filter(str_starts(COD_MUN, "35")) %>%
  #Traduzir SEXO
  mutate(
    SEXO = case_when(
      SEXO == "1" ~ "Masculino",
      SEXO == "2" ~ "Feminino",
      SEXO == "Total" ~ "Total",
      TRUE ~ NA_character_
    )
  ) %>%
  #Renomear
  rename(
    CODMUNRES = COD_MUN,
    ANO = ANO
  ) %>%
  #Padronizar CODMUNRES
  mutate(
    CODMUNRES = str_sub(CODMUNRES, start = 1L, end = 6L)
  )

pop_geo <- left_join(DENOMINADOR, RRAS_RS, by = c("CODMUNRES" = "COD_6_mun"))

#----LA----#
cols_lap <- c("AG_FORCA", "AG_ENFOR", "AG_OBJETO", "AG_CORTE", "AG_QUENTE 
              ", "AG_ENVEN", "AG_FOGO", "AG_AMEACA")

for (col in cols_lap) { if (!(col %in% names(df_final))) df_final[[col]] <- NA }

df_lap <- df_final %>%
  filter(str_detect(toupper(as.character(LES_AUTOP)), "1|S|SIM")) %>%
  mutate(
    CODMUNRES = str_sub(as.character(ID_MN_RESI), 1, 6),
    COD_IDADE = str_sub(as.character(NU_IDADE_N), 1, 1),
    VAL_IDADE = as.numeric(str_sub(as.character(NU_IDADE_N), 2, 4)),
    IDADE_ANOS = case_when(
      COD_IDADE == "4" ~ VAL_IDADE,
      COD_IDADE == "5" ~ VAL_IDADE + 100,
      COD_IDADE %in% c("1", "2", "3") ~ 0,
      TRUE ~ NA_real_
    ),
    FAIXA_ETARIA = case_when(
      IDADE_ANOS < 5  ~ "00-04 anos",
      IDADE_ANOS >= 5  & IDADE_ANOS <= 9  ~ "05-09 anos",
      IDADE_ANOS >= 10 & IDADE_ANOS <= 14 ~ "10-14 anos",
      IDADE_ANOS >= 15 & IDADE_ANOS <= 19 ~ "15-19 anos",
      IDADE_ANOS >= 20 & IDADE_ANOS <= 29 ~ "20-29 anos",
      IDADE_ANOS >= 30 & IDADE_ANOS <= 39 ~ "30-39 anos",
      IDADE_ANOS >= 40 & IDADE_ANOS <= 49 ~ "40-49 anos",
      IDADE_ANOS >= 50 & IDADE_ANOS <= 59 ~ "50-59 anos",
      IDADE_ANOS >= 60 ~ "60 anos e mais",
      TRUE ~ NA_character_
    ),
    RACA_COR = case_when(
      str_starts(toupper(CS_RACA), "1|BRA") ~ 'Branca',
      str_starts(toupper(CS_RACA), "2|PRE") ~ 'Preta',
      str_starts(toupper(CS_RACA), "3|AMA") ~ 'Amarela',
      str_starts(toupper(CS_RACA), "4|PAR") ~ 'Parda',
      str_starts(toupper(CS_RACA), "5|IND") ~ 'Indigena',
      TRUE ~ NA_character_
    ),
    SEXO = case_when(
      str_starts(toupper(CS_SEXO), "M") ~ "Masculino",
      str_starts(toupper(CS_SEXO), "F") ~ "Feminino",
      TRUE ~ NA_character_
    ),
    MEIO_AGRESSAO = case_when(
      str_detect(toupper(as.character(AG_ENFOR)), "1|S") ~ "Enforcamento",
      str_detect(toupper(as.character(AG_FOGO)), "1|S") ~ "Arma de Fogo",
      str_detect(toupper(as.character(AG_ENVEN)), "1|S") ~ "Envenenamento/Intoxicação",
      str_detect(toupper(as.character(AG_CORTE)), "1|S") ~ "Objeto Cortante",
      str_detect(toupper(as.character(AG_QUENTE)), "1|S") ~ "Substancia/objeto quente",
      str_detect(toupper(as.character(AG_FORCA)), "1|S") ~ "Força corporal/espancamento",
      str_detect(toupper(as.character(AG_OBJETO)), "1|S") ~ "Objeto contundente",
      str_detect(toupper(as.character(AG_AMEACA)), "1|S") ~ "Ameaça",
      TRUE ~ "Outros Meios"
    )
  )

lap_geo <- left_join(df_lap, RRAS_RS, by = c("CODMUNRES" = "COD_6_mun")) %>%
  filter(IDADE_ANOS >= 5)

#----SEXO TOTAL----#
lap_geo_sexo_total <- lap_geo %>% mutate(SEXO = "Total")
lap_geo_completo <- bind_rows(lap_geo, lap_geo_sexo_total)

#------TAXAS-----#
pop_mun <- pop_geo %>% group_by(ANO, NOME = MUNICIPIO, SEXO) %>% summarise(POP = sum(populacao, na.rm = TRUE), .groups = "drop") %>% mutate(nivel_geografico = "Município")
pop_rs <- pop_geo %>% group_by(ANO, NOME = NOME_RS_2025, SEXO) %>% summarise(POP = sum(populacao, na.rm = TRUE), .groups = "drop") %>% mutate(nivel_geografico = "RS")
pop_rras <- pop_geo %>% group_by(ANO, NOME = RRAS_2025, SEXO) %>% summarise(POP = sum(populacao, na.rm = TRUE), .groups = "drop") %>% mutate(nivel_geografico = "RRAS")
pop_estado <- pop_geo %>% group_by(ANO, SEXO) %>% summarise(POP = sum(populacao, na.rm = TRUE), .groups = "drop") %>% mutate(NOME = "Estado SP", nivel_geografico = "Estado SP")

pop_final_niveis <- bind_rows(pop_mun, pop_rs, pop_rras, pop_estado) %>% filter(!is.na(NOME))

lap_mun <- lap_geo_completo %>% 
  filter(!is.na(SEXO)) %>% 
  group_by(ANO, NOME = MUNICIPIO, SEXO, MEIO_AGRESSAO) %>% 
  summarise(NOTIFICACOES = n(), .groups = "drop") %>% 
  mutate(nivel_geografico = "Município")

lap_rs <- lap_geo_completo %>% 
  filter(!is.na(SEXO)) %>% 
  group_by(ANO, NOME = NOME_RS_2025, SEXO, MEIO_AGRESSAO) %>% 
  summarise(NOTIFICACOES = n(), .groups = "drop") %>% 
  mutate(nivel_geografico = "RS")

lap_rras <- lap_geo_completo %>% 
  filter(!is.na(SEXO)) %>% 
  group_by(ANO, NOME = RRAS_2025, SEXO, MEIO_AGRESSAO) %>% 
  summarise(NOTIFICACOES = n(), .groups = "drop") %>% 
  mutate(nivel_geografico = "RRAS")

lap_estado <- lap_geo_completo %>% 
  filter(!is.na(SEXO)) %>% 
  group_by(ANO, SEXO, MEIO_AGRESSAO) %>% 
  summarise(NOTIFICACOES = n(), .groups = "drop") %>% 
  mutate(NOME = "Estado SP", nivel_geografico = "Estado SP")

lap_final_niveis <- bind_rows(lap_mun, lap_rs, lap_rras, lap_estado) %>% filter(!is.na(NOME))

taxa_lap_completa <- lap_final_niveis %>%
  left_join(pop_final_niveis, by = c("ANO", "NOME", "nivel_geografico", "SEXO")) %>%
  mutate(
    TAXA_100MIL = round((NOTIFICACOES / POP) * 100000, 2)
  ) %>%
  select(ANO, NOME, nivel_geografico, SEXO, NOTIFICACOES, TAXA_100MIL, MEIO_AGRESSAO) %>%
  arrange(nivel_geografico, NOME, ANO, SEXO)

#FALTA RAÇA
