#------------VIOLÊNCIA------------#
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
library(arrow)

#BANCO NACIONAL
#"ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/VIOLBR23.dbc"
#Filtros: SG_UF_OCOR == 35, "SEX_ESTUPR" == 1

#-------------------#
#------EXTRAÇÃO-----#
#-------------------#

ano_atual <-  as.numeric(format(Sys.Date(), "%Y"))
anos <- 2016:ano_atual

processar_dbc_ftp <- function(ano) {
  
  ano_curto <- substr(ano, 3, 4)
  nome_arquivo <- paste0("VIOLBR", ano_curto, ".dbc")
  
  if (ano >= (ano_atual - 1)) {
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
      filter(ANO >= 2016 & as.character(SG_UF_OCOR) == "35")
    
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

#Rodar o loop para todos os anos e empilhar em um único df
df_final <- map_dfr(anos, processar_dbc_ftp)

df_final$ANO_NASC <- as.character(df_final$ANO_NASC)
df_final$ANO_NASC <- as.numeric(df_final$ANO_NASC)

df_final <- df_final %>%
  mutate(
    IDADE = (ANO - ANO_NASC),
    CS_RACA = case_when(
        CS_RACA == 1 ~ 'Branca',
        CS_RACA == 2 ~ 'Preta',
        CS_RACA == 3 ~ 'Amarela',
        CS_RACA == 4 ~ 'Parda',
        CS_RACA == 5 ~ 'Indígena',
        TRUE ~ NA_character_
  ),
  CS_SEXO = case_when(
    CS_SEXO == "F" ~ 'Feminino',
    CS_SEXO == "M" ~ 'Masculino',
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(CS_RACA)) %>%
  filter(!is.na(CS_SEXO))

#----------------------------------------#
#-------------VIOLÊNCIA SEXUAL-----------#
#----------------------------------------#

df_estupro <- df_final %>%
  select("ANO", "ID_AGRAVO","IDADE", "DT_OCOR", "CS_SEXO", "CS_RACA","SG_UF_OCOR", "ID_MN_OCOR", "HORA_OCOR", "VIOL_SEXU", 
         "SEX_ESTUPR", "PROC_DST", "PROC_HIV", "PROC_CONTR", "CLASSI_FIN", "DT_NOTIFIC", "ANO_NASC") %>%
  filter(as.character(SEX_ESTUPR) == "1") 

df_estupro <- df_estupro %>%
  mutate(
    FX = case_when(
           IDADE <= 9 ~ "9 anos ou menos",
           IDADE > 9 & IDADE <= 14 ~ "10-14 anos",
           IDADE > 14 & IDADE <= 19 ~ "15-19 anos",
           IDADE > 19 & IDADE <= 39 ~ "20-39 anos",
           IDADE > 39 & IDADE <= 59 ~ "40-59 anos",
           IDADE >= 60 ~ "60 anos ou mais",
           TRUE ~ NA_character_
         ),
    TIPO = "Violência Sexual"
    ) %>%
  filter(!is.na(FX))

#----RRAS----#
RRAS_Municipios <- import("https://github.com/guidoval8/DCNT/blob/main/dados/RRAS_Municipios.xlsx?raw=true")

#Padronização
RRAS_RS <- RRAS_Municipios %>%
  mutate(COD_6_mun = as.character(COD_6_mun)) %>%
  mutate(COD_6_mun = str_sub(COD_6_mun, start = 1L, end = 6L)) %>%
  select(COD_6_mun, MUNICIPIO, RRAS_2025, COD_RS_2025, NOME_RS_2025) %>%
  distinct(COD_6_mun, .keep_all = TRUE)

estupro <- left_join(df_estupro, RRAS_RS, by = c("ID_MN_OCOR" = "COD_6_mun"))

est_sexo_total <- estupro %>% mutate(CS_SEXO = "Total")
est_fx_total   <- estupro %>% mutate(FX = "Total")
est_ambos_tot  <- estupro %>% mutate(CS_SEXO = "Total", FX = "Total")

#Primeiro agrupa as combinações de sexo e faixa etária mantendo a raça original
base_estupro_parcial <- bind_rows(estupro, est_sexo_total, est_fx_total, est_ambos_tot)

#duplica substituindo a raça por "Total"
est_raca_total <- base_estupro_parcial %>% mutate(CS_RACA = "Total")

#Empilha tudo no dataframe final que será usado nos cálculos
estupro_agrupar <- bind_rows(base_estupro_parcial, est_raca_total)

#----------------------------------------#
#---NÚMERO DE NOTIFICAÇÕES POR ESTUPRO---#
#----------------------------------------#

n_estupro <- estupro_agrupar %>%
  group_by(ANO, MUNICIPIO, CS_SEXO, FX, CS_RACA) %>%
  rename(NOME = MUNICIPIO) %>%
  summarise(n = n()) %>%
  mutate(indicador = "n_estupro",
         nivel_geografico = "Município")%>%
  ungroup()

#rs
n_estupro_rs <- estupro_agrupar %>%
  group_by(ANO, NOME_RS_2025, CS_SEXO, FX, CS_RACA) %>%
  rename(NOME = NOME_RS_2025) %>%
  summarise(n = n()) %>%
  mutate(indicador = "n_estupro",
         nivel_geografico = "RS")%>%
  ungroup()

#RRAS
n_estupro_rass <- estupro_agrupar %>%
  group_by(ANO, RRAS_2025, CS_SEXO, FX, CS_RACA) %>%
  rename(NOME = RRAS_2025) %>%
  summarise(n = n()) %>%
  mutate(indicador = "n_estupro",
         nivel_geografico = "RRAS")%>%
  ungroup()

#estado
n_estupro_estado <- estupro_agrupar %>%
  group_by(ANO, CS_SEXO, FX, CS_RACA) %>%
  summarise(n = n()) %>%
  mutate(indicador = "n_estupro",
         nivel_geografico = "Estado SP",
         NOME = "Estado SP") %>%
  ungroup()

n_estupro_final <- rbind(n_estupro,n_estupro_estado,n_estupro_rass,n_estupro_rs)

#----------------------------------------------#
#---Proporção de notificações estupro em 72h---#
#----------------------------------------------#

n_estupro_72 <- estupro_agrupar %>%
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
  group_by(ANO, MUNICIPIO, CS_SEXO, FX, CS_RACA) %>%
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
  group_by(ANO, NOME_RS_2025, CS_SEXO, FX, CS_RACA) %>%
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
  group_by(ANO, RRAS_2025, CS_SEXO, FX, CS_RACA) %>%
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
  group_by(ANO, CS_SEXO, FX, CS_RACA) %>%
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
  group_by(ANO, MUNICIPIO, CS_SEXO, FX, CS_RACA) %>%
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
  group_by(ANO, NOME_RS_2025, CS_SEXO, FX, CS_RACA) %>%
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
  group_by(ANO, RRAS_2025, CS_SEXO, FX, CS_RACA) %>%
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
  group_by(ANO, CS_SEXO, FX, CS_RACA) %>%
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
  filter(PROC_CONTR == "1" & CS_SEXO == "Feminino" & IDADE >= 10 & IDADE <= 49) %>%
  group_by(ANO, MUNICIPIO, CS_SEXO, FX, CS_RACA) %>%
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
  filter(PROC_CONTR == "1" & CS_SEXO == "Feminino" & IDADE >= 10 & IDADE <= 49) %>%
  group_by(ANO, NOME_RS_2025, CS_SEXO, FX, CS_RACA) %>%
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
  filter(PROC_CONTR == "1" & CS_SEXO == "Feminino" & IDADE >= 10 & IDADE <= 49) %>%
  group_by(ANO, RRAS_2025, CS_SEXO, FX, CS_RACA) %>%
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
  filter(PROC_CONTR == "1" & CS_SEXO == "Feminino" & IDADE >= 10 & IDADE <= 49) %>%
  group_by(ANO, CS_SEXO, FX, CS_RACA) %>%
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
  reduce(left_join, by = c("ANO", "NOME", "nivel_geografico", "CS_SEXO", "FX", "CS_RACA")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(TIPO_VIOLENCIA = "Violência Sexual") %>% #
  rename(
    Notificacoes_Estupro = n,
    Oportunidade_72h = prop_72h,
    PEP_72h = prop_72h_pep,
    Contracepcao_72h_MIF = prop_72h_contr
  )

#Limpar memória
manter <- c("df_final", "RRAS_RS", "tabela_mestra_estupro_pbi", "ano_atual")

rm(list = setdiff(ls(), manter))

gc()

#------------------------------------#
#---------LESÃO AUTOPROVOCADA--------#
#------------------------------------#
cols_lap <- c("AG_FORCA", "AG_ENFOR", "AG_OBJETO", "AG_CORTE", "AG_QUENTE", "AG_ENVEN", "AG_FOGO", "AG_AMEACA")

for (col in cols_lap) { if (!(col %in% names(df_final))) df_final[[col]] <- NA }

df_lap <- df_final %>%
  filter(LES_AUTOP ==  1) %>%
  filter(IDADE >= 5) %>%
  mutate(
    FX = case_when(
      IDADE >= 5 & IDADE <= 9 ~ '5-9 anos',
      IDADE >= 10 & IDADE <= 19 ~ '10-19 anos',
      IDADE >= 20 & IDADE < 40 ~ '20-39 anos',
      IDADE >= 40 & IDADE < 60 ~ '40-59 anos',
      IDADE >= 60 ~ '60 anos ou mais',
      TRUE ~ NA_character_
    ),
    MEIO_AGRESSAO = case_when(
      str_detect(toupper(as.character(AG_FOGO)), "1|S") ~ "Arma de Fogo",
      str_detect(toupper(as.character(AG_ENFOR)), "1|S") ~ "Enforcamento",
      str_detect(toupper(as.character(AG_ENVEN)), "1|S") ~ "Envenenamento/Intoxicação",
      str_detect(toupper(as.character(AG_CORTE)), "1|S") ~ "Objeto Cortante",
      str_detect(toupper(as.character(AG_QUENTE)), "1|S") ~ "Substância/Objeto Quente",
      str_detect(toupper(as.character(AG_FORCA)), "1|S") ~ "Força Corporal/Espancamento",
      str_detect(toupper(as.character(AG_OBJETO)), "1|S") ~ "Objeto Contundente",
      str_detect(toupper(as.character(AG_AMEACA)), "1|S") ~ "Ameaça",
      TRUE ~ "Outros Meios"
    ),
    TIPO = "Lesão Autoprovocada"
  )%>%
  filter(!is.na(FX))

df_lap_geo <- left_join(df_lap, RRAS_RS, by = c("ID_MN_RESI" = "COD_6_mun"))

#-----------------------------------------#
#---NÚMERO DE NOTIFICAÇÕES LAP POR MEIO---#
#-----------------------------------------#

lap_base <- df_lap_geo

lap_sexo_total <- lap_base %>% mutate(CS_SEXO = "Total")
lap_fx_total   <- lap_base %>% mutate(FX = "Total")
lap_ambos_total <- lap_base %>% mutate(CS_SEXO = "Total", FX = "Total")

lap_meio_total <- bind_rows(lap_base, lap_sexo_total, lap_fx_total, lap_ambos_total) %>%
  mutate(MEIO_AGRESSAO = "Todos os Meios")

lap_agrupar <- bind_rows(lap_base, lap_sexo_total, lap_fx_total, lap_ambos_total, lap_meio_total)

#Município
lap_mun <- lap_agrupar %>%
  group_by(ANO, MUNICIPIO, CS_SEXO, FX, MEIO_AGRESSAO) %>%
  summarise(Notificacoes_LAP = n(), .groups = "drop") %>%
  rename(NOME = MUNICIPIO) %>%
  mutate(nivel_geografico = "Município")

#RS
lap_rs <- lap_agrupar %>%
  filter(!is.na(NOME_RS_2025)) %>%
  group_by(ANO, NOME_RS_2025, CS_SEXO, FX, MEIO_AGRESSAO) %>%
  summarise(Notificacoes_LAP = n(), .groups = "drop") %>%
  rename(NOME = NOME_RS_2025) %>%
  mutate(nivel_geografico = "RS")

#RRAS
lap_rras <- lap_agrupar %>%
  filter(!is.na(RRAS_2025)) %>%
  group_by(ANO, RRAS_2025, CS_SEXO, FX, MEIO_AGRESSAO) %>%
  summarise(Notificacoes_LAP = n(), .groups = "drop") %>%
  rename(NOME = RRAS_2025) %>%
  mutate(nivel_geografico = "RRAS")

#Estado
lap_estado <- lap_agrupar %>%
  group_by(ANO, CS_SEXO, FX, MEIO_AGRESSAO) %>%
  summarise(Notificacoes_LAP = n(), .groups = "drop") %>%
  mutate(
    NOME = "Estado SP",
    nivel_geografico = "Estado SP"
  )

lap_n <- bind_rows(lap_mun, lap_rs, lap_rras, lap_estado)

#-------------------------------#
#---PROPORÇÃO DE LAP POR RAÇA---#
#-------------------------------#

#Município
lap_raca_mun <- lap_agrupar %>%
  group_by(ANO, MUNICIPIO, CS_SEXO, FX, MEIO_AGRESSAO, CS_RACA) %>%
  summarise(casos_raca = n(), .groups = "drop") %>%
  rename(NOME = MUNICIPIO) %>%
  mutate(nivel_geografico = "Município")

#RS
lap_raca_rs <- lap_agrupar %>%
  filter(!is.na(NOME_RS_2025)) %>%
  group_by(ANO, NOME_RS_2025, CS_SEXO, FX, MEIO_AGRESSAO, CS_RACA) %>%
  summarise(casos_raca = n(), .groups = "drop") %>%
  rename(NOME = NOME_RS_2025) %>%
  mutate(nivel_geografico = "RS")

#RRAS
lap_raca_rras <- lap_agrupar %>%
  filter(!is.na(RRAS_2025)) %>%
  group_by(ANO, RRAS_2025, CS_SEXO, FX, MEIO_AGRESSAO, CS_RACA) %>%
  summarise(casos_raca = n(), .groups = "drop") %>%
  rename(NOME = RRAS_2025) %>%
  mutate(nivel_geografico = "RRAS")

#Estado
lap_raca_estado <- lap_agrupar %>%
  group_by(ANO, CS_SEXO, FX, MEIO_AGRESSAO, CS_RACA) %>%
  summarise(casos_raca = n(), .groups = "drop") %>%
  mutate(
    NOME = "Estado SP",
    nivel_geografico = "Estado SP"
  )

#Empilhar e Pivotar
#Empilhar as raças da LAP e criar o "Total"
lap_raca_long <- bind_rows(lap_raca_mun, lap_raca_rs, lap_raca_rras, lap_raca_estado)

lap_raca_tot_raca <- lap_raca_long %>%
  group_by(ANO, NOME, nivel_geografico, CS_SEXO, FX, MEIO_AGRESSAO) %>%
  summarise(casos_raca = sum(casos_raca, na.rm = TRUE), .groups = "drop") %>%
  mutate(CS_RACA = "Total")

lap_raca_final <- bind_rows(lap_raca_long, lap_raca_tot_raca) %>%
  rename(Notificacoes_LAP = casos_raca)

#---------------------------------------------------#
#---POPULAÇÃO GERAL E POPULAÇÃO ESTIMADA POR RAÇA---#
#---------------------------------------------------#
pop_raca_2022 <- import("https://github.com/guidoval8/DCNT/blob/86c286ea3a05f3e5b3bebb4d57a9a10cb3084df3/dados/pop_raca_2022.xlsx?raw=true")

options(download.file.method = "libcurl")
anos_baixar <- 2016:ano_atual
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
    FX = case_when(
      IDADE >= 5 & IDADE <= 9 ~ '5-9 anos',
      IDADE >= 10 & IDADE <= 19 ~ '10-19 anos',
      IDADE >= 20 & IDADE < 40 ~ '20-39 anos',
      IDADE >= 40 & IDADE < 60 ~ '40-59 anos',
      IDADE >= 60 ~ '60 anos ou mais',
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FX)) %>%
  mutate(
    CODMUNRES = str_sub(as.character(COD_MUN), 1, 6),
    ANO = as.double(as.character(ANO)),
    CS_SEXO = case_when(
      SEXO == "1" ~ "Masculino",
      SEXO == "2" ~ "Feminino",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(str_starts(CODMUNRES, "35"), !is.na(CS_SEXO)) %>%
  group_by(CODMUNRES, ANO, CS_SEXO, FX) %>%
  summarise(populacao = sum(POP, na.rm = TRUE), .groups = "drop")

DENOMINADOR <- DENOMINADOR %>%
  mutate(FX_TEMP = if_else(FX %in% c("5-9 anos", "10-19 anos"), "5-19 anos", FX)) %>%
  left_join(pop_raca_2022, by = c("CODMUNRES", "CS_SEXO", "FX_TEMP" = "FX")) %>%
  mutate(pop_estimada = populacao * peso_raca) %>%
  filter(!is.na(CS_RACA)) %>%
  left_join(RRAS_RS, by = c("CODMUNRES" = "COD_6_mun")) %>%
  select(-FX_TEMP)

#CRIANDO AS COMBINAÇÕES DE TOTAIS (SEXO E FAIXA ETÁRIA)
df_pop_sexo_total <- DENOMINADOR %>% mutate(CS_SEXO = "Total")
df_pop_fx_total   <- DENOMINADOR %>% mutate(FX = "Total")
df_pop_ambos_tot  <- DENOMINADOR %>% mutate(CS_SEXO = "Total", FX = "Total")

#EMPILHANDO TUDO
pop_raca_agrupar <- bind_rows(
  DENOMINADOR,
  df_pop_sexo_total,
  df_pop_fx_total,
  df_pop_ambos_tot
)

#AGRUPANDO E SOMANDO A POPULAÇÃO
pop_raca_mun <- pop_raca_agrupar %>% 
  group_by(ANO, MUNICIPIO, CS_SEXO, FX, CS_RACA) %>% 
  summarise(pop = sum(pop_estimada, na.rm=TRUE), .groups="drop") %>% 
  rename(NOME = MUNICIPIO) %>% mutate(nivel_geografico = "Município")

pop_raca_rs <- pop_raca_agrupar %>% 
  filter(!is.na(NOME_RS_2025)) %>% 
  group_by(ANO, NOME_RS_2025, CS_SEXO, FX, CS_RACA) %>% 
  summarise(pop = sum(pop_estimada, na.rm=TRUE), .groups="drop") %>% 
  rename(NOME = NOME_RS_2025) %>% mutate(nivel_geografico = "RS")

pop_raca_rras <- pop_raca_agrupar %>% 
  filter(!is.na(RRAS_2025)) %>% 
  group_by(ANO, RRAS_2025, CS_SEXO, FX, CS_RACA) %>% 
  summarise(pop = sum(pop_estimada, na.rm=TRUE), .groups="drop") %>% 
  rename(NOME = RRAS_2025) %>% mutate(nivel_geografico = "RRAS")

pop_raca_estado <- pop_raca_agrupar %>% 
  group_by(ANO, CS_SEXO, FX, CS_RACA) %>% 
  summarise(pop = sum(pop_estimada, na.rm=TRUE), .groups="drop") %>% 
  mutate(NOME = "Estado SP", nivel_geografico = "Estado SP")

#Empilhar as raças da População e criar o "Total"
pop_raca_long <- bind_rows(pop_raca_mun, pop_raca_rs, pop_raca_rras, pop_raca_estado)

pop_raca_tot_raca <- pop_raca_long %>%
  group_by(ANO, NOME, nivel_geografico, CS_SEXO, FX) %>%
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  mutate(CS_RACA = "Total")

pop_raca_final <- bind_rows(pop_raca_long, pop_raca_tot_raca) %>%
  rename(Populacao = pop) %>%
  mutate(Populacao = as.integer(Populacao))

#Criar uma base auxiliar do Total de LAP para calcular a Proporção
lap_totais_estrato <- lap_raca_final %>%
  filter(CS_RACA == "Total") %>%
  select(ANO, NOME, nivel_geografico, CS_SEXO, FX, MEIO_AGRESSAO, Total_LAP_Estrato = Notificacoes_LAP)

tabela_mestra_lap_pbi <- lap_raca_final %>%
  #Join com a População
  left_join(pop_raca_final, by = c("ANO", "NOME", "nivel_geografico", "CS_SEXO", "FX", "CS_RACA")) %>%
  #Join com o Total de LAP para o cálculo da proporção
  left_join(lap_totais_estrato, by = c("ANO", "NOME", "nivel_geografico", "CS_SEXO", "FX", "MEIO_AGRESSAO")) %>%
  mutate(
    Populacao = replace_na(Populacao, 0),
    Total_LAP_Estrato = replace_na(Total_LAP_Estrato, 0),
    
    #Proporção de Notificações (%)
    Prop_LAP = if_else(Total_LAP_Estrato > 0, round((Notificacoes_LAP / Total_LAP_Estrato) * 100, 2), 0),
    
    #Taxa de Notificação por 100 mil habitantes
    Taxa_Notificacao_LAP = if_else(Populacao > 0, (Notificacoes_LAP / Populacao) * 100000, 0),
    
    TIPO_VIOLENCIA = "Lesão Autoprovocada"
  ) %>%
  select(-Total_LAP_Estrato)

#Limpar memória (mantendo a pop_raca_final viva para usarmos no suicídio)
manter <- c("df_final", "RRAS_RS", "tabela_mestra_estupro_pbi", "ano_atual", "tabela_mestra_lap_pbi", "pop_raca_final")

rm(list = setdiff(ls(), manter))
gc()

#--------------#
#---SUICÍDIO---#
#--------------#

#----EXTRAÇÃO----#
SIM <- fetch_datasus(year_start = 2016, year_end = ano_atual, uf = "SP", information_system = "SIM-DO")
SIM <- process_sim(SIM)
#----------------#

#----PADRONIZAÇÃO----#
#CLASSIFICAR ANO
SIM <- SIM %>%
  mutate(DTOBITO = ymd(DTOBITO)) %>%
  mutate(ANOOBITO = year(DTOBITO))

#PADRONIZAR CAUSA BÁSICA
SIM_CID <- SIM %>%
  mutate(
    CAUSABAS = str_trim(toupper(as.character(CAUSABAS))),
    CID3 = str_sub(CAUSABAS, 1,3)
  )

#----#
SIM <- NULL
#----#

SIM_suicidio <- SIM_CID %>%
  mutate(IDADEanos = as.numeric(IDADEanos)) %>%
  filter(IDADEanos >= 5) %>%
  mutate(
    TIPO = case_when(
      CID3 >= 'X60' & CID3 <= 'X84' ~ 'Suicídio'
    ),
    #Mapeamento do CID para os Meios de Agressão do SINAN
    MEIO_AGRESSAO = case_when(
      CID3 >= 'X60' & CID3 <= 'X69' ~ "Envenenamento/Intoxicação",
      CID3 == 'X70' ~ "Enforcamento",
      CID3 %in% c('X72', 'X73', 'X74') ~ "Arma de Fogo",
      CID3 %in% c('X76', 'X77') ~ "Substância/Objeto Quente",
      CID3 == 'X78' ~ "Objeto Cortante",
      CID3 == 'X79' ~ "Objeto Contundente",
      TRUE ~ "Outros Meios"
    )
  ) %>%
  filter(TIPO == 'Suicídio') %>%
  mutate(
    FX = case_when(
      IDADEanos >= 5 & IDADEanos <= 9 ~ '5-9 anos',
      IDADEanos >= 10 & IDADEanos <= 19 ~ '10-19 anos',
      IDADEanos >= 20 & IDADEanos < 40 ~ '20-39 anos',
      IDADEanos >= 40 & IDADEanos < 60 ~ '40-59 anos',
      IDADEanos >= 60 ~ '60 anos ou mais',
      TRUE ~ NA_character_
    )) %>%
  filter(!is.na(FX)) %>%
  filter(!is.na(RACACOR)) %>%
  filter(!is.na(SEXO))

#----#
SIM_CID <- NULL
#----#

df_suicidio_geo <- SIM_suicidio %>%
  mutate(CODMUNRES = str_sub(as.character(CODMUNRES), start = 1L, end = 6L)) %>%
  left_join(RRAS_RS, by = c("CODMUNRES" = "COD_6_mun"))

sui_sexo_total  <- df_suicidio_geo %>% mutate(SEXO = "Total")
sui_fx_total    <- df_suicidio_geo %>% mutate(FX = "Total")
sui_ambos_total <- df_suicidio_geo %>% mutate(SEXO = "Total", FX = "Total")

sui_meio_total <- bind_rows(df_suicidio_geo, sui_sexo_total, sui_fx_total, sui_ambos_total) %>%
  mutate(MEIO_AGRESSAO = "Todos os Meios")

sui_agrupar <- bind_rows(df_suicidio_geo, sui_sexo_total, sui_fx_total, sui_ambos_total, sui_meio_total)

#Município
sui_raca_mun <- sui_agrupar %>%
  group_by(ANOOBITO, MUNICIPIO, SEXO, FX, RACACOR, MEIO_AGRESSAO) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  rename(NOME = MUNICIPIO) %>% mutate(nivel_geografico = "Município")

#RS
sui_raca_rs <- sui_agrupar %>%
  filter(!is.na(NOME_RS_2025)) %>%
  group_by(ANOOBITO, NOME_RS_2025, SEXO, FX, RACACOR, MEIO_AGRESSAO) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  rename(NOME = NOME_RS_2025) %>% mutate(nivel_geografico = "RS")

#RRAS
sui_raca_rras <- sui_agrupar %>%
  filter(!is.na(RRAS_2025)) %>%
  group_by(ANOOBITO, RRAS_2025, SEXO, FX, RACACOR, MEIO_AGRESSAO) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  rename(NOME = RRAS_2025) %>% mutate(nivel_geografico = "RRAS")

#Estado
sui_raca_estado <- sui_agrupar %>%
  group_by(ANOOBITO, SEXO, FX, RACACOR, MEIO_AGRESSAO) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  mutate(NOME = "Estado SP", nivel_geografico = "Estado SP")

#Empilhar geografias
sui_raca_long <- bind_rows(sui_raca_mun, sui_raca_rs, sui_raca_rras, sui_raca_estado) %>%
  rename(ANO = ANOOBITO)

#----CRIAR A RAÇA TOTAL----#
sui_raca_tot_raca <- sui_raca_long %>%
  group_by(ANO, NOME, nivel_geografico, SEXO, FX, MEIO_AGRESSAO) %>%
  summarise(obitos = sum(obitos, na.rm = TRUE), .groups = "drop") %>%
  mutate(RACACOR = "Total")

sui_raca_final <- bind_rows(sui_raca_long, sui_raca_tot_raca) %>%
  rename(Obitos_Suicidio = obitos,
         CS_SEXO = SEXO,
         CS_RACA = RACACOR)

#---- TABELA MESTRA SUICÍDIO (CÁLCULOS NO FORMATO LONG) ----#

tabela_mestra_suicidio_pbi <- sui_raca_final %>%
  # Faz o Join com a pop_raca_final
  left_join(pop_raca_final, by = c("ANO", "NOME", "nivel_geografico", "CS_SEXO", "FX", "CS_RACA")) %>%
  mutate(
    Populacao = replace_na(Populacao, 0),
    
    # Taxa Bruta Específica por 100 mil habitantes
    Taxa_Mortalidade_Suicidio = if_else(Populacao > 0, (Obitos_Suicidio / Populacao) * 100000, 0),
    
    TIPO_VIOLENCIA = "Suicídio"
  )

# Limpar memória
manter <- c(
  "df_final", "RRAS_RS", "ano_atual", 
  "tabela_mestra_estupro_pbi", "tabela_mestra_lap_pbi", "tabela_mestra_suicidio_pbi")

rm(list = setdiff(ls(), manter))
gc()

#----------------------------------------#
#-------------VIOLÊNCIAS MACRO-----------#
#----------------------------------------#
df_macro_prep <- df_final %>%
  select(ANO, ID_MN_OCOR, CS_SEXO, CS_RACA, IDADE, 
         VIOL_FISIC, VIOL_PSICO, VIOL_NEGLI, VIOL_SEXU, LES_AUTOP) %>%
  mutate(
    FX = case_when(
      IDADE <= 9 ~ "9 anos ou menos",
      IDADE > 9 & IDADE <= 14 ~ "10-14 anos",
      IDADE > 14 & IDADE <= 19 ~ "15-19 anos",
      IDADE > 19 & IDADE <= 39 ~ "20-39 anos",
      IDADE > 39 & IDADE <= 59 ~ "40-59 anos",
      IDADE >= 60 ~ "60 anos ou mais",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FX))

df_macro_tipos <- df_macro_prep %>%
  pivot_longer(
    cols = c(VIOL_FISIC, VIOL_PSICO, VIOL_NEGLI, VIOL_SEXU, LES_AUTOP),
    names_to = "TIPO_MACRO",
    values_to = "OCORREU"
  ) %>%
  filter(as.character(OCORREU) == "1") %>%
  mutate(
    TIPO_VIOLENCIA = case_when(
      TIPO_MACRO == "VIOL_FISIC" ~ 'Violência Física',
      TIPO_MACRO == "VIOL_PSICO" ~ 'Violência Psicológica',
      TIPO_MACRO == "VIOL_NEGLI" ~ 'Violência Negligenciada',
      TIPO_MACRO == "VIOL_SEXU" ~ 'Violência Sexual (Geral)',
      TIPO_MACRO == "LES_AUTOP" ~ 'Lesão Autoprovocada (Geral)'
    ),
    MEIO_AGRESSAO = "Não se aplica"
  ) %>%
  select(-TIPO_MACRO, -OCORREU)

df_macro_total <- df_macro_prep %>%
  #Filtra para garantir que a notificação teve ao menos uma das violências preenchidas como sim
  filter(as.character(VIOL_FISIC) == "1" | as.character(VIOL_PSICO) == "1" | 
           as.character(VIOL_NEGLI) == "1" | as.character(VIOL_SEXU) == "1" | as.character(LES_AUTOP) == "1") %>%
  mutate(
    TIPO_VIOLENCIA = "Total de Violências",
    MEIO_AGRESSAO = "Não se aplica"
  ) %>%
  select(ANO, ID_MN_OCOR, CS_SEXO, CS_RACA, IDADE, FX, TIPO_VIOLENCIA, MEIO_AGRESSAO)

df_violencias_macro <- bind_rows(df_macro_tipos, df_macro_total)

# Join Geográfico
df_violencias_macro <- left_join(df_violencias_macro, RRAS_RS, by = c("ID_MN_OCOR" = "COD_6_mun"))

#------------------------------------------------------#
#---MUNICÍPIOS SILENCIOSOS (ZERO NOTIFICAÇÕES MACRO)---#
#------------------------------------------------------#

#grid de todos os municípios x todos os anos
base_municipios_anos <- expand_grid(
  ANO = 2016:ano_atual,
  MUNICIPIO = unique(RRAS_RS$MUNICIPIO)
) %>%
  left_join(RRAS_RS %>% select(MUNICIPIO, NOME_RS_2025, RRAS_2025) %>% distinct(), by = "MUNICIPIO")

#Contar casos totais por município/ano AGORA USANDO A BASE MACRO
casos_totais_mun <- df_violencias_macro %>%
  group_by(ANO, MUNICIPIO) %>%
  summarise(total_casos = n(), .groups = "drop")

#criar a flag de silencioso (1 = zero casos, 0 = teve casos)
flag_silenciosos <- base_municipios_anos %>%
  left_join(casos_totais_mun, by = c("ANO", "MUNICIPIO")) %>%
  mutate(
    total_casos = replace_na(total_casos, 0),
    Flag_Silencioso = if_else(total_casos == 0, 1, 0)
  )

#Calcular a proporção de silenciosos por nível geográfico
#Município
ind_silencioso_mun <- flag_silenciosos %>%
  select(ANO, NOME = MUNICIPIO, Flag_Silencioso) %>%
  mutate(nivel_geografico = "Município", Prop_Mun_Silenciosos = Flag_Silencioso * 100) %>%
  select(-Flag_Silencioso)

#RS
ind_silencioso_rs <- flag_silenciosos %>%
  group_by(ANO, NOME_RS_2025) %>%
  summarise(mun_total = n(), mun_silenc = sum(Flag_Silencioso), .groups = "drop") %>%
  mutate(Prop_Mun_Silenciosos = round((mun_silenc / mun_total) * 100, 2), nivel_geografico = "RS") %>%
  rename(NOME = NOME_RS_2025) %>% select(ANO, NOME, nivel_geografico, Prop_Mun_Silenciosos)

#RRAS
ind_silencioso_rras <- flag_silenciosos %>%
  group_by(ANO, RRAS_2025) %>%
  summarise(mun_total = n(), mun_silenc = sum(Flag_Silencioso), .groups = "drop") %>%
  mutate(Prop_Mun_Silenciosos = round((mun_silenc / mun_total) * 100, 2), nivel_geografico = "RRAS") %>%
  rename(NOME = RRAS_2025) %>% select(ANO, NOME, nivel_geografico, Prop_Mun_Silenciosos)

#Estado
ind_silencioso_estado <- flag_silenciosos %>%
  group_by(ANO) %>%
  summarise(mun_total = n(), mun_silenc = sum(Flag_Silencioso), .groups = "drop") %>%
  mutate(
    Prop_Mun_Silenciosos = round((mun_silenc / mun_total) * 100, 2), 
    nivel_geografico = "Estado SP", 
    NOME = "Estado SP"
  ) %>%
  select(ANO, NOME, nivel_geografico, Prop_Mun_Silenciosos)

#Tabela final consolidada dos silenciosos
ind_silencioso_final <- bind_rows(ind_silencioso_mun, ind_silencioso_rs, ind_silencioso_rras, ind_silencioso_estado)

#--------------------------------------------------#

#Criando as cópias para os Totais (Sexo, FX, Raça, Ambos)
vm_sexo_tot  <- df_violencias_macro %>% mutate(CS_SEXO = "Total")
vm_fx_tot    <- df_violencias_macro %>% mutate(FX = "Total")
vm_ambos_tot <- df_violencias_macro %>% mutate(CS_SEXO = "Total", FX = "Total")

base_vm_parcial <- bind_rows(df_violencias_macro, vm_sexo_tot, vm_fx_tot, vm_ambos_tot)
vm_raca_tot     <- base_vm_parcial %>% mutate(CS_RACA = "Total")
vm_agrupar      <- bind_rows(base_vm_parcial, vm_raca_tot)

#Mun
vm_mun <- vm_agrupar %>%
  group_by(ANO, MUNICIPIO, CS_SEXO, FX, CS_RACA, TIPO_VIOLENCIA, MEIO_AGRESSAO) %>%
  summarise(Notificacoes_Macro = n(), .groups = "drop") %>%
  rename(NOME = MUNICIPIO) %>% mutate(nivel_geografico = "Município")

#RS
vm_rs <- vm_agrupar %>% filter(!is.na(NOME_RS_2025)) %>%
  group_by(ANO, NOME_RS_2025, CS_SEXO, FX, CS_RACA, TIPO_VIOLENCIA, MEIO_AGRESSAO) %>%
  summarise(Notificacoes_Macro = n(), .groups = "drop") %>%
  rename(NOME = NOME_RS_2025) %>% mutate(nivel_geografico = "RS")

#RRAS
vm_rras <- vm_agrupar %>% filter(!is.na(RRAS_2025)) %>%
  group_by(ANO, RRAS_2025, CS_SEXO, FX, CS_RACA, TIPO_VIOLENCIA, MEIO_AGRESSAO) %>%
  summarise(Notificacoes_Macro = n(), .groups = "drop") %>%
  rename(NOME = RRAS_2025) %>% mutate(nivel_geografico = "RRAS")

#Estado
vm_estado <- vm_agrupar %>%
  group_by(ANO, CS_SEXO, FX, CS_RACA, TIPO_VIOLENCIA, MEIO_AGRESSAO) %>%
  summarise(Notificacoes_Macro = n(), .groups = "drop") %>%
  mutate(NOME = "Estado SP", nivel_geografico = "Estado SP")

tabela_mestra_macro_pbi <- bind_rows(vm_mun, vm_rs, vm_rras, vm_estado)

#-------------------------------#
#---ESTRUTURA PARA O POWER BI---#
#-------------------------------#

#PADRONIZAÇÃO ANTES DO EMPILHAMENTO GERAL
tabela_mestra_estupro_pbi <- tabela_mestra_estupro_pbi %>%
  mutate(
    MEIO_AGRESSAO = "Não se aplica"
  )

tabela_mestra_lap_pbi <- tabela_mestra_lap_pbi %>%
  mutate(TIPO_VIOLENCIA = "Lesão Autoprovocada")

tabela_mestra_suicidio_pbi <- tabela_mestra_suicidio_pbi %>%
  mutate(
    TIPO_VIOLENCIA = "Suicídio"
  )

#Empilhar as três tabelas na Fato Vertical
tabela_mestra_todas <- bind_rows(
  tabela_mestra_estupro_pbi,
  tabela_mestra_lap_pbi,
  tabela_mestra_suicidio_pbi,
  tabela_mestra_macro_pbi
)

#ESQUELETO GEOGRÁFICO
geo_mun <- RRAS_RS %>% transmute(nivel_geografico = "Município", ID_LOCALIDADE = COD_6_mun, NOME = MUNICIPIO) %>% distinct()
geo_rs <- RRAS_RS %>% transmute(nivel_geografico = "RS", ID_LOCALIDADE = NOME_RS_2025, NOME = NOME_RS_2025) %>% distinct() %>% filter(!is.na(ID_LOCALIDADE))
geo_rras <- RRAS_RS %>% transmute(nivel_geografico = "RRAS", ID_LOCALIDADE = RRAS_2025, NOME = RRAS_2025) %>% distinct() %>% filter(!is.na(ID_LOCALIDADE))
geo_estado <- data.frame(nivel_geografico = "Estado SP", ID_LOCALIDADE = "35", NOME = "Estado SP")

geo_completa <- bind_rows(geo_mun, geo_rs, geo_rras, geo_estado)

#PARÂMETROS PARA A GRID DINÂMICA
anos_unicos <- 2016:ano_atual
sexos_unicos <- unique(tabela_mestra_todas$CS_SEXO) %>% na.omit()
faixas_unicas <- unique(tabela_mestra_todas$FX) %>% na.omit()
racas_unicas <- unique(tabela_mestra_todas$CS_RACA) %>% na.omit()
meios_unicos <- unique(tabela_mestra_todas$MEIO_AGRESSAO) %>% na.omit()

tipos_violencia <- c(
  "Violência Sexual", "Lesão Autoprovocada", "Suicídio",
  "Violência Física", "Violência Psicológica", "Violência Negligenciada",
  "Violência Sexual (Geral)", "Lesão Autoprovocada (Geral)", "Total de Violências"
)
tipos_macro_e_estupro <- c(
  "Violência Sexual", "Violência Física", "Violência Psicológica", 
  "Violência Negligenciada", "Violência Sexual (Geral)", "Lesão Autoprovocada (Geral)", "Total de Violências"
)

#Criando a Grid Completa
esqueleto_violencia <- expand_grid(
  geo_completa,
  ANO = anos_unicos,
  CS_SEXO = sexos_unicos,
  FX = faixas_unicas,
  CS_RACA = racas_unicas,          
  TIPO_VIOLENCIA = tipos_violencia,
  MEIO_AGRESSAO = meios_unicos
) %>%
  # MEIO DE AGRESSÃO AGORA DEVE EXISTIR TANTO PARA LAP QUANTO PARA SUICÍDIO
  filter(
    (TIPO_VIOLENCIA %in% c("Lesão Autoprovocada", "Suicídio") & MEIO_AGRESSAO != "Não se aplica") |
      (TIPO_VIOLENCIA %in% tipos_macro_e_estupro & MEIO_AGRESSAO == "Não se aplica")
  ) %>%
  #Manter apenas as faixas etárias correspondentes a cada agravo
  filter(
    (TIPO_VIOLENCIA %in% tipos_macro_e_estupro & FX %in% unique(tabela_mestra_estupro_pbi$FX)) |
      (TIPO_VIOLENCIA == "Lesão Autoprovocada" & FX %in% unique(tabela_mestra_lap_pbi$FX)) |
      (TIPO_VIOLENCIA == "Suicídio" & FX %in% unique(tabela_mestra_suicidio_pbi$FX))
  )

#JOIN FINAL
tabela_final_power_bi <- esqueleto_violencia %>%
  left_join(
    tabela_mestra_todas, 
    by = c("nivel_geografico", "NOME", "ANO", "CS_SEXO", "FX", "CS_RACA", "TIPO_VIOLENCIA", "MEIO_AGRESSAO")
  ) %>%
  #Municipio silencioso
  left_join(ind_silencioso_final, by = c("ANO", "NOME", "nivel_geografico")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  #Mantém o indicador de Silencioso para os tipos Macro, ocultando apenas em bases de fontes distintas (SIM)
  mutate(Prop_Mun_Silenciosos = if_else(TIPO_VIOLENCIA %in% tipos_macro_e_estupro, Prop_Mun_Silenciosos, NA_real_)) %>%
  filter(MEIO_AGRESSAO != "Ameaça" & MEIO_AGRESSAO != "Força Corporal/Espancamento")

#-----------------#
#---STAR SCHEMA---#
#-----------------#

#DIMENSÃO ESPAÇO
viol_d_localidade <- tabela_final_power_bi %>%
  select(nivel_geografico, ID_LOCALIDADE, NOME) %>%
  distinct() %>%
  mutate(ID_GEO = row_number())

#DIMENSÃO AGRAVO
viol_d_agravo <- tabela_final_power_bi %>%
  select(TIPO_VIOLENCIA, MEIO_AGRESSAO) %>%
  distinct() %>%
  arrange(TIPO_VIOLENCIA, MEIO_AGRESSAO) %>%
  mutate(ID_AGRAVO = row_number())

#DIMENSÃO TEMPO
viol_d_tempo <- tabela_final_power_bi %>%
  select(ANO) %>%
  distinct() %>%
  arrange(ANO) %>%
  mutate(ID_TEMPO = row_number())

#DIMENSÃO DEMOGRAFIA
viol_d_demografia <- tabela_final_power_bi %>%
  select(CS_SEXO, FX, CS_RACA) %>%
  distinct() %>%
  arrange(CS_SEXO, FX, CS_RACA) %>%
  mutate(ID_DEMOGRAFIA = row_number()) %>%
  mutate(ORDEM_FX = case_when(
    FX == "9 anos ou menos" ~ 1,
    FX == "5-9 anos" ~ 2,
    FX == "10-14 anos" ~ 3,
    FX == "10-19 anos" ~ 4,
    FX == "15-19 anos" ~ 5,
    FX == "20-39 anos" ~ 6,
    FX == "40-59 anos" ~ 7,
    FX == "60 anos ou mais" ~ 8,
    FX == "Total" ~ 9
  ))

#FATO
viol_f_indicadores <- tabela_final_power_bi %>%
  left_join(viol_d_localidade, by = c("nivel_geografico", "ID_LOCALIDADE", "NOME")) %>%
  left_join(viol_d_agravo, by = c("TIPO_VIOLENCIA", "MEIO_AGRESSAO")) %>%
  left_join(viol_d_tempo, by = "ANO") %>%
  left_join(viol_d_demografia, by = c("CS_SEXO", "FX", "CS_RACA")) %>%
  select(
    ID_GEO, 
    ID_AGRAVO, 
    ID_TEMPO, 
    ID_DEMOGRAFIA,
    
    #Estupro
    Notificacoes_Estupro,
    Oportunidade_72h,
    total_72h,
    PEP_72h,
    total_pep,
    Contracepcao_72h_MIF,
    total_contr,
    
    #LAP
    Notificacoes_LAP,
    Prop_LAP,
    Taxa_Notificacao_LAP,
    
    #Suicídio
    Obitos_Suicidio,
    Taxa_Mortalidade_Suicidio,
    
    #Macro
    Notificacoes_Macro,
    Prop_Mun_Silenciosos,
    
    Populacao
  )

#Exportação
write.csv2(tabela_final_power_bi, "C:\\R\\DCNT\\Paineis\\Violencia\\violencia.csv", row.names = FALSE)
write.csv2(viol_d_localidade, "C:\\R\\DCNT\\Paineis\\Violencia\\viol_d_localidade.csv", row.names = FALSE)
write.csv2(viol_d_agravo, "C:\\R\\DCNT\\Paineis\\Violencia\\viol_d_agravo.csv", row.names = FALSE)
write.csv2(viol_d_tempo, "C:\\R\\DCNT\\Paineis\\Violencia\\viol_d_tempo.csv", row.names = FALSE)
write.csv2(viol_d_demografia, "C:\\R\\DCNT\\Paineis\\Violencia\\viol_d_demografia.csv", row.names = FALSE)
write.csv2(viol_f_indicadores, "C:\\R\\DCNT\\Paineis\\Violencia\\viol_f_indicadores.csv", row.names = FALSE, na ="")

#-------------FIM------------#
#-------------FIM------------#
#-------------FIM------------#
#-------------FIM------------#
#-------------FIM------------#
#-------------FIM------------#
#-------------FIM------------#

#---------------------------#
#---PLANILHA DE VALIDAÇÃO---#
#---------------------------#

# 1.1 Preparar os dados gerais de Violência Sexual (Buscando no Total)
validacao_estupro_geral <- tabela_final_power_bi %>%
  filter(TIPO_VIOLENCIA == "Violência Sexual") %>%
  filter(CS_SEXO == "Total", FX == "Total", CS_RACA == "Total") %>%
  select(
    nivel_geografico, NOME, TIPO_VIOLENCIA, MEIO_AGRESSAO, ANO,
    Notificacoes_Estupro, Oportunidade_72h, PEP_72h, Prop_Mun_Silenciosos
  ) %>%
  pivot_longer(
    cols = c(Notificacoes_Estupro, Oportunidade_72h, PEP_72h, Prop_Mun_Silenciosos),
    names_to = "INDICADOR",
    values_to = "VALOR"
  )

# 1.2 Preparar APENAS a Contracepção (Buscando no Feminino)
validacao_estupro_contr <- tabela_final_power_bi %>%
  filter(TIPO_VIOLENCIA == "Violência Sexual") %>%
  filter(CS_SEXO == "Feminino", FX == "Total", CS_RACA == "Total") %>%
  select(
    nivel_geografico, NOME, TIPO_VIOLENCIA, MEIO_AGRESSAO, ANO,
    Contracepcao_72h_MIF
  ) %>%
  pivot_longer(
    cols = c(Contracepcao_72h_MIF),
    names_to = "INDICADOR",
    values_to = "VALOR"
  )

# 1.3 Empilhar toda a Violência Sexual
validacao_estupro <- bind_rows(validacao_estupro_geral, validacao_estupro_contr)

# 2. Preparar os dados de LAP (casos + proporções + taxas)
validacao_lap <- tabela_final_power_bi %>%
  filter(TIPO_VIOLENCIA == "Lesão Autoprovocada") %>%
  filter(CS_SEXO == "Total", FX == "Total", CS_RACA == "Total") %>%
  select(
    nivel_geografico, NOME, TIPO_VIOLENCIA, MEIO_AGRESSAO, ANO,
    Notificacoes_LAP, Prop_LAP, Taxa_Notificacao_LAP
  ) %>%
  pivot_longer(
    cols = c(Notificacoes_LAP, Prop_LAP, Taxa_Notificacao_LAP),
    names_to = "INDICADOR",
    values_to = "VALOR"
  )

# 3. Preparar os dados de Suicídio (casos + taxas)
validacao_suicidio <- tabela_final_power_bi %>%
  filter(TIPO_VIOLENCIA == "Suicídio") %>%
  filter(CS_SEXO == "Total", FX == "Total", CS_RACA == "Total") %>%
  select(
    nivel_geografico, NOME, TIPO_VIOLENCIA, MEIO_AGRESSAO, ANO,
    Obitos_Suicidio, Taxa_Mortalidade_Suicidio
  ) %>%
  pivot_longer(
    cols = c(Obitos_Suicidio, Taxa_Mortalidade_Suicidio),
    names_to = "INDICADOR",
    values_to = "VALOR"
  )

# 4. Empilhar as três tabelas e pivotar os ANOS para as colunas
validacao_larga_completa <- bind_rows(validacao_estupro, validacao_lap, validacao_suicidio) %>%
  pivot_wider(
    names_from = ANO,
    values_from = VALOR,
    names_prefix = "Ano_"
  ) %>%
  # Organizar o layout final
  arrange(
    factor(nivel_geografico, levels = c("Estado SP", "RRAS", "RS", "Município")),
    NOME,
    TIPO_VIOLENCIA,
    MEIO_AGRESSAO,
    INDICADOR
  )

# Exportando a planilha definitiva para Excel
write.xlsx(validacao_larga_completa, "C:\\R\\DCNT\\Paineis\\Violencia\\Validacao_Completa_Violencia.xlsx")
