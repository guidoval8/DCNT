#--------------SIH - DCNT------------#
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

#-------------------#
#------EXTRAÇÃO-----#
#-------------------#

ano_atual <-  as.numeric(format(Sys.Date(), "%Y"))
anos <- 2015:ano_atual
meses <- str_pad(1:12, width = 2, pad = "0")

lista_dcnt <- list()

cols_cid <- c("DIAG_PRINC", "DIAG_SECUN", "DIAGSEC1", "DIAGSEC2", "DIAGSEC3",
              "DIAGSEC4", "DIAGSEC5", "DIAGSEC6", "DIAGSEC7", "DIAGSEC8", "DIAGSEC9")

regex_circulatorio <- "^I[0-9]{2}"
regex_digestivo    <- "^(C1[5-9]|C2[0-5]|C260|C268|C269|C451|C48|C772|C78[4-8])"
regex_mama         <- "^C50"
regex_colo         <- "^C53"
regex_diabetes     <- "^E1[0-4]"
regex_respiratoria <- "^(J3[0-57-9]|J[4-8][0-9]|J9[0-8])"

regex_dcnt_todas <- paste(regex_circulatorio, regex_digestivo, regex_mama, 
                          regex_colo, regex_diabetes, regex_respiratoria, sep = "|")

for (ano in anos) {
  ano_2_dig <- substr(as.character(ano), 3, 4)
  
  for (mes in meses) {
    nome_arquivo <- paste0("RDSP", ano_2_dig, mes, ".dbc")
    url_ftp <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/", nome_arquivo)
    
    cat("Processando:", nome_arquivo, "\n")
    
    temp_file <- tempfile(fileext = ".dbc")
    
    tryCatch({
      download.file(url_ftp, temp_file, mode = "wb", quiet = TRUE)
      
      bruto <- read.dbc::read.dbc(temp_file, as.is = TRUE) 
      
      processado <- process_sih(bruto)
      
      filtrado <- processado %>%
        select("MUNIC_RES" ,"N_AIH", "SEXO" , "DT_INTER", "DT_SAIDA","QT_DIARIAS", "VAL_TOT", "DIAG_PRINC", "DIAG_SECUN", "COD_IDADE", "IDADE", "MORTE",
               "DIAGSEC1","DIAGSEC2","DIAGSEC3","DIAGSEC4","DIAGSEC5","DIAGSEC6","DIAGSEC7","DIAGSEC8" ,"DIAGSEC9") %>%
        mutate(IDADE = as.numeric(IDADE)) %>%
        filter(str_starts(as.character(MUNIC_RES), "35")) %>%
        filter(
          if_any(
            any_of(cols_cid),
            ~grepl(regex_dcnt_todas, .x)
          )
        )
      
      if (nrow(filtrado) > 0) {
        lista_dcnt[[nome_arquivo]] <- filtrado
      }
      
    }, error = function(e) {
      cat(" -> Aviso: Falha ao processar", nome_arquivo, "- Pode não existir ainda. Erro:", e$message, "\n")
    })
    
    unlink(temp_file)
    suppressWarnings(rm(bruto, processado, filtrado))
    gc()
  }
}

df_dcnt_final <- bind_rows(lista_dcnt)

#write.csv2(df_dcnt_final, "C:\\R\\DCNT\\SIH\\sih_dcnt.csv", row.names = FALSE)

#--------------------------#

#----POPULAÇÃO----#
options(download.file.method = "libcurl")
ano_atual <-  as.numeric(format(Sys.Date(), "%Y"))
anos_baixar <- 2015:ano_atual
lista_pop_bruta <- list()

for (ano in anos_baixar) {
  
  # Pega os dois últimos dígitos do ano (ex: 2022 -> "22")
  sufixo_ano <- substr(as.character(ano), 3, 4)
  
  # Cria os nomes dos arquivos dinamicamente
  url_pop_zip  <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/POPSBR", sufixo_ano, ".zip")
  nome_pop_zip <- paste0("POPSBR", sufixo_ano, ".zip")
  nome_dbf     <- paste0("POP", sufixo_ano, ".dbf") # O nome do arquivo DENTRO do zip
  
  tryCatch({
    # Baixa o arquivo
    print(paste("Baixando:", nome_pop_zip))
    download.file(url = url_pop_zip, destfile = nome_pop_zip, mode = "wb", quiet = TRUE)
    
    # Descompacta
    unzip(nome_pop_zip)
    
    # Lê o arquivo .dbf
    print(paste("Lendo:", nome_dbf))
    df_ano_atual <- read.dbf(nome_dbf)
    
    names(df_ano_atual) <- toupper(names(df_ano_atual))
    
    # Adiciona o dataframe lido à nossa lista
    lista_pop_bruta[[as.character(ano)]] <- df_ano_atual
    
    print(paste("Ano", ano, "processado com sucesso."))
    
  }, error = function(e) {
    # Se der erro (ex: arquivo não existe no FTP), ele avisa e continua
    print(paste("ERRO ao processar o ano", ano, ":", e$message))
  })
}

pop_bruta_total <- bind_rows(lista_pop_bruta)

#Tirando SEXO
pop <- pop_bruta_total %>%
  group_by(COD_MUN, ANO) %>%
  summarise(POP_TOTAL = sum(POP, na.rm = TRUE), .groups = "drop") %>%
  mutate(COD_MUN = str_sub(COD_MUN, start = 1L, end = 6L)) %>%
  mutate(ANO = as.double(as.character(ANO))) %>%
  rename(MUNIC_RES = COD_MUN)

#-----------------------------------------------#
#-----------------SIH - DCNT--------------------#
#-----------------------------------------------#

sih_dcnt <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(GRUPO_DCNT = case_when(
    CID3 >= 'I00' & CID3 <= 'I99' ~ 'Circulatorio',
    CID3 >= 'C15' & CID3 <= 'C25' | 
    DIAG_PRINC %in% c("C260", "C268", "C269", "C451", "C772", 
                      "C784", "C785", "C786", "C787", "C788") ~ 'Cancer do Aparelho Digestivo',
    CID3 == 'C50' ~ 'Cancer de Mama',
    CID3 == 'C53' ~ 'Cancer de Colo de Utero', 
    CID3 >= 'E10' & CID3 <= 'E14' ~ 'Diabetes',
    CID3 >= 'J30' & CID3 <= 'J98' & CID3 != 'J36' ~ 'Respiratoria',
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(GRUPO_DCNT))

#Todos os cancers
sih_cancer_todos <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      CID3 >= 'C00' & CID3 <= 'C97' ~ 'Cancer'
    ) 
  ) %>%
  filter(GRUPO_DCNT == 'Cancer')

#Classificação que agrupa as DCNT
sih_todas <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      (CID3 >= 'I00' & CID3 <= 'I99') |
        (CID3 >= 'C00' & CID3 <= 'C97') |
        (CID3 >= 'E10' & CID3 <= 'E14') |
        ((CID3 >= 'J30' & CID3 <= 'J98') & (CID3 != 'J36')) ~ "Todas_DCNT",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(GRUPO_DCNT))

#CSAP
sih_hipertensao <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      CID3 >= 'I10' & CID3 <= 'I14' ~ 'Hipertensao'
    )
  ) %>%
  filter(GRUPO_DCNT == 'Hipertensao')

sih_csap_has_dm <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      (CID3 >= 'I10' & CID3 <= 'I14') | (CID3 >= 'E10' & CID3 <= 'E14') ~ 'CSAP_HAS_DM'
    )
  ) %>%
  filter(GRUPO_DCNT == 'CSAP_HAS_DM')

# DIC - Doença Isquêmica do Coração
sih_dic <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      CID3 >= 'I20' & CID3 <= 'I25' ~ 'DIC'
    )
  ) %>%
  filter(GRUPO_DCNT == 'DIC')

# AVC
sih_avc <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      CID3 >= 'I60' & CID3 <= 'I69' ~ 'AVC'
    )
  ) %>%
  filter(GRUPO_DCNT == 'AVC')

#EMPILHAR AS CLASSIFICAÇÕES
sih <- rbind(sih_dcnt, sih_cancer_todos, sih_todas, sih_hipertensao, sih_csap_has_dm, sih_dic, sih_avc)

#limpando a RAM
sih_dcnt <- NULL
sih_cancer_todos <- NULL
sih_todas <- NULL
sih_hipertensao <- NULL
sih_csap_has_dm <- NULL
sih_dic <- NULL
sih_avc <- NULL

gc()

sih <- sih %>%
  mutate(ANO = year(DT_INTER)) %>%
  mutate(MORTE = case_when(
    MORTE == "Sim" ~ 1,
    MORTE == "Não" ~ 0 
  )) %>%
  filter(ANO >= 2015)

#----RRAS----#
RRAS_Municipios <- import("https://github.com/guidoval8/DCNT/blob/main/dados/RRAS_Municipios.xlsx?raw=true")
RRAS_Municipios <- RRAS_Municipios %>%
  mutate(COD_6_mun = as.character(COD_6_mun)) %>%
  mutate(COD_6_mun = str_sub(COD_6_mun, start = 1L, end = 6L)) %>%
  select(COD_6_mun, MUNICIPIO, RRAS_2025, COD_RS_2025, NOME_RS_2025) %>%
  distinct(COD_6_mun, .keep_all = TRUE) %>%
  rename(MUNIC_RES = COD_6_mun)

#--------------------------------------#
#-------TAXA BRUTA DE INTERNAÇÃO-------#
#--------------------------------------#

sih_geo <- sih %>%
  left_join(RRAS_Municipios, by = "MUNIC_RES")

pop_geo <- left_join(pop, RRAS_Municipios, by="MUNIC_RES") %>%
  filter(!is.na(MUNICIPIO))

#Município
pop_mun <- pop_geo %>%
  group_by(ANO, MUNICIPIO) %>%
  summarise(pop = sum(POP_TOTAL, na.rm = TRUE), .groups = "drop")

taxa_municipio <- sih_geo %>%
  group_by(ANO, MUNICIPIO, GRUPO_DCNT) %>%
  summarise(n_internacoes = n(), .groups = "drop") %>%
  left_join(pop_mun, by = c("ANO", "MUNICIPIO")) %>%
  mutate(taxa_bruta_internacao = n_internacoes / pop * 10000) %>%
  mutate(nivel_geografico = "Município") %>% rename(NOME = MUNICIPIO)

#RS
pop_rs <- pop_geo %>%
  group_by(ANO, NOME_RS_2025) %>%
  summarise(pop = sum(POP_TOTAL, na.rm = TRUE), .groups = "drop")

taxa_rs <- sih_geo %>%
  group_by(ANO, NOME_RS_2025, GRUPO_DCNT) %>%
  summarise(n_internacoes = n(), .groups = "drop") %>%
  left_join(pop_rs, by = c("ANO", "NOME_RS_2025")) %>%
  mutate(taxa_bruta_internacao = n_internacoes / pop * 10000) %>%
  mutate(nivel_geografico = "RS") %>% rename(NOME = NOME_RS_2025)

#RRAS
pop_rras <- pop_geo %>%
  group_by(ANO, RRAS_2025) %>%
  summarise(pop = sum(POP_TOTAL, na.rm = TRUE), .groups = "drop")

taxa_rras <- sih_geo %>%
  group_by(ANO, RRAS_2025, GRUPO_DCNT) %>%
  summarise(n_internacoes = n(), .groups = "drop") %>%
  left_join(pop_rras, by = c("ANO", "RRAS_2025")) %>%
  mutate(taxa_bruta_internacao = n_internacoes / pop * 10000) %>%
  mutate(nivel_geografico = "RRAS") %>% rename(NOME = RRAS_2025)

#Estado
pop_estado <- pop_geo %>%
  group_by(ANO) %>%
  summarise(pop = sum(POP_TOTAL, na.rm = TRUE), .groups = "drop")

taxa_estado <- sih_geo %>%
  group_by(ANO, GRUPO_DCNT) %>%
  summarise(n_internacoes = n(), .groups = "drop") %>%
  left_join(pop_estado, by = "ANO") %>%
  mutate(taxa_bruta_internacao = n_internacoes / pop * 10000) %>%
  mutate(nivel_geografico = "Estado SP") %>% mutate(NOME = "Estado SP")

tabela_mestra_taxa <- rbind(taxa_municipio,taxa_rs,taxa_rras,taxa_estado)

#--------------------------------------#
#-------VALOR MÉDIO E TOTAL------------#
#--------------------------------------#

sih_geo <- sih_geo %>%
  mutate(VAL_TOT = as.numeric(VAL_TOT))

#Município
valor_municipio <- sih_geo %>%
  group_by(ANO, MUNICIPIO, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(valor_medio = valor_total / n_internacoes) %>%
  mutate(nivel_geografico = "Município") %>% rename(NOME = MUNICIPIO)

#RS
valor_rs <- sih_geo %>%
  group_by(ANO, NOME_RS_2025, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(valor_medio = valor_total / n_internacoes) %>%
  mutate(nivel_geografico = "RS") %>% rename(NOME = NOME_RS_2025)

#RRAS
valor_rras <- sih_geo %>%
  group_by(ANO, RRAS_2025, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(valor_medio = valor_total / n_internacoes) %>%
  mutate(nivel_geografico = "RRAS") %>% rename(NOME = RRAS_2025)

#Estado
valor_estado <- sih_geo %>%
  group_by(ANO, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(valor_medio = valor_total / n_internacoes) %>%
  mutate(nivel_geografico = "Estado SP") %>% mutate(NOME = "Estado SP")

tabela_mestra_valor <- rbind(valor_municipio, valor_rs, valor_rras, valor_estado)

#-------------------------------------------------#
#-------TAXA DE MORTALIDADE HOSPITALAR------------#
#-------------------------------------------------#

#Numerador: Número de óbitos que ocorreram em internações por DIC (CID-10 I20 a I25) ou por AVC (CID-10 I60 a I69) no período.
#Denominador: Número de saídas do hospital (por alta, evasão, desistência do tratamento, transferência externa ou óbito) por DIC (CID-10 I20 a I25) ou por AVC (CID-10 I60 a I69), no mesmo período. O resultado é multiplicado por 100.

# Município
mortalidade_municipio <- sih_geo %>%
  group_by(ANO, MUNICIPIO, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    obitos = sum(MORTE, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(taxa_mortalidade = (obitos / n_internacoes) * 100) %>%
  mutate(nivel_geografico = "Município") %>% rename(NOME = MUNICIPIO)

# RS
mortalidade_rs <- sih_geo %>%
  group_by(ANO, NOME_RS_2025, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    obitos = sum(MORTE, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(taxa_mortalidade = (obitos / n_internacoes) * 100) %>%
  mutate(nivel_geografico = "RS") %>% rename(NOME = NOME_RS_2025)

# RRAS
mortalidade_rras <- sih_geo %>%
  group_by(ANO, RRAS_2025, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    obitos = sum(MORTE, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(taxa_mortalidade = (obitos / n_internacoes) * 100) %>%
  mutate(nivel_geografico = "RRAS") %>% rename(NOME = RRAS_2025)

# Estado
mortalidade_estado <- sih_geo %>%
  group_by(ANO, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    obitos = sum(MORTE, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(taxa_mortalidade = (obitos / n_internacoes) * 100) %>%
  mutate(nivel_geografico = "Estado SP") %>% mutate(NOME = "Estado SP")

tabela_mestra_mortalidade <- rbind(mortalidade_municipio, mortalidade_rs, mortalidade_rras, mortalidade_estado)

#FINAL
chaves_join <- c("ANO", "NOME", "nivel_geografico", "GRUPO_DCNT")

mestra_sih_dcnt <- tabela_mestra_taxa %>%
  left_join(tabela_mestra_valor %>% select(-n_internacoes), by = chaves_join) %>%
  
  left_join(tabela_mestra_mortalidade %>% select(-n_internacoes), by = chaves_join)
