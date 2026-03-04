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

#-------------------#
#------EXTRAÇÃO-----#
#-------------------#

ano_atual <-  as.numeric(format(Sys.Date(), "%Y"))
anos <- 2015:ano_atual
lista_quedas <- list()

cols_cid <- c("DIAG_PRINC", "DIAG_SECUN", "DIAGSEC1", "DIAGSEC2", "DIAGSEC3",
              "DIAGSEC4", "DIAGSEC5", "DIAGSEC6", "DIAGSEC7", "DIAGSEC8", "DIAGSEC9")

#Loop de extração ano a ano
for (ano in anos) {
  cat("Extraindo e processando SIH - Ano:", ano, "\n")
  
  df_ano <- tryCatch({
    bruto <- fetch_datasus(
      year_start = ano, month_start = 1,
      year_end = ano, month_end = 12,
      uf = "SP", information_system = "SIH-RD"
    )
    
    processado <- process_sih(bruto)
    
    filtrado <- processado %>%
      select("MUNIC_RES" ,"N_AIH", "SEXO" , "DT_INTER", "VAL_TOT", "DIAG_PRINC", "DIAG_SECUN", "COD_IDADE", "IDADE", "MORTE",
             "DIAGSEC1","DIAGSEC2","DIAGSEC3","DIAGSEC4","DIAGSEC5","DIAGSEC6","DIAGSEC7","DIAGSEC8" ,"DIAGSEC9") %>%
      mutate(IDADE = as.numeric (IDADE)) %>%
      filter(IDADE >= 60) %>%
      filter(str_starts(as.character(MUNIC_RES), "35")) %>%
      filter(
        if_any(
          any_of(cols_cid),
          ~grepl("^W(0[0-9]|1[0-9])", .x)
        )
      )
    
    filtrado
    
  }, error = function(e) {
    cat("Aviso no ano:", ano, "- Pode não haver dados disponíveis ainda. Mensagem:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(df_ano) && nrow(df_ano) > 0) {
    lista_quedas[[as.character(ano)]] <- df_ano
  
  }
}

df_quedas_final <- bind_rows(lista_quedas)

#write.csv2(df_quedas_final, "C:\\R\\DCNT\\SIH\\sih_quedas.csv", na="", row.names = FALSE)

#-----------------#

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
df_ano_atual <- NULL

#-----------------------------------------------#
#-----------------SIH - QUEDAS------------------#
#-----------------------------------------------#

#Taxa de internações de idosos por quedas acidentais (N de internaçoes por mil habitantes)
#Valor total e médio das internações de idosos por quedas acidentais
#Taxa de mortalidade hospitalar por quedas acidentais entre idosos

sih_quedas <- df_quedas_final %>%
  mutate(ANO = year(DT_INTER)) %>%
  mutate(MORTE = case_when(
    MORTE == "Sim" ~ 1,
    MORTE == "Não" ~ 0 
  )) %>%
  filter(ANO >= 2015)


quedas <- sih_quedas %>%
  group_by(MUNIC_RES, ANO) %>%
  mutate(VAL_TOT = as.numeric(VAL_TOT)) %>%
  summarise(
    n = n(),
    OBITO = sum(MORTE, na.rm = TRUE),
    VALOR_TOT = sum(VAL_TOT, na.rm = TRUE),
    VALOR_MED = mean(VAL_TOT, na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(TAXA_MORT_H = (OBITO / n) * 1000,
         Nivel = "Municipio") %>%
  rename('COD_MUN' = 'MUNIC_RES') %>%
  select("COD_MUN", "ANO","n", "OBITO", "VALOR_TOT", "VALOR_MED","TAXA_MORT_H", "Nivel")

quedas$COD_MUN <- as.character(quedas$COD_MUN)

DENOMINADOR_SIH_QUEDAS <- pop_bruta_total %>%
  filter(str_starts(COD_MUN, "35")) %>%
  mutate(
    COD_MUN = str_sub(as.character(COD_MUN), 1, 6),
    ANO = as.numeric(as.character(ANO)),
    IDADE_NUM = as.numeric(IDADE), 
    SEXO = case_when(
      SEXO == "1" ~ "Masculino",
      SEXO == "2" ~ "Feminino",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    FAIXA_ETARIA = case_when(
      IDADE_NUM >= 60 ~ "60 anos e mais",
      TRUE ~ NA_character_ 
    )
  ) %>%
  filter(!is.na(FAIXA_ETARIA)) %>% 
  group_by(COD_MUN, ANO, SEXO, FAIXA_ETARIA) %>%
  summarise(populacao = sum(POP, na.rm = TRUE), .groups = "drop")

DENOMINADOR_SIH_QUEDAS <- DENOMINADOR_SIH_QUEDAS %>%
  group_by(COD_MUN, ANO) %>%
  summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")

quedas <- left_join(quedas, DENOMINADOR_SIH_QUEDAS, by = c("COD_MUN", "ANO"))

quedas_estado <- quedas %>%
  group_by(ANO) %>%
  summarise(n = sum(n, na.rm = TRUE),
            OBITO = sum(OBITO, na.rm = TRUE),
            VALOR_TOT = sum(VALOR_TOT, na.rm = TRUE),
            VALOR_MED = mean(VALOR_MED, na.rm = TRUE),
            populacao = sum(populacao, na.rm = TRUE), .groups = "drop") %>%
  mutate(TAXA_MORT_H = (OBITO / n) * 1000,
         Nivel = "Estado SP")

#----RRAS----#
RRAS_Municipios <- import("https://github.com/guidoval8/DCNT/blob/main/dados/RRAS_Municipios.xlsx?raw=true")
RRAS_Municipios <- RRAS_Municipios %>%
  mutate(COD_6_mun = as.character(COD_6_mun)) %>%
  mutate(COD_6_mun = str_sub(COD_6_mun, start = 1L, end = 6L)) %>%
  select(COD_6_mun, MUNICIPIO, RRAS_2025, COD_RS_2025, NOME_RS_2025) %>%
  distinct(COD_6_mun, .keep_all = TRUE)

quedas <- left_join(quedas, RRAS_Municipios, by = c("COD_MUN" = "COD_6_mun"))

quedas_rras <- quedas %>%
  group_by(RRAS_2025, ANO) %>%
  summarise(n = sum(n, na.rm = TRUE),
            OBITO = sum(OBITO, na.rm = TRUE),
            VALOR_TOT = sum(VALOR_TOT, na.rm = TRUE),
            VALOR_MED = mean(VALOR_MED, na.rm = TRUE),
            populacao = sum(populacao, na.rm = TRUE), .groups = "drop") %>%
    mutate(TAXA_MORT_H = (OBITO / n) * 1000,
           Nivel = "RRAS")

quedas <- bind_rows(quedas, quedas_estado, quedas_rras)

quedas <- quedas %>%
   mutate(Categoria = 'Quedas',
          TAXA_INTERNACAO = n / populacao * 100000) %>%
   filter(ANO != 2014)

write.csv2(quedas, r"(C:\R\DCNT\SIH\SIH_quedas.csv)", row.names = FALSE,na="")


