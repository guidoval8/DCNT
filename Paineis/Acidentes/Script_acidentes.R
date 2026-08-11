#--------------ACIDENTES------------#
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

ano_atual <-  as.numeric(format(Sys.Date(), "%Y"))

#-----------------------#
#---TRÂNSITO e QUEDAS---#
#-----------------------#

#----EXTRAÇÃO----#
SIM <- fetch_datasus(year_start = 2015, year_end = ano_atual, uf = "SP", information_system = "SIM-DO")
SIM <- process_sim(SIM)
#----------------#
SIM <- read_parquet('C:\\R\\DCNT\\Paineis\\Acidentes\\sim_acidentes.parquet')
#----PADRONIZAÇÃO----#
#CLASSIFICAR ANO
SIM <- SIM %>%
  mutate(ANOOBITO = year(DTOBITO))

#PADRONIZAR CAUSA BÁSICA
SIM_CID <- SIM %>%
  mutate(
    CAUSABAS = str_trim(toupper(as.character(CAUSABAS))),
    CID3 = str_sub(CAUSABAS, 1,3)
  )

SIM_BASE <- SIM_CID %>%
  mutate(IDADEanos = as.numeric(IDADEanos)) %>%
  filter(!is.na(SEXO)) %>%
  mutate(
    FX = case_when(
      IDADEanos >= 0 & IDADEanos <= 9 ~ '9 anos ou menos',
      IDADEanos >= 10 & IDADEanos <= 19 ~ '10-19 anos',
      IDADEanos >= 20 & IDADEanos < 30 ~ '20-29 anos',
      IDADEanos >= 30 & IDADEanos < 40 ~ '30-39 anos',
      IDADEanos >= 40 & IDADEanos < 60 ~ '40-59 anos',
      IDADEanos >= 60 & IDADEanos < 80 ~ '60-79 anos',
      IDADEanos >= 80 ~ '80 anos ou mais',
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FX))

#TRÂNSTIO
SIM_transito_grupos <- SIM_BASE %>%
  filter(CID3 >= 'V00' & CID3 <= 'V89') %>%
  mutate(
    AGRAVO = "Acidente de Trânsito",
    TIPO_VITIMA = case_when(
      CID3 >= 'V01' & CID3 <= 'V09' ~ "Pedestres",
      CID3 >= 'V10' & CID3 <= 'V19' ~ "Ciclistas",
      CID3 >= 'V20' & CID3 <= 'V39' ~ "Motociclistas e veículos de 3 rodas",
      CID3 >= 'V40' & CID3 <= 'V79' ~ "Ocupantes de veículos de 4 rodas",
      CID3 >= 'V80' & CID3 <= 'V89' ~ "Outros e não especificados",
      TRUE ~ NA_character_
    )
  )

SIM_transito_todos <- SIM_transito_grupos %>% mutate(TIPO_VITIMA = "Todos os Tipos")
SIM_transito_final <- bind_rows(SIM_transito_grupos, SIM_transito_todos)
 
#QUEDAS
SIM_quedas <- SIM_BASE %>%
  filter(CID3 >= 'W00' & CID3 <= 'W19') %>%
  filter(FX == '60-79 anos' | FX == '80 anos ou mais') %>% 
  mutate(
    AGRAVO = "Quedas",
    TIPO_VITIMA = "Todos os Tipos"
  )

#EMPILHAR
SIM_CAUSAS_EXTERNAS <- bind_rows(SIM_transito_final, SIM_quedas)

#---#
SIM <- NULL
SIM_CID <- NULL
SIM_BASE <- NULL
gc()
#---#

#AGRUPAR ESTRATOS
#----RRAS----#
RRAS_Municipios <- import("https://github.com/guidoval8/DCNT/blob/main/dados/RRAS_Municipios.xlsx?raw=true")

#Padronização
RRAS_RS <- RRAS_Municipios %>%
  mutate(COD_6_mun = as.character(COD_6_mun)) %>%
  mutate(COD_6_mun = str_sub(COD_6_mun, start = 1L, end = 6L)) %>%
  select(COD_6_mun, MUNICIPIO, RRAS_2025, COD_RS_2025, NOME_RS_2025) %>%
  distinct(COD_6_mun, .keep_all = TRUE)

#Função para preparar e empilhar os totais
preparar_agrupamentos <- function(df_base, col_mun) {
  df_geo <- df_base %>%
    mutate(COD_MUN_TEMP = str_sub(as.character(.data[[col_mun]]), start = 1L, end = 6L)) %>%
    left_join(RRAS_RS, by = c("COD_MUN_TEMP" = "COD_6_mun"))
  
  df_sexo_total  <- df_geo %>% mutate(SEXO = "Total")
  df_fx_total <- df_geo %>% filter(AGRAVO != "Quedas") %>% mutate(FX = "Total")
  df_ambos_total <- df_geo %>% filter(AGRAVO != "Quedas") %>% mutate(SEXO = "Total", FX = "Total")
  
  bind_rows(df_geo, df_sexo_total, df_fx_total, df_ambos_total)
}

#Aplica para CODMUNRES e Ocorrência CODMUNOCOR
agrupado_res <- preparar_agrupamentos(SIM_CAUSAS_EXTERNAS, "CODMUNRES")
agrupado_ocor <- preparar_agrupamentos(SIM_CAUSAS_EXTERNAS, "CODMUNOCOR")

#Função para agregar os óbitos nos diferentes níveis geográficos
agregar_niveis <- function(df_agrupado, nome_coluna_obitos) {
  mun <- df_agrupado %>%
    group_by(ANOOBITO, MUNICIPIO, SEXO, FX, AGRAVO, TIPO_VITIMA) %>%
    summarise(!!sym(nome_coluna_obitos) := n(), .groups = 'drop') %>%
    rename(NOME = MUNICIPIO) %>% mutate(nivel_geografico = "Município")
  
  rs <- df_agrupado %>%
    filter(!is.na(NOME_RS_2025)) %>%
    group_by(ANOOBITO, NOME_RS_2025, SEXO, FX, AGRAVO, TIPO_VITIMA) %>%
    summarise(!!sym(nome_coluna_obitos) := n(), .groups = 'drop') %>%
    rename(NOME = NOME_RS_2025) %>% mutate(nivel_geografico = "RS")
  
  rras <- df_agrupado %>%
    filter(!is.na(RRAS_2025)) %>%
    group_by(ANOOBITO, RRAS_2025, SEXO, FX, AGRAVO, TIPO_VITIMA) %>%
    summarise(!!sym(nome_coluna_obitos) := n(), .groups = 'drop') %>%
    rename(NOME = RRAS_2025) %>% mutate(nivel_geografico = "RRAS")
  
  estado <- df_agrupado %>%
    group_by(ANOOBITO, SEXO, FX, AGRAVO, TIPO_VITIMA) %>%
    summarise(!!sym(nome_coluna_obitos) := n(), .groups = 'drop') %>%
    mutate(NOME = "Estado SP", nivel_geografico = "Estado SP")
  
  bind_rows(mun, rs, rras, estado)
}

#Executa a agregação para Residência e Ocorrência
acidentes_res <- agregar_niveis(agrupado_res, "obitos_residencia")
acidentes_ocor <- agregar_niveis(agrupado_ocor, "obitos_ocorrencia")

#Junta as duas bases
acidentes_long <- full_join(
  acidentes_res,
  acidentes_ocor,
  by = c("ANOOBITO", "NOME", "nivel_geografico", "SEXO", "FX", "AGRAVO", "TIPO_VITIMA")
) %>%
  mutate(
    obitos_residencia = replace_na(obitos_residencia, 0),
    obitos_ocorrencia = replace_na(obitos_ocorrencia, 0)
  )

#------------------------------#

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

pop_bruta_total <- read_parquet('C:\\R\\DCNT\\Paineis\\Acidentes\\pop_bruta_total_acidentes.parquet')

#CLASSIFICAÇÃO DE FAIXA ETÁRIA
DENOMINADOR <- pop_bruta_total %>%
  mutate(IDADE = as.numeric(IDADE)) %>%
  mutate(
    FX = case_when(
      IDADE >= 0 & IDADE <= 9 ~ '9 anos ou menos',
      IDADE >= 10 & IDADE <= 19 ~ '10-19 anos',
      IDADE >= 20 & IDADE < 30 ~ '20-29 anos',
      IDADE >= 30 & IDADE < 40 ~ '30-39 anos',
      IDADE >= 40 & IDADE < 60 ~ '40-59 anos',
      IDADE >= 60 & IDADE < 80 ~ '60-79 anos',
      IDADE >= 80 ~ '80 anos ou mais',
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FX))

#Criando o estrato por sexo e ambos
pop_base <- DENOMINADOR

pop_sexo_total  <- pop_base %>% mutate(SEXO = "Total")
pop_fx_total    <- pop_base %>% mutate(FX = "Total")
pop_ambos_total <- pop_base %>% mutate(SEXO = "Total", FX = "Total")

DENOMINADOR <- bind_rows(pop_base, pop_sexo_total, pop_fx_total, pop_ambos_total) %>%
  group_by(COD_MUN, ANO, SEXO, FX) %>%
  summarise(
    populacao = sum(POP, na.rm = TRUE),
    .groups = "drop"
  )
#--------------------------------# 
  
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
    ANOOBITO = ANO
  ) %>%
  #Padronizar CODMUNRES
  mutate(
    CODMUNRES = str_sub(CODMUNRES, start = 1L, end = 6L)
  )

DENOMINADOR <- left_join(DENOMINADOR, RRAS_RS, by =c("CODMUNRES" = "COD_6_mun"))

#pop
pop_mun <- DENOMINADOR %>%
  group_by(ANOOBITO, MUNICIPIO, SEXO, FX) %>%
  summarise(populacao = sum(populacao), .groups = "drop") %>%
  rename(NOME = MUNICIPIO) %>% mutate(nivel_geografico = "Município")

pop_rs <- DENOMINADOR %>%
  group_by(ANOOBITO, NOME_RS_2025, SEXO, FX) %>%
  summarise(populacao = sum(populacao), .groups = "drop") %>%
  rename(NOME = NOME_RS_2025) %>% mutate(nivel_geografico = "RS")

pop_rras <- DENOMINADOR %>%
  group_by(ANOOBITO, RRAS_2025, SEXO, FX) %>%
  summarise(populacao = sum(populacao), .groups = "drop") %>%
  rename(NOME = RRAS_2025) %>% mutate(nivel_geografico = "RRAS")

pop_estado <- DENOMINADOR %>%
  group_by(ANOOBITO, SEXO, FX) %>%
  summarise(populacao = sum(populacao), .groups = "drop") %>%
  mutate(NOME = "Estado SP") %>% mutate(nivel_geografico = "Estado SP")

pop <- bind_rows(pop_mun, pop_rs, pop_rras, pop_estado)

#---Tabela mestra acidentes longa---#

tipos_vitima <- c(
  "Pedestres", 
  "Ciclistas", 
  "Motociclistas e veículos de 3 rodas", 
  "Ocupantes de veículos de 4 rodas", 
  "Outros e não especificados", 
  "Todos os Tipos"
)

grid_transito <- expand_grid(
  AGRAVO = "Acidente de Trânsito",
  TIPO_VITIMA = tipos_vitima,
  FX = unique(pop$FX) 
)

grid_quedas <- expand_grid(
  AGRAVO = "Quedas",
  TIPO_VITIMA = "Todos os Tipos",
  FX = c("60-79 anos", "80 anos ou mais")
)

#Empilhar o mapeamento
grid_agravos <- bind_rows(grid_transito, grid_quedas)

pop_expandida <- pop %>%
  inner_join(grid_agravos, by = "FX", relationship = "many-to-many")

tabela_mestra_acidentes <- pop_expandida %>%
  left_join(
    acidentes_long, 
    by = c("ANOOBITO", "NOME", "nivel_geografico", "SEXO", "FX", "AGRAVO", "TIPO_VITIMA")
  ) %>%
  mutate(
    obitos_residencia = replace_na(obitos_residencia, 0),
    obitos_ocorrencia = replace_na(obitos_ocorrencia, 0),
    TAXA_MORT_RESIDENCIA = if_else(populacao > 0, (obitos_residencia / populacao) * 100000, 0),
    TAXA_MORT_OCORRENCIA = if_else(populacao > 0, (obitos_ocorrencia / populacao) * 100000, 0)
  )

#----CÁLCULO DE METAS----#
#----REDUÇÃO 3,4% AO ANO----#
#M = C(1-i)^t

taxa_base_2015 <- tabela_mestra_acidentes %>%
  filter(ANOOBITO == 2015) %>%
  select(nivel_geografico, NOME,  SEXO, FX , TIPO_VITIMA, AGRAVO, 
         TAXA_2015_RES = TAXA_MORT_RESIDENCIA,
         TAXA_2015_OCOR = TAXA_MORT_OCORRENCIA)

mestra_base_2015 <- left_join(
  tabela_mestra_acidentes,
  taxa_base_2015,
  by = c("nivel_geografico", "NOME", "SEXO", "FX" ,"AGRAVO", "TIPO_VITIMA")
)

#Aplica a lógica condicional para a meta de redução
mestra_acidentes <- mestra_base_2015 %>%
  mutate(
    ANOOBITO = as.numeric(ANOOBITO),
    TAXA_ANUAL_REDUCAO = case_when(
      AGRAVO == 'Acidente de Trânsito' & ANOOBITO <= 2030 ~ 0.034, #3,4% ano
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    TAXA_META_RES = TAXA_2015_RES * (1 - TAXA_ANUAL_REDUCAO)^(ANOOBITO - 2015),
    TAXA_META_OCOR = TAXA_2015_OCOR * (1 - TAXA_ANUAL_REDUCAO)^(ANOOBITO - 2015)
  ) %>%
  filter(populacao != 0) %>%
  select(-TAXA_ANUAL_REDUCAO)


manter <- c("mestra_acidentes", "RRAS_RS", "pop")
rm(list = setdiff(ls(), manter))

gc()

#--------------------------#
#------SIH - ACIDENTES-----#
#--------------------------#

#-------------------#
#------EXTRAÇÃO-----#
#-------------------#

ano_atual <-  as.numeric(format(Sys.Date(), "%Y"))
anos <- 2015:ano_atual
meses <- str_pad(1:12, width = 2, pad = "0")

lista_sih_causas_externas <- list()

cols_cid <- c("DIAG_PRINC", "DIAG_SECUN", "DIAGSEC1", "DIAGSEC2", "DIAGSEC3",
              "DIAGSEC4", "DIAGSEC5", "DIAGSEC6", "DIAGSEC7", "DIAGSEC8", "DIAGSEC9")

#Regex atualizada para Acidentes de Transporte (V00-V89) OU Quedas (W00-W19)
regex_causas_externas <- "^(V[0-8][0-9]|W[0-1][0-9])"

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
        select("MUNIC_RES" ,"N_AIH", "SEXO", "DT_INTER", "DT_SAIDA","QT_DIARIAS", "VAL_TOT", "DIAG_PRINC", "DIAG_SECUN", "COD_IDADE", "IDADE", "MORTE",
               "DIAGSEC1","DIAGSEC2","DIAGSEC3","DIAGSEC4","DIAGSEC5","DIAGSEC6","DIAGSEC7","DIAGSEC8" ,"DIAGSEC9") %>%
        mutate(IDADE = as.numeric(IDADE)) %>%
        filter(str_starts(as.character(MUNIC_RES), "35")) %>%
        filter(
          # Procura a nova regex combinada em qualquer coluna
          if_any(
            any_of(cols_cid),
            ~grepl(regex_causas_externas, .x)
          )
        )
      
      if (nrow(filtrado) > 0) {
        lista_sih_causas_externas[[nome_arquivo]] <- filtrado
      }
      
    }, error = function(e) {
      cat(" -> Aviso: Falha ao processar", nome_arquivo, "- Pode não existir ainda. Erro:", e$message, "\n")
    })
    
    unlink(temp_file)
    suppressWarnings(rm(bruto, processado, filtrado))
    gc()
  }
}

#Junta tudo
df_sih_bruto <- bind_rows(lista_sih_causas_externas)

write.csv2(df_sih_bruto, "C:\\R\\DCNT\\Paineis\\Acidentes\\sih_causas_externas.csv", row.names = FALSE)

#df_sih_bruto <- read.csv2("C:\\R\\DCNT\\Paineis\\Acidentes\\sih_causas_externas.csv")

df_sih_bruto <- read_parquet('C:\\R\\DCNT\\Paineis\\Acidentes\\sih_causas_externas.parquet')

#----------------------#
#---SIH - ACIDENTES----#
#----------------------#

sih_base <- df_sih_bruto %>%
  unite("TODOS_DIAGS", any_of(cols_cid), sep = " ", remove = FALSE, na.rm = TRUE) %>%
  mutate(
    #Extrai o primeiro CID V ou W encontrado
    CID_CAUSA = str_extract(TODOS_DIAGS, "V[0-8][0-9]|W[0-1][0-9]")
  ) %>%
  filter(!is.na(CID_CAUSA)) %>%
  mutate(
    ANOOBITO = year(DT_INTER),
    MORTE = if_else(MORTE == "Sim", 1, 0),
    VAL_TOT = as.numeric(VAL_TOT),
    IDADE = as.numeric(IDADE),
    SEXO = case_when(
      SEXO %in% c("1", "M", "Masculino") ~ "Masculino",
      SEXO %in% c("3", "F", "Feminino")  ~ "Feminino",
      TRUE ~ as.character(SEXO)
    ),
    FX = case_when(
      IDADE >= 0 & IDADE <= 9 ~ '9 anos ou menos',
      IDADE >= 10 & IDADE <= 19 ~ '10-19 anos',
      IDADE >= 20 & IDADE < 30 ~ '20-29 anos',
      IDADE >= 30 & IDADE < 40 ~ '30-39 anos',
      IDADE >= 40 & IDADE < 60 ~ '40-59 anos',
      IDADE >= 60 & IDADE < 80 ~ '60-79 anos',
      IDADE >= 80 ~ '80 anos ou mais',
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FX), !is.na(SEXO), ANOOBITO >= 2015)

#TRÂNSITO
sih_transito <- sih_base %>%
  filter(str_starts(CID_CAUSA, "V")) %>%
  mutate(
    AGRAVO = "Acidente de Trânsito",
    TIPO_VITIMA = case_when(
      CID_CAUSA >= 'V01' & CID_CAUSA <= 'V09' ~ "Pedestres",
      CID_CAUSA >= 'V10' & CID_CAUSA <= 'V19' ~ "Ciclistas",
      CID_CAUSA >= 'V20' & CID_CAUSA <= 'V39' ~ "Motociclistas e veículos de 3 rodas",
      CID_CAUSA >= 'V40' & CID_CAUSA <= 'V79' ~ "Ocupantes de veículos de 4 rodas",
      CID_CAUSA >= 'V80' & CID_CAUSA <= 'V89' ~ "Outros e não especificados",
      TRUE ~ NA_character_
    )
  )

sih_transito_todos <- sih_transito %>% mutate(TIPO_VITIMA = "Todos os Tipos")
sih_transito_final <- bind_rows(sih_transito, sih_transito_todos)

#QUEDAS
sih_quedas <- sih_base %>%
  filter(str_starts(CID_CAUSA, "W")) %>%
  filter(FX %in% c('60-79 anos', '80 anos ou mais')) %>%
  mutate(
    AGRAVO = "Quedas",
    TIPO_VITIMA = "Todos os Tipos"
  )

#EMPILHAR
sih_causas_externas <- bind_rows(sih_transito_final, sih_quedas)

#---AGRUPAR ESTRATOS SIH---#
sih_geo <- sih_causas_externas %>%
  mutate(MUNIC_RES = str_sub(as.character(MUNIC_RES), 1, 6)) %>%
  left_join(RRAS_RS, by = c("MUNIC_RES" = "COD_6_mun"))

#Criar os estratos de Total
sih_sexo_total  <- sih_geo %>% mutate(SEXO = "Total")

sih_fx_total <- sih_geo %>% 
  filter(AGRAVO != "Quedas") %>% 
  mutate(FX = "Total")

sih_ambos_total <- sih_geo %>% 
  filter(AGRAVO != "Quedas") %>% 
  mutate(SEXO = "Total", FX = "Total")

sih_agrupar <- bind_rows(sih_geo, sih_sexo_total, sih_fx_total, sih_ambos_total)

#---------------------------------------#
#---CÁLCULO DE INDICADORES DO SIH-------#
#---------------------------------------#

#Município
sih_mun <- sih_agrupar %>%
  filter(!is.na(MUNICIPIO)) %>%
  group_by(ANOOBITO, MUNICIPIO, SEXO, FX, AGRAVO, TIPO_VITIMA) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE),
    obitos_sih = sum(MORTE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(NOME = MUNICIPIO) %>%
  mutate(
    nivel_geografico = "Município",
    valor_medio = valor_total / n_internacoes,
    taxa_mortalidade_hospitalar = (obitos_sih / n_internacoes) * 100
  )

#RS
sih_rs <- sih_agrupar %>%
  filter(!is.na(NOME_RS_2025)) %>%
  group_by(ANOOBITO, NOME_RS_2025, SEXO, FX, AGRAVO, TIPO_VITIMA) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE),
    obitos_sih = sum(MORTE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(NOME = NOME_RS_2025) %>%
  mutate(
    nivel_geografico = "RS",
    valor_medio = valor_total / n_internacoes,
    taxa_mortalidade_hospitalar = (obitos_sih / n_internacoes) * 100
  )

#RRAS
sih_rras <- sih_agrupar %>%
  filter(!is.na(RRAS_2025)) %>%
  group_by(ANOOBITO, RRAS_2025, SEXO, FX, AGRAVO, TIPO_VITIMA) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE),
    obitos_sih = sum(MORTE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(NOME = RRAS_2025) %>%
  mutate(
    nivel_geografico = "RRAS",
    valor_medio = valor_total / n_internacoes,
    taxa_mortalidade_hospitalar = (obitos_sih / n_internacoes) * 100
  )

#Estado SP
sih_estado <- sih_agrupar %>%
  group_by(ANOOBITO, SEXO, FX, AGRAVO, TIPO_VITIMA) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE),
    obitos_sih = sum(MORTE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    NOME = "Estado SP",
    nivel_geografico = "Estado SP",
    valor_medio = valor_total / n_internacoes,
    taxa_mortalidade_hospitalar = (obitos_sih / n_internacoes) * 100
  )

#Tabela Mestra SIH Longa
tabela_mestra_sih_acidentes <- bind_rows(sih_mun, sih_rs, sih_rras, sih_estado)

#FINAL: Unificando SIM e SIH em uma única Tabela Fato
chaves_join <- c("ANOOBITO", "NOME", "nivel_geografico", "SEXO", "FX", "AGRAVO", "TIPO_VITIMA")

mestra_causas_externas_final <- mestra_acidentes %>%
  left_join(tabela_mestra_sih_acidentes, by = chaves_join) %>%
  mutate(
    #Trata os NAs do SIH transformando em 0
    n_internacoes = replace_na(n_internacoes, 0),
    valor_total = replace_na(valor_total, 0),
    obitos_sih = replace_na(obitos_sih, 0),
    valor_medio = replace_na(valor_medio, 0),
    taxa_mortalidade_hospitalar = replace_na(taxa_mortalidade_hospitalar, 0)
  )

#--------------------------------------------------#
#----GRID GEOGRÁFICO E UNIFICAÇÃO (SIM + SIH)------#
#--------------------------------------------------#

#ID_LOCALIDADE para o Star Schema
geo_mun <- RRAS_RS %>%
  transmute(nivel_geografico = "Município", ID_LOCALIDADE = COD_6_mun, NOME = MUNICIPIO) %>%
  distinct()

geo_rs <- RRAS_RS %>%
  transmute(nivel_geografico = "RS", ID_LOCALIDADE = NOME_RS_2025, NOME = NOME_RS_2025) %>%
  distinct() %>% filter(!is.na(ID_LOCALIDADE))

geo_rras <- RRAS_RS %>%
  transmute(nivel_geografico = "RRAS", ID_LOCALIDADE = RRAS_2025, NOME = RRAS_2025) %>%
  distinct() %>% filter(!is.na(ID_LOCALIDADE))

geo_estado <- data.frame(
  nivel_geografico = "Estado SP",
  ID_LOCALIDADE = "35",
  NOME = "Estado SP"
)

#Esqueleto geográfico completo
geo_completa <- bind_rows(geo_mun, geo_rs, geo_rras, geo_estado)

#Juntar as chaves geográficas na base do SIM
mestra_acidentes_geo <- mestra_acidentes %>%
  left_join(geo_completa, by = c("nivel_geografico", "NOME"))

#Join Final: SIM + SIH
chaves_join <- c("ANOOBITO", "NOME", "nivel_geografico", "SEXO", "FX", "AGRAVO", "TIPO_VITIMA")

tabela_final_power_bi <- mestra_acidentes_geo %>%
  left_join(tabela_mestra_sih_acidentes, by = chaves_join) %>%
  rename(ANO = ANOOBITO)

#Tratamento de NAs para o SIH
ano_max_sim <- max(tabela_final_power_bi$ANO, na.rm = TRUE)
ano_max_sih <- max(tabela_mestra_sih_acidentes$ANOOBITO, na.rm = TRUE)

metricas_sih <- c(
  "n_internacoes", 
  "valor_total", 
  "obitos_sih", 
  "valor_medio", 
  "taxa_mortalidade_hospitalar"
)

tabela_final_power_bi <- tabela_final_power_bi %>%
  mutate(
    across(
      .cols = any_of(metricas_sih),
      .fns = ~ if_else(ANO <= ano_max_sih, replace_na(as.numeric(.x), 0), as.numeric(.x))
    )
  )

#--------------------------------------------------#
#----MODELAGEM PARA O POWER BI (STAR SCHEMA)-------#
#--------------------------------------------------#

#DIMENSÃO ESPAÇO
acidente_d_localidade <- geo_completa %>%
  select(nivel_geografico, ID_LOCALIDADE, NOME) %>%
  distinct() %>%
  mutate(ID_GEO = row_number())

#DIMENSÃO AGRAVO E VÍTIMA
acidente_d_agravo <- tabela_final_power_bi %>%
  select(AGRAVO, TIPO_VITIMA) %>%
  distinct() %>%
  mutate(ID_AGRAVO = row_number())

#DIMENSÃO DEMOGRAFIA
acidente_d_demografia <- tabela_final_power_bi %>%
  select(SEXO, FX) %>%
  distinct() %>%
  mutate(ID_DEMOGRAFIA = row_number())

#DIMENSÃO TEMPO
acidente_d_tempo <- tabela_final_power_bi %>%
  select(ANO) %>%
  distinct() %>%
  arrange(ANO) %>%
  mutate(ID_TEMPO = row_number())

#FATO
fato_causas_externas <- tabela_final_power_bi %>%
  left_join(acidente_d_localidade, by = c("nivel_geografico", "ID_LOCALIDADE", "NOME")) %>%
  left_join(acidente_d_agravo, by = c("AGRAVO", "TIPO_VITIMA")) %>%
  left_join(acidente_d_demografia, by = c("SEXO", "FX")) %>%
  left_join(acidente_d_tempo, by = "ANO") %>%
  select(
    ID_GEO, 
    ID_AGRAVO, 
    ID_TEMPO, 
    ID_DEMOGRAFIA,
    obitos_residencia_sim = obitos_residencia,
    obitos_ocorrencia_sim = obitos_ocorrencia,
    populacao,
    TAXA_MORTALIDADE_RESIDENCIA = TAXA_MORT_RESIDENCIA,
    TAXA_MORTALIDADE_OCORRENCIA = TAXA_MORT_OCORRENCIA,
    TAXA_MORTALIDADE_2015_RES = TAXA_2015_RES,
    TAXA_MORTALIDADE_2015_OCOR = TAXA_2015_OCOR,
    TAXA_MORTALIDADE_META_RES = TAXA_META_RES,
    TAXA_MORTALIDADE_META_OCOR = TAXA_META_OCOR,
    n_internacoes,
    valor_total,
    obitos_sih,
    valor_medio,
    taxa_mortalidade_hospitalar
  )

#--------------------#
#------EXPORTAR------#
#--------------------#

caminho_base <- "C:\\R\\DCNT\\Paineis\\Acidentes\\"

write.csv2(acidente_d_localidade, paste0(caminho_base, "acidente_d_localidade.csv"), row.names = FALSE)
write.csv2(acidente_d_agravo, paste0(caminho_base, "acidente_d_agravo.csv"), row.names = FALSE)
write.csv2(acidente_d_demografia, paste0(caminho_base, "acidente_d_demografia.csv"), row.names = FALSE)
write.csv2(acidente_d_tempo, paste0(caminho_base, "acidente_d_tempo.csv"), row.names = FALSE)
write.csv2(fato_causas_externas, paste0(caminho_base, "acidente_f_indicadores.csv"), row.names = FALSE, na = "")

write.xlsx(tabela_final_power_bi, paste0(caminho_base, "causas_externas.xlsx"))

gc()
