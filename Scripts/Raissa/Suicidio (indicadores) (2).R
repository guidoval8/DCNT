# ==============================================================================
# SCRIPT: SUICÍDIO 
# Fonte: SIM-DO (Óbitos)
# Período: 2015-2024
# Indicadores de Mortalidade, Meio de Agressão e Raça/Cor
# ==============================================================================

# 1. PACOTES
# ------------------------------------------------------------------------------
garantir_pacotes <- function(pct) {
  if (!require(pct, character.only = TRUE)) {
    install.packages(pct, dependencies = TRUE)
    library(pct, character.only = TRUE)
  }
}

pacotes <- c("dplyr", "lubridate", "tidyr", "stringr", "rio", "openxlsx", "curl", "remotes", "foreign", "read.dbc")
sapply(pacotes, garantir_pacotes)

if (!require("microdatasus", character.only = TRUE)) {
  remotes::install_github("rfsaldanha/microdatasus")
  library(microdatasus)
}

# Aumenta timeout para 3000s (50min) para evitar quedas em conexões lentas
options(timeout = 3000)

# 2. PARAMETROS
# ------------------------------------------------------------------------------
UF_ALVO      <- "SP"
ANO_INICIO   <- 2015
ANO_FIM      <- 2024
# CID-10 para Suicídio (X60-X84)
CID_SUICIDIO <- "X60|X61|X62|X63|X64|X65|X66|X67|X68|X69|X70|X71|X72|X73|X74|X75|X76|X77|X78|X79|X80|X81|X82|X83|X84"
NOME_CACHE_SIM   <- paste0("CACHE_SIM_PROTOCOL_", UF_ALVO, "_", ANO_INICIO, "_", ANO_FIM, ".rds")

# 3. EXTRAÇÃO E TRATAMENTO (SIM - NUMERADOR)
# ------------------------------------------------------------------------------

if (file.exists(NOME_CACHE_SIM)) {
  message("Carregando cache local do SIM...")
  SIM_RAW <- readRDS(NOME_CACHE_SIM)
} else {
  message("-----------------------------------------------------------------------")
  message("ETAPA 1/2: Baixando dados do SIM (Mortalidade)...")
  message("Isso pode demorar. Se falhar, o script tentará novamente automaticamente.")
  message("-----------------------------------------------------------------------")
  
  # Loop de tentativas para o SIM (microdatasus)
  dados_raw <- NULL
  tentativas_sim <- 3
  
  for(i in 1:tentativas_sim) {
    tryCatch({
      message(paste0("Tentativa ", i, " de ", tentativas_sim, " para baixar o SIM..."))
      dados_raw <- fetch_datasus(
        year_start = ANO_INICIO,
        year_end = ANO_FIM,
        uf = UF_ALVO,
        information_system = "SIM-DO"
      )
      if(!is.null(dados_raw)) break # Sucesso
    }, error = function(e) {
      message(paste("Erro na tentativa", i, ":", e$message))
      Sys.sleep(5) # Espera 5 segundos antes de tentar de novo
    })
  }
  
  if(is.null(dados_raw)) {
    stop("ERRO CRÍTICO: Não foi possível baixar os dados do SIM após várias tentativas. Verifique sua conexão ou o servidor do DATASUS.")
  }
  
  message("Processando microdados...")
  dados_proc <- process_sim(dados_raw)
  
  message("Aplicando filtros e tratamento...")
  SIM_RAW <- dados_proc %>%
    mutate(DTOBITO = ymd(DTOBITO)) %>%
    mutate(ANO = year(DTOBITO)) %>%
    mutate(
      CAUSABAS = str_trim(toupper(as.character(CAUSABAS))),
      CID3 = str_sub(CAUSABAS, 1, 3)
    ) %>%
    # Filtro Suicídio
    filter(str_detect(CAUSABAS, CID_SUICIDIO)) %>%
    mutate(CODMUNRES = str_sub(as.character(CODMUNRES), 1, 6)) %>%
    
    # --- TRATAMENTO ROBUSTO DE RAÇA E SEXO ---
    mutate(RACACOR_TEMP = str_trim(toupper(as.character(RACACOR)))) %>%
    mutate(
      RACA_COR = case_when(
        str_starts(RACACOR_TEMP, "1") | str_starts(RACACOR_TEMP, "BRA") | str_starts(RACACOR_TEMP, "WHI") ~ 'Branca',
        str_starts(RACACOR_TEMP, "2") | str_starts(RACACOR_TEMP, "PRE") | str_starts(RACACOR_TEMP, "BLA") ~ 'Preta',
        str_starts(RACACOR_TEMP, "3") | str_starts(RACACOR_TEMP, "AMA") | str_starts(RACACOR_TEMP, "YEL") ~ 'Amarela',
        str_starts(RACACOR_TEMP, "4") | str_starts(RACACOR_TEMP, "PAR") | str_starts(RACACOR_TEMP, "BRO") ~ 'Parda',
        str_starts(RACACOR_TEMP, "5") | str_starts(RACACOR_TEMP, "IND") ~ 'Indigena',
        TRUE ~ 'Ignorado' 
      )
    ) %>%
    mutate(SEXO_TEMP = str_trim(toupper(as.character(SEXO)))) %>%
    mutate(
      SEXO = case_when(
        str_starts(SEXO_TEMP, "M") | SEXO_TEMP == "1" ~ "Masculino",
        str_starts(SEXO_TEMP, "F") | SEXO_TEMP == "2" ~ "Feminino",
        TRUE ~ 'Ignorado'
      )
    )
  
  saveRDS(SIM_RAW, NOME_CACHE_SIM)
}

# 4. DOWNLOAD E TRATAMENTO DA POPULAÇÃO (DENOMINADOR)
# ------------------------------------------------------------------------------
message("-----------------------------------------------------------------------")
message("ETAPA 2/2: Iniciando processamento da população (FTP DATASUS)...")
message("-----------------------------------------------------------------------")

# Função auxiliar para tentar múltiplos métodos e protocolos (FTP e HTTP)
download_seguro <- function(url_base, nome_arquivo) {
  # Tenta primeiro FTP, depois HTTP (fallback comum do Datasus)
  urls <- c(
    paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/", nome_arquivo),
    paste0("http://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/", nome_arquivo) # Tenta via HTTP se FTP falhar
  )
  
  metodos <- c("libcurl", "curl", "auto", "wininet")
  sucesso <- FALSE
  
  for(url in urls) {
    if(sucesso) break
    for(m in metodos) {
      if(!sucesso) {
        tryCatch({
          # message(paste("Tentando:", url, "via", m)) # Debug opcional
          download.file(url, nome_arquivo, mode = "wb", method = m, quiet = TRUE)
          if(file.exists(nome_arquivo) && file.info(nome_arquivo)$size > 1000) { # Verifica se baixou algo > 1kb
            sucesso <- TRUE
            message(paste(" -> Sucesso baixando", nome_arquivo))
          }
        }, error = function(e) { })
      }
    }
  }
  return(sucesso)
}

lista_pop <- list()

for (ano in ANO_INICIO:ANO_FIM) {
  
  sufixo_ano <- substr(as.character(ano), 3, 4)
  nome_pop_zip <- paste0("POPSBR", sufixo_ano, ".zip")
  nome_dbf     <- paste0("POP", sufixo_ano, ".dbf")
  
  # Lógica de Download
  if (!file.exists(nome_pop_zip)) {
    message(paste("Baixando população de", ano, "..."))
    if (!download_seguro(NULL, nome_pop_zip)) { # URL montada dentro da função agora
      message(paste("ALERTA: Falha ao baixar", nome_pop_zip, "- O servidor pode estar instável."))
    }
  }
  
  # Lógica de Descompactação e Leitura
  if (file.exists(nome_pop_zip)) {
    tryCatch({
      unzip(nome_pop_zip, overwrite = TRUE)
      
      if(file.exists(nome_dbf)){
        df_ano <- read.dbf(nome_dbf)
        df_ano$ANO <- ano 
        lista_pop[[as.character(ano)]] <- df_ano
      } else {
        # Tenta verificar se o DBF tem outro nome dentro do zip (ex: maiúsculas/minúsculas)
        arquivos_zip <- unzip(nome_pop_zip, list = TRUE)
        dbf_interno <- arquivos_zip$Name[grep(".dbf$", arquivos_zip$Name, ignore.case = TRUE)]
        if(length(dbf_interno) > 0) {
          df_ano <- read.dbf(dbf_interno[1])
          df_ano$ANO <- ano
          lista_pop[[as.character(ano)]] <- df_ano
        } else {
          warning(paste("Arquivo DBF não encontrado dentro do ZIP para o ano", ano))
        }
      }
    }, error = function(e) { 
      message(paste("Erro ao ler/descompactar ano", ano, ":", e$message)) 
    })
  }
}

POP_BRUTA <- bind_rows(lista_pop)

if(nrow(POP_BRUTA) == 0) {
  stop("ERRO CRÍTICO: Nenhum dado de população foi carregado. Verifique a conexão com a internet e tente novamente.")
}

# Correção nome coluna MUNIC/COD_MUN
names(POP_BRUTA) <- toupper(names(POP_BRUTA))
if ("MUNIC" %in% names(POP_BRUTA)) { POP_BRUTA <- rename(POP_BRUTA, COD_MUN = MUNIC) }

# Tratamento da População (Agregada por Ano e Sexo para o Estado de SP)
POP_ESTADO_SEXO <- POP_BRUTA %>%
  filter(str_starts(COD_MUN, "35")) %>% # Apenas SP
  mutate(
    SEXO = case_when(
      SEXO == "1" ~ "Masculino",
      SEXO == "2" ~ "Feminino",
      TRUE ~ "Outros"
    )
  ) %>%
  filter(SEXO %in% c("Masculino", "Feminino")) %>%
  group_by(ANO, SEXO) %>%
  summarise(POPULACAO = sum(POP, na.rm = TRUE), .groups = 'drop')

# População Total do Estado por Ano
POP_ESTADO_TOTAL <- POP_ESTADO_SEXO %>%
  group_by(ANO) %>%
  summarise(POPULACAO = sum(POPULACAO, na.rm = TRUE), .groups = 'drop') %>%
  mutate(SEXO = "Total")

# Unindo Populações para cálculo de taxa
POP_FINAL <- bind_rows(POP_ESTADO_SEXO, POP_ESTADO_TOTAL)


# 5. CLASSIFICAÇÃO DE VARIÁVEIS DO PROTOCOLO E GEOGRAFIA
# ------------------------------------------------------------------------------
message("Classificando variáveis conforme protocolo...")

# Importar Geografia
url_geo <- "https://github.com/guidoval8/DCNT/blob/main/dados/RRAS_Municipios.xlsx?raw=true"
RRAS_Municipios <- import(url_geo)

GEO_SP <- RRAS_Municipios %>%
  mutate(COD_6_mun = str_sub(as.character(COD_6_mun), 1, 6)) %>%
  select(CODMUNRES = COD_6_mun, NOME_MUNICIPIO = MUNICIPIO, RRAS = RRAS_2025, RS = NOME_RS_2025) %>%
  distinct(CODMUNRES, .keep_all = TRUE)

# Tratamento SIM
SIM_GEO <- SIM_RAW %>%
  mutate(IDADE_ANOS = as.numeric(IDADEanos)) %>%
  
  # 1. FAIXA ETÁRIA
  mutate(FAIXA_ETARIA = case_when(
    IDADE_ANOS < 5  ~ "00-04 anos",
    IDADE_ANOS >= 5  & IDADE_ANOS <= 9  ~ "05-09 anos",
    IDADE_ANOS >= 10 & IDADE_ANOS <= 14 ~ "10-14 anos",
    IDADE_ANOS >= 15 & IDADE_ANOS <= 19 ~ "15-19 anos",
    IDADE_ANOS >= 20 & IDADE_ANOS <= 29 ~ "20-29 anos",
    IDADE_ANOS >= 30 & IDADE_ANOS <= 39 ~ "30-39 anos",
    IDADE_ANOS >= 40 & IDADE_ANOS <= 49 ~ "40-49 anos",
    IDADE_ANOS >= 50 & IDADE_ANOS <= 59 ~ "50-59 anos",
    IDADE_ANOS >= 60 ~ "60 anos e mais",
    TRUE ~ "Ignorado"
  )) %>%
  
  # 2. MEIO DE AGRESSÃO
  mutate(MEIO_AGRESSAO = case_when(
    str_starts(CID3, "X70") ~ "Enforcamento",
    str_starts(CID3, "X72") | str_starts(CID3, "X73") | str_starts(CID3, "X74") ~ "Arma de Fogo",
    str_starts(CID3, "X6") ~ "Envenenamento/Intoxicação",
    str_starts(CID3, "X80") ~ "Precipitação (Lugar Elevado)",
    str_starts(CID3, "X78") ~ "Objeto Cortante",
    str_starts(CID3, "X76") ~ "Fogo/Chamas",
    TRUE ~ "Outros Meios"
  )) %>%
  
  left_join(GEO_SP, by = "CODMUNRES")

# 6. GERAÇÃO DOS INDICADORES
# ------------------------------------------------------------------------------

# --- INDICADOR 1: Suicídios por Sexo e Faixa Etária (Estado, RRAS, RS) ---
# Filtro: 5 anos e mais
SIM_FILTER_5ANOS <- SIM_GEO %>% filter(IDADE_ANOS >= 5)

# Função auxiliar para agregar Ind 1
agg_ind1 <- function(data, group_cols, nivel) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(OBITOS = n(), .groups = "drop") %>%
    mutate(NIVEL_GEOGRAFICO = nivel) %>%
    rename(LOCALIDADE = !!group_cols[1])
}

ind1_estado <- SIM_FILTER_5ANOS %>% 
  mutate(ESTADO = "São Paulo") %>%
  agg_ind1(c("ESTADO", "ANO", "SEXO", "FAIXA_ETARIA"), "Estado")

ind1_rras <- SIM_FILTER_5ANOS %>% 
  filter(!is.na(RRAS)) %>%
  agg_ind1(c("RRAS", "ANO", "SEXO", "FAIXA_ETARIA"), "RRAS")

ind1_rs <- SIM_FILTER_5ANOS %>% 
  filter(!is.na(RS)) %>%
  agg_ind1(c("RS", "ANO", "SEXO", "FAIXA_ETARIA"), "RS")

IND_1_FINAL <- bind_rows(ind1_estado, ind1_rras, ind1_rs)


# --- INDICADOR 2: Suicídios por Meio de Agressão e Sexo (Estado, RRAS, RS) ---
# Função auxiliar para agregar Ind 2
agg_ind2 <- function(data, group_cols, nivel) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(OBITOS = n(), .groups = "drop") %>%
    mutate(NIVEL_GEOGRAFICO = nivel) %>%
    rename(LOCALIDADE = !!group_cols[1])
}

ind2_estado <- SIM_GEO %>% 
  mutate(ESTADO = "São Paulo") %>%
  agg_ind2(c("ESTADO", "ANO", "MEIO_AGRESSAO", "SEXO"), "Estado")

ind2_rras <- SIM_GEO %>% 
  filter(!is.na(RRAS)) %>%
  agg_ind2(c("RRAS", "ANO", "MEIO_AGRESSAO", "SEXO"), "RRAS")

ind2_rs <- SIM_GEO %>% 
  filter(!is.na(RS)) %>%
  agg_ind2(c("RS", "ANO", "MEIO_AGRESSAO", "SEXO"), "RS")

IND_2_FINAL <- bind_rows(ind2_estado, ind2_rras, ind2_rs)


# --- INDICADOR 3: Taxa de Mortalidade por Suicídio (Total e Sexo) - ESTADO ---
# Numerador (Óbitos Estado SP, 5 anos e mais conforme protocolo, agrupado por Ano/Sexo)
num_ind3_sexo <- SIM_FILTER_5ANOS %>%
  group_by(ANO, SEXO) %>%
  summarise(OBITOS = n(), .groups = "drop")

num_ind3_total <- SIM_FILTER_5ANOS %>%
  group_by(ANO) %>%
  summarise(OBITOS = n(), .groups = "drop") %>%
  mutate(SEXO = "Total")

num_ind3_final <- bind_rows(num_ind3_sexo, num_ind3_total)

# Cálculo da Taxa (Join com População)
IND_3_FINAL <- left_join(num_ind3_final, POP_FINAL, by = c("ANO", "SEXO")) %>%
  mutate(
    TAXA_100MIL = (OBITOS / POPULACAO) * 100000,
    INDICADOR = "Taxa de Mortalidade por Suicídio (Estado SP)"
  ) %>%
  select(ANO, SEXO, OBITOS, POPULACAO, TAXA_100MIL)


# 7. EXPORTAÇÃO
# ------------------------------------------------------------------------------
message("Exportando resultados...")

lista_saida <- list(
  "1_Sexo_FaixaEtaria_Geo" = IND_1_FINAL,
  "2_Meio_Sexo_Geo" = IND_2_FINAL,
  "3_Taxas_Estado" = IND_3_FINAL,
  "Base_Completa" = SIM_GEO %>% select(ANO, RRAS, RS, NOME_MUNICIPIO, SEXO, FAIXA_ETARIA, MEIO_AGRESSAO, RACA_COR)
)

nome_arquivo <- paste0("Protocolo_Suicidio_SIM_SP_V2_", ANO_INICIO, "_", ANO_FIM, ".xlsx")

# Salvar com tratamento de erro (caso o arquivo esteja aberto)
tryCatch({
  write.xlsx(lista_saida, file = nome_arquivo, overwrite = TRUE)
  message(paste0("Processo concluído! Arquivo salvo: ", nome_arquivo))
}, error = function(e) {
  message("ERRO: Não foi possível salvar o arquivo Excel.")
  message("Verifique se o arquivo já está aberto em outro programa e tente novamente.")
  message(paste("Detalhes do erro:", e$message))
})