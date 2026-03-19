# ==============================================================================
# SCRIPT DE ANALISE DE INTERNACOES (SIH) - DCNT, CSAP E MORTALIDADE
# NIVEIS: MUNICIPIO, RRAS E ESTADO
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              # ==============================================================================

# 1. CONFIGURACAO E PACOTES
pacotes_necessarios <- c("dplyr", "tidyr", "microdatasus", "readxl", "stringr", "writexl", "lubridate", "janitor", "read.dbc", "curl")
pacotes_ausentes <- pacotes_necessarios[!(pacotes_necessarios %in% installed.packages()[,"Package"])]
if (length(pacotes_ausentes) > 0) install.packages(pacotes_ausentes)
invisible(lapply(pacotes_necessarios, library, character.only = TRUE))

# DIRETORIO 
setwd("C:/Users/raiss/OneDrive/Desktop/R") 

# ==============================================================================
# 2. CARREGAR TABELAS AUXILIARES
# ==============================================================================

# 2.1. TABELA DE RRAS
RRAS_municipios_path <- "RRAS/RRAS Municipios (1).xlsx" 
if (!file.exists(RRAS_municipios_path)) stop("Arquivo de RRAS nao encontrado!")

RRAS_municipios <- readxl::read_excel(RRAS_municipios_path) %>%
  janitor::clean_names() %>%
  mutate(cod_6_mun = as.character(cod_6_mun)) %>%
  select(cod_6_mun, rras_2025, nome_rs_2025)

# 2.2. TABELA DE POPULACAO
POPSVS_CACHE_PATH <- "DENOMINADOR_cache.rds"

if (file.exists(POPSVS_CACHE_PATH)) {
  message("Carregando dados populacionais do cache...")
  cache_pop <- readRDS(POPSVS_CACHE_PATH)
  # Prepara populacao para calculo de taxas
  POP_BASE <- cache_pop$DENOMINADOR %>%
    mutate(
      CODMUNRES = stringr::str_sub(as.character(COD_MUN), 1, 6),
      ANO = as.numeric(ANO),
      POP = as.numeric(POP)
    ) %>%
    inner_join(RRAS_municipios, by = c("CODMUNRES" = "cod_6_mun"))
  
} else {
  stop("ERRO: Cache de Populacao (DENOMINADOR_cache.rds) nao encontrado. Rode o script de populacao anterior primeiro!")
}

# ==============================================================================
# 3. DOWNLOAD E PROCESSAMENTO DO SIH (MANUAL E SEGURO)
# ==============================================================================
SIH_CACHE_PATH <- "SIH_SP_2015_2024_cache.rds"

if (file.exists(SIH_CACHE_PATH)) {
  message("Carregando dados do SIH do cache final...")
  SIH_SP <- readRDS(SIH_CACHE_PATH)
} else {
  message("Cache SIH nao encontrado. Verificando parcial ou baixando...")
  
  # FUNCAO DE DOWNLOAD MANUAL
  baixar_sih_manual_seguro <- function(anos, uf = "SP") {
    base_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados"
    lista_final <- list()
    
    for (ano in anos) {
      cache_ano_path <- paste0("SIH_", uf, "_", ano, "_temp.rds")
      
      if (file.exists(cache_ano_path)) {
        message(paste("Ano", ano, "ja baixado! Carregando do disco..."))
        lista_final[[as.character(ano)]] <- readRDS(cache_ano_path)
        next 
      }
      
      message(paste("Baixando Ano:", ano, "(Aguarde, arquivos mensais)..."))
      lista_meses <- list()
      ano_dois_digitos <- substr(as.character(ano), 3, 4)
      
      for (mes in 1:12) {
        mes_formatado <- sprintf("%02d", mes)
        nome_arquivo <- paste0("RD", uf, ano_dois_digitos, mes_formatado, ".dbc")
        url_arquivo <- paste0(base_url, "/", nome_arquivo)
        dest_arquivo <- file.path(tempdir(), nome_arquivo)
        
        tryCatch({
          if (!file.exists(dest_arquivo)) {
            curl::curl_download(url_arquivo, dest_arquivo, quiet = TRUE, handle = curl::new_handle(timeout = 300))
          }
          dados_temp <- read.dbc::read.dbc(dest_arquivo)
          
          dados_limpos <- dados_temp %>%
            mutate(
              CODMUNRES = as.character(MUNIC_RES),
              VAL_TOT = as.numeric(as.character(VAL_TOT)),
              ANO_CMPT = as.character(ANO_CMPT),
              DIAG_PRINC = as.character(DIAG_PRINC),
              MORTE = as.numeric(as.character(MORTE)),
              SEXO = as.character(SEXO),
              IDADE = as.numeric(as.character(IDADE))
            ) %>%
            select(ANO_CMPT, CODMUNRES, DIAG_PRINC, VAL_TOT, MORTE, SEXO, IDADE)
          
          lista_meses[[mes]] <- dados_limpos
          cat(".") 
        }, error = function(e) {})
      }
      
      if (length(lista_meses) > 0) {
        dados_ano <- bind_rows(lista_meses)
        saveRDS(dados_ano, cache_ano_path) 
        lista_final[[as.character(ano)]] <- dados_ano
        message(paste(" Ano", ano, "concluido!"))
      }
      rm(lista_meses); gc()
    }
    bind_rows(lista_final)
  }
  
  dados_brutos <- baixar_sih_manual_seguro(2015:2024)
  
  message("Finalizando processamento SIH...")
  SIH_SP <- dados_brutos %>%
    mutate(
      CODMUNRES = str_sub(CODMUNRES, 1, 6),
      ANO_INTERNACAO = as.numeric(substr(ANO_CMPT, 1, 4)),
      DIAG_PRINC = str_trim(DIAG_PRINC),
      SEXO = case_when(
        SEXO == "1" ~ "Masculino",
        SEXO == "3" ~ "Feminino",
        TRUE ~ "Outros"
      )
    ) %>%
    select(ANO_INTERNACAO, CODMUNRES, DIAG_PRINC, VAL_TOT, MORTE, SEXO, IDADE)
  
  saveRDS(SIH_SP, SIH_CACHE_PATH)
  message("Dados salvos!")
  rm(dados_brutos); gc()
}

# ==============================================================================
# 4. CLASSIFICACAO DAS CAUSAS (ATUALIZADO COM 4 CATEGORIAS DE CANCER)
# ==============================================================================
message("Classificando doencas e detalhando cancer...")

SIH_CLASSIFICADO <- SIH_SP %>%
  mutate(
    # Remove pontos dos codigos CID para garantir padronizacao (Ex: C45.1 vira C451)
    DIAG_LIMPO = str_remove_all(DIAG_PRINC, "\\."),
    
    # --- GRUPO DCNT DETALHADO ---
    GRUPO_DCNT = case_when(
      # 1. DOENCAS CIRCULATORIAS
      str_starts(DIAG_LIMPO, "I") ~ "Circulatorias (I00-I99)",
      
      # 2. DIABETES
      str_starts(DIAG_LIMPO, "E10") | str_starts(DIAG_LIMPO, "E11") | 
        str_starts(DIAG_LIMPO, "E12") | str_starts(DIAG_LIMPO, "E13") | 
        str_starts(DIAG_LIMPO, "E14") ~ "Diabetes (E10-E14)",
      
      # 3. RESPIRATORIAS CRONICAS
      (str_starts(DIAG_LIMPO, "J3") | str_starts(DIAG_LIMPO, "J4") | 
         str_starts(DIAG_LIMPO, "J5") | str_starts(DIAG_LIMPO, "J6") | 
         str_starts(DIAG_LIMPO, "J7") | str_starts(DIAG_LIMPO, "J8") | 
         str_starts(DIAG_LIMPO, "J9")) & DIAG_LIMPO != "J36" ~ "Respiratorias Cronicas",
      
      # 4. CANCER DETALHADO (4 Categorias)
      str_starts(DIAG_LIMPO, "C50") ~ "Cancer - Mama (C50)",
      str_starts(DIAG_LIMPO, "C53") ~ "Cancer - Colo de Utero (C53)",
      str_starts(DIAG_LIMPO, "C61") ~ "Cancer - Prostata (C61)",
      (DIAG_LIMPO >= "C15" & DIAG_LIMPO <= "C26") ~ "Cancer - Ap. Digestivo (C15-C26)",
      
      # Outros Canceres (Para compor o total, mas ficam separados aqui)
      str_starts(DIAG_LIMPO, "C") ~ "Cancer - Outros",
      
      TRUE ~ "Outros"
    ),
    
    # Flags para Totais
    IS_DCNT_TOTAL = GRUPO_DCNT != "Outros",
    IS_CANCER_TOTAL = str_starts(DIAG_LIMPO, "C"), # Flag para somar todo o Cancer depois
    
    # --- GRUPO CSAP ---
    IS_HIPERTENSAO = (DIAG_LIMPO >= "I10" & DIAG_LIMPO <= "I14"),
    IS_DIABETES    = (DIAG_LIMPO >= "E10" & DIAG_LIMPO <= "E14"),
    IS_CSAP_SEL    = IS_HIPERTENSAO | IS_DIABETES,
    
    # --- MORTALIDADE ---
    IS_DIC = (DIAG_LIMPO >= "I20" & DIAG_LIMPO <= "I25"),
    IS_AVC = (DIAG_LIMPO >= "I60" & DIAG_LIMPO <= "I69"),
    IS_CARDIO_GRAVE = IS_DIC | IS_AVC
  ) %>%
  inner_join(RRAS_municipios, by = c("CODMUNRES" = "cod_6_mun"))

# ==============================================================================
# 5. CALCULO DOS INDICADORES (COM CANCER DETALHADO E TOTAIS)
# ==============================================================================

# Funcao auxiliar de populacao
get_populacao <- function(nivel, cols_agg_pop) {
  POP_BASE %>%
    group_by(across(all_of(cols_agg_pop))) %>%
    summarise(Populacao_Total = sum(POP, na.rm=T), .groups="drop")
}

calcular_indicadores <- function(df_sih, pop_agg, cols_agrupamento, nivel_nome) {
  
  # Join com Populacao
  cols_join_pop <- cols_agrupamento
  names(cols_join_pop)[names(cols_join_pop) == "ANO_INTERNACAO"] <- "ANO"
  
  # 1. DCNT POR GRUPOS (Inclui as 4 categorias de Cancer + Outros Canceres + Cardio/Resp/Diab)
  dcnt_grupos <- df_sih %>%
    filter(GRUPO_DCNT != "Outros") %>%
    group_by(across(all_of(c(cols_agrupamento, "GRUPO_DCNT")))) %>%
    summarise(Valor_Total = sum(VAL_TOT, na.rm=T), Internacoes = n(), .groups="drop")
  
  # 2. TOTAL DCNT
  dcnt_total <- df_sih %>%
    filter(IS_DCNT_TOTAL) %>%
    group_by(across(all_of(cols_agrupamento))) %>%
    summarise(Valor_Total = sum(VAL_TOT, na.rm=T), Internacoes = n(), .groups="drop") %>%
    mutate(GRUPO_DCNT = "TOTAL DCNT (Todos Grupos)")
  
  # 3. TOTAL CANCER (Novo! Soma C00-C97 independente da categoria)
  cancer_total <- df_sih %>%
    filter(IS_CANCER_TOTAL) %>%
    group_by(across(all_of(cols_agrupamento))) %>%
    summarise(Valor_Total = sum(VAL_TOT, na.rm=T), Internacoes = n(), .groups="drop") %>%
    mutate(GRUPO_DCNT = "TOTAL CANCER (C00-C97)")
  
  # Junta tudo de DCNT
  res_dcnt <- bind_rows(dcnt_grupos, dcnt_total, cancer_total) %>%
    left_join(pop_agg, by = setNames(nm = cols_agrupamento, object = names(pop_agg)[1:length(cols_agrupamento)])) %>%
    mutate(
      Valor_Medio = Valor_Total / Internacoes,
      Taxa_Internacao_100k = (Internacoes / Populacao_Total) * 100000,
      Nivel = nivel_nome
    )
  
  # 4. CSAP
  res_csap <- df_sih %>%
    filter(IS_CSAP_SEL) %>%
    group_by(across(all_of(cols_agrupamento))) %>%
    summarise(Internacoes_CSAP = n(), Valor_Total_CSAP = sum(VAL_TOT, na.rm=T), .groups="drop") %>%
    mutate(Valor_Medio_CSAP = Valor_Total_CSAP / Internacoes_CSAP, Nivel = nivel_nome)
  
  # 5. Mortalidade
  res_mort <- df_sih %>%
    filter(IS_CARDIO_GRAVE) %>%
    mutate(Causa_Morte = ifelse(IS_DIC, "DIC (I20-I25)", "AVC (I60-I69)")) %>%
    group_by(across(all_of(c(cols_agrupamento, "Causa_Morte")))) %>%
    summarise(Obitos = sum(MORTE, na.rm=T), Saidas = n(), .groups="drop") %>%
    mutate(Taxa_Mortalidade = (Obitos / Saidas) * 100, Nivel = nivel_nome)
  
  return(list(DCNT = res_dcnt, CSAP = res_csap, MORT = res_mort))
}

# --- A. PREPARA POPULACAO ---
message("Agregando populacoes...")
pop_mun <- get_populacao("Municipio", c("ANO", "CODMUNRES"))
pop_rras <- get_populacao("RRAS", c("ANO", "rras_2025"))
pop_uf <- get_populacao("Estado", c("ANO"))

# --- B. EXECUTA CALCULOS ---
# 1. MUNICIPIO
message("Calculando Nivel: Municipios...")
cols_mun_sih <- c("ANO_INTERNACAO", "CODMUNRES")
pop_mun_ren <- pop_mun %>% rename(ANO_INTERNACAO = ANO)
res_mun <- calcular_indicadores(SIH_CLASSIFICADO, pop_mun_ren, cols_mun_sih, "Municipio") %>%
  lapply(function(df) left_join(df, RRAS_municipios, by=c("CODMUNRES"="cod_6_mun")))

# 2. RRAS
message("Calculando Nivel: RRAS...")
cols_rras_sih <- c("ANO_INTERNACAO", "rras_2025")
pop_rras_ren <- pop_rras %>% rename(ANO_INTERNACAO = ANO)
res_rras <- calcular_indicadores(SIH_CLASSIFICADO, pop_rras_ren, cols_rras_sih, "RRAS")

# 3. ESTADO
message("Calculando Nivel: Estado SP...")
cols_uf_sih <- c("ANO_INTERNACAO")
pop_uf_ren <- pop_uf %>% rename(ANO_INTERNACAO = ANO)
res_uf <- calcular_indicadores(SIH_CLASSIFICADO, pop_uf_ren, cols_uf_sih, "Estado SP")

# ==============================================================================
# 6. CONSOLIDACAO E EXPORTACAO
# ==============================================================================

message("Consolidando tabela Mestra...")

padronizar_tabela <- function(df, tipo_indicador) {
  df %>%
    mutate(
      TIPO_INDICADOR = tipo_indicador,
      CODMUNRES = if("CODMUNRES" %in% names(.)) CODMUNRES else NA_character_,
      rras_2025 = if("rras_2025" %in% names(.)) rras_2025 else NA_character_,
      nome_rs_2025 = if("nome_rs_2025" %in% names(.)) nome_rs_2025 else NA_character_
    ) %>%
    mutate(across(c(CODMUNRES, rras_2025, nome_rs_2025), as.character))
}

# 1. DCNT (Agora inclui linhas especificas de Cancer + Total Cancer + Total DCNT)
df_dcnt <- bind_rows(
  padronizar_tabela(res_mun$DCNT, "DCNT"),
  padronizar_tabela(res_rras$DCNT, "DCNT"),
  padronizar_tabela(res_uf$DCNT, "DCNT")
) %>%
  rename(Categoria = GRUPO_DCNT)

# 2. CSAP
df_csap <- bind_rows(
  padronizar_tabela(res_mun$CSAP, "CSAP"),
  padronizar_tabela(res_rras$CSAP, "CSAP"),
  padronizar_tabela(res_uf$CSAP, "CSAP")
) %>%
  mutate(Categoria = "Hipertensao e Diabetes") %>%
  rename(Internacoes = Internacoes_CSAP, Valor_Total = Valor_Total_CSAP, Valor_Medio = Valor_Medio_CSAP)

# 3. Mortalidade
df_mort <- bind_rows(
  padronizar_tabela(res_mun$MORT, "Mortalidade_Hosp"),
  padronizar_tabela(res_rras$MORT, "Mortalidade_Hosp"),
  padronizar_tabela(res_uf$MORT, "Mortalidade_Hosp")
) %>%
  rename(Categoria = Causa_Morte, Internacoes = Saidas)

# 4. JUNTA TUDO
TABELA_MESTRA <- bind_rows(df_dcnt, df_csap, df_mort) %>%
  select(
    TIPO_INDICADOR, Nivel, ANO_INTERNACAO, 
    CODMUNRES, nome_rs_2025, rras_2025,
    Categoria, 
    Internacoes, Valor_Total, Valor_Medio, Taxa_Internacao_100k,
    Obitos, Taxa_Mortalidade
  ) %>%
  arrange(TIPO_INDICADOR, ANO_INTERNACAO, Nivel)

nome_arquivo <- paste0("Indicadores_SIH_SP_CONSOLIDADO_CancerDetalh_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
write_xlsx(list("DADOS_GERAIS" = TABELA_MESTRA), nome_arquivo)

message(paste("Sucesso!:", nome_arquivo))