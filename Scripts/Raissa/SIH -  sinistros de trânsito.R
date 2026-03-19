# ==============================================================================
# SCRIPT SIH -SINISTROS DE TRÂNSITO 
# PERÍODO: 2015-2024
# ==============================================================================


# 1. PACOTES
pacotes <- c("dplyr", "tidyr", "readxl", "stringr", "writexl", "read.dbc", "curl", "janitor", "readr")
instalados <- pacotes %in% installed.packages()[,"Package"]
if(any(!instalados)) install.packages(pacotes[!instalados])
invisible(lapply(pacotes, library, character.only = TRUE))

setwd("C:/Users/raiss/OneDrive/Desktop/R") 

# FUNÇÃO DE LIMPEZA DE CHAVE
limpar_chave <- function(x) {
  str_sub(str_replace_all(as.character(x), "[^0-9]", ""), 1, 6)
}

# ==============================================================================
# 2. CARREGAR DADOS AUXILIARES
# ==============================================================================
message("\n>>> 1. Carregando Tabelas de Apoio...")

# 2.1 RRAS
if (!file.exists("RRAS/RRAS Municipios (1).xlsx")) stop("ERRO: Arquivo RRAS não encontrado!")
TAB_RRAS <- read_excel("RRAS/RRAS Municipios (1).xlsx") %>%
  janitor::clean_names() %>%
  mutate(cod_6_mun = limpar_chave(cod_6_mun)) %>%
  select(cod_6_mun, rras_2025, nome_rs_2025) %>%
  distinct(cod_6_mun, .keep_all = TRUE)

# 2.2 POPULAÇÃO TOTAL
if (!file.exists("DENOMINADOR_cache.rds")) stop("ERRO: Cache População não encontrado!")
cache_pop <- readRDS("DENOMINADOR_cache.rds")

POP_TOTAL <- cache_pop$DENOMINADOR %>%
  janitor::clean_names() %>%
  mutate(
    cod_mun = limpar_chave(cod_mun), 
    ano = as.numeric(ano), 
    pop = as.numeric(pop)
  ) %>%
  group_by(ano, cod_mun) %>% 
  summarise(pop_total = sum(pop, na.rm=TRUE), .groups="drop")

message("   -> Tabela RRAS e População Total carregadas.")

# ==============================================================================
# 3. PROCESSAMENTO DO SIH (VARREDURA UNIVERSAL - TRÂNSITO)
# ==============================================================================
message("\n>>> 2. Processando SIH (Varredura Universal de Trânsito)...")

CACHE_TRANSITO <- "SIH_TRANSITO_VARREDURA_V2.rds"

processar_transito_ano <- function(ano) {
  ano_2d <- substr(ano, 3, 4)
  base_ftp <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados"
  lista_meses <- list()
  
  message(paste("\n   -> Iniciando Ano:", ano))
  
  for (mes in 1:12) {
    arq <- paste0("RDSP", ano_2d, sprintf("%02d", mes), ".dbc")
    dest <- file.path(tempdir(), arq)
    
    # 3.1. DOWNLOAD ROBUSTO COM RETRY
    if (!file.exists(dest)) {
      tentativa <- 1; sucesso <- FALSE
      while(tentativa <= 3 && !sucesso) {
        tryCatch({
          h <- new_handle(timeout = 600)
          curl::curl_download(paste0(base_ftp, "/", arq), dest, quiet=TRUE, mode="wb", handle = h)
          if (file.info(dest)$size > 50000) sucesso <- TRUE else file.remove(dest)
        }, error = function(e) {})
        if(!sucesso) { Sys.sleep(2); tentativa <- tentativa + 1 }
      }
    }
    
    # 3.2. LEITURA E VARREDURA
    if (file.exists(dest) && file.info(dest)$size > 50000) {
      tryCatch({
        d <- read.dbc(dest) %>% janitor::clean_names()
        
        col_mun <- names(d)[grepl("munic_res|munic", names(d))][1]
        
        if(!is.na(col_mun)) {
          
          # VARREDURA UNIVERSAL
          d$IS_PEDESTRE <- FALSE
          d$IS_CICLISTA <- FALSE
          d$IS_MOTO     <- FALSE
          d$IS_AUTO     <- FALSE
          
          cols_teste <- names(d)
          
          for(cx in cols_teste) {
            valores <- tryCatch({
              str_to_upper(gsub("[^A-Z0-9]", "", as.character(d[[cx]])))
            }, error = function(e) return(NULL))
            
            if(!is.null(valores)) {
              d$IS_PEDESTRE <- d$IS_PEDESTRE | str_detect(valores, "^V0[1-9]")
              d$IS_CICLISTA <- d$IS_CICLISTA | str_detect(valores, "^V1[0-9]")
              d$IS_MOTO     <- d$IS_MOTO     | str_detect(valores, "^V2[0-9]|^V3[0-9]")
              d$IS_AUTO     <- d$IS_AUTO     | str_detect(valores, "^V[4-7][0-9]")
            }
          }
          
          d_classificado <- d %>%
            mutate(
              CATEGORIA = case_when(
                IS_PEDESTRE ~ "Trânsito - Pedestres (V01-V09)",
                IS_CICLISTA ~ "Trânsito - Ciclistas (V10-V19)",
                IS_MOTO     ~ "Trânsito - Motociclistas (V20-V39)",
                IS_AUTO     ~ "Trânsito - Ocup. Veíc. 4 Rodas (V40-V79)",
                TRUE ~ NA_character_
              )
            ) %>%
            filter(!is.na(CATEGORIA))
          
          if (nrow(d_classificado) > 0) {
            resumo_mes <- d_classificado %>%
              mutate(
                cod_mun = limpar_chave(!!sym(col_mun)),
                val_tot = if("val_tot" %in% names(d)) as.numeric(as.character(val_tot)) else 0,
                morte = if("morte" %in% names(d)) as.numeric(as.character(morte)) else 0
              ) %>%
              group_by(cod_mun, CATEGORIA) %>%
              summarise(
                internacoes = n(), 
                valor_total = sum(val_tot, na.rm=T), 
                obitos = sum(morte, na.rm=T), 
                .groups="drop"
              )
            lista_meses[[mes]] <- resumo_mes
            cat(".") 
          } else { cat("x") }
        }
      }, error = function(e) { cat("!") }) 
    } else { cat("!") }
  }
  
  resumo <- bind_rows(lista_meses)
  if (nrow(resumo) > 0) {
    resumo <- resumo %>%
      group_by(cod_mun, CATEGORIA) %>%
      summarise(internacoes=sum(internacoes), valor_total=sum(valor_total), obitos=sum(obitos), .groups="drop") %>%
      mutate(ano = ano)
    
    message(paste("\n      -> SUCESSO: Total de", sum(resumo$internacoes), "sinistros em", ano))
    return(resumo)
  } else {
    message(paste("\n      -> AVISO: Ano", ano, "zerado para trânsito."))
    return(NULL)
  }
}

# --- EXECUÇÃO (COM CACHE) ---
if (file.exists(CACHE_TRANSITO)) {
  message("   -> Cache encontrado! Carregando dados processados do disco...")
  DADOS_TRANSITO <- readRDS(CACHE_TRANSITO)
} else {
  message("   -> Cache não encontrado. Iniciando processamento de 2015 a 2024...")
  LISTA_ANOS <- lapply(2015:2024, processar_transito_ano)
  DADOS_TRANSITO <- bind_rows(LISTA_ANOS)
  
  if (nrow(DADOS_TRANSITO) == 0) {
    stop("ERRO: Nenhuma internação de trânsito encontrada.")
  } else {
    saveRDS(DADOS_TRANSITO, CACHE_TRANSITO)
    message("   -> Dados processados e salvos no cache!")
  }
}

# ==============================================================================
# 4. UNIFICAÇÃO, CÁLCULO DE MUNICÍPIOS E AGREGAÇÃO ESTADUAL
# ==============================================================================
message("\n>>> 3. Gerando Tabela Final (Municípios + Estado)...")

categorias_oficiais <- c(
  "Trânsito - Pedestres (V01-V09)",
  "Trânsito - Ciclistas (V10-V19)",
  "Trânsito - Motociclistas (V20-V39)",
  "Trânsito - Ocup. Veíc. 4 Rodas (V40-V79)"
)

# 4.1. TABELA DE MUNICÍPIOS (GRID COMPLETO)
grid <- expand.grid(
  ano = 2015:2024,
  cod_mun = unique(TAB_RRAS$cod_6_mun),
  CATEGORIA = categorias_oficiais,
  stringsAsFactors = FALSE
)

tabela_mun <- grid %>%
  left_join(TAB_RRAS, by = c("cod_mun" = "cod_6_mun")) %>%
  left_join(DADOS_TRANSITO, by = c("ano", "cod_mun", "CATEGORIA")) %>%
  mutate(
    internacoes = replace_na(internacoes, 0),
    valor_total = replace_na(valor_total, 0),
    obitos = replace_na(obitos, 0)
  ) %>%
  left_join(POP_TOTAL, by = c("ano", "cod_mun")) %>%
  mutate(
    pop_total = replace_na(pop_total, 0),
    valor_medio = ifelse(internacoes > 0, valor_total / internacoes, 0),
    taxa_mort_hosp = ifelse(internacoes > 0, (obitos / internacoes) * 100, 0),
    taxa_inter_100k = ifelse(pop_total > 0, (internacoes / pop_total) * 100000, 0)
  ) %>%
  select(
    ANO = ano, 
    CODMUNRES = cod_mun, 
    RRAS = rras_2025, 
    RS = nome_rs_2025, 
    Categoria = CATEGORIA, 
    Internacoes = internacoes, 
    Valor_Total = valor_total, 
    Valor_Medio = valor_medio, 
    Obitos = obitos, 
    Taxa_Mortalidade_Hosp = taxa_mort_hosp, 
    Taxa_Internacao_100k = taxa_inter_100k, 
    Populacao_Ref = pop_total
  )

# 4.2. TABELA TOTAL ESTADO DE SP
message("   -> Calculando Total do Estado...")

tabela_estado <- tabela_mun %>%
  group_by(ANO, Categoria) %>%
  summarise(
    Internacoes = sum(Internacoes, na.rm = TRUE),
    Valor_Total = sum(Valor_Total, na.rm = TRUE),
    Obitos = sum(Obitos, na.rm = TRUE),
    Populacao_Ref = sum(Populacao_Ref, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Recalcula indicadores para o total (não pode somar taxas!)
    Valor_Medio = ifelse(Internacoes > 0, Valor_Total / Internacoes, 0),
    Taxa_Mortalidade_Hosp = ifelse(Internacoes > 0, (Obitos / Internacoes) * 100, 0),
    Taxa_Internacao_100k = ifelse(Populacao_Ref > 0, (Internacoes / Populacao_Ref) * 100000, 0),
    
    # Preenche colunas geográficas para identificar o Total
    CODMUNRES = "35", # Código da UF
    RRAS = "ESTADO DE SAO PAULO",
    RS = "ESTADO DE SAO PAULO"
  ) %>%
  select(ANO, CODMUNRES, RRAS, RS, Categoria, 
         Internacoes, Valor_Total, Valor_Medio, Obitos, Taxa_Mortalidade_Hosp, Taxa_Internacao_100k, Populacao_Ref)

# 4.3. CONSOLIDAÇÃO FINAL (MUNICÍPIOS + ESTADO)
tabela_final_consolidada <- bind_rows(tabela_mun, tabela_estado)

# DIAGNÓSTICO E EXPORTAÇÃO
check <- sum(tabela_final_consolidada$Internacoes)
message(paste("\n>>> TOTAL GERAL TRÂNSITO (MUN + ESTADO):", format(check, big.mark=".")))

ts <- format(Sys.time(), "%H%M%S")
arq_excel <- paste0("Indicadores_Transito_SP_V2_ComEstado_", ts, ".xlsx")
writexl::write_xlsx(tabela_final_consolidada, arq_excel)
message(paste("Salvo Excel:", arq_excel))

arq_csv <- paste0("Indicadores_Transito_SP_V2_ComEstado_", ts, ".csv")
write.csv2(tabela_final_consolidada, arq_csv, row.names = FALSE, fileEncoding = "latin1")
message(paste("Salvo CSV:", arq_csv))
