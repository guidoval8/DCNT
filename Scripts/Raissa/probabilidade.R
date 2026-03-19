# SCRIPT REFORMULADO – DATASUS (SIM + POPSVS)
# Taxas Padronizadas e Probabilidade Incondicional de Morte
# Série histórica 2015–2024 – Estado de São Paulo
#
# v3.5: Correção forçada de cache. Remove DENOMINADOR_cache.rds para garantir que 
# a coluna FAIXA_ETARIA_5ANOS seja gerada corretamente.
# ============================================================

# ------------------------------
# 1. PACOTES NECESSÁRIOS, CONFIGURAÇÃO E CONSTANTES GLOBAIS
# ------------------------------

# Lista de pacotes necessários
pacotes_necessarios <- c(
  "dplyr", "tidyr", "lubridate", "stringr", "readxl",
  "janitor", "read.dbc", "purrr", "microdatasus", "foreign",
  "stringi", "writexl", "readr"
)

# Instala pacotes ausentes e carrega todos
pacotes_ausentes <- pacotes_necessarios[!(pacotes_necessarios %in% installed.packages()[,"Package"])]
if (length(pacotes_ausentes) > 0) {
  message("Instalando pacotes ausentes: ", paste(pacotes_ausentes, collapse = ", "))
  install.packages(pacotes_ausentes)
}

# Carrega todas as bibliotecas necessárias silenciosamente
invisible(lapply(pacotes_necessarios, library, character.only = TRUE))



#ABIR A MINHA PASTA DE TRABALHOOO NO R 
setwd("C:/Users/raiss/OneDrive/Desktop/R")

# ============================================================
# 2. EXTRAÇÃO E PRÉ-PROCESSAMENTO DOS ÓBITOS (SIM) – DATASUS
# === CRIA O OBJETO 'DCNT_BASE' (ESSENCIAL PARA UOP E TPM) ====
# ============================================================

# Define o caminho do cache para os dados do SIM
SIM_CACHE_PATH <- "SIM_filtrado_final_cache.rds"

# Função para filtrar e criar DCNT_BASE a partir do SIM_base
# DCNT_BASE contém todos os óbitos de 30 a 69 anos por DCNT (dados brutos)
filtrar_DCNT_BASE <- function(SIM_base) {
  SIM_base %>%
    # Filtra a faixa etária de 30 a 69 anos
    filter(IDADEanos >= 30 & IDADEanos < 70, !is.na(DTOBITO)) %>%
    # 2.2. Identifica e filtra a base de DCNT (Doenças Crônicas Não Transmissíveis)
    mutate(
      IS_DCNT = (CAUSABAS >= 'I00' & CAUSABAS <= 'I99') |
        (CAUSABAS >= 'C00' & CAUSABAS <= 'C97') |
        (CAUSABAS >= 'E10' & CAUSABAS <= 'E14') |
        (CAUSABAS >= 'J30' & CAUSABAS <= 'J98' & CAUSABAS != 'J36')
    ) %>%
    filter(IS_DCNT) %>%
    # Seleciona apenas as colunas essenciais
    select(ANOOBITO, SEXO, CODMUNRES, munResNome, IDADEanos, DTOBITO, CAUSABAS)
}

# Objeto para armazenar a base final de óbitos
SIM_filtrado_final <- NULL
DCNT_BASE <- NULL

# Tenta carregar do cache
if (file.exists(SIM_CACHE_PATH)) {
  message("Carregando dados do SIM do cache (SIM_filtrado_final_cache.rds)...")
  SIM_filtrado_final <- readRDS(SIM_CACHE_PATH)
  
  # Recria DCNT_BASE (dados brutos 30-69 DCNT) a partir do cache
  DCNT_BASE <- SIM_filtrado_final %>%
    mutate(
      IDADEanos = as.numeric(IDADEanos),
      DTOBITO = lubridate::ymd(DTOBITO)
    ) %>%
    # Filtra para garantir apenas a faixa 30-69 e colunas essenciais
    filter(IDADEanos >= 30 & IDADEanos < 70) %>%
    select(ANOOBITO, SEXO, CODMUNRES, munResNome, IDADEanos, DTOBITO, CAUSABAS)
  
} else {
  # Cache não encontrado, processa e salva
  message("Cache do SIM não encontrado. Baixando e processando dados (isso pode levar tempo)...")
  
  # Baixa dados brutos do SIM
  SIM <- microdatasus::fetch_datasus(
    year_start = 2015, year_end = 2024,
    uf = "SP", information_system = "SIM-DO"
  )
  
  # Processa o banco de dados do SIM para padronização
  SIM <- microdatasus::process_sim(SIM)
  
  # 2.1. Filtragem de idade e preparação de datas
  SIM_base <- SIM %>%
    mutate(
      IDADEanos = as.numeric(IDADEanos),
      DTOBITO = lubridate::ymd(DTOBITO), # Converte data
      ANOOBITO = lubridate::year(DTOBITO)
    )
  
  # Cria o DCNT_BASE (Óbitos de 30-69 anos por DCNT - RAW data para UOP)
  DCNT_BASE <- filtrar_DCNT_BASE(SIM_base)
  
  # --- Criação da base de óbitos agrupados (para Taxa Bruta) ---
  
  # 2.3. Criação dos grupos detalhados de DCNT (Para a Taxa Bruta - Numerador)
  df_DCNT_detalhado <- DCNT_BASE %>%
    mutate(
      GRUPO_DCNT = case_when(
        # Grupos básicos
        CAUSABAS >= 'I00' & CAUSABAS <= 'I99' ~ 'Circulatória (I00-I99)',
        CAUSABAS >= 'E10' & CAUSABAS <= 'E14' ~ 'Diabetes (E10-E14)',
        CAUSABAS >= 'J30' & CAUSABAS <= 'J98' & CAUSABAS != 'J36' ~ 'Respiratória Crônica (J30-J98 exceto J36)',
        
        # Detalhamento do Câncer
        CAUSABAS == 'C50' ~ 'Câncer - Mama (C50)',
        CAUSABAS == 'C53' ~ 'Câncer - Colo de Útero (C53)',
        # Câncer de Aparelho Digestivo
        (CAUSABAS >= 'C15' & CAUSABAS <= 'C26') |
          CAUSABAS == 'C45.1' | CAUSABAS == 'C48' | CAUSABAS == 'C77.2' |
          (CAUSABAS >= 'C78.4' & CAUSABAS <= 'C78.8') ~ 'Câncer - Aparelho Digestivo (C15-C26, C45, C48, C77, C78)',
        
        TRUE ~ 'OUTROS (Remover)'
      )
    ) %>%
    filter(GRUPO_DCNT != 'OUTROS (Remover)')
  
  # 2.4. Criação dos grupos totais adicionais via bind_rows
  
  # 2.4a. Grupo Total Câncer (Todos C00-C97)
  SIM_total_cancer <- DCNT_BASE %>%
    filter(CAUSABAS >= 'C00' & CAUSABAS <= 'C97') %>%
    mutate(GRUPO_DCNT = "Total Câncer (C00-C97)") %>%
    select(names(df_DCNT_detalhado))
  
  # 2.4b. Grupo Total DCNT (Todos os DCNT)
  SIM_todas_DCNT <- DCNT_BASE %>%
    mutate(GRUPO_DCNT = "Total DCNT") %>%
    select(names(df_DCNT_detalhado))
  
  # 2.5. Combina os grupos detalhados e os totais
  SIM_obitos_final_grupos <- bind_rows(
    df_DCNT_detalhado,
    SIM_total_cancer,
    SIM_todas_DCNT
  )
  
  # 2.7. Criação de estrato por sexo e total
  df_obitos_sexo <- SIM_obitos_final_grupos
  df_obitos_sexototal <- df_obitos_sexo %>%
    mutate(SEXO = "Total")
  
  # SIM filtrado e pronto para o join com o denominador (Crude Rate Numerator)
  SIM_filtrado_final <- bind_rows(df_obitos_sexo, df_obitos_sexototal)
  
  #---------------------------------------------------------------------
  # SALVA O CACHE
  saveRDS(SIM_filtrado_final, SIM_CACHE_PATH)
  message("Dados do SIM processados e salvos em SIM_filtrado_final_cache.rds.")
}

# Verifica se os dados do SIM foram carregados corretamente
if (is.null(SIM_filtrado_final) || is.null(DCNT_BASE)) {
  stop("Erro: O processamento do SIM falhou ou DCNT_BASE não foi criado. Verifique os logs de download.")
}

# ============================================================
# 3. INCORPORAÇÃO DE RRAS E REGIÕES DE SAÚDE
# ============================================================

RRAS_municipios_path <- "C:/Users/raiss/OneDrive/Desktop/R/RRAS/RRAS Municipios (1).xlsx"


# Checagem de existência do arquivo
if (!file.exists(RRAS_municipios_path)) {
  stop(paste("ERRO: Arquivo RRAS não encontrado em", RRAS_municipios_path,
             "\nPor favor, garanta que o arquivo 'RRAS_Municipios.xlsx' esteja na pasta de trabalho."))
}

RRAS_municipios <- readxl::read_excel(RRAS_municipios_path) %>%
  janitor::clean_names() %>%
  mutate(
    # Garante que o código do município seja texto
    cod_6_mun = as.character(cod_6_mun)
  )

# Junta dados de óbitos com RRAS e Região de Saúde (pelo CÓDIGO DO MUNICÍPIO)
SIM_com_RRAS <- left_join(SIM_filtrado_final, RRAS_municipios %>% select(cod_6_mun, rras_2025, nome_rs_2025),
                          by = c("CODMUNRES" = "cod_6_mun"))

# Contagem de óbitos por estrato (NUMERADOR) 
# (waldrey) Alterei FAIXA_ETARIA para IDADEanos
NUMERADOR_SIM <- SIM_com_RRAS %>%
  group_by(
    ANOOBITO, SEXO, CODMUNRES, rras_2025, nome_rs_2025, IDADEanos, GRUPO_DCNT
  ) %>%
  summarise(obitos = n(), .groups = "drop")


# ============================================================
# 4. POPULAÇÃO – FTP DATASUS / IBGE POPSVS (DENOMINADOR)
# ============================================================

POPSVS_CACHE_PATH <- "C:/Users/raiss/OneDrive/Desktop/R/DENOMINADOR_cache.rds"
usar_cache <- TRUE

# ----------------------------------------------------------------
# PASSO 1: Carregamento do Cache
# ----------------------------------------------------------------
if (usar_cache && file.exists(POPSVS_CACHE_PATH)) {
  
  message("✔ Cache encontrado! Carregando dados da população...")
  
  # O cache agora contém uma lista com DENOMINADOR (5 anos) e DENOMINADOR_TBM (agregado)
  cache_data <- readRDS(POPSVS_CACHE_PATH)
  DENOMINADOR <- cache_data$DENOMINADOR
  DENOMINADOR_TBM <- cache_data$DENOMINADOR_TBM
  DENOMINADOR_5ANOS <-cache_data$DENOMINADOR_5ANOS
  
} else {
  
  message("Cache ausente (ou uso desativado). Baixando POPSVS 2015–2024...")
  
  # ----------------------------------------------------------------
  # FUNÇÃO PARA BAIXAR E LER POPULAÇÃO DO POPSVS VIA FTP
  # ----------------------------------------------------------------
  ler_pop <- function(ano) {
    ftp_base <- "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/"
    nome_zip <- sprintf("POPSBR%02d.zip", ano - 2000)
    nome_dbf <- sprintf("POP%02d.dbf", ano - 2000)
    url_zip  <- paste0(ftp_base, nome_zip)
    temp_zip <- tempfile(fileext = ".zip")
    temp_dbf <- tempfile(fileext = ".dbf")
    
    tryCatch({
      download.file(url_zip, destfile = temp_zip, mode = "wb", quiet = TRUE)
      utils::unzip(temp_zip, exdir = tempdir())
      file.rename(file.path(tempdir(), nome_dbf), temp_dbf)
      
      # Remove a coluna FAIXA_ETARIA (10 anos) existente na base DBF para evitar conflito
      pop <- foreign::read.dbf(temp_dbf) %>% 
        dplyr::mutate(ANO = ano) %>%
        dplyr::select(-dplyr::any_of("FAIXA_ETARIA")) 
      return(pop)
      
    }, error = function(e) {
      warning(paste("Erro no ano", ano, ":", e$message))
      return(NULL)
    }, finally = {
      if (file.exists(temp_zip)) file.remove(temp_zip)
      if (file.exists(temp_dbf)) file.remove(temp_dbf)
    })
  }
  
  # ----------------------------------------------------------------
  # BAIXA POPULAÇÃO BRUTA 2015–2024
  # ----------------------------------------------------------------
  anos <- 2015:2024
  pop_bruta <- purrr::map_dfr(anos, ler_pop)
  
  # ----------------------------------------------------------------
  # CRIA BASE RAW DENOMINADOR (5 ANOS)
  # ----------------------------------------------------------------
  DENOMINADOR <- pop_bruta %>%
    mutate(
      IDADE = as.numeric(as.character(IDADE)),
      
      # FAIXA_ETARIA (5 anos) para o cálculo do UOP - ESSA É A CORRETA!
      FAIXA_ETARIA = case_when(
        IDADE >= 30 & IDADE < 35 ~ "30-34 anos",
        IDADE >= 35 & IDADE < 40 ~ "35-39 anos",
        IDADE >= 40 & IDADE < 45 ~ "40-44 anos",
        IDADE >= 45 & IDADE < 50 ~ "45-49 anos",
        IDADE >= 50 & IDADE < 55 ~ "50-54 anos",
        IDADE >= 55 & IDADE < 60 ~ "55-59 anos",
        IDADE >= 60 & IDADE < 65 ~ "60-64 anos",
        IDADE >= 65 & IDADE < 70 ~ "65-69 anos",
        TRUE ~ NA_character_
      ),
      
      # Padronização de Colunas
      CODMUNRES = stringr::str_sub(as.character(COD_MUN), 1, 6),
      ANOOBITO = as.double(ANO),
      SEXO = case_when(
        SEXO == "1" ~ "Masculino",
        SEXO == "2" ~ "Feminino",
        TRUE ~ NA_character_ 
      )
    ) %>%
    
    # Filtros Finais: Apenas Faixa 30-69, Apenas SP, Apenas Sexo definido
    filter(!is.na(FAIXA_ETARIA)) %>% 
    filter(stringr::str_starts(CODMUNRES, "35")) %>% 
    filter(!is.na(SEXO))
  
  # ----------------------------------------------------------------
  # CRIA BASE RAW DENOMINADOR_5ANOS (5 ANOS)
  # ----------------------------------------------------------------
  DENOMINADOR_5ANOS <- pop_bruta %>%
    mutate(
      IDADE = as.numeric(as.character(IDADE)),
      
      # FAIXA_ETARIA (5 anos) para o cálculo do UOP - ESSA É A CORRETA!
      FAIXA_ETARIA = case_when(
        IDADE >= 30 & IDADE < 35 ~ "30-34 anos",
        IDADE >= 35 & IDADE < 40 ~ "35-39 anos",
        IDADE >= 40 & IDADE < 45 ~ "40-44 anos",
        IDADE >= 45 & IDADE < 50 ~ "45-49 anos",
        IDADE >= 50 & IDADE < 55 ~ "50-54 anos",
        IDADE >= 55 & IDADE < 60 ~ "55-59 anos",
        IDADE >= 60 & IDADE < 65 ~ "60-64 anos",
        IDADE >= 65 & IDADE < 70 ~ "65-69 anos",
        TRUE ~ NA_character_
      ),
      
      # Padronização de Colunas
      CODMUNRES = stringr::str_sub(as.character(COD_MUN), 1, 6),
      ANOOBITO = as.double(ANO),
      SEXO = case_when(
        SEXO == "1" ~ "Masculino",
        SEXO == "2" ~ "Feminino",
        TRUE ~ NA_character_ 
      )
    ) %>%
    
    # Filtros Finais: Apenas Faixa 30-69, Apenas SP, Apenas Sexo definido
    filter(!is.na(FAIXA_ETARIA)) %>% 
    filter(stringr::str_starts(CODMUNRES, "35")) %>% 
    filter(!is.na(SEXO))
  
  # ----------------------------------------------------------------
  # CRIA BASE AGREGADA DENOMINADOR_TBM (PARA TAXA BRUTA DE MORTALIDADE)
  # ----------------------------------------------------------------
  # Agrega a população total de 30-69 anos por ano, para o cálculo da TBM
  DENOMINADOR_TBM <- DENOMINADOR %>%
    group_by(ANOOBITO) %>%
    summarise(populacao = sum(POP, na.rm = TRUE), .groups = 'drop') %>%
    ungroup() %>%
    mutate(
      FAIXA_ETARIA = "30-69 anos", # Rótulo para o resultado da TBM
      SEXO = "Total" # Rótulo para o resultado da TBM
    )
  

  
  # ----------------------------------------------------------------
  # SALVA CACHE: salva a lista dos dois objetos (raw e agregado)
  # ----------------------------------------------------------------
  saveRDS(list(DENOMINADOR = DENOMINADOR, DENOMINADOR_TBM = DENOMINADOR_TBM, DENOMINADOR_5ANOS = DENOMINADOR_5ANOS), POPSVS_CACHE_PATH)
  
  message("✔ Cache atualizado e salvo com sucesso!")
}

# ============================================================
# VERIFICAÇÃO FINAL
# ============================================================
if (!exists("DENOMINADOR")) {
  stop("❌ ERRO: Base DENOMINADOR (População 5 anos) não carregada corretamente!")
}

if (!exists("DENOMINADOR_TBM")) {
  stop("❌ ERRO: Base DENOMINADOR_TBM (População agregada) não carregada corretamente!")
}






# ============================================================
# 5. CÁLCULO DAS TAXAS  (TPM)
# ============================================================
# Dependências: NUMERADOR_SIM, DENOMINADOR, DENOMINADOR_TBM, pop_padrao_2010
# --------------------------------------------------------------

library(stringr)
library(dplyr)
library(tidyr)

# ----------------------------------------------------------------
# ETAPA 0: HARMONIZAÇÃO E INTEGRIDADE DE CHAVES - REFORÇADA
# ----------------------------------------------------------------

# Funções de limpeza
limpar_chave <- function(x) { str_trim(as.character(x)) }
limpar_numero <- function(x) { as.double(str_replace_all(as.character(x), ",", ".")) }

# Function to convert number of years into age range. (waldrey)
# Anteriormente essa parte do seu código estava filtrando como se fosse range de idades, mas na verdade ele é númerico
# logo precisei criar essa função para converter idade em faixa etaria para seguir fluxo existente do DENOMINADOR
faixa_etaria_10anos <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x_num) ~ NA_character_,
    x_num >= 30 & x_num <= 39 ~ "30-39 anos",
    x_num >= 40 & x_num <= 49 ~ "40-49 anos",
    x_num >= 50 & x_num <= 59 ~ "50-59 anos",
    x_num >= 60 & x_num <= 69 ~ "60-69 anos",
    TRUE ~ NA_character_
  )
}

# 0.1. Prepara e Filtra Óbitos (NUMERADOR_SIM)
NUMERADOR_SIM <- NUMERADOR_SIM %>%
  mutate(
    # Limpeza rigorosa das chaves
    CODMUNRES = limpar_chave(CODMUNRES),
    ANOOBITO  = limpar_chave(ANOOBITO),
    SEXO      = limpar_chave(SEXO),
    IDADEanos = faixa_etaria_10anos(IDADEanos),
    
    # Limpeza das chaves regionais
    rras_2025     = limpar_chave(rras_2025),
    nome_rs_2025  = limpar_chave(nome_rs_2025)
  ) %>%
  # Filtra chaves válidas e faixa etária 30-69
  filter(
    CODMUNRES != "" & !is.na(CODMUNRES),
    ANOOBITO  != "" & !is.na(ANOOBITO),
    SEXO      != "" & !is.na(SEXO),
    rras_2025     != "" & !is.na(rras_2025),
    nome_rs_2025  != "" & !is.na(nome_rs_2025),
    !is.na(IDADEanos)
  )


# 0.2. Prepara e Agrega População (DENOMINADOR)
DENOMINADOR <- DENOMINADOR %>%
  mutate(
    CODMUNRES = limpar_chave(CODMUNRES),
    ANOOBITO = limpar_chave(ANOOBITO),
    SEXO = case_when( # Mapeamento do SEXO
      limpar_chave(SEXO) == "1" ~ "Masculino",
      limpar_chave(SEXO) == "2" ~ "Feminino",
      TRUE ~ limpar_chave(SEXO)
    ),
    POP = limpar_numero(POP),
    
    # AGREGA IDADEanos de 5/5 anos para 10/10 anos (Chave de Junção)
    FAIXA_ETARIA_5_anos = limpar_chave(FAIXA_ETARIA),
    FAIXA_ETARIA = case_when(
      FAIXA_ETARIA_5_anos %in% c("30-34 anos", "35-39 anos") ~ "30-39 anos",
      FAIXA_ETARIA_5_anos %in% c("40-44 anos", "45-49 anos") ~ "40-49 anos",
      FAIXA_ETARIA_5_anos %in% c("50-54 anos", "55-59 anos") ~ "50-59 anos",
      FAIXA_ETARIA_5_anos %in% c("60-64 anos", "65-69 anos") ~ "60-69 anos",
      TRUE ~ FAIXA_ETARIA_5_anos
    )
  ) %>%
  # Filtra chaves válidas e agrega
  filter(CODMUNRES != "" & !is.na(CODMUNRES),
         ANOOBITO != "" & !is.na(ANOOBITO),
         SEXO != "" & !is.na(SEXO),
         FAIXA_ETARIA %in% c("30-39 anos", "40-49 anos", "50-59 anos", "60-69 anos")) %>%
  
  group_by(CODMUNRES, ANOOBITO, SEXO, FAIXA_ETARIA) %>%
  summarise(
    POP = sum(POP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

# ----------------------------------------------------------------
# ETAPA 1: CRIAÇÃO DA BASE TOTAL (Agregação de Óbitos)
# ----------------------------------------------------------------

# O NUMERADOR_SIM deve conter GRUPO_DCNT (os grupos específicos) e a contagem 'obitos'.
# Se 'CAUSABAS' foi removida antes, o NUMERADOR_SIM já está pronto para ser agregado.

# 1.1. Agrega Óbitos para o GRUPO TOTAL (Soma de todos os GRUPO_DCNT)
# Aqui, a soma é feita por município/demografia para obter o total de óbitos DCNT
# (waldrey) Aqui nessa parte alterei para IDADEanos já que não existe FAIXA_ETARIA
# no NUMERADOR_SIM
obitos_total <- NUMERADOR_SIM %>%
  # Agrupa por todas as chaves, exceto GRUPO_DCNT
  group_by(CODMUNRES, ANOOBITO, SEXO, IDADEanos, rras_2025, nome_rs_2025) %>%
  summarise(
    obitos = sum(obitos, na.rm = TRUE),
    GRUPO_DCNT = "TOTAL_DCNTs", # Define o grupo como "TOTAL"
    .groups = "drop"
  ) %>%
  ungroup()

# 1.2. Combina Óbitos por GRUPO_DCNT e o TOTAL
# Utilizamos apenas as colunas essenciais do NUMERADOR_SIM, evitando o erro do select(-CAUSABAS)
obitos_especificos <- NUMERADOR_SIM %>%
  select(CODMUNRES, ANOOBITO, SEXO, IDADEanos, rras_2025, nome_rs_2025, obitos, GRUPO_DCNT)

obitos_combinados <- bind_rows(
  obitos_especificos, # Base por GRUPO_DCNT específico
  obitos_total        # Base com a agregação TOTAL
) %>%
  distinct() 

# ----------------------------------------------------------------
# ETAPA 2: FUNÇÃO DE CÁLCULO GERAL (APLICADA A TODOS OS NÍVEIS GEOGRÁFICOS)
# ----------------------------------------------------------------

n_anos <- 10 # Intervalo de cada faixa etária (30-39, 40-49, 50-59, 60-69)

# Função para realizar a agregação, o recálculo da TEM e a fórmula da PMP
calcular_pmp_por_nivel <- function(obitos_base, pop_base, nivel_cols, nivel_nome, id_col, nome_col) {
  
  # Renomeia coluna para ficar igual à base DENOMINADOR
  obitos_base <- obitos_base %>% 
    rename(FAIXA_ETARIA = IDADEanos)
  
  if (nivel_nome != "Estado") {
    
    # 1. Agrega População (DENOMINADOR) no nível desejado
    pop_agregada <- pop_base %>%
      left_join(
        obitos_base %>% select(CODMUNRES, rras_2025, nome_rs_2025) %>% distinct(), 
        by = "CODMUNRES"
      ) %>%
      filter(!is.na(!!sym(nivel_cols[1]))) %>%
      group_by(ANOOBITO, SEXO, FAIXA_ETARIA, !!sym(nivel_cols[1])) %>%
      summarise(POP = sum(POP, na.rm = TRUE), .groups = "drop") %>%
      ungroup()
    
    # 2. Agrega os Óbitos (NUMERADOR) no nível desejado
    obitos_agregados <- obitos_base %>%
      group_by(ANOOBITO, SEXO, GRUPO_DCNT, FAIXA_ETARIA, !!sym(nivel_cols[1])) %>%
      summarise(obitos = sum(obitos, na.rm = TRUE), .groups = "drop") %>%
      ungroup()
    
    # 3. Junção no Nível Agregado
    grouping_keys <- c("ANOOBITO", "SEXO", "FAIXA_ETARIA", nivel_cols)
    
    base_calculo <- obitos_agregados %>%
      left_join(pop_agregada, by = grouping_keys)
    
    # Base agregada de População e Óbitos para exibir no final
    obitos_pop <- base_calculo %>%
      group_by(ANOOBITO, SEXO, GRUPO_DCNT, FAIXA_ETARIA, !!sym(nivel_cols[1])) %>%
      summarise(
        Total_Populacao = sum(POP, na.rm = TRUE),
        Total_Obitos = sum(obitos, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    # Nível Estadual
    pop_agregada <- pop_base %>%
      group_by(ANOOBITO, SEXO, FAIXA_ETARIA) %>%
      summarise(POP = sum(POP, na.rm = TRUE), .groups = "drop") %>%
      ungroup()
    
    obitos_agregados <- obitos_base %>%
      group_by(ANOOBITO, SEXO, GRUPO_DCNT, FAIXA_ETARIA) %>%
      summarise(obitos = sum(obitos, na.rm = TRUE), .groups = "drop") %>%
      ungroup()
    
    grouping_keys <- c("ANOOBITO", "SEXO", "FAIXA_ETARIA")
    base_calculo <- obitos_agregados %>%
      left_join(pop_agregada, by = grouping_keys)
    
    obitos_pop <- base_calculo %>%
      group_by(ANOOBITO, SEXO, GRUPO_DCNT, FAIXA_ETARIA) %>%
      summarise(
        Total_Populacao = sum(POP, na.rm = TRUE),
        Total_Obitos = sum(obitos, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # 4. Recalcula a TEM (Taxa Específica de Mortalidade) no nível de agregação
  base_recalculo <- base_calculo %>%
    filter(!is.na(POP), POP > 0) %>%
    mutate(
      TEM = (obitos / POP) * 100000,
      TEM_ajustada = TEM / 100000,
      qx = (TEM_ajustada * n_anos) / (1 + (TEM_ajustada * 0.5 * n_anos)),
      px = 1 - qx
    )
  
  # 5. Calcula o Produto px
  final_grouping_keys <- if (nivel_nome == "Estado") c("ANOOBITO", "SEXO", "GRUPO_DCNT") else c("ANOOBITO", "SEXO", "GRUPO_DCNT", nivel_cols)
  
  pmp_final <- base_recalculo %>%
    group_by(!!!syms(final_grouping_keys)) %>%
    summarise(
      produto_px = prod(px, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup()
  
  # Junta Total_Populacao e Total_Obitos para exibir
  if (nivel_nome == "Estado") {
    obitos_totais_join <- obitos_pop %>%
      group_by(ANOOBITO, SEXO, GRUPO_DCNT) %>%
      summarise(
        Total_Populacao = sum(Total_Populacao, na.rm = TRUE),
        Total_Obitos = sum(Total_Obitos, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    obitos_totais_join <- obitos_pop %>%
      group_by(ANOOBITO, SEXO, GRUPO_DCNT, !!sym(nivel_cols[1])) %>%
      summarise(
        Total_Populacao = sum(Total_Populacao, na.rm = TRUE),
        Total_Obitos = sum(Total_Obitos, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # 6. Calcula PMP Incondicional Final e adiciona Total_Populacao e Total_Obitos
  pmp_final %>%
    left_join(obitos_totais_join, by = final_grouping_keys) %>%
    mutate(
      Taxa_Calculada = (1 - produto_px) * 100, # Resultado em %
      Nivel_Geografico = nivel_nome,
      ID_Localidade = if (nivel_nome == "Estado") "35" else !!sym(id_col),
      Nome_Localidade = if (nivel_nome == "Estado") "São Paulo" else !!sym(nome_col),
      FAIXA_ETARIA = "30-69 anos",
      Tipo_Taxa = "PMP_Incondicional"
    ) %>%
    filter(!is.na(Taxa_Calculada), Taxa_Calculada >= 0) %>%
    select(Nivel_Geografico, ID_Localidade, Nome_Localidade, ANOOBITO, 
           SEXO, GRUPO_DCNT, Taxa_Calculada, FAIXA_ETARIA, Tipo_Taxa,
           Total_Populacao, Total_Obitos)
}


# ----------------------------------------------------------------
# ETAPA 3: CÁLCULO PMP POR NÍVEL GEOGRÁFICO (RRAS, RS, ESTADO)
# ----------------------------------------------------------------

# 1. Nível RRAS PMP
pmp_rras <- calcular_pmp_por_nivel(
  obitos_combinados, DENOMINADOR, 
  nivel_cols = c("rras_2025"), 
  nivel_nome = "RRAS", 
  id_col = "rras_2025", 
  nome_col = "rras_2025"
)

# 2. Nível Região de Saúde (RS) PMP
pmp_rs <- calcular_pmp_por_nivel(
  obitos_combinados, DENOMINADOR, 
  nivel_cols = c("nome_rs_2025"), 
  nivel_nome = "Região de Saúde", 
  id_col = "nome_rs_2025", 
  nome_col = "nome_rs_2025"
)

# 3. Nível Estadual PMP
pmp_estado <- calcular_pmp_por_nivel(
  obitos_combinados, DENOMINADOR, 
  nivel_cols = c(), 
  nivel_nome = "Estado", 
  id_col = "ID_Localidade", 
  nome_col = "Nome_Localidade"
)

# 4. Consolidação da base final de PMP (apenas RRAS, RS e Estado)
TAXAS_FIMAIS_FORMATADAS <- bind_rows(pmp_estado, pmp_rras, pmp_rs)

message("✅ FINALIZADO: O erro 'CAUSABAS' foi corrigido. A base TAXAS_FIMAIS_FORMATADAS foi gerada contendo a PMP Incondicional para os GRUPOS ESPECÍFICOS e o TOTAL, nos níveis RRAS, Região de Saúde e Estado. Verifique o preenchimento dos dataframes pmp_rras, pmp_rs e pmp_estado.")



# ============================================================
# VERIFICAÇÃO DE MISSING E CONSISTÊNCIA DA PMP
# ============================================================
# Objetivo: Garantir que não há valores ausentes (NA) ou inconsistentes (NaN/Inf)
# na base final TAXAS_FIMAIS_FORMATADAS.

library(dplyr)
library(tidyr)
library(stringr)

# 1. VERIFICAÇÃO GERAL DE MISSING (NA)
# Contagem de NAs por coluna na base final
missing_por_coluna <- TAXAS_FIMAIS_FORMATADAS %>%
  summarise(across(everything(), ~sum(is.na(.)), .names = "NA_em_{.col}")) %>%
  pivot_longer(everything(), names_to = "Variavel", values_to = "Total_NA") %>%
  filter(Total_NA > 0)

if (nrow(missing_por_coluna) > 0) {
  message("⚠️ ALERTA: Foram encontrados valores NA nas seguintes colunas:")
  print(missing_por_coluna)
} else {
  message("✅ SUCESSO: Nenhuma coluna em TAXAS_FIMAIS_FORMATADAS possui valores NA (Missing).")
}

# 2. VERIFICAÇÃO DE VALORES INCONSISTENTES (NaN, Inf) NA TAXA CALCULADA
# O cálculo da PMP pode gerar NaN (0/0) ou Inf (X/0) se a população for 0, 
# embora tenhamos filtrado a população > 0 nas etapas anteriores.
inconsistencias_taxa <- TAXAS_FIMAIS_FORMATADAS %>%
  filter(is.nan(Taxa_Calculada) | is.infinite(Taxa_Calculada)) %>%
  # Limita a 10 observações para visualização, se houver muitas
  head(10) 

if (nrow(inconsistencias_taxa) > 0) {
  message("❌ ERRO GRAVE: Foram encontrados valores NaN ou Inf em Taxa_Calculada. Verifique as 10 primeiras ocorrências:")
  print(inconsistencias_taxa)
} else {
  message("✅ SUCESSO: A coluna Taxa_Calculada não possui valores NaN ou Inf.")
}

# 3. VERIFICAÇÃO DE TAXAS INVÁLIDAS (Taxa < 0 ou Taxa > 100)
# Como a PMP é uma probabilidade entre 0 e 1, a Taxa em porcentagem deve ser entre 0 e 100.
taxas_invalidas <- TAXAS_FIMAIS_FORMATADAS %>%
  filter(Taxa_Calculada < 0 | Taxa_Calculada > 100) %>%
  # Limita a 10 observações para visualização
  head(10)

if (nrow(taxas_invalidas) > 0) {
  message("⚠️ ALERTA: Foram encontradas taxas calculadas fora do intervalo [0, 100]%. Verifique as 10 primeiras ocorrências:")
  print(taxas_invalidas)
} else {
  message("✅ SUCESSO: Todas as taxas calculadas estão no intervalo esperado [0, 100]%.")
}

message("\nRelatório de Verificação de Missing e Consistência Concluído.")

# ============================================================
# 6. CÁLCULO DA PROBABILIDADE INCONDICIONAL DE MORTE (UOP)
# ============================================================
DADOS_BASE <- TAXAS_FIMAIS_FORMATADAS

# ==============================================================================
# PASSO 2: PREPARAÇÃO (GARANTIR TIPOS DE DADOS E NOMES)
# ==============================================================================

# ⚠️ AJUSTE CRÍTICO: Confirme os nomes das colunas de ÓBITOS e POPULAÇÃO
# Use os nomes EXATOS das colunas do seu arquivo
COL_OBITOS <- "Total_Obitos"       # Ajuste se for diferente (ex: "OBITOS")   
COL_POPULACAO <- "Total_Populacao" # Ajuste se for diferente (ex: "populacao")

# Chaves de estratificação (colunas usadas para agrupar o cálculo)
# Mantenha os nomes das colunas da sua base (ex: "Nivel_Geografico", "Nome_Localidade", "ANOOBITO", etc.)
CHAVES_ESTRATIFICACAO <- c(
  "Nivel_Geografico", "ID_Localidade", "Nome_Localidade", "ANOOBITO", 
  "SEXO", "GRUPO_DCNT", "FAIXA_ETARIA", "Tipo_Taxa"
)

# Se os nomes das suas colunas de ÓBITOS e POPULAÇÃO forem as originais do DENOMINADOR, 
# você pode precisar de ajustes diferentes.

# Se a base carregada (DADOS_BASE) for uma das bases PMP que você tem na imagem, 
# podemos tentar usar as colunas padrões que parecem estar no seu ambiente:
# COL_OBITOS <- "Total_Obitos"
# COL_POPULACAO <- "Total_Populacao" 

# No entanto, se a base for a DCNT_BASE, os nomes são:
# COL_OBITOS <- "OBITOS" # Supondo que você tenha renomeado para OBITOS
# COL_POPULACAO <- "POP"  # Supondo que você tenha renomeado para POP
# Por segurança, vou usar os nomes 'Total_Obitos' e 'Total_Populacao' que são comuns após a agregação.
# Se o script der erro na coluna, volte e mude as duas linhas acima.

# Converte colunas para o tipo correto e garante nomes (usando !!sym para referenciar as variáveis de string)
DADOS_PREPARADOS <- DADOS_BASE %>%
  
  # Converte as colunas numéricas essenciais
  mutate(
    !!COL_OBITOS := as.numeric(!!sym(COL_OBITOS)),
    !!COL_POPULACAO := as.numeric(!!sym(COL_POPULACAO))
  ) %>%
  
  # Remove linhas com valores NA nos campos críticos (Obitos, População e Chaves)
  filter(
    !is.na(!!sym(COL_OBITOS)),
    !is.na(!!sym(COL_POPULACAO)),
    # Verifica NA em todas as colunas de estratificação
    across(all_of(CHAVES_ESTRATIFICACAO), ~!is.na(.))
  )

message(paste0("Linhas após preparação e limpeza: ", nrow(DADOS_PREPARADOS)))

# ==============================================================================
# PASSO 3: CÁLCULO DA PROBABILIDADE DE MORTE (qx)
# ==============================================================================

# Se a base já estiver agregada, pulamos o group_by/summarise.
# Se a base for uma base desagregada (como DCNT_BASE), faremos a agregação.
# Para evitar erro, vamos tentar agregar de novo.

TAXAS_AGREGADAS <- DADOS_PREPARADOS %>%
  group_by(across(all_of(CHAVES_ESTRATIFICACAO))) %>%
  
  summarise(
    Total_Obitos = sum(!!sym(COL_OBITOS), na.rm = TRUE),
    Total_Populacao = sum(!!sym(COL_POPULACAO), na.rm = TRUE),
    .groups = 'drop'
  )

message(paste0("Total de Combinações de Taxas Agregadas: ", nrow(TAXAS_AGREGADAS)))

# 2. Cálculo da PMP (%) e da Probabilidade de Morte (qx)
TAXAS_FIMAIS_EXPORT <- TAXAS_AGREGADAS %>%
  # Limpa casos onde a população é 0 (para evitar divisão por zero)
  filter(Total_Populacao > 0) %>%
  
  mutate(
    # Cálculo da Taxa de Mortalidade (PMP - Proporção de Mortalidade Prematura)
    Taxa_Calculada = (Total_Obitos / Total_Populacao) * 100,
    
    # Cálculo da Probabilidade de Morte (qx) - PMP em decimal
    Prob_Morte = Taxa_Calculada / 100
  ) %>%
  
  # Ajustes de precisão
  mutate(
    Taxa_Calculada = round(Taxa_Calculada, 6),
    Prob_Morte = round(Prob_Morte, 8)
  )

message(paste0("Total de Taxas Finais Calculadas: ", nrow(TAXAS_FIMAIS_EXPORT)))


# ==============================================================================
# PASSO 4: EXPORTAÇÃO FINAL
# ==============================================================================
data_atual <- format(Sys.Date(), "%Y%m%d")
nome_arquivo_saida <- paste0("TAXAS_PMP_FINAL_ESTRATIFICADAS_QX_", data_atual, ".csv")

# Exportação para CSV (será salvo no seu diretório de trabalho)
write_csv(
  TAXAS_FIMAIS_EXPORT,
  file = nome_arquivo_saida,
  na = "", 
  append = FALSE
)

message(paste0("\n✅ SUCESSO: O cálculo $q_x$ foi realizado e a base final foi exportada para: ", nome_arquivo_saida))
message("\nPrimeiras 5 linhas da base final:")
print(head(TAXAS_FIMAIS_EXPORT))
#-----------------------------------------
#---------------------------
# 6.1. Reagrupamento do Numerador (Óbitos) em faixas de 5 anos (apenas DCNT Total)

# 1.1. Agrupa e limpa a base de Óbitos (DCNT_BASE)
obitos_uop <- DCNT_BASE %>%
  mutate(
    FAIXA_ETARIA_5ANOS = case_when(
      IDADEanos >= 30 & IDADEanos < 35 ~ "30-34 anos",
      IDADEanos >= 35 & IDADEanos < 40 ~ "35-39 anos",
      IDADEanos >= 40 & IDADEanos < 45 ~ "40-44 anos",
      IDADEanos >= 45 & IDADEanos < 50 ~ "45-49 anos",
      IDADEanos >= 50 & IDADEanos < 55 ~ "50-54 anos",
      IDADEanos >= 55 & IDADEanos < 60 ~ "55-59 anos",
      IDADEanos >= 60 & IDADEanos < 65 ~ "60-64 anos",
      IDADEanos >= 65 & IDADEanos < 70 ~ "65-69 anos",
      TRUE ~ NA_character_
    ),
    # (waldrey) Ignorei essa filtragem pois já está no padrão
    # Recodifica SEXO apenas para filtrar NA's
    #SEXO_FILTRO = case_when(
    #  SEXO == 1 ~ "Masculino",
    #  SEXO == 2 ~ "Feminino",
    #  TRUE ~ NA_character_
    #)
  ) %>%
  filter(!is.na(FAIXA_ETARIA_5ANOS)) %>%
  filter(!is.na(SEXO)) %>% # Remove óbitos com sexo NA
  select(ANOOBITO, CODMUNRES, FAIXA_ETARIA_5ANOS) # *** VARIÁVEL SEXO REMOVIDA ***

# 1.2. Contagem de Óbitos (Numerador UOP) por Regional
NUMERADOR_UOP <- obitos_uop %>%
  left_join(RRAS_municipios %>% select(cod_6_mun, rras_2025, nome_rs_2025),
            by = c("CODMUNRES" = "cod_6_mun")) %>%
  # Agrega todos os óbitos (total de sexos) por Regional
  group_by(ANOOBITO, rras_2025, nome_rs_2025, FAIXA_ETARIA_5ANOS) %>% 
  summarise(obitos_5anos = n(), .groups = "drop")


# --- 2. PREPARAÇÃO E AGREGAÇÃO DO DENOMINADOR (POPULAÇÃO) ---

# 2.1. Cria faixas e códigos na base Populacional (DENOMINADOR_base)
# (waldrey) Raissa, aqui altrei de DENOMINADOR_base para DCNT_BASE
DENOMINADOR_UOP_base_pop <- DENOMINADOR_5ANOS %>%
  mutate(
    CODMUNRES = str_sub(as.character(CODMUNRES), 1, 6),
    ANOOBITO = as.double(ANOOBITO),
    # (waldrey) Ignorei essa filtragem pois já está no padrão
    #SEXO_FILTRO = case_when( # Recodifica SEXO apenas para filtro
    #  SEXO == "1" ~ "Masculino",
    #  SEXO == "2" ~ "Feminino",
    #  TRUE ~ NA_character_
    #),
    FAIXA_ETARIA_5ANOS = case_when( # Usa a coluna IDADE
      IDADE >= 30 & IDADE < 35 ~ "30-34 anos",
      IDADE >= 35 & IDADE < 40 ~ "35-39 anos",
      IDADE >= 40 & IDADE < 45 ~ "40-44 anos",
      IDADE >= 45 & IDADE < 50 ~ "45-49 anos",
      IDADE >= 50 & IDADE < 55 ~ "50-54 anos",
      IDADE >= 55 & IDADE < 60 ~ "55-59 anos",
      IDADE >= 60 & IDADE < 65 ~ "60-64 anos",
      IDADE >= 65 & IDADE < 70 ~ "65-69 anos",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FAIXA_ETARIA_5ANOS)) %>%
  filter(str_starts(CODMUNRES, "35")) %>% # Apenas SP
  filter(!is.na(SEXO)) %>% # Exclui população com sexo NA
  left_join(
    RRAS_municipios %>%
      rename(CODMUNRES = cod_6_mun) %>%
      select(CODMUNRES, rras_2025, nome_rs_2025),
    by = "CODMUNRES"
  )

# 2.2. Soma População (Denominador UOP) por Regional
DENOMINADOR_UOP <- DENOMINADOR_UOP_base_pop %>%
  # Agrega toda a população (total de sexos) por Regional
  group_by(ANOOBITO, rras_2025, nome_rs_2025, FAIXA_ETARIA_5ANOS) %>% 
  summarise(populacao_5anos = sum(POP, na.rm = TRUE), .groups = "drop")


# --- 3. CÁLCULO DA PROBABILIDADE INCONDICIONAL DE MORTE (UOP) - REGIONAL ---

# 3.1. Cálculo da Taxa Específica (mx) e Probabilidade (qx)
df_uop_calc <- left_join(NUMERADOR_UOP, DENOMINADOR_UOP,
                         by = c("ANOOBITO", "rras_2025", "nome_rs_2025", "FAIXA_ETARIA_5ANOS")) %>% 
  filter(populacao_5anos > 0, !is.na(populacao_5anos)) %>%
  mutate(
    # 1. Taxa de mortalidade específica por idade (mx)
    mx = obitos_5anos / populacao_5anos,
    # 2. Probabilidade de morte (qx)
    qx = (mx * 5) / (1 + mx * 2.5)
  )

# 3.2. UOP Final por Regional (Total de Sexos)
probabilidade_incondicional_morte <- df_uop_calc %>%
  group_by(ANOOBITO, rras_2025, nome_rs_2025) %>% 
  summarise(
    produto_sobrevivencia = prod(1 - qx, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    probabilidade_incondicional_morte = 1 - produto_sobrevivencia
  ) %>%
  select(ANOOBITO, rras_2025, nome_rs_2025, probabilidade_incondicional_morte)


# --- 4. CÁLCULO DA PROBABILIDADE INCONDICIONAL DE MORTE (UOP) - ESTADUAL ---

# 4.1. Agrega Óbitos (Numerador Estadual)
NUMERADOR_ESTADO <- obitos_uop %>%
  group_by(ANOOBITO, FAIXA_ETARIA_5ANOS) %>% 
  summarise(obitos_5anos = n(), .groups = "drop")

# 4.2. Agrega População (Denominador Estadual)
DENOMINADOR_ESTADO <- DENOMINADOR_UOP_base_pop %>%
  group_by(ANOOBITO, FAIXA_ETARIA_5ANOS) %>% 
  summarise(populacao_5anos = sum(POP, na.rm = TRUE), .groups = "drop")

# 4.3. Cálculo Final da UOP para o Estado (Total de Sexos)
probabilidade_incondicional_morte_estado <- left_join(NUMERADOR_ESTADO, DENOMINADOR_ESTADO,
                                                      by = c("ANOOBITO", "FAIXA_ETARIA_5ANOS")) %>%
  filter(populacao_5anos > 0, !is.na(populacao_5anos)) %>%
  mutate(
    mx = obitos_5anos / populacao_5anos,
    qx = (mx * 5) / (1 + mx * 2.5)
  ) %>%
  group_by(ANOOBITO) %>% 
  summarise(
    produto_sobrevivencia = prod(1 - qx, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    probabilidade_incondicional_morte = 1 - produto_sobrevivencia,
    Estado = "SP - Total"
  ) %>%
  select(ANOOBITO, Estado, probabilidade_incondicional_morte)


# --- 5. EXPORTAÇÃO DOS RESULTADOS ---

# Exporta os resultados para um único arquivo Excel (.xlsx) com duas abas.
# Você precisa ter o pacote 'writexl' instalado para isso.

# Junte os resultados finais em uma lista
lista_resultados_finais <- list(
  UOP_Regional = probabilidade_incondicional_morte,
  UOP_Estadual = probabilidade_incondicional_morte_estado
)




# ==============================================================================
# ETAPA ANÁLISE DE META DA OMS (REDUÇÃO DE 2% AO ANO BASE 2015)
# ==============================================================================
message("🔄 Calculando Meta OMS para UOP...")

if (!exists("probabilidade_incondicional_morte")) stop("❌ ERRO: 'probabilidade_incondicional_morte' não encontrado.")

# --- REGIONAL (Usando nome da imagem) ---
base_2015_reg <- probabilidade_incondicional_morte %>%
  filter(ANOOBITO == 2015) %>%
  select(rras_2025, nome_rs_2025, probabilidade_incondicional_morte) %>%
  rename(Prob_Base_2015 = probabilidade_incondicional_morte)

probabilidade_incondicional_morte_meta <- probabilidade_incondicional_morte %>%
  left_join(base_2015_reg, by = c("rras_2025", "nome_rs_2025")) %>%
  mutate(
    Prob_Esperada_OMS = Prob_Base_2015 * (1 - 0.02)^(ANOOBITO - 2015),
    Atingiu_Meta = ifelse(probabilidade_incondicional_morte <= Prob_Esperada_OMS, "Sim", "Não")
  )

# --- ESTADUAL (Usando nome da imagem) ---
base_2015_est <- probabilidade_incondicional_morte_estado %>%
  filter(ANOOBITO == 2015) %>%
  select(Estado, probabilidade_incondicional_morte) %>%
  rename(Prob_Base_2015 = probabilidade_incondicional_morte)

probabilidade_incondicional_morte_estado_meta <- probabilidade_incondicional_morte_estado %>%
  left_join(base_2015_est, by = "Estado") %>%
  mutate(
    Prob_Esperada_OMS = Prob_Base_2015 * (1 - 0.02)^(ANOOBITO - 2015),
    Atingiu_Meta = ifelse(probabilidade_incondicional_morte <= Prob_Esperada_OMS, "Sim", "Não")
  )

message("✅ Metas aplicadas!")

# ==============================================================================
# ETAPA FINAL: EXPORTAÇÃO COMPLETA
# ==============================================================================
data_atual <- format(Sys.Date(), "%Y%m%d")
arquivo_saida <- paste0("RESULTADOS_COMPLETOS_PMP_UOP_", data_atual, ".xlsx")

lista_saida <- list(
  "PMP_WHO_30a69" = TAXAS_FIMAIS_FORMATADAS, 
  "probabilidade_incondicional_morte_meta" = probabilidade_incondicional_morte_meta,
  "probabilidade_incondicional_morte_estado_meta" = probabilidade_incondicional_morte_estado_meta
)

write_xlsx(lista_saida, arquivo_saida)

message(paste0("\n🎉 PROCESSO CONCLUÍDO! Arquivo salvo em: ", arquivo_saida))
