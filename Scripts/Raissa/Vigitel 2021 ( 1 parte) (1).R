  #==============================================================================
  # SCRIPT VIGITEL SP 2021 (CÁLCULO DE PREVALÊNCIAS E EXPORTAÇÃO)
  # ==============================================================================
  # Indicadores: 
  # 1. Obesidade  e execesso de peso em adultos 
  # 2. Prática de atividade física no tempo livre 
  # 3. Consumo recomendado de frutas e hortaliças 
  # 4. Consumo de alimentos ultraprocessados 
  # 5. Consumo regular de bebidas adoçadas 
  #  ------------------------------------------------------------------------------
  # ---- INSTALAÇÃO E CARREGAMENTO DE BIBLIOTECAS ----
  # Verifica se os pacotes estão instalados, caso contrário, instala-os
  pacotes <- c("tidyverse", "readxl", "srvyr", "openxlsx", "janitor")
  instalados <- pacotes %in% installed.packages()[, "Package"]
  if (any(!instalados)) {
    install.packages(pacotes[!instalados])
  }
  
  library(tidyverse)    
  library(readxl)       
  library(srvyr)        
  library(openxlsx)     
  library(janitor)      # Para limpeza de nomes de colunas
  
  
  # ---- 1. CARREGAMENTO DE AMBIENTE ----
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, readxl, srvyr, openxlsx, janitor)
  
  # ---- 2. IMPORTAÇÃO E PADRONIZAÇÃO ----
  caminho_arquivo <- "C:/Users/raiss/OneDrive/Desktop/R/BD_Vigitel_pesorake_indicadores_TESTE.xlsx"
  
  if (!file.exists(caminho_arquivo)) {
    stop("Erro: Ficheiro não encontrado no caminho especificado.")
  }
  
  df_raw <- read_xlsx(caminho_arquivo) %>% 
    clean_names(case = "parsed") %>% 
    rename_all(toupper)
  
  # ---- 3. TRATAMENTO DE DADOS 
  
  colunas_disponiveis <- names(df_raw)
  peso_selecionado <- case_when(
    "PESORAKE"   %in% colunas_disponiveis ~ "PESORAKE",
    "VIGIPESO"   %in% colunas_disponiveis ~ "VIGIPESO",
    "PESO_FINAL" %in% colunas_disponiveis ~ "PESO_FINAL",
    TRUE ~ "PESO"
  )
  
  # Garantir coluna de gestante (Q78)
  if (!"Q78" %in% colunas_disponiveis) {
    df_raw$Q78 <- NA_real_
  }
  
  df_proc <- df_raw %>%
    mutate(
      # Tratamento de Missings Oficiais (777 = Não sabe, 888 = Recusa)
      across(any_of(c("Q6", "Q9", "Q11", "Q15", "Q16", "Q18", "Q20", "Q25", "Q26", "Q27", "Q28", "Q29", 
                      "Q42", "Q43A", "Q45", "Q46", "Q50", "Q51", "Q53", "Q54", "Q55", "Q56", "Q48", "Q49", "Q8_ANOS", "Q78")), 
             ~ as.numeric(if_else(.x >= 777, NA_real_, as.numeric(.x)))),
      
      PESO_VAR = as.numeric(.data[[peso_selecionado]]),
      IDADE = Q6,
      
      # --- ESTRATIFICADORES ---
      SEXO = factor(Q7, levels = c(1, 2), labels = c("Masculino", "Feminino")),
      
      FAIXA_ETARIA = case_when(
        Q6 %in% 18:24 ~ "18 a 24",
        Q6 %in% 25:34 ~ "25 a 34",
        Q6 %in% 35:44 ~ "35 a 44",
        Q6 %in% 45:54 ~ "45 a 54",
        Q6 %in% 55:64 ~ "55 a 64",
        Q6 >= 65      ~ "65 e mais",
        TRUE ~ NA_character_
      ),
      
      ESCOLARIDADE = case_when(
        Q8_ANOS %in% 0:8   ~ "0 a 8",
        Q8_ANOS %in% 9:11  ~ "9 a 11",
        Q8_ANOS >= 12      ~ "12 e mais",
        TRUE ~ NA_character_
      ),
      
      # --- ANTROPOMETRIA ---
      mulher_gravida = if_else(SEXO == "Feminino" & Q78 == 1, 1, 0, missing = 0),
      
      IMC_I_calc = case_when(
        mulher_gravida == 1 ~ NA_real_,
        is.na(Q9) | is.na(Q11) ~ NA_real_, # Exclui do denominador
        TRUE ~ Q9 / ((Q11 / 100)^2)
      ),
      
      # Filtro Oficial do Ministério da Saúde: Elimina valores biologicamente implausíveis do denominador
      IMC_I = if_else(IMC_I_calc < 15 | IMC_I_calc > 115, NA_real_, IMC_I_calc),
      
      # Espelhando o cond() do Stata: mantem NA para quem não respondeu/implausível, aplica 0 ou 1 para o resto
      excpeso_i = case_when(
        is.na(IMC_I) ~ NA_real_,
        IMC_I >= 25 ~ 1,
        TRUE ~ 0
      ),
      
      obesid_i = case_when(
        is.na(IMC_I) ~ NA_real_,
        IMC_I >= 30 ~ 1,
        TRUE ~ 0
      ),
      
      # --- DIETA / ALIMENTAÇÃO ---
      # Correção Crucial: "missing = 0" garante que se a pessoa pulou a pergunta porque não come, vira 0, igual no Stata.
      hortareg = if_else(Q16 %in% c(3, 4), 1, 0, missing = 0),
      frutareg = if_else(Q25 %in% c(3, 4) | Q27 %in% c(3, 4), 1, 0, missing = 0),
      flvreg   = if_else(frutareg == 1 & hortareg == 1, 1, 0, missing = 0),
      
      cru_p    = case_when(Q18 %in% 1:2 ~ 1, Q18 == 3 ~ 2, TRUE ~ 0),
      coz_p    = case_when(Q20 %in% 1:2 ~ 1, Q20 == 3 ~ 2, TRUE ~ 0),
      suc_p    = case_when(Q26 %in% 1:3 ~ 1, TRUE ~ 0),
      fru_p    = case_when(Q28 == 1 ~ 1, Q28 == 2 ~ 2, Q28 >= 3 ~ 3, TRUE ~ 0),
      
      flv_soma = cru_p + coz_p + suc_p + fru_p,
      
      # Correção: Adicionado o limite <= 8 do Stata
      flvreco  = if_else((flv_soma >= 5 & flv_soma <= 8) & flvreg == 1, 1, 0, missing = 0),
      
      feijao5  = if_else(Q15 %in% c(3, 4), 1, 0, missing = 0),
      refritl5 = if_else(Q29 %in% c(3, 4), 1, 0, missing = 0),
      
      # --- ULTRAPROCESSADOS ---
      # Transforma '2' (Não) em 0. '1' (Sim) continua 1.
      across(starts_with("R302_"), ~ if_else(.x == 1, 1, 0, missing = 0)),
      score_upp = rowSums(select(., starts_with("R302_")), na.rm = TRUE),
      score_upp_2cat = if_else(score_upp >= 5, 1, 0, missing = 0),
      
      # --- ATIVIDADE FÍSICA NO LAZER ---
      # Correção da Frequência
      freq_af = case_when(
        Q45 == 1 ~ 1.5, 
        Q45 == 2 ~ 3.5, 
        Q45 == 3 ~ 5.5, 
        Q45 == 4 ~ 7.0, 
        TRUE ~ 0
      ),
      
      # Correção Exata do Tempo do Stata
      time_af = case_when(
        Q46 == 1 ~ 0,
        Q46 == 2 ~ 14.5,
        Q46 == 3 ~ 24.5,
        Q46 == 4 ~ 34.5,
        Q46 == 5 ~ 44.5,
        Q46 == 6 ~ 54.5,
        Q46 == 7 ~ 60,
        TRUE ~ 0
      ),
      
      # Correção da Intensidade: 
      # Stata categorizou: Moderada (1)= 1,2,5,7,8,9,10,11,14,16,17 / Vigorosa (2)= 3,4,6,12,13,15
      intensidade = case_when(
        Q43A %in% c(3, 4, 6, 12, 13, 15) ~ 2, 
        Q43A %in% c(1, 2, 5, 7, 8, 9, 10, 11, 14, 16, 17) ~ 1, 
        TRUE ~ 0
      ),
      
      min_lazer = intensidade * freq_af * time_af,
      
      # O Stata avalia min_lazer. Se for nulo ou zero, é inativo (0).
      ativo_livre = if_else(min_lazer >= 150, 1, 0, missing = 0)
    )
  
  # ---- 4. DESENHO AMOSTRAL ----
  # Filtro oficial
  df_final <- df_proc %>% filter(!is.na(PESO_VAR) & IDADE >= 18)
  
  # Configuração do Design
  # Dica: No Vigitel costuma ser prudente lidar com variância de PSUs únicos, 
  # ative a opção abaixo se receber avisos de "lonely psu"
  options(survey.lonely.psu = "adjust") 
  
  dsn <- df_final %>%
    as_survey_design(ids = 1, weights = PESO_VAR)
  
  # ---- 5. FUNÇÃO DE CÁLCULO DE MATRIZ ----
  indicadores <- list(
    c("excpeso_i", "Excesso de Peso (IMC >= 25)"),
    c("obesid_i", "Obesidade (IMC >= 30)"),
    c("flvreg", "Consumo Regular de FLV"),
    c("flvreco", "Consumo Recomendado de FLV"),
    c("feijao5", "Consumo Regular de Feijão"),
    c("refritl5", "Consumo Regular de Refrigerantes"),
    c("score_upp_2cat", "Consumo 5+ Grupos Ultraprocessados"),
    c("ativo_livre", "Ativo no Lazer (150min+)")
  )
  
  calcular_matriz <- function(ind_info, group_vars) {
    ind_col <- ind_info[1]
    ind_label <- ind_info[2]
    
    res <- dsn %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        prev = survey_mean(.data[[ind_col]], na.rm = TRUE, vartype = "ci")
      )
    
    cat_principal <- group_vars[1]
    has_sub <- length(group_vars) > 1
    
    res %>%
      mutate(
        Indicador = ind_label,
        Prevalencia = round(prev * 100, 1),
        IC_95 = paste0(round(prev_low * 100, 1), " - ", round(prev_upp * 100, 1)),
        Estratificacao = paste(group_vars, collapse = " x "),
        Categoria = as.character(.data[[cat_principal]]),
        SubCategoria = if(has_sub) as.character(.data[[group_vars[2]]]) else "Total"
      ) %>%
      select(Indicador, Estratificacao, Categoria, SubCategoria, Prevalencia, IC_95)
  }
  
  # ---- 6. EXECUÇÃO E CONSOLIDAÇÃO ----
  list_simples <- map_dfr(indicadores, function(i) {
    # RRAS adicionado na estratificação simples
    map_dfr(c("SEXO", "FAIXA_ETARIA", "ESCOLARIDADE", "RRAS"), ~ calcular_matriz(i, .x))
  })
  
  list_cruzada <- map_dfr(indicadores, function(i) {
    bind_rows(
      calcular_matriz(i, c("FAIXA_ETARIA", "SEXO")),
      calcular_matriz(i, c("ESCOLARIDADE", "SEXO")),
      calcular_matriz(i, c("RRAS", "SEXO")) # Adicionado cruzamento por RRAS e Sexo
    )
  })
  
  matriz_final <- bind_rows(list_simples, list_cruzada)
  
  # ---- 7. EXPORTAÇÃO E VISUALIZAÇÃO ----
  
  # Mostrar a tabela gerada no ecrã/console do R
  print(matriz_final)
  
  # Se estiver a utilizar o RStudio, remova o '#' da linha abaixo para visualizar a tabela numa nova janela
  # View(matriz_final)
  
  # Exportar diretamente para a sua pasta do Ambiente de Trabalho
  write.xlsx(matriz_final, "C:/Users/raiss/OneDrive/Desktop/R/Matriz_Vigitel_2021_Estatistica.xlsx")
  
  print("Processamento finalizado com espelhamento exato da sintaxe Stata e estratificação por RRAS incluída.")