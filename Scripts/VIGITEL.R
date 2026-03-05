library(haven)
library(dplyr)
library(readxl)
library(survey)
library(labelled)
library(janitor)
library(summarytools)
library(openxlsx)
library(gmodels)

#----IMPORTANDO BASE----#
df <- read_xlsx("C:\\R\\DCNT\\VIGITEL\\BD_Vigitel-SP_16-12-2021.xlsx") 

names(df) <- toupper(names(df))

#--------------------------------------#
#---FAIXA ETÁRIA E ESCOLARIDADE GERAL--#
#--------------------------------------#

#Faixa Etária Padrão VIGITEL (documento 2023)

df <- df %>%
  mutate(
    faixa_etaria = case_when(
      Q6 >= 18 & Q6 <= 24 ~ 1,
      Q6 >= 25 & Q6 <= 34 ~ 2,
      Q6 >= 35 & Q6 <= 44 ~ 3,
      Q6 >= 45 & Q6 <= 54 ~ 4,
      Q6 >= 55 & Q6 <= 64 ~ 5,
      Q6 >= 65 ~ 6,
      TRUE ~ NA_real_
    ),
    faixa_etaria = factor(faixa_etaria, 
                          levels = 1:6, 
                          labels = c("18 a 24", "25 a 34", "35 a 44", 
                                     "45 a 54", "55 a 64", "65+")),
    
    #Escolaridade
    escolaridade = case_when(
      Q8_ANOS >= 0 & Q8_ANOS <= 8 ~ 1,  
      Q8_ANOS >= 9 & Q8_ANOS <= 11 ~ 2,  
      Q8_ANOS >= 12 ~ 3,            
      TRUE ~ NA_real_
    ),
    escolaridade = factor(escolaridade, 
                          levels = 1:3, 
                          labels = c("0 a 8 anos", "9 a 11 anos", "12 ou mais anos"))
  )

#-------------#
#--TABAGISMO--#
#-------------#
df <- df %>%
  mutate(
    fumante = case_when(Q60 < 3 & !is.na(Q60) ~ 1, TRUE ~ 0),
    mais20 = case_when(Q61_FX >= 5 & Q61_FX <= 7 ~ 1, TRUE ~ 0),
    fumocasa = case_when(fumante == 1 ~ 0, Q67 == 1 ~ 1, TRUE ~ 0),
    fumotrab = case_when(fumante == 1 ~ 0, Q68 == 1 ~ 1, TRUE ~ 0),
    eletronico = case_when(R403 %in% c(1, 2) ~ 1, TRUE ~ 0)
  )

#-----------------#
#--Antropometria--#
#-----------------#

#ARQUIVO 2020 QUE EU TENHO NAO TEM IMPUTAÇÃO (i)

df <- df %>%
  mutate(
    Q9_I = coalesce(Q9_I, Q9),
    Q11_I = coalesce(Q11_I, Q11),
    imc_i = if_else(Q11 >= 700 | Q9 >= 700, NA_real_, Q9 / ((Q11 / 100)^2)),
    excpeso_i = case_when(imc_i >= 25 & imc_i <= 115 ~ 1, TRUE ~ 0),
    obesid_i = case_when(imc_i >= 30 & imc_i <= 115 ~ 1, TRUE ~ 0)
  )

#-----------------------#
#--DIETA / ALIMENTAÇÃO--#
#-----------------------#

df <- df %>%
  mutate(
  #Consumo regular
    
    #Hortaliças
    hortareg = if_else(Q16 %in% c(3, 4), 1, 0),
    #Frutas
    frutareg = if_else(Q25 %in% c(3, 4) | Q27 %in% c(3, 4), 1, 0),
    #Frutas E Hortaliças
    flvreg = if_else(frutareg == 1 & hortareg == 1, 1, 0),
    
  #Consumo recomendado
    
    #Hortaliças (Cruas e Cozidas)
    cruadia = case_when(
      Q18 %in% c(1, 2) ~ 1, 
      Q18 == 3 ~ 2, 
      TRUE ~ 0
    ),
    
    cozidadia = case_when(
      Q20 %in% c(1, 2) ~ 1, 
      Q20 == 3 ~ 2, 
      TRUE ~ 0
    ),
    
    hortadia = cruadia + cozidadia,
    
    #Frutas e Sucos
    sucodia = if_else(Q26 >= 1 & Q26 <= 3, 1, 0),
    
    sofrutadia = case_when(
      Q28 == 1 ~ 1, 
      Q28 == 2 ~ 2, 
      Q28 == 3 ~ 3, 
      TRUE ~ 0
    ),
    
    frutadia = sofrutadia + sucodia,
    
    #Total de Frutas e Hortaliças
    flvdia = hortadia + frutadia,
    
    #Indicador Final: Recomendado (5 a 8 porções/dia em >= 5 dias/sem)
    flvreco = if_else(flvdia >= 5 & flvdia <= 8 & flvreg == 1, 1, 0),
    
  #Outros alimentos
    refritl5 = if_else(Q29 %in% c(3, 4), 1, 0),
    feijao5  = if_else(Q15 %in% c(3, 4), 1, 0)
  )

#-----------------------#
#---SCORE ALIMENTAÇÃO---#
#-----------------------#

df <- df %>%
  mutate(
    #Recodificação em massa: transforma todos os 2 em 0 da coluna R301_a até a R302_m
    across(R301_A:R302_M, ~ if_else(.x == 2, 0, .x)),
    
    #score de alimentos não ou minimamente processados (Protetores)
    score_sf = R301_A + R301_B + R301_C + R301_D + R301_E + R301_G + R301_L,
    
    #Recodifica para 2 categorias: 0 a 4 = 0 | 5 a 7 = 1
    score_sf_2cat = if_else(score_sf >= 5, 1, 0),
    
    #score de alimentos ultraprocessados
    score_upp = R302_A + R302_B + R302_C + R302_D + R302_E + R302_F + 
      R302_G + R302_H + R302_I + R302_J + R302_K + R302_L + R302_M,
    
    #Recodifica para 2 categorias: 0 a 4 = 0 | 5 a 13 = 1
    score_upp_2cat = if_else(score_upp >= 5, 1, 0)
  )

#-----------------------#
#---ATIVIDADE FÍSICA----#
#-----------------------#

df <- df %>%
  mutate(
  #Atividade Física de Lazer
    #Tipo e Frequência
    af = case_when(
      Q43A %in% c(1, 2, 5, 7, 8, 9, 10, 11, 14, 16, 17) ~ 1,
      Q43A %in% c(3, 4, 6, 12, 13, 15) ~ 2,
      TRUE ~ 0
    ),
    
    freq = case_when(
      Q45 == 1 ~ 1.5, 
      Q45 == 2 ~ 3.5, 
      Q45 == 3 ~ 5.5, 
      Q45 == 4 ~ 7.0, 
      TRUE ~ 0
    ),
    
    #Duração
    time = case_when(
      Q46 == 1 ~ 0, 
      Q46 == 2 ~ 14.5, 
      Q46 == 3 ~ 24.5, 
      Q46 == 4 ~ 34.5,
      Q46 == 5 ~ 44.5, 
      Q46 == 6 ~ 54.5, 
      Q46 == 7 ~ 60, 
      TRUE ~ 0
    ),
    
    #Variável contínua e Dicotômica
    ati_livre = af * freq * time,
    ativo_livre = if_else(ati_livre >= 150, 1, 0),
    
  #Atividade Física em Outros Domínios e Inatividade
  
    #Deslocamento, Doméstico e Ocupação
    atitrans = if_else((Q51 > 3 & !is.na(Q51)) | (Q54 > 3 & !is.na(Q54)), 1, 0),
    atidom = if_else(Q55 == 1 | Q56 == 1, 1, 0),
    atiocu = if_else(Q48 == 1 | Q49 == 1, 1, 0),
  
    #Inatividade Física
    inativo = if_else(
      Q42 == 2 & 
        (Q50 == 3 | is.na(Q50) | Q51 %in% c(1, 2)) & 
        (Q53 == 3 | is.na(Q53) | Q54 %in% c(1, 2)) & 
        atiocu == 0 & 
        atidom == 0, 
      1, 0
    ),
  
  #Indicador Global de AF
    #Duração diária de deslocamento para trabalho e escola
    Q51medio = case_when(
      Q51 %in% c(1, 2) ~ 0,
      Q51 == 3 ~ 24.5, Q51 == 4 ~ 34.5, Q51 == 5 ~ 44.5, Q51 == 6 ~ 54.5, Q51 == 7 ~ 60,
      TRUE ~ 0
    ),
    Q54medio = case_when(
      Q54 %in% c(1, 2) ~ 0,
      Q54 == 3 ~ 24.5, Q54 == 4 ~ 34.5, Q54 == 5 ~ 44.5, Q54 == 6 ~ 54.5, Q54 == 7 ~ 60,
      TRUE ~ 0
    ),
    
    deslocdia = Q51medio + Q54medio,
    deslocsemana = deslocdia * 5,
    
    #Atividade Física Laboral
    atiocusemana = if_else(
      R147 %in% 555:888 | R148_HH == 777 | R148_MM == 777 | is.na(R147), 
      0, 
      ((R148_HH * 60) + R148_MM) * R147
    ),
  
    #Atividade Física Doméstica (Tratando códigos de recusa/não sabe)
    faxinasemana = if_else(
      R149 %in% 555:888 | R150_HH == 777 | R150_MM == 777 | is.na(R149), 
      0, 
      ((R150_HH * 60) + R150_MM) * R149
    ),
  
    #AF global nos 3 domínios (Lazer + Deslocamento + Laboral)
    af3dominios = if_else((ati_livre + deslocsemana + atiocusemana) >= 150, 1, 0),
    af3dominios_insu = if_else(af3dominios == 0, 1, 0),
  
  #Tempo Sentado e Telas
    #Hábito de TV e Tela
    tv_d_3 = if_else(Q59A >= 4 & Q59A != 8 & !is.na(Q59A), 1, 0),
    tempo_tela_stv = if_else(Q59C >= 4 & !is.na(Q59C), 1, 0),
    
    #Convertendo categorias em horas para somar
    Q59A_horas = case_when(
      Q59A == 1 ~ 1, Q59A == 2 ~ 1.5, Q59A == 3 ~ 2.5, Q59A == 4 ~ 3.5, 
      Q59A == 5 ~ 4.5, Q59A == 6 ~ 5.5, Q59A == 7 ~ 6, 
      TRUE ~ 0
    ),
    
    Q59C_horas = case_when(
      Q59C == 1 ~ 1, Q59C == 2 ~ 1.5, Q59C == 3 ~ 2.5, Q59C == 4 ~ 3.5, 
      Q59C == 5 ~ 4.5, Q59C == 6 ~ 5.5, Q59C == 7 ~ 6, 
      TRUE ~ 0
    ),
    
    tempo_tela_total = if_else((Q59A_horas + Q59C_horas) >= 3, 1, 0)
  )
  
#-----------------------------------#
#---Avaliação do Estado de Saúde----#
#-----------------------------------#

df <- df %>%
  mutate(
    #Consumo abusivo de bebidas alcoólicas
    alcabu = if_else(Q37 %in% 1 | Q38 %in% 1, 1, 0),
    
    #Condução de veículo após consumo ABUSIVO de bebida alcoólica
    direcao = if_else(Q40 == 1, 1, 0),
    
    #Condução de veículo após consumo QUALQUER de bebida alcoólica
    direcao_alc = if_else(Q40B %in% c(1, 2, 3) | Q40 == 1, 1, 0)
  )

#-----------------------#
#---ATIVIDADE FÍSICA----#
#-----------------------#

df <- df %>%
  mutate(
    saruim = if_else(Q74 %in% c(4, 5), 1, 0)
  )

#--------------------------#
#---Prevenção do Câncer----#
#--------------------------#

df <- df %>%
  mutate(
    #Prevenção do Câncer - Mamografia
      #Faixas de idade alvo (Mulheres Q7==2, de 50 a 69 anos)
      iddmamo = case_when(
        Q6 >= 50 & Q6 <= 59 & Q7 == 2 ~ 1,
        Q6 >= 60 & Q6 <= 69 & Q7 == 2 ~ 2,
        TRUE ~ NA_real_ 
      ),
      #Realização de mamografia na vida 
      mamo = case_when(
        is.na(iddmamo) ~ NA_real_,
        Q81 == 1 ~ 1,              
        TRUE ~ 0                   
      ),
      #Realização de mamografia nos últimos dois anos
      mamodois = case_when(
        is.na(iddmamo) ~ NA_real_,
        Q82 %in% c(1, 2) ~ 1,
        TRUE ~ 0
      ),
    
    #Prevenção do Câncer - Papanicolau
      #Faixas de idade alvo antigas (25 a 59 anos)
      iddpapa_old = case_when(
        Q7 == 2 & Q6 %in% 25:34 ~ 1,
        Q7 == 2 & Q6 %in% 35:44 ~ 2,
        Q7 == 2 & Q6 %in% 45:54 ~ 3,
        Q7 == 2 & Q6 %in% 55:59 ~ 4,
        TRUE ~ NA_real_
      ),
      
      #Faixas de idade alvo atuais (25 a 64 anos)
      iddpapa = case_when(
        Q7 == 2 & Q6 %in% 25:34 ~ 1,
        Q7 == 2 & Q6 %in% 35:44 ~ 2,
        Q7 == 2 & Q6 %in% 45:54 ~ 3,
        Q7 == 2 & Q6 %in% 55:64 ~ 4,
        TRUE ~ NA_real_
      ),
      
      #Papanicolau alguma vez na vida
      papa = case_when(
        is.na(iddpapa) ~ NA_real_,
        Q79A == 1 ~ 1,
        TRUE ~ 0
      ),
      
      #Papanicolau nos últimos três anos
      papatres = case_when(
        is.na(iddpapa) ~ NA_real_,
        Q80 %in% 1:3 ~ 1,
        TRUE ~ 0
      )
  )

#--------------------------#
#----MORBIDADE REFERIDA----#
#--------------------------#

df <- df %>%
  mutate(
    #Hipertensão Arterial
    hart = if_else(Q75 == 1, 1, 0),
    
    #Diabetes
    diab = if_else(Q76 == 1, 1, 0)
  )

#--------------------------------#
#----TRATAMENTO MEDICAMENTOSO----#
#--------------------------------#

df <- df %>%
  mutate(
    #Hipertensão
    ind_med_has = case_when(
      hart == 0 ~ NA_real_,
      R203 == 1 ~ 1,
      TRUE ~ 0
    ),
    med_has = case_when(
      hart == 0 ~ NA_real_,
      R129 == 1 ~ 1,
      TRUE ~ 0
    ),
    trat_med_has = case_when(
      hart == 0 ~ NA_real_,
      hart == 1 & ind_med_has == 1 & med_has == 1 ~ 1,
      TRUE ~ 0
    ),
    
    #Diabetes
    ind_med_db = case_when(
      diab == 0 ~ NA_real_,
      R204 == 1 ~ 1,
      TRUE ~ 0
    ),
    med_db = case_when(
      diab == 0 ~ NA_real_,
      R133A == 1 ~ 1,
      TRUE ~ 0
    ),
    insulina = case_when(
      diab == 0 ~ NA_real_,
      R133B == 1 ~ 1,
      TRUE ~ 0
    ),
    trat_med_db = case_when(
      diab == 0 ~ NA_real_,
      diab == 1 & ind_med_db == 1 & (med_db == 1 | insulina == 1) ~ 1,
      !is.na(R133C) ~ 0,
      TRUE ~ 0
    ),
    
    #Variáveis Geográficas e Plano de Saúde
    
    #Transforma "RRAS01" em 1
    RRAS = as.numeric(gsub("RRAS", "", REGIAO)),
    
    #Plano de Saúde
    tem_plano = if_else(Q88 %in% c(1, 2), 1, 0)
  ) %>%
  select(-any_of(c("pinterno", "sexofxesc", "ttsexofxesc", "ttfet"))
  )

#--------------------------------#
#-------------DOMÍNIO------------#
#--------------------------------#

#NÃO TENHO ESSA PLANILHA

# dominio_df <- read_excel("C:\\R\\DCNT\\VIGITEL\\2-Tabela_Dominio.xlsx", sheet = "dominio")
# 
# df <- df %>%
#   left_join(dominio_df, by = "CIDADE") %>%
# 
#   mutate(
#     DOMINIO = factor(DOMINIO, 
#                      levels = c(1, 2, 3), 
#                      labels = c("Sao Paulo", "Gde SP", "Interior"))
#   )

#--------------------------------#
#----INDICADORES DIABETES--------#
#--------------------------------#

df <- df %>%
  mutate(
    #Faixa etária para diabéticos
    fet_diab = case_when(
      Q6 >= 18 & Q6 <= 44 ~ 1,
      Q6 >= 45 & Q6 <= 54 ~ 2,
      Q6 >= 55 & Q6 <= 64 ~ 3,
      Q6 >= 65 ~ 4,
      TRUE ~ NA_real_
    ),
    
    #Fator para aplicar os rótulos
    fet_diab = factor(fet_diab, 
                      levels = 1:4, 
                      labels = c("18 a 44", "45 a 54", "55 a 64", "65+")),
    
    #Assistência médica há menos de 1 ano
    ass_medida_db = case_when(R133C == 1 ~ 1, diab == 1 ~ 0, TRUE ~ NA_real_),
    
    #Atendidos pelo mesmo médico de consultas anteriores
    cs_medica_db = case_when(R133D == 1 ~ 1, diab == 1 ~ 0, TRUE ~ NA_real_),
    
    #Exame de hemoglobina no último ano
    hemoglobina_db = case_when(R133E == 1 ~ 1, diab == 1 ~ 0, TRUE ~ NA_real_),
    
    #Exame de vista ou fundo de olho há menos de 1 ano
    exame_vista_db = case_when(R133F == 1 ~ 1, diab == 1 ~ 0, TRUE ~ NA_real_),
    
    #Exame de pés há menos de 1 ano
    exame_pes_db = case_when(R133G == 1 ~ 1, diab == 1 ~ 0, TRUE ~ NA_real_),
    
    #Prescrição de algum medicamento
    presc_med_db = case_when(R204 == 1 ~ 1, diab == 1 ~ 0, TRUE ~ NA_real_),
    
    #Faz uso atual de medicamento oral
    uso1_medic_db = case_when(R133A == 1 ~ 1, diab == 1 ~ 0, TRUE ~ NA_real_),
    
  #Indicadores de adesão
    # 9. Tomaram TODOS os comprimidos nas duas últimas semanas
    uso2_medic_db = case_when(R133H == 1 ~ 1, R133A == 1 ~ 0, TRUE ~ NA_real_),
    
    # 10. Tomaram ALGUNS dos comprimidos
    uso2_alguns_compr_db = case_when(R133H == 2 ~ 1, R133A == 1 ~ 0, TRUE ~ NA_real_),
    
    # 11. NENHUM dos comprimidos
    uso2_nenhum_compr_db = case_when(R133H == 3 ~ 1, R133A == 1 ~ 0, TRUE ~ NA_real_),
      
  #Insulina
    #Prescrição médica de insulina
    uso_insulina_db = case_when(R133B == 1 ~ 1, diab == 1 ~ 0, TRUE ~ NA_real_),
    
    #Uso de insulina nas duas últimas semanas
    uso2sem_insulina_db = case_when(R133I == 1 ~ 1, R133B == 1 ~ 0, TRUE ~ NA_real_),
    
    #Medicamentos E insulina nas duas últimas semanas 
    uso_med_insu_db = case_when(
      R133H == 1 & R133I == 1 ~ 1, 
      R133A == 1 & R133B == 1 ~ 0, 
      TRUE ~ NA_real_
    )
  )

write.xlsx(df, "C:\\R\\DCNT\\VIGITEL\\BD_Vigitel_pesorake_indicadores_TESTE.xlsx")

#-----------------------------#
#--------INDICADORES----------#
#-----------------------------#

### INÍCIO ####
options(survey.lonely.psu = "adjust")

### DESIGN ####

# variaveis de delineamento da amostra
dsn <- svydesign(
  ids = ~CHAVE,
  weights = ~PESORAKE,
  nest = TRUE,
  data = df)

#Consumo abusivo de bebidas alcoólicas	11
prev_alcool <- svymean(~alcabu, design = dsn, na.rm = TRUE)
confint(prev_alcool) * 100

  #rras
  svyby(~alcabu, by = ~RRAS, design = dsn, svymean, na.rm = TRUE)
  
  #SEXO (Q7)
  svyby(~alcabu, by = ~Q7, design = dsn,svymean, na.rm = TRUE)
  
  #FAIXA ETARIA
  svyby(~alcabu, by = ~faixa_etaria, design = dsn,svymean, na.rm = TRUE)
  
  #Escolaridade
  svyby(~alcabu, by = ~escolaridade, design = dsn,svymean, na.rm = TRUE)

#-------#
  
#Prevalência de tabagismo	11
prev_fumante <- svymean(~fumante, design = dsn, na.rm = TRUE)
confint(prev_fumante) * 100

  #rras
  svyby(~fumante, by = ~RRAS, design = dsn,svymean, na.rm = TRUE)
  
  #SEXO (Q7)
  svyby(~fumante, by = ~Q7, design = dsn,svymean, na.rm = TRUE)
  
  #FAIXA ETARIA
  svyby(~fumante, by = ~faixa_etaria, design = dsn,svymean, na.rm = TRUE)
  
  #Escolaridade
  svyby(~fumante, by = ~escolaridade, design = dsn,svymean, na.rm = TRUE)

#Prevalência de hipertensão arterial	11
prev_hart <- svymean(~hart, design = dsn, na.rm = TRUE)
confint(prev_hart) * 100
  
  #rras
  svyby(~hart, by = ~RRAS, design = dsn,svymean, na.rm = TRUE)
  
  #SEXO (Q7)
  svyby(~hart, by = ~Q7, design = dsn,svymean, na.rm = TRUE)
  
  #FAIXA ETARIA
  svyby(~hart, by = ~faixa_etaria, design = dsn,svymean, na.rm = TRUE)
  
  #Escolaridade
  svyby(~hart, by = ~escolaridade, design = dsn,svymean, na.rm = TRUE)

#Prevalência de diabetes mellitus	12
prev_diab <- svymean(~diab, design = dsn, na.rm = TRUE)
confint(prev_diab) * 100
  
  #rras
  svyby(~diab, by = ~RRAS, design = dsn,svymean, na.rm = TRUE)
  
  #SEXO (Q7)
  svyby(~diab, by = ~Q7, design = dsn,svymean, na.rm = TRUE)
  
  #FAIXA ETARIA
  svyby(~diab, by = ~faixa_etaria, design = dsn,svymean, na.rm = TRUE)
  
  #Escolaridade
  svyby(~diab, by = ~escolaridade, design = dsn,svymean, na.rm = TRUE)



