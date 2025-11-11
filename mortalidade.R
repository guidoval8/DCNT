#--------------MORTALIDADE------------#
library(dplyr)
library(microdatasus)
#library(devtools)
library(summarytools)
library(lubridate)
library(tidyr)
library(foreign)
library(stringr)
library(openxlsx)
library(rtabnetsp)

#devtools::install_github("danicat/read.dbc")
#devtools::install_github("rfsaldanha/microdatasus")
#devtools::install_github("joaohmorais/rtabnetsp")

#Download: SIM, SINASC, SIH, CNES, SIA, SINAN-DENGUE, SINAN-CHIKUNGUNYA, SINAN-ZIKA, SINAN-MALARIA, SINAN-CHAGAS, SINAN-LEISHMANIOSE-VISCERAL, SINAN-LEISHMANIOSE-TEGUMENTAR, SINAN-LEPTOSPIROSE.
#Pré-processamento: SIM, SINASC, SIH, CNES, SIA, SINAN-DENGUE, SINAN-CHIKUNGUNYA, SINAN-ZIKA, SINAN-MALARIA, SINAN-CHAGAS, SINAN-LEISHMANIOSE-TEGUMENTAR, SINAN-LEISHMANNIOSE-VISCERAL.

#----EXTRAÇÃO----#
SIM <- fetch_datasus(year_start = 2024, year_end = 2024, uf = "SP", information_system = "SIM-DO")
SIM <- process_sim(SIM)
#----------------#

#----PADRONIZAÇÃO----#
#CLASSIFICAR ANO
SIM <- SIM %>%
  mutate(DTOBITO = ymd(DTOBITO)) %>%
  mutate(ANOOBITO = year(DTOBITO))

#PADRONIZAR CAUSA BÁSICA
SIM_filtrado <- SIM %>%
  mutate(
    CAUSABAS = str_trim(toupper(as.character(CAUSABAS))),
    CID3 = str_sub(CAUSABAS, 1,3)
  )

#CLASSIFICAÇÃO
SIM_filtrado <- SIM_filtrado %>%
  #Filtrar idade
  mutate(IDADEanos = as.numeric(IDADEanos)) %>%
  filter(IDADEanos >= 30 & IDADEanos < 70) %>%
  #Classificação de CID (CAUSABAS)
  mutate(
    GRUPO_DCNT = case_when(
      CAUSABAS >= 'I00' & CAUSABAS <= 'I99' ~ 'Circulatorio',
      CAUSABAS >= 'C00' & CAUSABAS <= 'C97' ~ 'Cancer',
      CAUSABAS >= 'E10' & CAUSABAS <= 'E14' ~ 'Diabetes',
      CAUSABAS >= 'J30' & CAUSABAS <= 'J98' & CAUSABAS != 'J36' ~ 'Respiratoria',
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(GRUPO_DCNT))

#Classificação que agrupa as DCNT
SIM_todas <- SIM_filtrado %>%
  mutate(
    GRUPO_DCNT = case_when(
      (CAUSABAS >= 'I00' & CAUSABAS <= 'I99') |
      (CAUSABAS >= 'C00' & CAUSABAS <= 'C97') |
      (CAUSABAS >= 'E10' & CAUSABAS <= 'E14') |
      ((CAUSABAS >= 'J30' & CAUSABAS <= 'J98') & (CAUSABAS != 'J36')) ~ "Todas_DCNT",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(GRUPO_DCNT))

#EMPILHAR AS CLASSIFICAÇÕES
SIM_filtrado <- rbind(SIM_filtrado, SIM_todas)

#CLASSIFICAÇÃO FAIXA ETÁRIA
SIM_filtrado <- SIM_filtrado %>%
  mutate(
    FAIXA_ETARIA = case_when(
      IDADEanos >= 30 & IDADEanos < 40 ~ '30-39 anos',
      IDADEanos >= 40 & IDADEanos < 50 ~ '40-49 anos',
      IDADEanos >= 50 & IDADEanos < 60 ~ '50-59 anos',
      IDADEanos >= 60 & IDADEanos < 70 ~ '60-69 anos',
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FAIXA_ETARIA))

#AGRUPAR NÚMERO DE ÓBITOS POR ESTRATO
#Criando o estrato por sexo e ambos os sexos
df_obitos_sexo <- SIM_filtrado
df_obitos_sexototal <- SIM_filtrado %>%
  mutate(SEXO = "Total")

SIM_filtrado_final <- rbind(df_obitos_sexo, df_obitos_sexototal)

#Contando o número de óbitos por estrato
NUMERADOR_SIM <-  SIM_filtrado_final %>%
  group_by(ANOOBITO, SEXO, CODMUNRES, GRUPO_DCNT, FAIXA_ETARIA) %>%
  summarise(
    obitos = n()
  ) %>%
  ungroup()

#----POPULAÇÃO----#
#BUSCAR NO FTP
url_pop_zip <- "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/POPSBR24.zip"
nome_pop_zip <- "POPSBR24.zip"

download.file(url = url_pop_zip, nome_pop_zip, mode = "wb")
unzip(nome_pop_zip)

pop_bruta2024 <- read.dbf("POP24.dbf")

#CLASSIFICAÇÃO DE FAIXA ETÁRIA
DENOMINADOR <- pop_bruta2024 %>%
  mutate(
    FAIXA_ETARIA = case_when(
      IDADE == "030" | IDADE == "035" ~ "30-39 anos",
      IDADE == "040" | IDADE == "045" ~ "40-49 anos",
      IDADE == "050" | IDADE == "055" ~ "50-59 anos",
      IDADE == "060" | IDADE == "065" ~ "60-69 anos",
      TRUE ~ NA_character_
    )
  ) %>%
    filter(!is.na(FAIXA_ETARIA))

#Criando o estrato por sexo e ambos
df_pop_sexo <- DENOMINADOR
df_pop_sexo_total <- DENOMINADOR %>%
  mutate(SEXO = "Total")

DENOMINADOR <- rbind(df_pop_sexo, df_pop_sexo_total)

DENOMINADOR <- DENOMINADOR %>%
    group_by(COD_MUN, ANO, SEXO, FAIXA_ETARIA) %>%
    summarise(
      populacao = sum(POP, na.rm = TRUE)
    ) %>%
    ungroup()

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

#LINKAGE ENTRE OS DADOS
#Chaves de junção
chaves <- c("CODMUNRES", "ANOOBITO", "SEXO", "FAIXA_ETARIA")
join <- left_join(NUMERADOR_SIM, DENOMINADOR, by = chaves) %>%
  filter(!is.na(populacao))

teste <- join %>%
  filter(is.na(populacao))

#CALCULAR TAXA BRUTA (100.000)
df_taxas <- join %>%
  mutate(
    taxa_bruta_especifica = (obitos / populacao) * 100000
  ) %>%
  mutate(
    taxa_bruta_pessoa = obitos / populacao
  )

#Pop padrão
pop_padrao_2010 <- data.frame(
  FAIXA_ETARIA = c("30-39 anos", "40-49 anos", "50-59 anos", "60-69 anos"),
  populacao_padrao = c(30031077,25176600,18664323,
                      11502710)
)

pop_padrao_2010 <- pop_padrao_2010 %>%
  group_by(FAIXA_ETARIA) %>%
  summarise(
    populacao_padrao = sum(populacao_padrao)
  ) %>%
  ungroup()

#Óbitos esperados
df_padrao <-left_join(df_taxas, pop_padrao_2010, by = "FAIXA_ETARIA")

df_OE <- df_padrao %>%
  mutate(OE = taxa_bruta_pessoa * populacao_padrao)

#----TAXA PADRONIZADA FINAL----#
#população padrão total
pop_padrao_total <- sum(pop_padrao_2010$populacao_padrao)

taxa_padronizada_mun <- df_OE %>%
  group_by(CODMUNRES, ANOOBITO, SEXO, GRUPO_DCNT) %>%
  summarise(
    total_obitos_esperados = sum(OE, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    taxa_padronizada_100mil = (total_obitos_esperados / pop_padrao_total) * 100000
  )

#----ESTADO----#
estado_bruto <- df_taxas %>%
  group_by(ANOOBITO, SEXO, GRUPO_DCNT, FAIXA_ETARIA) %>%
  summarise(
    total_obito_estado = sum(obitos, na.rm = TRUE),
    total_pop_estado = sum(populacao, na.rm=TRUE)
  ) %>%
  ungroup()

#Taxa específica ESTADO
estado_taxa_especifica <- estado_bruto %>%
  mutate(
    taxa_especifica_estado = total_obito_estado / total_pop_estado
  )

#Juntar com pop padrão
estado_padrao <- left_join(estado_taxa_especifica, pop_padrao_2010, by = 'FAIXA_ETARIA')

#OE estado
estado_oe <- estado_padrao %>%
  mutate(
    oe_estado = taxa_especifica_estado * populacao_padrao
  )

#Taxa padronizada final
taxa_padronizada_estado <-  estado_oe %>%
  group_by(ANOOBITO, SEXO, GRUPO_DCNT) %>%
  summarise(
    total_oe_estado = sum(oe_estado, na.rm = TRUE)) %>%
  mutate(
    taxa_padronizada_estado = (total_oe_estado / pop_padrao_total) * 100000
  ) %>%
  ungroup()

#----RRAS----#
RRAS_RS <- read.xlsx(r"(C:\Users\x504402\Documents\DCNT\RRAS_Municipios.xlsx)")

#Padronização
RRAS_RS <- RRAS_RS %>%
  mutate(COD_6_mun = as.character(COD_6_mun)) %>%
  mutate(COD_6_mun = str_sub(COD_6_mun, start = 1L, end = 6L)) %>%
  select(COD_6_mun, MUNICIPIO, RRAS_2025, COD_RS_2025, NOME_RS_2025) %>%
  distinct(COD_6_mun, .keep_all = TRUE)

RRAS_bruto <- left_join(df_taxas, RRAS_RS, by=c("CODMUNRES" = "COD_6_mun"))

RRAS_bruto <- RRAS_bruto %>%
  filter(!is.na(RRAS_2025)) %>%
  group_by(RRAS_2025, ANOOBITO, SEXO, GRUPO_DCNT, FAIXA_ETARIA) %>%
  summarise(obito_rras = sum(obitos, na.rm = TRUE),
            populacao_rras = sum(populacao, na.rm=TRUE)) %>%
  ungroup()

#Taxa específica RRAS
RRAS_taxa_especifica <- RRAS_bruto %>%
  mutate(
    taxa_especifica_rras = obito_rras / populacao_rras
  )

#Juntar com população padrão
RRAS_padrao <- left_join(RRAS_taxa_especifica, pop_padrao_2010, by="FAIXA_ETARIA")

#OE RRAS
RRAS_oe <- RRAS_padrao %>%
  mutate(oe_rras = taxa_especifica_rras * populacao_padrao)

#Taxa padronizada final RRAS
taxa_padronizada_rras <- RRAS_oe %>%
  group_by(RRAS_2025, ANOOBITO, SEXO, GRUPO_DCNT) %>%
  summarise(total_oe_rras = sum(oe_rras, na.rm = TRUE)) %>%
  mutate(taxa_padronizada_rras = (total_oe_rras / pop_padrao_total) * 100000) %>%
  ungroup()

#----RS----#
RRAS_RS <- left_join(df_taxas, RRAS_RS, by=c("CODMUNRES" = "COD_6_mun"))

RS_bruto <- RRAS_RS %>%
  filter(!is.na(NOME_RS_2025)) %>%
  group_by(NOME_RS_2025, ANOOBITO, SEXO, GRUPO_DCNT, FAIXA_ETARIA) %>%
  summarise(obito_rs = sum(obitos, na.rm = TRUE),
            populacao_rs = sum(populacao, na.rm=TRUE)) %>%
  ungroup()

#Taxa específica RRAS
RS_taxa_especifica <- RS_bruto %>%
  mutate(
    taxa_especifica_rs = obito_rs / populacao_rs
  )

#Juntar com população padrão
RS_padrao <- left_join(RS_taxa_especifica, pop_padrao_2010, by="FAIXA_ETARIA")

#OE RRAS
RS_oe <- RS_padrao %>%
  mutate(oe_rs = taxa_especifica_rs * populacao_padrao)

#Taxa padronizada final RRAS
taxa_padronizada_rs <- RS_oe %>%
  group_by(NOME_RS_2025, ANOOBITO, SEXO, GRUPO_DCNT) %>%
  summarise(total_oe_rs = sum(oe_rs, na.rm = TRUE)) %>%
  mutate(taxa_padronizada_rs = (total_oe_rs / pop_padrao_total) * 100000) %>%
  ungroup()


