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
library(rio)
library(slider)
library(sf)
library(ggplot2)

options(download.file.method = "wininet")
#devtools::install_github("danicat/read.dbc")
#devtools::install_github("rfsaldanha/microdatasus")
#devtools::install_github("joaohmorais/rtabnetsp")

#Download: SIM, SINASC, SIH, CNES, SIA, SINAN-DENGUE, SINAN-CHIKUNGUNYA, SINAN-ZIKA, SINAN-MALARIA, SINAN-CHAGAS, SINAN-LEISHMANIOSE-VISCERAL, SINAN-LEISHMANIOSE-TEGUMENTAR, SINAN-LEPTOSPIROSE.
#Pré-processamento: SIM, SINASC, SIH, CNES, SIA, SINAN-DENGUE, SINAN-CHIKUNGUNYA, SINAN-ZIKA, SINAN-MALARIA, SINAN-CHAGAS, SINAN-LEISHMANIOSE-TEGUMENTAR, SINAN-LEISHMANNIOSE-VISCERAL.

#----EXTRAÇÃO----#
SIM <- fetch_datasus(year_start = 2015, year_end = 2024, uf = "SP", information_system = "SIM-DO")
SIM <- process_sim(SIM)
#----------------#

#----PADRONIZAÇÃO----#
#CLASSIFICAR ANO
SIM <- SIM %>%
  mutate(DTOBITO = ymd(DTOBITO)) %>%
  mutate(ANOOBITO = year(DTOBITO))

#PADRONIZAR CAUSA BÁSICA
SIM_CID <- SIM %>%
  mutate(
    CAUSABAS = str_trim(toupper(as.character(CAUSABAS))),
    CID3 = str_sub(CAUSABAS, 1,3)
  )

SIM_padrao <- SIM_CID %>%
  #Filtrar idade
  mutate(IDADEanos = as.numeric(IDADEanos)) %>%
  filter(IDADEanos >= 30 & IDADEanos < 70)

#CLASSIFICAÇÃO CAUSABAS
SIM_grupos <- SIM_padrao %>%
  #Classificação de CID
  mutate(
    GRUPO_DCNT = case_when(
      CID3 >= 'I00' & CID3 <= 'I99' ~ 'Circulatorio',
      CID3 >= 'C15' & CID3 <= 'C25' | CAUSABAS == 'C260' | CAUSABAS == 'C268'
      | CAUSABAS == 'C269' | CAUSABAS == 'C451' | CID3 == 'C48' | CAUSABAS == 'C772'
      | CAUSABAS == 'C784' | CAUSABAS == 'C785' | CAUSABAS == 'C786' | CAUSABAS == 'C787'
      | CAUSABAS == 'C788' ~ 'Cancer do Aparelho Digestivo',
      CID3 == 'C50' ~ 'Cancer de Mama',
      CID3 == 'C53' ~ 'Cancer de Colo de Utero', 
      CID3 >= 'E10' & CID3 <= 'E14' ~ 'Diabetes',
      CID3 >= 'J30' & CID3 <= 'J98' & CID3 != 'J36' ~ 'Respiratoria',
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(GRUPO_DCNT))

SIM_cancer_todos <- SIM_padrao %>%
  mutate(
    GRUPO_DCNT = case_when(
      CAUSABAS >= 'C00' & CAUSABAS <= 'C97' ~ 'Cancer'
    ) 
  ) %>%
  filter(GRUPO_DCNT == 'Cancer')

#Classificação que agrupa as DCNT
SIM_todas <- SIM_padrao %>%
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
SIM_filtrado <- rbind(SIM_grupos, SIM_todas, SIM_cancer_todos)

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
NUMERADOR_SIM <- SIM_filtrado_final %>%
  group_by(ANOOBITO, SEXO, CODMUNRES, GRUPO_DCNT, FAIXA_ETARIA) %>%
  summarise(
    obitos = n()
  ) %>%
  ungroup() %>%
  filter(!(GRUPO_DCNT %in% c('Cancer de Mama', 'Cancer de Colo de Utero') & SEXO != 'Feminino'))
#------------------------------#

#----POPULAÇÃO----#
options(download.file.method = "libcurl")
anos_baixar <- 2015:2024
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
    
    # Adiciona o dataframe lido à nossa lista
    lista_pop_bruta[[as.character(ano)]] <- df_ano_atual
    
    print(paste("Ano", ano, "processado com sucesso."))
    
  }, error = function(e) {
    # Se der erro (ex: arquivo não existe no FTP), ele avisa e continua
    print(paste("ERRO ao processar o ano", ano, ":", e$message))
  })
}

pop_bruta_total <- bind_rows(lista_pop_bruta)

# #BUSCAR NO FTP
# url_pop_zip <- "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/POPSBR24.zip"
# nome_pop_zip <- "POPSBR24.zip"
# 
# 
# download.file(url = url_pop_zip, nome_pop_zip, mode = "wb")
# unzip(nome_pop_zip)
# 
# pop_bruta2024 <- read.dbf("POP24.dbf")

#CLASSIFICAÇÃO DE FAIXA ETÁRIA
DENOMINADOR <- pop_bruta_total %>%
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

#LINKAGE ENTRE OS DADOS
#Chaves de junção
chaves <- c("CODMUNRES", "ANOOBITO", "SEXO", "FAIXA_ETARIA")
join <- left_join(NUMERADOR_SIM, DENOMINADOR, by = chaves) %>%
  filter(!is.na(populacao))

#---Cálculos----# 
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

#----TAXA PADRONIZADA MUN----#
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
RRAS_Municipios <- import("https://github.com/guidoval8/DCNT/blob/main/dados/RRAS_Municipios.xlsx?raw=true")

#Padronização
RRAS_RS <- RRAS_Municipios %>%
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
#---------------------------------#

#----TAXA BRUTA----#
#Estado
taxa_bruta_estado <- estado_bruto %>%
  group_by(ANOOBITO, SEXO, GRUPO_DCNT) %>%
  summarise(
    total_obito_estado = sum(total_obito_estado, na.rm=TRUE),
    total_pop_estado = sum(total_pop_estado, na.rm=TRUE)
  ) %>%
  mutate(
    taxa_bruta_estado = (total_obito_estado / total_pop_estado) * 100000
  ) %>%
  ungroup()

#RRAS
taxa_bruta_rras <- RRAS_bruto %>%
  group_by(RRAS_2025, ANOOBITO, SEXO, GRUPO_DCNT) %>%
  summarise(
    total_obito_rras = sum(obito_rras, na.rm = TRUE),
    total_pop_rras = sum(populacao_rras, na.rm = TRUE)
  )%>%
  mutate(taxa_bruta_rras = (total_obito_rras / total_pop_rras) * 100000)%>%
  ungroup()

#RS
taxa_bruta_rs <- RS_bruto %>%
  group_by(NOME_RS_2025, ANOOBITO, SEXO, GRUPO_DCNT) %>%
  summarise(
    total_obito_rs = sum(obito_rs, na.rm = TRUE),
    total_pop_rs = sum(populacao_rs, na.rm = TRUE)
  )%>%
  mutate(taxa_bruta_rs = (total_obito_rs / total_pop_rs) * 100000)%>%
  ungroup()

#MUN
taxa_bruta_mun <- df_taxas %>%
  group_by(CODMUNRES, ANOOBITO, SEXO, GRUPO_DCNT) %>%
  summarise(
    total_obito_mun = sum(obitos, na.rm = TRUE),
    total_pop_mun = sum(populacao, na.rm = TRUE)
  )%>%
  mutate(taxa_bruta_mun = (total_obito_mun / total_pop_mun) * 100000)%>%
  ungroup()
#------------------------------------------#

#----AGREGAR PARA BI----#
#ESTADO
estado_bi <- taxa_padronizada_estado %>%
  full_join(taxa_bruta_estado, by=c('ANOOBITO', 'SEXO', 'GRUPO_DCNT'))
#RRAS
rras_bi <- taxa_padronizada_rras %>%
  full_join(taxa_bruta_rras, by=c('RRAS_2025','ANOOBITO', 'SEXO', 'GRUPO_DCNT'))
#RS
rs_bi <- taxa_padronizada_rs %>%
  full_join(taxa_bruta_rs, by=c('NOME_RS_2025','ANOOBITO', 'SEXO', 'GRUPO_DCNT'))
#MUN
mun_bi <- taxa_padronizada_mun %>%
  full_join(taxa_bruta_mun, by=c('CODMUNRES','ANOOBITO', 'SEXO', 'GRUPO_DCNT'))
#------------------------#

#----PADRONIZAR PARA O BI----#
geo_de_para_nomes <- RRAS_Municipios %>%
  select(CODMUNRES = COD_6_mun, Nome_Municipio = MUNICIPIO) %>%
  distinct(CODMUNRES, .keep_all = TRUE)

#Padronizar ESTADO
estado_bi_padronizado <- estado_bi %>%
  transmute(
    Nivel_Geografico = 'Estado',
    ID_Localidade = '35',
    Nome_Localidade = 'São Paulo (Estado)',
    ANOOBITO, SEXO, GRUPO_DCNT,
    Taxa_Padronizada = taxa_padronizada_estado,
    Taxa_Bruta = taxa_bruta_estado
  )

#Padronizar RRAS
rras_bi_padronizado <- rras_bi %>%
  transmute(
    Nivel_Geografico = "RRAS",
    ID_Localidade = RRAS_2025,
    Nome_Localidade = RRAS_2025,
    ANOOBITO, SEXO, GRUPO_DCNT,
    Taxa_Padronizada = taxa_padronizada_rras,
    Taxa_Bruta = taxa_bruta_rras
  )

#Padronizar RS
rs_bi_padronizado <- rs_bi %>%
  transmute(
    Nivel_Geografico = "Região de Saúde",
    ID_Localidade = NOME_RS_2025,
    Nome_Localidade = NOME_RS_2025,
    ANOOBITO, SEXO, GRUPO_DCNT,
    Taxa_Padronizada = taxa_padronizada_rs,
    Taxa_Bruta = taxa_bruta_rs
  )

#Padronizar MUN
mun_bi_padronizado <- mun_bi %>%
  left_join(geo_de_para_nomes, by = "CODMUNRES")%>%
  transmute(
    Nivel_Geografico = "Município",
    ID_Localidade = CODMUNRES,
    Nome_Localidade = Nome_Municipio,
    ANOOBITO, SEXO, GRUPO_DCNT,
    Taxa_Padronizada = taxa_padronizada_100mil,
    Taxa_Bruta = taxa_bruta_mun
  )

#----EMPILHAR----#
tabela_mestra_mortalidade <- bind_rows(
  estado_bi_padronizado,
  rras_bi_padronizado,
  rs_bi_padronizado,
  mun_bi_padronizado
)

#----CÁLCULO DE METAS----#
#----REDUÇÃO 2,2% AO ANO----#
#----juros composto----#
#M = C(1-i)^t

taxa_base_2015 <- tabela_mestra_mortalidade %>%
  filter(ANOOBITO == 2015) %>%
  rename(Taxa_Padronizada_2015 = Taxa_Padronizada)

mestra_base_2015 <- left_join(
  tabela_mestra_mortalidade,
  taxa_base_2015,
  by = c("Nivel_Geografico","ID_Localidade","Nome_Localidade","SEXO","GRUPO_DCNT" )) %>%
  select("Nivel_Geografico","ID_Localidade","Nome_Localidade","ANOOBITO.x","SEXO","GRUPO_DCNT","Taxa_Padronizada_2015", "Taxa_Padronizada", "Taxa_Bruta.x", "Taxa_Bruta.y") %>%
  rename("ANOOBITO" = "ANOOBITO.x", "Taxa_Bruta" = "Taxa_Bruta.x" , "Taxa_Bruta_2015" = "Taxa_Bruta.y")

# Aplica a lógica condicional para a meta de redução

mestra <- mestra_base_2015 %>%
  mutate(
    TAXA_ANUAL_REDUCAO = case_when(
      GRUPO_DCNT == 'Cancer de Mama' & ANOOBITO <= 2030 ~ 0.0067, #0,67% ano
      GRUPO_DCNT == 'Cancer de Colo de Utero' & ANOOBITO <= 2030 ~ 0.0137, #1,33% ano
      GRUPO_DCNT == 'Cancer do Aparelho Digestivo' & ANOOBITO <= 2030 ~ 0.007, #0,67% ano
      TRUE ~ 0.022 # Meta de 2.2% ao ano para os demais grupos
    )
  ) %>%
  mutate(
    taxa_esperada_meta = Taxa_Bruta_2015 * (1 - TAXA_ANUAL_REDUCAO)^(ANOOBITO - 2015)
  ) %>%
  mutate(
    taxa_padronizada_esperada_meta = Taxa_Padronizada_2015 * (1 - TAXA_ANUAL_REDUCAO)^(ANOOBITO - 2015)
  ) %>%
  mutate(
    reduc_meta_bruta = ifelse(Taxa_Padronizada < taxa_esperada_meta, 1, 0)
  ) %>%
  mutate(
    reduc_meta_padro = ifelse(Taxa_Padronizada < taxa_esperada_meta, 1, 0)
  ) %>%
  select(
    "Nivel_Geografico", "ID_Localidade", "Nome_Localidade", "ANOOBITO", "SEXO", "GRUPO_DCNT",
    "Taxa_Padronizada", "Taxa_Padronizada_2015","taxa_padronizada_esperada_meta","Taxa_Bruta", "Taxa_Bruta_2015", "taxa_esperada_meta", "reduc_meta_bruta", "reduc_meta_padro"
  )

write.csv2(mestra, r"(C:\R\DCNT\t1.csv)", na = "", row.names = FALSE)

#--------------------------------------------------------#
#-----------------MORTALIDADE TRÂNSITO-------------------#
#--------------------------------------------------------#

#Shape de municípios
mun_shp <- read_sf(r'(C:\R\DCNT\mun_shp.gpkg)')

#CLASSIFICAÇÃO CAUSABAS
SIM_transito_grupos <- SIM_CID %>%
  #Classificação de CID
  mutate(
    GRUPO_transito = case_when(
      CID3 >= 'V01' & CID3 <= 'V09' ~ 'Pedestre',
      CID3 >= 'V10' & CID3 <= 'V19' ~ 'Ciclista',
      CID3 >= 'V20' & CID3 <= 'V39' ~ 'Moto',
      CID3 >= 'V40' & CID3 <= 'V79' ~ 'Carro',
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(GRUPO_transito))

SIM_transito_todos <- SIM_padrao %>%
  mutate(
    GRUPO_transito = case_when(
      (CID3 >= 'V01' & CID3 <= 'V89' ~ 'Transito_todas'),
      TRUE ~ NA_character_
      )
    ) %>%
  filter(!is.na(GRUPO_transito))

#EMPILHAR AS CLASSIFICAÇÕES
SIM_filtrado_transito <- rbind(SIM_transito_grupos, SIM_transito_todos)

#CONTAGEM DE ÓBITOS
NUMERADOR_TRANSITO <- SIM_filtrado_transito %>%
  group_by(CODMUNOCOR, ANOOBITO, GRUPO_transito) %>%
  summarise(obitos = n())%>%
  ungroup() %>%
  filter(CODMUNOCOR >= 350000 & CODMUNOCOR <= 359999) #Filtra municipio de São Paulo

#UNIR COM O MAPA
mapa_transito <- NUMERADOR_TRANSITO %>%
  filter(ANOOBITO %in% c(2015,2024)) %>%
  filter(GRUPO_transito == 'Transito_todas') %>%
  group_by(CODMUNOCOR, ANOOBITO, GRUPO_transito) %>%
  summarise(obitos = sum(obitos, na.rm = TRUE), .groups = 'drop') %>%
  
  #Pivot para ter ano lado a lado
  pivot_wider(names_from = ANOOBITO,
              names_prefix = "ano_",
              values_from = obitos,
              values_fill = 0) %>%
  mutate(
    var_pct = ((ano_2024 - ano_2015) / ano_2015) * 100
  ) %>%
  #caso 2015 for 0
  mutate(var_pct = ifelse(is.infinite(var_pct), 100, var_pct))

#MAPA
mapa_transito <- mapa_transito %>%
  mutate(categoria_var = cut(var_pct,
                             breaks = quantile(var_pct, probs = seq(0, 1, 0.25), na.rm = TRUE),
                             include.lowest  = TRUE,
                             labels = c("1º Quartil (Menor variação)", "2º Quartil", "3º Quartil", "4º Quartil (Maior variação)"))
         )

#UNIR MAPA X DADOS
mapa_transito_final <- left_join(mun_shp, mapa_transito, by = c('CD_MUN' = 'CODMUNOCOR'))

#CONTORNO RS
rs_shp <- mun_shp %>%
  group_by(NOME_RS_2025) %>%
  summarise(geometry = st_union(geom)) %>%
  ungroup()

#MAPA
ggplot(data = mapa_transito_final) +
  geom_sf(aes(fill = categoria_var), color = 'white', size = 0.0) +
  geom_sf(data = rs_shp, fill= 'transparent', color = 'black', size = 0.6) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1, name = 'Variação %') +
  theme_void() +
  labs(
    title = "Variação Percentual do Número de Óbitos por lesões de Trânsito (2024 vs 2015)",
    subtitle = "Municípios de São Paulo com contorno das Regionais de Saúde",
    caption = "Fonte: SIM/DATASUS"
  )


#--------------------------------------------------------#
#-----------------------SUICÍDIO-------------------------#
#--------------------------------------------------------#

SIM_sui <- SIM_CID %>%
  mutate(IDADEanos = as.numeric(IDADEanos)) %>%
  filter(IDADEanos >= 5) %>%
  filter(CID3 >= 'X60' & CID3 <= 'X84' | CAUSABAS == 'Y870') %>%
  mutate(
    FAIXA_ETARIA = case_when(
      IDADEanos >= 5 & IDADEanos < 20 ~ '05-19 anos',
      IDADEanos >= 20 & IDADEanos < 39 ~ '20-39 anos',
      IDADEanos >= 40 & IDADEanos < 59 ~ '40-59 anos',
      IDADEanos >= 60 ~ '60 e mais',
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FAIXA_ETARIA))

#Criando o estrato por sexo e ambos os sexos
df_obitos_sexo_sui <- SIM_sui
df_obitos_sexototal_sui <- SIM_sui %>%
  mutate(SEXO = "Total")

SIM_sui_final <- rbind(df_obitos_sexo_sui, df_obitos_sexototal_sui)

NUMERADOR_SUI <- SIM_sui_final %>%
  group_by(CODMUNRES, SEXO, ANOOBITO, FAIXA_ETARIA) %>%
  summarise(obitos = n()) %>%
  ungroup()

DENOMINADOR_SUI <- pop_bruta_total %>%
  mutate(
    FAIXA_ETARIA = case_when(
        IDADE %in% c("005", "010", "015") ~ "5-19 anos",
        IDADE %in% c("020", "025", "030", "035") ~ "20-39 anos",
        IDADE %in% c("040", "045", "050", "055") ~ "40-59 anos",
        as.numeric(IDADE) >= 60 ~ "60 anos e mais",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FAIXA_ETARIA))

#TRANSFORMAÇÃO PARA LINKAGE ENTRE NUMERADOR E DENOMINADOR
DENOMINADOR_SUI <- DENOMINADOR_SUI %>%
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

chaves_sui <- c("CODMUNRES", "ANOOBITO", "SEXO", "FAIXA_ETARIA")
join_sui <- left_join(NUMERADOR_SUI, DENOMINADOR_SUI, by = chaves_sui) %>%
  filter(!is.na(POP))

sui <- join_sui %>%
  mutate(taxa_mortalidade = (obitos / POP) * 100000 )

#--------------------------------------------------------#
#-------------------------QUEDAS-------------------------#
#--------------------------------------------------------#

SIM_quedas <- SIM_CID %>%
  mutate(IDADEanos = as.numeric(IDADEanos)) %>%
  filter(IDADEanos >= 60) %>%
  mutate(FAIXA_ETARIA = '60 anos e mais') %>%
  filter(CID3 >= 'W00' & CID3 <= 'W19')

#Criando o estrato por sexo e ambos os sexos
df_obitos_sexo_quedas <- SIM_quedas
df_obitos_sexototal_quedas <- SIM_quedas %>%
  mutate(SEXO = "Total")

SIM_quedas_final <- rbind(df_obitos_sexo_quedas, df_obitos_sexototal_quedas)

NUMERADOR_quedas <- SIM_quedas_final %>%
  group_by(CODMUNRES, SEXO, ANOOBITO, FAIXA_ETARIA) %>%
  summarise(obitos = n()) %>%
  ungroup()

DENOMINADOR_quedas <- pop_bruta_total %>%
  mutate(
    FAIXA_ETARIA = case_when(
      as.numeric(IDADE) >= 60 ~ "60 anos e mais",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FAIXA_ETARIA) | IDADE == 059)

#TRANSFORMAÇÃO PARA LINKAGE ENTRE NUMERADOR E DENOMINADOR
DENOMINADOR_quedas <- DENOMINADOR_quedas %>%
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

chaves_quedas <- c("CODMUNRES", "ANOOBITO", "SEXO", "FAIXA_ETARIA")
join_quedas <- left_join(NUMERADOR_quedas, DENOMINADOR_quedas, by = chaves_quedas) %>%
  filter(!is.na(POP))

quedas <- join_quedas %>%
  mutate(taxa_mortalidade = (obitos / POP) * 100000 )

