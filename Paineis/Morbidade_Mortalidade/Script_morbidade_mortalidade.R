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
library(purrr)

options(download.file.method = "wininet")

#------------------------------------------------------------#
#---TAXAS DE MORTALIDADE PREMATURA POR DCNT (30 A 69 ANOS)---#
#------------------------------------------------------------#

#----EXTRAÇÃO----#
ano_atual <-  as.numeric(format(Sys.Date(), "%Y"))

SIM <- fetch_datasus(year_start = 2015, year_end = ano_atual, uf = "SP", information_system = "SIM-DO")
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

#----#
SIM <- NULL
#----#

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
      CID3 >= 'C00' & CID3 <= 'C97' ~ 'Cancer'
    ) 
  ) %>%
  filter(GRUPO_DCNT == 'Cancer')

#Classificação que agrupa as DCNT
SIM_todas <- SIM_padrao %>%
  mutate(
    GRUPO_DCNT = case_when(
      (CID3 >= 'I00' & CID3 <= 'I99') |
        (CID3 >= 'C00' & CID3 <= 'C97') |
        (CID3 >= 'E10' & CID3 <= 'E14') |
        ((CID3 >= 'J30' & CID3 <= 'J98') & (CID3 != 'J36')) ~ "Todas_DCNT",
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

#CLASSIFICAÇÃO DE FAIXA ETÁRIA
DENOMINADOR <- pop_bruta_total %>%
  mutate(IDADE = as.numeric(IDADE)) %>%
  mutate(
    FAIXA_ETARIA = case_when(
      IDADE >= 30 & IDADE < 40 ~ "30-39 anos",
      IDADE >= 40 & IDADE < 50 ~ "40-49 anos",
      IDADE >= 50 & IDADE < 60 ~ "50-59 anos",
      IDADE >= 60 & IDADE < 70 ~ "60-69 anos",
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

#----LINKAGE ENTRE OS DADOS----#
grupo_dcnt_sim <- c(
  "Cancer",
  "Cancer de Colo de Utero",
  "Cancer de Mama",
  "Cancer do Aparelho Digestivo",
  "Circulatorio",
  "Diabetes",
  "Respiratoria",
  "Todas_DCNT"
)

df_sim_completo <- DENOMINADOR %>%
  tidyr::expand_grid(GRUPO_DCNT = grupo_dcnt_sim) %>%
  filter(!(GRUPO_DCNT %in% c("Cancer de Mama", "Cancer de Colo de Utero") & SEXO != "Feminino"))

#Chaves de junção
chaves <- c("CODMUNRES", "ANOOBITO", "SEXO", "FAIXA_ETARIA", "GRUPO_DCNT")

#---Cálculos----# 
df_taxas <- df_sim_completo %>%
  left_join(NUMERADOR_SIM, by = chaves) %>%
  mutate(
    obitos = replace_na(obitos, 0),
    taxa_bruta_especifica = (obitos / populacao) * 100000,
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

#-----------------------------------------------------------------------#
#---TAXA PADRONIZADA DE MORTALIDADE PREMATURA POR DCNT (30 A 69 ANOS)---#
#-----------------------------------------------------------------------#

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
pop_estado_sim <- DENOMINADOR %>%
  group_by(ANOOBITO, SEXO, FAIXA_ETARIA) %>%
  summarise(total_pop_estado = sum(populacao, na.rm = TRUE)) %>%
  ungroup()

obitos_estado <- NUMERADOR_SIM %>%
  group_by(ANOOBITO, SEXO, GRUPO_DCNT, FAIXA_ETARIA) %>%
  summarise(total_obito_estado = sum(obitos, na.rm = TRUE)) %>%
  ungroup()

#Taxa específica ESTADO
estado_taxa_especifica <- obitos_estado %>%
  left_join(pop_estado_sim, by = c("ANOOBITO", "SEXO", "FAIXA_ETARIA")) %>%
  mutate(taxa_especifica_estado = total_obito_estado / total_pop_estado)

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

#-----------------------------------------------------------------#
#---TAXA BRUTA DE MORTALIDADE PREMATURA POR DCNT (30 A 69 ANOS)---#
#-----------------------------------------------------------------#

#Estado
taxa_bruta_estado <- estado_taxa_especifica %>%
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
    Nivel_Geografico = 'Estado de São Paulo',
    ID_Localidade = '35',
    Nome_Localidade = 'Estado de São Paulo',
    ANOOBITO, SEXO, GRUPO_DCNT,
    obitos_sim = total_obito_estado,
    POP_SIM = total_pop_estado,
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
    obitos_sim = total_obito_rras,
    POP_SIM = total_pop_rras,
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
    obitos_sim = total_obito_rs,
    POP_SIM = total_pop_rs,
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
    obitos_sim = total_obito_mun,
    POP_SIM = total_pop_mun,
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
  select(Nivel_Geografico, ID_Localidade, Nome_Localidade, SEXO, GRUPO_DCNT,
         Taxa_Padronizada_2015 = Taxa_Padronizada,
         Taxa_Bruta_2015 = Taxa_Bruta)
  

mestra_base_2015 <- left_join(
    tabela_mestra_mortalidade,
    taxa_base_2015,
    by = c("Nivel_Geografico", "ID_Localidade", "Nome_Localidade", "SEXO", "GRUPO_DCNT")
  )

#Aplica a lógica condicional para a meta de redução

mestra_mortalidade <- mestra_base_2015 %>%
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
  select(
    "Nivel_Geografico", "ID_Localidade", "Nome_Localidade", "ANOOBITO", "SEXO", "GRUPO_DCNT","obitos_sim", "POP_SIM",
    "Taxa_Padronizada", "Taxa_Padronizada_2015","taxa_padronizada_esperada_meta","Taxa_Bruta", "Taxa_Bruta_2015", "taxa_esperada_meta"
  ) %>%
  filter(POP_SIM != 0)

#Limpar memória
manter <- c(
  "SIM_filtrado", "pop_bruta_total", "RRAS_Municipios", "mestra_mortalidade", "tabela_mestra_mortalidade"
)

rm(list = setdiff(ls(), manter))

gc()

#----------------------------------------------------------------------------#
#---PROBABILIDADE INCONDICIONAL DE MORTE PREMATURA POR DCNT (30 A 69 ANOS)---#
#----------------------------------------------------------------------------#

#Reclassificação de faixa etaria (5 anos)
SIM_prob <- SIM_filtrado %>% 
  mutate(
    FAIXA_5A = case_when(
      IDADEanos >= 30 & IDADEanos < 35 ~ '30-34 anos',
      IDADEanos >= 35 & IDADEanos < 40 ~ '35-39 anos',
      IDADEanos >= 40 & IDADEanos < 45 ~ '40-44 anos',
      IDADEanos >= 45 & IDADEanos < 50 ~ '45-49 anos',
      IDADEanos >= 50 & IDADEanos < 55 ~ '50-54 anos',
      IDADEanos >= 55 & IDADEanos < 60 ~ '55-59 anos',
      IDADEanos >= 60 & IDADEanos < 65 ~ '60-64 anos',
      IDADEanos >= 65 & IDADEanos < 70 ~ '65-69 anos',
      TRUE ~ NA_character_
    )
  ) %>% filter(!is.na(FAIXA_5A))

#Empilhar o total por sexo
obitos_prob_sexo <- SIM_prob
obitos_prob_total <- SIM_prob %>% mutate(SEXO = "Total")
SIM_prob_final <- bind_rows(obitos_prob_sexo, obitos_prob_total)

#Numerador
NUMERADOR_PROB <- SIM_prob_final %>%
  group_by(ANOOBITO, SEXO, CODMUNRES, GRUPO_DCNT, FAIXA_5A) %>%
  summarise(obitos = n(), .groups = 'drop') %>%
  filter(!(GRUPO_DCNT %in% c('Cancer de Mama', 'Cancer de Colo de Utero') & SEXO != 'Feminino'))

#Reclassificação de faixa etaria (5 anos)
DENOMINADOR_PROB <- pop_bruta_total %>%
  mutate(IDADE = as.numeric(IDADE)) %>%
  mutate(
    FAIXA_5A = case_when(
      IDADE >= 30 & IDADE < 35 ~ '30-34 anos',
      IDADE >= 35 & IDADE < 40 ~ '35-39 anos',
      IDADE >= 40 & IDADE < 45 ~ '40-44 anos',
      IDADE >= 45 & IDADE < 50 ~ '45-49 anos',
      IDADE >= 50 & IDADE < 55 ~ '50-54 anos',
      IDADE >= 55 & IDADE < 60 ~ '55-59 anos',
      IDADE >= 60 & IDADE < 65 ~ '60-64 anos',
      IDADE >= 65 & IDADE < 70 ~ '65-69 anos',
      TRUE ~ NA_character_
    )
  ) %>% filter(!is.na(FAIXA_5A))

pop_prob_sexo <- DENOMINADOR_PROB
pop_prob_total <- DENOMINADOR_PROB %>% mutate(SEXO = "Total")

DENOMINADOR_PROB_FINAL <- bind_rows(pop_prob_sexo, pop_prob_total) %>%
  group_by(COD_MUN, ANO, SEXO, FAIXA_5A) %>%
  summarise(populacao = sum(POP, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    CODMUNRES = str_sub(as.character(COD_MUN), 1, 6),
    ANOOBITO = as.double(as.character(ANO)),
    SEXO = case_when(SEXO == "1" ~ "Masculino", SEXO == "2" ~ "Feminino", TRUE ~ SEXO)
  ) %>%
  filter(str_starts(CODMUNRES, "35")) %>%
  select(CODMUNRES, ANOOBITO, SEXO, FAIXA_5A, populacao)

#Padronização
RRAS_RS <- RRAS_Municipios %>%
  mutate(COD_6_mun = as.character(COD_6_mun)) %>%
  mutate(COD_6_mun = str_sub(COD_6_mun, start = 1L, end = 6L)) %>%
  select(CODMUNRES = COD_6_mun, MUNICIPIO, RRAS_2025, NOME_RS_2025) %>%
  distinct(CODMUNRES, .keep_all = TRUE)

POP_GEO <- DENOMINADOR_PROB_FINAL %>%
  left_join(RRAS_RS, by = c("CODMUNRES"))

grupos_dcnt <- unique(NUMERADOR_PROB$GRUPO_DCNT)
BASE_PROB_EXPANDIDA <- expand_grid(POP_GEO, GRUPO_DCNT = grupos_dcnt)

#Filtrar inconsistências antes do join
BASE_PROB_EXPANDIDA <- BASE_PROB_EXPANDIDA %>%
  filter(!(GRUPO_DCNT %in% c('Cancer de Mama', 'Cancer de Colo de Utero') & SEXO != 'Feminino'))

BASE_PROB_COMPLETA <- BASE_PROB_EXPANDIDA %>%
  left_join(NUMERADOR_PROB, by = c("CODMUNRES", "ANOOBITO", "SEXO", "FAIXA_5A", "GRUPO_DCNT")) %>%
  mutate(obitos = replace_na(obitos, 0))

#Cálculo das Probabilidades nas Esferas

calcular_probabilidade <- function(df, ...){
  df %>%
    group_by(..., ANOOBITO, SEXO, GRUPO_DCNT, FAIXA_5A) %>%
    summarise(obitos = sum(obitos), populacao = sum(populacao), .groups = 'drop') %>%
    mutate(
      Mx = obitos / populacao,
      qx = (Mx * 5) / (1 + Mx * 2.5)
    ) %>%
    group_by(..., ANOOBITO, SEXO, GRUPO_DCNT) %>%
    summarise(
      Probabilidade_Incondicional = 1 - prod(1 - qx, na.rm = TRUE),
      .groups = 'drop'
    )
}

prob_estado <- calcular_probabilidade(BASE_PROB_COMPLETA) %>%
  mutate(NIVEL_GEOGRAFICO = 'Estado de São Paulo', ID_Localidade = '35', NOME = 'Estado de São Paulo')

prob_rras <- calcular_probabilidade(BASE_PROB_COMPLETA %>% filter(!is.na(RRAS_2025)), RRAS_2025) %>%
  rename(ID_Localidade = RRAS_2025) %>%
  mutate(NIVEL_GEOGRAFICO  = 'RRAS', NOME = ID_Localidade)

prob_rs <- calcular_probabilidade(BASE_PROB_COMPLETA %>% filter(!is.na(NOME_RS_2025)), NOME_RS_2025) %>%
  rename(ID_Localidade = NOME_RS_2025) %>%
  mutate(NIVEL_GEOGRAFICO  = 'Região de Saúde', NOME = ID_Localidade)

prob_mun <- calcular_probabilidade(BASE_PROB_COMPLETA, CODMUNRES, MUNICIPIO) %>%
  rename(ID_Localidade = CODMUNRES, NOME = MUNICIPIO) %>%
  mutate(NIVEL_GEOGRAFICO  = 'Município')

#Empilhar resultado final
tabela_mestra_probabilidade <- bind_rows(prob_estado, prob_rras, prob_rs, prob_mun) %>%
  select(NIVEL_GEOGRAFICO, ID_Localidade, NOME, ANOOBITO, SEXO, GRUPO_DCNT, Probabilidade_Incondicional) %>%
  mutate(Probabilidade_Incondicional = Probabilidade_Incondicional * 100)

#----CÁLCULO DE METAS DA PROBABILIDADE INCONDICIONAL----#
#Meta de redução: 2% ao ano a partir de 2015

#Isolar o ano-base (2015)
prob_base_2015 <- tabela_mestra_probabilidade %>%
  filter(ANOOBITO == 2015) %>%
  rename(Probabilidade_2015 = Probabilidade_Incondicional) %>%
  select(-ANOOBITO) 

mestra_probabilidade <- left_join(
  tabela_mestra_probabilidade,
  prob_base_2015,
  by = c("NIVEL_GEOGRAFICO", "ID_Localidade", "NOME", "SEXO", "GRUPO_DCNT")
) %>%
  mutate(
    TAXA_ANUAL_REDUCAO_PROB = 0.02, #Redução de 2% ao ano
    prob_esperada_meta = Probabilidade_2015 * (1 - TAXA_ANUAL_REDUCAO_PROB)^(ANOOBITO - 2015)
  ) %>%
  select(
    NIVEL_GEOGRAFICO, ID_Localidade, NOME, ANOOBITO, SEXO, GRUPO_DCNT,
    Probabilidade_Incondicional, Probabilidade_2015, prob_esperada_meta
  )

#Limpar anos sem dados do SIM
ano_max_sim <- max(NUMERADOR_PROB$ANOOBITO, na.rm = TRUE)

mestra_mortalidade <- mestra_mortalidade %>%
  filter(ANOOBITO <= ano_max_sim)

tabela_mestra_mortalidade <- tabela_mestra_mortalidade %>%
  filter(ANOOBITO <= ano_max_sim)

tabela_mestra_probabilidade <- tabela_mestra_probabilidade %>%
  filter(ANOOBITO <= ano_max_sim)

mestra_probabilidade <- mestra_probabilidade %>%
  filter(ANOOBITO <= ano_max_sim)

#Limpar memória
manter <- c(
  "mestra_mortalidade", "tabela_mestra_mortalidade", "tabela_mestra_probabilidade", "mestra_probabilidade"
)

rm(list = setdiff(ls(), manter))

gc()

#---------#
#---SIH---#
#---------#

#-------------------#
#------EXTRAÇÃO-----#
#-------------------#

ano_atual <-  as.numeric(format(Sys.Date(), "%Y"))
anos <- 2015:ano_atual
meses <- str_pad(1:12, width = 2, pad = "0")

lista_dcnt <- list()

cols_cid <- c("DIAG_PRINC", "DIAG_SECUN", "DIAGSEC1", "DIAGSEC2", "DIAGSEC3",
              "DIAGSEC4", "DIAGSEC5", "DIAGSEC6", "DIAGSEC7", "DIAGSEC8", "DIAGSEC9")

regex_circulatorio <- "^I[0-9]{2}"
regex_digestivo    <- "^(C1[5-9]|C2[0-5]|C260|C268|C269|C451|C48|C772|C78[4-8])"
regex_mama         <- "^C50"
regex_colo         <- "^C53"
regex_diabetes     <- "^E1[0-4]"
regex_respiratoria <- "^(J3[0-57-9]|J[4-8][0-9]|J9[0-8])"

regex_dcnt_todas <- paste(regex_circulatorio, regex_digestivo, regex_mama, 
                          regex_colo, regex_diabetes, regex_respiratoria, sep = "|")

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
        select("MUNIC_RES" ,"N_AIH", "SEXO" , "DT_INTER", "DT_SAIDA","QT_DIARIAS", "VAL_TOT", "DIAG_PRINC", "DIAG_SECUN", "COD_IDADE", "IDADE", "MORTE",
               "DIAGSEC1","DIAGSEC2","DIAGSEC3","DIAGSEC4","DIAGSEC5","DIAGSEC6","DIAGSEC7","DIAGSEC8" ,"DIAGSEC9") %>%
        mutate(IDADE = as.numeric(IDADE)) %>%
        filter(str_starts(as.character(MUNIC_RES), "35")) %>%
        filter(
          if_any(
            any_of(cols_cid),
            ~grepl(regex_dcnt_todas, .x)
          )
        )
      
      if (nrow(filtrado) > 0) {
        lista_dcnt[[nome_arquivo]] <- filtrado
      }
      
    }, error = function(e) {
      cat(" -> Aviso: Falha ao processar", nome_arquivo, "- Pode não existir ainda. Erro:", e$message, "\n")
    })
    
    unlink(temp_file)
    suppressWarnings(rm(bruto, processado, filtrado))
    gc()
  }
}

df_dcnt_final <- bind_rows(lista_dcnt)

write.csv2(df_dcnt_final, "C:\\R\\DCNT\\SIH\\sih_dcnt.csv", row.names = FALSE)

#--------------------------#

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

#Tirando SEXO
pop <- pop_bruta_total %>%
  group_by(COD_MUN, ANO) %>%
  summarise(POP_TOTAL = sum(POP, na.rm = TRUE), .groups = "drop") %>%
  mutate(COD_MUN = str_sub(COD_MUN, start = 1L, end = 6L)) %>%
  mutate(ANO = as.double(as.character(ANO))) %>%
  rename(MUNIC_RES = COD_MUN)

#-----------------------------------------------#
#-----------------SIH - DCNT--------------------#
#-----------------------------------------------#

sih_dcnt <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(GRUPO_DCNT = case_when(
    CID3 >= 'I00' & CID3 <= 'I99' ~ 'Circulatorio',
    CID3 >= 'C15' & CID3 <= 'C25' | 
      DIAG_PRINC %in% c("C260", "C268", "C269", "C451", "C772", 
                        "C784", "C785", "C786", "C787", "C788") ~ 'Cancer do Aparelho Digestivo',
    CID3 == 'C50' ~ 'Cancer de Mama',
    CID3 == 'C53' ~ 'Cancer de Colo de Utero', 
    CID3 >= 'E10' & CID3 <= 'E14' ~ 'Diabetes',
    CID3 >= 'J30' & CID3 <= 'J98' & CID3 != 'J36' ~ 'Respiratoria',
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(GRUPO_DCNT))

#Todos os cancers
sih_cancer_todos <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      CID3 >= 'C00' & CID3 <= 'C97' ~ 'Cancer'
    ) 
  ) %>%
  filter(GRUPO_DCNT == 'Cancer')

#Classificação que agrupa as DCNT
sih_todas <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      (CID3 >= 'I00' & CID3 <= 'I99') |
        (CID3 >= 'C00' & CID3 <= 'C97') |
        (CID3 >= 'E10' & CID3 <= 'E14') |
        ((CID3 >= 'J30' & CID3 <= 'J98') & (CID3 != 'J36')) ~ "Todas_DCNT",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(GRUPO_DCNT))

#CSAP
sih_hipertensao <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      CID3 >= 'I10' & CID3 <= 'I14' ~ 'Hipertensao'
    )
  ) %>%
  filter(GRUPO_DCNT == 'Hipertensao')

sih_csap_has_dm <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      (CID3 >= 'I10' & CID3 <= 'I14') | (CID3 >= 'E10' & CID3 <= 'E14') ~ 'CSAP_HAS_DM'
    )
  ) %>%
  filter(GRUPO_DCNT == 'CSAP_HAS_DM')

# DIC - Doença Isquêmica do Coração
sih_dic <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      CID3 >= 'I20' & CID3 <= 'I25' ~ 'DIC'
    )
  ) %>%
  filter(GRUPO_DCNT == 'DIC')

# AVC
sih_avc <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT = case_when(
      CID3 >= 'I60' & CID3 <= 'I69' ~ 'AVC'
    )
  ) %>%
  filter(GRUPO_DCNT == 'AVC')

# AVC e DIC
sih_dic_avc <- df_dcnt_final %>%
  mutate(CID3 = substr(DIAG_PRINC, 1, 3)) %>%
  mutate(
    GRUPO_DCNT= case_when(
      (CID3 >= 'I20' & CID3 <= 'I25') | (CID3 >= 'I60' & CID3 <= 'I69') ~ 'DIC_AVC'
    )
  ) %>%
  filter(GRUPO_DCNT == "DIC_AVC")

#EMPILHAR AS CLASSIFICAÇÕES
sih <- rbind(sih_dcnt, sih_cancer_todos, sih_todas, sih_hipertensao, sih_csap_has_dm, sih_dic, sih_avc,sih_dic_avc)

#limpando a RAM
sih_dcnt <- NULL
sih_cancer_todos <- NULL
sih_todas <- NULL
sih_hipertensao <- NULL
sih_csap_has_dm <- NULL
sih_dic <- NULL
sih_avc <- NULL

gc()

sih <- sih %>%
  mutate(ANO = year(DT_INTER)) %>%
  mutate(MORTE = case_when(
    MORTE == "Sim" ~ 1,
    MORTE == "Não" ~ 0 
  )) %>%
  filter(ANO >= 2015)

#----RRAS----#
RRAS_Municipios <- import("https://github.com/guidoval8/DCNT/blob/main/dados/RRAS_Municipios.xlsx?raw=true")
RRAS_Municipios <- RRAS_Municipios %>%
  mutate(COD_6_mun = as.character(COD_6_mun)) %>%
  mutate(COD_6_mun = str_sub(COD_6_mun, start = 1L, end = 6L)) %>%
  select(COD_6_mun, MUNICIPIO, RRAS_2025, COD_RS_2025, NOME_RS_2025) %>%
  distinct(COD_6_mun, .keep_all = TRUE) %>%
  rename(MUNIC_RES = COD_6_mun)

#--------------------------------------#
#-------TAXA BRUTA DE INTERNAÇÃO-------#
#--------------------------------------#

sih_geo <- sih %>%
  left_join(RRAS_Municipios, by = "MUNIC_RES")

pop_geo <- left_join(pop, RRAS_Municipios, by="MUNIC_RES") %>%
  filter(!is.na(MUNICIPIO))

#Município
pop_mun <- pop_geo %>%
  group_by(ANO, MUNICIPIO) %>%
  summarise(pop = sum(POP_TOTAL, na.rm = TRUE), .groups = "drop")

taxa_municipio <- sih_geo %>%
  group_by(ANO, MUNICIPIO, GRUPO_DCNT) %>%
  summarise(n_internacoes = n(), .groups = "drop") %>%
  left_join(pop_mun, by = c("ANO", "MUNICIPIO")) %>%
  mutate(taxa_bruta_internacao = n_internacoes / pop * 10000) %>%
  mutate(nivel_geografico = "Município") %>% rename(NOME = MUNICIPIO)

#RS
pop_rs <- pop_geo %>%
  group_by(ANO, NOME_RS_2025) %>%
  summarise(pop = sum(POP_TOTAL, na.rm = TRUE), .groups = "drop")

taxa_rs <- sih_geo %>%
  group_by(ANO, NOME_RS_2025, GRUPO_DCNT) %>%
  summarise(n_internacoes = n(), .groups = "drop") %>%
  left_join(pop_rs, by = c("ANO", "NOME_RS_2025")) %>%
  mutate(taxa_bruta_internacao = n_internacoes / pop * 10000) %>%
  mutate(nivel_geografico = "Região de Saúde") %>% rename(NOME = NOME_RS_2025)

#RRAS
pop_rras <- pop_geo %>%
  group_by(ANO, RRAS_2025) %>%
  summarise(pop = sum(POP_TOTAL, na.rm = TRUE), .groups = "drop")

taxa_rras <- sih_geo %>%
  group_by(ANO, RRAS_2025, GRUPO_DCNT) %>%
  summarise(n_internacoes = n(), .groups = "drop") %>%
  left_join(pop_rras, by = c("ANO", "RRAS_2025")) %>%
  mutate(taxa_bruta_internacao = n_internacoes / pop * 10000) %>%
  mutate(nivel_geografico = "RRAS") %>% rename(NOME = RRAS_2025)

#Estado
pop_estado <- pop_geo %>%
  group_by(ANO) %>%
  summarise(pop = sum(POP_TOTAL, na.rm = TRUE), .groups = "drop")

taxa_estado <- sih_geo %>%
  group_by(ANO, GRUPO_DCNT) %>%
  summarise(n_internacoes = n(), .groups = "drop") %>%
  left_join(pop_estado, by = "ANO") %>%
  mutate(taxa_bruta_internacao = n_internacoes / pop * 10000) %>%
  mutate(nivel_geografico = "Estado de São Paulo") %>% mutate(NOME = "Estado de São Paulo")

tabela_mestra_taxa <- rbind(taxa_municipio,taxa_rs,taxa_rras,taxa_estado)

#--------------------------------------#
#-------VALOR MÉDIO E TOTAL------------#
#--------------------------------------#

sih_geo <- sih_geo %>%
  mutate(VAL_TOT = as.numeric(VAL_TOT))

#Município
valor_municipio <- sih_geo %>%
  group_by(ANO, MUNICIPIO, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(valor_medio = valor_total / n_internacoes) %>%
  mutate(nivel_geografico = "Município") %>% rename(NOME = MUNICIPIO)

#RS
valor_rs <- sih_geo %>%
  group_by(ANO, NOME_RS_2025, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(valor_medio = valor_total / n_internacoes) %>%
  mutate(nivel_geografico = "Região de Saúde") %>% rename(NOME = NOME_RS_2025)

#RRAS
valor_rras <- sih_geo %>%
  group_by(ANO, RRAS_2025, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(valor_medio = valor_total / n_internacoes) %>%
  mutate(nivel_geografico = "RRAS") %>% rename(NOME = RRAS_2025)

#Estado
valor_estado <- sih_geo %>%
  group_by(ANO, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    valor_total = sum(VAL_TOT, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(valor_medio = valor_total / n_internacoes) %>%
  mutate(nivel_geografico = "Estado de São Paulo") %>% mutate(NOME = "Estado de São Paulo")

tabela_mestra_valor <- rbind(valor_municipio, valor_rs, valor_rras, valor_estado)

#-------------------------------------------------#
#-------TAXA DE MORTALIDADE HOSPITALAR------------#
#-------------------------------------------------#

#Numerador: Número de óbitos que ocorreram em internações por DIC (CID-10 I20 a I25) ou por AVC (CID-10 I60 a I69) no período.
#Denominador: Número de saídas do hospital (por alta, evasão, desistência do tratamento, transferência externa ou óbito) por DIC (CID-10 I20 a I25) ou por AVC (CID-10 I60 a I69), no mesmo período. O resultado é multiplicado por 100.

# Município
mortalidade_municipio <- sih_geo %>%
  group_by(ANO, MUNICIPIO, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    obitos = sum(MORTE, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(taxa_mortalidade = case_when(
    GRUPO_DCNT == "DIC_AVC" ~ (obitos / n_internacoes) * 100,
    TRUE ~ NA_real_
  )) %>%
  mutate(nivel_geografico = "Município") %>% rename(NOME = MUNICIPIO)

# RS
mortalidade_rs <- sih_geo %>%
  group_by(ANO, NOME_RS_2025, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    obitos = sum(MORTE, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(taxa_mortalidade = case_when(
    GRUPO_DCNT == "DIC_AVC" ~ (obitos / n_internacoes) * 100,
    TRUE ~ NA_real_
  )) %>%
  mutate(nivel_geografico = "Região de Saúde") %>% rename(NOME = NOME_RS_2025)

# RRAS
mortalidade_rras <- sih_geo %>%
  group_by(ANO, RRAS_2025, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    obitos = sum(MORTE, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(taxa_mortalidade = case_when(
    GRUPO_DCNT == "DIC_AVC" ~ (obitos / n_internacoes) * 100,
    TRUE ~ NA_real_
  )) %>%
  mutate(nivel_geografico = "RRAS") %>% rename(NOME = RRAS_2025)

# Estado
mortalidade_estado <- sih_geo %>%
  group_by(ANO, GRUPO_DCNT) %>%
  summarise(
    n_internacoes = n(),
    obitos = sum(MORTE, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(taxa_mortalidade = case_when(
    GRUPO_DCNT == "DIC_AVC" ~ (obitos / n_internacoes) * 100,
    TRUE ~ NA_real_
  )) %>%
  mutate(nivel_geografico = "Estado de São Paulo") %>% mutate(NOME = "Estado de São Paulo")

tabela_mestra_sih_mortalidade <- rbind(mortalidade_municipio, mortalidade_rs, mortalidade_rras, mortalidade_estado) %>%
  rename(obitos_sih = obitos)

#FINAL
chaves_join <- c("ANO", "NOME", "nivel_geografico", "GRUPO_DCNT")

mestra_sih_dcnt <- tabela_mestra_taxa %>%
  left_join(tabela_mestra_valor %>% select(-n_internacoes), by = chaves_join) %>%
  
  left_join(tabela_mestra_sih_mortalidade %>% select(-n_internacoes), by = chaves_join)

write.csv2(mestra_sih_dcnt, "C:\\R\\DCNT\\Paineis\\Morbidade_Mortalidade\\mestra_sih_mortalidade.csv", row.names = FALSE)

#--------------------#
#----Padronização----#
#--------------------#
#mestra_mortalidade

names(mestra_mortalidade) <- toupper(names(mestra_mortalidade))

mestra_mortalidade <- mestra_mortalidade %>%
  select(-ID_LOCALIDADE) %>%
  rename(NOME = NOME_LOCALIDADE, ANO = ANOOBITO)

#mestra_probabilidade

names(mestra_probabilidade) <- toupper(names(mestra_probabilidade))

mestra_probabilidade <- mestra_probabilidade %>%
  select(-ID_LOCALIDADE) %>%
  rename(ANO = ANOOBITO)

#mestra_sih_dcnt

names(mestra_sih_dcnt) <- toupper(names(mestra_sih_dcnt))

mestra_sih_dcnt <- mestra_sih_dcnt %>%
  mutate(SEXO = "Total") %>%
  rename(POP_SIH = POP)

#------------#
#----GRID----#
#------------#

#Esqueleto com todas as possibilidades para evitar que municipios ou regioes com 0 sejam cortadas

geo_mun <- RRAS_Municipios %>%
  transmute(NIVEL_GEOGRAFICO = "Município", ID_LOCALIDADE = MUNIC_RES, NOME = MUNICIPIO) %>%
  distinct()

geo_rs <- RRAS_Municipios %>%
  transmute(NIVEL_GEOGRAFICO = "Região de Saúde", ID_LOCALIDADE = NOME_RS_2025, NOME = NOME_RS_2025) %>%
  distinct() %>% filter(!is.na(ID_LOCALIDADE))

geo_rras <- RRAS_Municipios %>%
  transmute(NIVEL_GEOGRAFICO = "RRAS", ID_LOCALIDADE = RRAS_2025, NOME = RRAS_2025) %>%
  distinct() %>% filter(!is.na(ID_LOCALIDADE))

geo_estado <- data.frame(
  NIVEL_GEOGRAFICO = "Estado de São Paulo",
  ID_LOCALIDADE = "35",
  NOME = "Estado de São Paulo"
)

#Empilhar o esqueleto geográfico (645 Mun + RS + RRAS + Estado)
geo_completa <- bind_rows(geo_mun, geo_rs, geo_rras, geo_estado)

#fixar último ano com dados
ultimo_ano <- max(mestra_mortalidade$ANO, na.rm = TRUE)

anos_unicos <- 2015:ultimo_ano
sexos_unicos <- c("Masculino", "Feminino", "Total")

grupos_dcnt_unicos <- unique(c(
  mestra_mortalidade$GRUPO_DCNT, 
  mestra_sih_dcnt$GRUPO_DCNT
)) %>% na.omit()

#Criar a grid com TODAS as combinações possíveis
esqueleto_bi <- expand_grid(
  geo_completa,
  ANO = anos_unicos,
  SEXO = sexos_unicos,
  GRUPO_DCNT = grupos_dcnt_unicos
)

#Limpar inconsistências lógicas do esqueleto
esqueleto_bi <- esqueleto_bi %>%
  #Remover câncer de mama e colo de útero para homens ou 'Total'
  filter(!(GRUPO_DCNT %in% c('Cancer de Mama', 'Cancer de Colo de Utero') & SEXO != 'Feminino'))
  #filter(!(GRUPO_DCNT %in% c('CSAP_HAS_DM', 'Hipertensao', 'DIC', 'AVC') & SEXO != 'Total'))

tabela_final_power_bi <- esqueleto_bi %>%
  left_join(mestra_mortalidade, by = c("NIVEL_GEOGRAFICO", "NOME", "ANO", "SEXO", "GRUPO_DCNT")) %>%
  left_join(mestra_probabilidade, by = c("NIVEL_GEOGRAFICO", "NOME", "ANO", "SEXO", "GRUPO_DCNT")) %>%
  left_join(mestra_sih_dcnt, by = c("NIVEL_GEOGRAFICO", "NOME", "ANO", "SEXO", "GRUPO_DCNT"))

#Transformar os NAs em 0 nas colunas de métricas
tabela_final_power_bi <- tabela_final_power_bi %>%
  mutate(across(
    .cols = c(contains("TAXA"), contains("PROB"), contains("VALOR"), contains("OBITOS"), contains("POP"), "N_INTERNACOES", -TAXA_MORTALIDADE), 
    .fns = ~replace_na(.x, 0)
  )) %>% #Formatar o nome das DCNT
  mutate(GRUPO_DCNT = case_when(
    GRUPO_DCNT == "Cancer" ~ "Câncer (C00-C97)",
    GRUPO_DCNT == "Cancer de Colo de Utero" ~ "Câncer de colo de útero (C53)",
    GRUPO_DCNT == "Cancer de Mama" ~ "Câncer de mama (C50)",
    GRUPO_DCNT == "Cancer do Aparelho Digestivo" ~ "Câncer de aparelho digestivo (C15-C25...)",
    GRUPO_DCNT == "Circulatorio" ~ "Doenças do aparelho circulatório (I00-I99)",
    GRUPO_DCNT == "Respiratoria" ~ "Doenças respiratórias crônicas (J30-J98)",
    GRUPO_DCNT == "Diabetes" ~ "Diabetes mellitus (E10-E14)",
    GRUPO_DCNT == "Hipertensao" ~ "Hipertensão arterial (I10-I14)",
    GRUPO_DCNT == "CSAP_HAS_DM" ~ "Condições sensíveis à atenção primária em saúde",
    GRUPO_DCNT == "Todas_DCNT" ~ "Todas as DCNT",
    GRUPO_DCNT == "AVC" ~ "AVC (I60-I69)",
    GRUPO_DCNT == "DIC" ~ "DIC (I20-I25)",
    GRUPO_DCNT == "DIC_AVC" ~ "Mortalidade Hospitalar por DIC e AVC",
    TRUE ~ GRUPO_DCNT
  ))

#--------------------------------------------------#
#----MODELAGEM PARA O POWER BI (STAR SCHEMA)-------#
#--------------------------------------------------#

#DIMENSÃO ESPAÇO
dcnt_d_localidade <- tabela_final_power_bi %>%
  select(NIVEL_GEOGRAFICO, ID_LOCALIDADE, NOME) %>%
  distinct() %>%
  mutate(ID_GEO = row_number())

#DIMENSÃO AGRAVO
dcnt_d_agravo <- tabela_final_power_bi %>%
  select(GRUPO_DCNT) %>%
  distinct() %>%
  mutate(ID_AGRAVO = row_number())

#DIMENSÃO TEMPO
dcnt_d_tempo <- tabela_final_power_bi %>%
  select(ANO) %>%
  distinct() %>%
  arrange(ANO) %>%
  mutate(ID_TEMPO = row_number())

#FATO
dcnt_f_indicadores <- tabela_final_power_bi %>%
  left_join(dcnt_d_localidade, by = c("NIVEL_GEOGRAFICO", "ID_LOCALIDADE", "NOME")) %>%
  left_join(dcnt_d_agravo, by = "GRUPO_DCNT") %>%
  left_join(dcnt_d_tempo, by = "ANO") %>%
  select(
    ID_GEO, 
    ID_AGRAVO, 
    ID_TEMPO, 
    SEXO,
    OBITOS_SIM,
    POP_SIM,
    TAXA_PADRONIZADA,
    TAXA_PADRONIZADA_2015,
    TAXA_PADRONIZADA_ESPERADA_META,
    TAXA_BRUTA,
    TAXA_BRUTA_2015,
    TAXA_ESPERADA_META,
    PROBABILIDADE_INCONDICIONAL,
    PROBABILIDADE_2015,
    PROB_ESPERADA_META,
    N_INTERNACOES,
    POP_SIH,
    TAXA_BRUTA_INTERNACAO,
    VALOR_TOTAL,
    VALOR_MEDIO,
    OBITOS_SIH,
    TAXA_MORTALIDADE
  )

write.csv2(dcnt_d_localidade, "C:\\R\\DCNT\\Paineis\\Morbidade_Mortalidade\\dcnt_d_localidade.csv", row.names = FALSE)
write.csv2(dcnt_d_agravo, "C:\\R\\DCNT\\Paineis\\Morbidade_Mortalidade\\dcnt_d_agravo.csv", row.names = FALSE)
write.csv2(dcnt_d_tempo, "C:\\R\\DCNT\\Paineis\\Morbidade_Mortalidade\\dcnt_d_tempo.csv", row.names = FALSE)
write.csv2(dcnt_f_indicadores, "C:\\R\\DCNT\\Paineis\\Morbidade_Mortalidade\\dcnt_f_indicadores.csv", row.names = FALSE, na = "")
write.xlsx(tabela_final_power_bi, "C:\\R\\DCNT\\Paineis\\Morbidade_Mortalidade\\morbidade_mortalidade.xlsx")

gc()

