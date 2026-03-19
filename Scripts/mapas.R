# Carregar os pacotes necessários
library(sf)
library(dplyr)
library(readxl) # Use library(readr) se a sua planilha for um arquivo .csv
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
# 1. Carregar o shapefile de municípios e a planilha
# Substitua pelos caminhos reais dos seus arquivos
mapa_municipios <- st_read("C:\\R\\DCNT\\mapa_municipios_sp\\mapa_municipios_sp.shp")

RRAS_Municipios <- import("https://github.com/guidoval8/DCNT/blob/main/dados/RRAS_Municipios.xlsx?raw=true")

RRAS_Municipios <- RRAS_Municipios %>%
  mutate(COD_6_mun = as.character(COD_6_mun)) %>%
  mutate(COD_6_mun = str_sub(COD_6_mun, start = 1L, end = 6L)) %>%
  select(COD_6_mun, MUNICIPIO, RRAS_2025, COD_RS_2025, NOME_RS_2025) %>%
  distinct(COD_6_mun, .keep_all = TRUE) %>%
  rename(CODMUNRES = COD_6_mun)

# 2. Fazer o Join (Cruzamento)
# IMPORTANTE: Substitua 'codigo_ibge' pelo nome exato da coluna que liga os dois arquivos.
# Recomendo fortemente usar o código IBGE em vez do nome da cidade para evitar erros de acentuação.
mapa_completo <- mapa_municipios %>%
  left_join(RRAS_Municipios, by = "CODMUNRES")

mapa_completo <- mapa_completo %>%
  distinct(CODMUNRES, .keep_all = TRUE)

# 3. Gerar o contorno das Regiões de Saúde (RS) - Operação de Dissolve
mapa_rs <- mapa_completo %>%
  group_by(NOME_RS_2025) %>% # Substitua pelo nome da coluna das RS na sua planilha
  summarise() # O summarise sem argumentos faz a união geométrica (dissolve)

# 4. Gerar o contorno das RRAS - Operação de Dissolve
mapa_rras <- mapa_completo %>%
  group_by(RRAS_2025) %>% # Substitua pelo nome da coluna das RRAS na sua planilha
  summarise()

# 5. Exportar os novos shapefiles
# O st_write exporta automaticamente para a extensão que você definir no nome (.shp)
st_write(mapa_rs, "C:\\R\\DCNT\\mapa_municipios_sp\\mapa_rs_sp.geojson", delete_layer = TRUE)
st_write(mapa_rras, "C:\\R\\DCNT\\mapa_municipios_sp\\mapa_rras_sp.geojson", delete_layer = TRUE)

print("Shapefiles exportados com sucesso!")