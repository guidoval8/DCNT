SIH15 <- fetch_datasus(year_start = 2015, year_end = 2015, month_start = 1, month_end = 12, uf = "SP", information_system = "SIH-RD")
SIH15 <- process_sih(SIH15)

library(read.dbc)
library(fst)
library(data.table)

sih <- read.dbc("C:\\R\\DCNT\\SIH\\RDSP1608.dbc")
SIH15 <- process_sih(sih)

freq(SIH15$DT_INTER)

names(SIH15)

colunas <- c("MUNIC_RES" ,"N_AIH", "SEXO" , "DT_INTER", "VAL_TOT", "DIAG_PRINC", "DIAG_SECUN", "COD_IDADE", "IDADE", "MORTE",
             "DIAGSEC1","DIAGSEC2","DIAGSEC3","DIAGSEC4","DIAGSEC5","DIAGSEC6","DIAGSEC7","DIAGSEC8" ,"DIAGSEC9")

T1 <- sih %>%
  filter(IDADE >= 60) %>%
  mutate(
    DIAG_PRINC = str_trim(toupper(as.character(DIAG_PRINC))),
    CID3 = str_sub(DIAG_PRINC, 1,3)) %>%
  filter(CID3 >= 'W00' & CID3 <= 'W19') 

freq(SIH15$DIAGSEC6)

arquivos <- list.files(
  path = "C:\\R\\DCNT\\SIH",
  pattern = "\\.dbc$",
  full.names = TRUE
)

for (arq in arquivos) {
  
  message("Processando: ", basename(arq))
  
  df <- read.dbc(arq) |>
    process_sih() |>
    select(any_of(colunas)) |>
    filter(IDADE >= 60) %>%
    filter(str_starts(MUNIC_RES, "35")) %>%
  
  # Converter para data.table (menos memória)
  setDT(df)
  
  # Nome do arquivo de saída
  nome_saida <- paste0(
    "C:\\R\\DCNT\\SIH\\resultado_parcial\\",
    tools::file_path_sans_ext(basename(arq)),
    ".fst"
  )
  
  write_fst(df, nome_saida, compress = 100)
  
  rm(df)
  gc()
}

arquivos_fst <- list.files(
  "C:\\R\\DCNT\\SIH\\resultado_parcial\\",
  pattern = "\\.fst$",
  full.names = TRUE
)

df_final <- rbindlist(
  lapply(arquivos_fst, read_fst),
  use.names = TRUE,
  fill = TRUE
)

write.csv2(df_quedas, "C:\\R\\DCNT\\sih_quedas.csv", row.names = FALSE)

cols_cid <- c("DIAG_PRINC", "DIAG_SECUN", "DIAGSEC1", "DIAGSEC2", "DIAGSEC3",
              "DIAGSEC4", "DIAGSEC5", "DIAGSEC6", "DIAGSEC7", "DIAGSEC8", "DIAGSEC9")

df_quedas <- df_final %>%
  filter(
    if_any(
      all_of(cols_cid),
      ~grepl("^W(0[0-9|1[0-9])", .x)
    )
  )
