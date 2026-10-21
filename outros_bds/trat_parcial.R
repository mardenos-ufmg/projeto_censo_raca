library(tidyverse)

# df_mesoreg =
#   readxl::read_xls("RELATORIO_DTB_BRASIL_2025_MUNICIPIOS.xls", skip = 6) |>
#   select(`Nome Região Geográfica Intermediária`, `Região Geográfica Intermediária`, `Código Município Completo`, `Nome_Município`) |>
#   rename(
#     nome_mesoreg   = "Nome Região Geográfica Intermediária",
#     cod_mesoreg    = "Região Geográfica Intermediária",
#     cod_municipio  = "Código Município Completo",
#     nome_municipio = "Nome_Município"
#     ) |>
#   # mutate(
#   #   cod_municipio_semdig = substring(cod_municipio, 1, 6),
#   #   across(c(cod_mesoreg, cod_municipio, cod_municipio_semdig), as.numeric)
#   # ) |>
#   mutate(
#     across(c(cod_mesoreg, cod_municipio), as.numeric)
#   ) |>
#   filter(substring(cod_municipio, 1, 2) == "31")
  
load("dados_snis.rda")

df_snis =
  dados_snis$`2021`$df |>
  filter(`tipo de serviço` != "Esgoto") |>
  select(`código do município`, `município`, `natureza jurídica`, `código da região intermediária`, `região intermediária`) |>
  rename(
    cod_municipio  = "código do município",
    nome_municipio  = "município",
    natureza_juridica  = "natureza jurídica",
    cod_mesoreg  = "código da região intermediária",
    nome_mesoreg = "região intermediária"
  ) |>
  mutate(
    natureza_juridica = forcats::fct_recode(natureza_juridica, "Economia mista" = "Sociedade de economia mista com administração pública"),
    natureza_juridica = relevel(natureza_juridica, ref = "Economia mista"),
    nome_mesoreg = relevel(nome_mesoreg, ref = "Belo Horizonte")
  )

# df_mesoreg |>
#   left_join(df_snis, by = "cod_municipio")

dados_snis$`2021`$df

rm(dados_snis)