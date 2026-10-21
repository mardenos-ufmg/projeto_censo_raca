library(tidyverse)

df_dom1 =
  "2022/universo_MG_CSV_2022/Agregados_por_setores_basico_BR.csv" |>
  read.csv(fileEncoding = "Latin1", sep = ";") |>
  as_tibble() |>
  filter(substr(CD_SETOR,1,2) == "31") |>
  select(CD_SETOR, SITUACAO) |>
  rename(
    Cod_setor   = "CD_SETOR",
    situacao    = "SITUACAO"
    ) |>
  mutate(
    situacao = tolower(situacao)
  )

df_dom2 =
  "2022/universo_MG_CSV_2022/Agregados_por_setores_caracteristicas_domicilio1_BR.csv" |>
  read.csv(fileEncoding = "Latin1", sep = ";") |>
  as_tibble() |>
  filter(substr(CD_setor,1,2) == "31") |>
  rename(
    Cod_setor = "CD_setor",
    dom_tot   = "V00001",
    pop_tot   = "V00070"
    ) |>
  select(Cod_setor, dom_tot, pop_tot)

df_dom3 =
  "2022/universo_MG_CSV_2022/Agregados_por_setores_caracteristicas_domicilio2_BR.csv" |>
  read.csv(fileEncoding = "Latin1", sep = ";") |>
  as_tibble() |>
  filter(substr(setor,1,2) == "31") |>
  rename(Cod_setor = "setor") |>
  select(Cod_setor, V00111, V00112, V00309, V00310, V00311) |>
  rename(
    dom_com_agua_rede = "V00111",
    dom_com_agua_poco = "V00112",
    dom_com_rede_esgoto = "V00309",  # Domicílios Particulares Permanentes Ocupados, Destinação do esgoto do banheiro ou sanitário ou buraco para dejeções é rede geral ou pluvial
    dom_com_fossa_na_rede = "V00310",  # Domicílios Particulares Permanentes Ocupados, Destinação do esgoto do banheiro ou sanitário ou buraco para dejeções é fossa séptica ou fossa filtro ligada à rede
    dom_com_fossa_fora_rede = "V00311"  # Domicílios Particulares Permanentes Ocupados, Destinação do esgoto do banheiro ou sanitário ou buraco para dejeções é fossa séptica ou fossa filtro não ligada à rede
  )
  
df_cor =
  "2022/universo_MG_CSV_2022/Agregados_por_setores_cor_ou_raca_BR.csv" |>
  read.csv(fileEncoding = "Latin1", sep = ";") |>
  as_tibble() |>
  filter(substr(CD_SETOR,1,2) == "31") |>
  rename(Cod_setor = "CD_SETOR") |>
  select(Cod_setor, V01317, V01318, V01319, V01320, V01321) |>
  rename(
    pop_branca   = "V01317",
    pop_preta    = "V01318",
    pop_amarela  = "V01319",
    pop_parda    = "V01320",
    pop_indigena = "V01321"
  )

df_renda = 
  "2022/universo_MG_CSV_2022/Agregados_por_setores_renda_responsavel_BR.csv" |>
  read.csv(fileEncoding = "Latin1", sep = ";") |>
  as_tibble() |>
  filter(substr(CD_SETOR,1,2) == "31") |>
  rename(Cod_setor = "CD_SETOR") |>
  select(Cod_setor, V06004) |>
  rename(
    renda_media_responsavel = "V06004"
  )

source("outros_bds/trat_snis.R")
df_2022 =
  df_dom1 |>
  left_join(df_dom2,  by = "Cod_setor") |>
  left_join(df_dom3,  by = "Cod_setor") |>
  left_join(df_cor,   by = "Cod_setor") |>
  left_join(df_renda, by = "Cod_setor") |>
  mutate(across(-c(Cod_setor,situacao), ~ as.numeric(na_if(.x, "X")))) |>
  rowwise() |>
  mutate(
    prop_brancos = 100*pop_branca / pop_tot,
    dom_com_abast_adeq  = sum(dom_com_agua_rede, dom_com_agua_poco, na.rm = T),
    dom_com_esgoto_adeq = sum(dom_com_rede_esgoto, dom_com_fossa_na_rede, dom_com_fossa_fora_rede, na.rm = T),
    dom_com_abast_adeq_missing = all(is.na(c(dom_com_agua_rede, dom_com_agua_poco))),
    dom_com_esgoto_adeq_missing = all(is.na(c(dom_com_rede_esgoto, dom_com_fossa_na_rede, dom_com_fossa_fora_rede)))
  ) |>
  ungroup() |>
  filter(!(dom_com_abast_adeq_missing|dom_com_esgoto_adeq_missing),!is.na(prop_brancos),!is.na(renda_media_responsavel)) |>
  select(Cod_setor, situacao, dom_tot, dom_com_abast_adeq, dom_com_esgoto_adeq, prop_brancos, pop_tot, pop_branca, pop_preta, pop_amarela, pop_parda, pop_indigena, renda_media_responsavel) |>
  rename(
    renda = "renda_media_responsavel"
  ) |>
  mutate(cod_municipio = substr(Cod_setor,1,7) |> as.numeric()) |>
  left_join(gerar_dados_snis(2021), by = "cod_municipio") |>
  filter(!is.na(natureza_juridica)) |>
  mutate(
    # renda = (renda - mean(renda)) / sd(renda)
    renda_std = (renda - median(renda)) / diff(quantile(renda,prob=c(0.25,0.75)))
  ) |>
  filter(dom_tot>0) |>
  rename(cod_setor = "Cod_setor")

df_2022 |> sapply(is.na) |> colSums()

rm(df_dom1, df_dom2, df_dom3, df_cor, df_renda)

write.csv(df_2022, "tratado/setores_2022.csv", row.names = F)