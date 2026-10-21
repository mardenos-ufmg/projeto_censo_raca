library(tidyverse)

df_dom =
  "2010/universo_MG_CSV_2010/Domicilio01_MG.csv" |>
  read.csv(sep = ";") |>
  as_tibble() |>
  select(Cod_setor, Situacao_setor, V002, V012, V013, V017, V018) |>
  rename(
    situacao = "Situacao_setor",
    dom_tot = "V002",
    dom_com_agua_rede = "V012",
    dom_com_agua_poco = "V013",
    dom_com_banheiro_e_rede_esgoto = "V017",
    dom_com_banheiro_e_fossa = "V018"
  )

df_cor = 
  "2010/universo_MG_CSV_2010/Pessoa03_MG.csv" |>
  read.csv(sep = ";") |>
  as_tibble() |>
  select(Cod_setor, V001, V002, V003, V004, V005, V006) |>
  rename(
    pop_tot      = "V001",
    pop_branca   = "V002",
    pop_preta    = "V003",
    pop_amarela  = "V004",
    pop_parda    = "V005",
    pop_indigena = "V006"
  )

df_renda = 
  "2010/universo_MG_CSV_2010/PessoaRenda_MG.csv" |>
  read.csv(sep = ";") |>
  as_tibble() |>
  select(Cod_setor, V022) |>
  rename(
    renda = "V022"
  )

df_2010 =
  df_dom |>
  left_join(df_cor,   by = "Cod_setor") |>
  left_join(df_renda, by = "Cod_setor") |>
  mutate(across(-c(Cod_setor,pop_tot,dom_tot,situacao), ~ as.numeric(na_if(.x, "X"))))

# df_2010[which(as.logical(rowSums(is.na(df_2010)))),] |> View()

source("outros_bds/trat_snis.R")
df_2010 =
  df_2010 |>
  filter(!as.logical(rowSums(is.na(df_2010)))) |>
  rowwise() |>
  mutate(
    prop_brancos = 100*pop_branca / pop_tot,
    renda_per_capita = renda / pop_tot,
    situacao = ifelse(situacao %in% 1:3, "urbana", "rural"),
    dom_com_abast_adeq  = dom_com_agua_rede + dom_com_agua_poco,
    dom_com_esgoto_adeq = dom_com_banheiro_e_rede_esgoto + dom_com_banheiro_e_fossa
  ) |>
  ungroup() |>
  select(Cod_setor, situacao, dom_tot, dom_com_abast_adeq, dom_com_esgoto_adeq, prop_brancos, pop_tot, pop_branca, pop_preta, pop_amarela, pop_parda, pop_indigena, renda_per_capita) |>
  rename(
    renda = "renda_per_capita"
  ) |>
  mutate(cod_municipio = substr(Cod_setor,1,7) |> as.numeric()) |>
  left_join(gerar_dados_snis(2010), by = "cod_municipio") |>
  filter(!is.na(natureza_juridica)) |>
  mutate(
    # renda = (renda - mean(renda)) / sd(renda)
    renda_std = (renda - median(renda)) / diff(quantile(renda,prob=c(0.25,0.75)))
  ) |>
  filter(dom_tot>0) |>
  rename(cod_setor = "Cod_setor")

rm(df_dom, df_cor, df_renda)

write.csv(df_2010, "tratado/setores_2010.csv", row.names = F)