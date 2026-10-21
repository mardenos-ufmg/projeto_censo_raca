library(tidyverse)

dados = list()

dados$setores2010 =
  read.csv("tratado/setores_2010.csv", stringsAsFactors = T) |>
  as_tibble() |>
  mutate(
    nome_mesoreg = relevel(nome_mesoreg, ref = "Belo Horizonte")
  )

dados$setores2022 =
  read.csv("tratado/setores_2022.csv", stringsAsFactors = T) |>
  as_tibble() |>
  mutate(
    nome_mesoreg = relevel(nome_mesoreg, ref = "Belo Horizonte")
  )

muni2010 =
  dados$setores2010 |>
  group_by(cod_municipio) |>
  summarise(
    renda = weighted.mean(renda, w = pop_tot, na.rm = TRUE),
    across(
      c(
        dom_tot,
        dom_com_abast_adeq,
        dom_com_esgoto_adeq,
        pop_tot,
        pop_branca,
        pop_preta,
        pop_amarela,
        pop_parda,
        pop_indigena
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    across(
      c(nome_municipio, natureza_juridica, cod_mesoreg, nome_mesoreg),
      ~ first(na.omit(.x))
    ),
    .groups = "drop"
  ) |>
  mutate(
    ano = 2010L,
    renda_std = (renda - median(renda)) / diff(quantile(renda,prob=c(0.25,0.75)))
  ) 

muni2022 =
  dados$setores2022 |>
  group_by(cod_municipio) |>
  summarise(
    renda = weighted.mean(renda, w = dom_tot, na.rm = TRUE),
    across(
      c(
        dom_tot,
        dom_com_abast_adeq,
        dom_com_esgoto_adeq,
        pop_tot,
        pop_branca,
        pop_preta,
        pop_amarela,
        pop_parda,
        pop_indigena
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    across(
      c(nome_municipio, natureza_juridica, cod_mesoreg, nome_mesoreg),
      ~ first(na.omit(.x))
    ),
    .groups = "drop"
  ) |>
  mutate(
    ano = 2022L,
    renda_std = (renda - median(renda)) / diff(quantile(renda,prob=c(0.25,0.75)))
  )

dados$municipios =
  bind_rows(muni2010, muni2022) |>
  group_by(cod_municipio) |>
  filter(n_distinct(ano) == 2) |>
  ungroup() |>
  rowwise() |>
  mutate(
    prop_brancos = pop_branca / pop_tot
  ) |>
  ungroup()

saveRDS(dados, "tratado/dados.rds")
