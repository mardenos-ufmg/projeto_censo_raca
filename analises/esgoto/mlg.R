library(tidyverse)
set.seed(12345)

df_2010 = read.csv("2010/tratado_2010.csv") |> as_tibble()
df_2022 = read.csv("2022/tratado_2022.csv") |> as_tibble()

df = df_2022

mod = glm(
  # cbind(dom_com_esgoto_adeq, dom_tot-dom_com_esgoto_adeq) ~ situacao + prop_brancos * nome_mesoreg,
  # cbind(dom_com_esgoto_adeq, dom_tot-dom_com_esgoto_adeq) ~ situacao + renda * nome_mesoreg,
  cbind(dom_com_esgoto_adeq, dom_tot-dom_com_esgoto_adeq) ~ situacao + renda + prop_brancos + nome_mesoreg,
  family = binomial(),
  data = df
)

broom::tidy(mod) |> print(n=30)
car::vif(mod)