library(tidyverse)
library(glmnet)

df_2010 = read.csv("2010/tratado_2010.csv", stringsAsFactors = T) |> as_tibble()
df_2022 = read.csv("2022/tratado_2022.csv", stringsAsFactors = T) |> as_tibble()

df = df_2022

x = model.matrix(
  # ~ situacao + (prop_brancos + renda) * nome_mesoreg,
  ~ (prop_brancos + renda) * nome_mesoreg,
  # ~ situacao + prop_brancos * renda * nome_mesoreg,
  data = df
)[, -1]

y = cbind(
  df$dom_tot - df$dom_com_esgoto_adeq,
  df$dom_com_esgoto_adeq
)

mod = cv.glmnet(
  x = x,
  y = y,
  alpha = 1,
  family = "binomial",
  type.measure = "mse"
)

coef(mod, s = "lambda.1se")
