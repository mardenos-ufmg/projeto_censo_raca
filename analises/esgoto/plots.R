library(tidyverse)

dados = readRDS("tratado/dados.rds")

df = dados$municipios |> filter(ano == 2010)

mod = glm(
  cbind(dom_com_esgoto_adeq, dom_tot-dom_com_esgoto_adeq) ~ prop_brancos*nome_mesoreg,
  family = binomial(),
  data = df
  )

graficos = list()
for (reg in unique(df$nome_mesoreg)) {
  quant = df |> filter(nome_mesoreg == reg) |> pull(prop_brancos) |> quantile(probs = c(0.05,0.95))
  # newdata = data.frame(
  #   prop_brancos = seq(quant[1], quant[2], length.out = 100),
  #   nome_mesoreg = reg#,
  #   # renda = 0
  # )
  # newdata$pred = predict(
  #   mod,
  #   newdata = newdata,
  #   type = "response"
  # )

  # newdata = data.frame(
  #   situacao = unique(df$situacao)[1],
  #   prop_brancos = seq(quant[1], quant[2], length.out = 100),
  #   renda = mean(df$renda, na.rm = TRUE),
  #   nome_mesoreg = reg
  # )
  # newx = model.matrix(
  #   ~ situacao + prop_brancos + renda + nome_mesoreg,
  #   data = newdata
  # )[, -1]
  
  newdata = tibble(
    # situacao = factor(
    #   unique(df$situacao)[1],
    #   levels = levels(df$situacao)
    # ),
    prop_brancos = seq(quant[1], quant[2], length.out = 100),
    renda = mean(df$renda, na.rm = TRUE),
    nome_mesoreg = factor(
      reg,
      levels = levels(df$nome_mesoreg)
    )
  )
  
  newx = model.matrix(
    # ~ situacao + prop_brancos + renda + nome_mesoreg,
    ~ (prop_brancos + renda) + nome_mesoreg,
    data = newdata
  )[, -1]
  
  head(newx)
  newdata$pred = predict.glmnet(
    mod,
    newx = newx,
    s = "lambda.1se",
    type = "response"
  )
  graficos[[reg]] =
    ggplot(newdata, aes(x = prop_brancos, y = pred)) +
    geom_line() +
    labs(
      title = reg,
      x = "Proporção de brancos",
      y = "Prob(esgoto adequado)"
    ) + 
    # scale_y_continuous(limits = c(0.6, 1))
    scale_y_continuous(limits = c(0, 1))
}
patchwork::wrap_plots(graficos)













avg_predictions(mod_esgoto, by = "regiao_nome") |>
  ggplot(aes(estimate, reorder(regiao_nome, estimate))) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  labs(x = "P(esgoto adequado) ajustada", y = NULL) +
  theme_minimal()

avg_predictions(mod_agua, by = "regiao_nome") |>
  ggplot(aes(estimate, reorder(regiao_nome, estimate))) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  labs(x = "P(agua adequado) ajustada", y = NULL) +
  theme_minimal()

avg_predictions(mod_agua, by = "natureza_juridica") |>
  ggplot(aes(estimate, reorder(natureza_juridica, estimate))) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  labs(x = "P(agua adequado) ajustada", y = NULL) +
  theme_minimal()
