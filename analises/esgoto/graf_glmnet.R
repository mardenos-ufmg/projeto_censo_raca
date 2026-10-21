newdata =
  tidyr::crossing(
    prop_brancos = seq(0, 100, length.out = 100),
    nome_mesoreg = factor(levels(df$nome_mesoreg), levels(df$nome_mesoreg))
  ) |>
  mutate(
    situacao = factor(as.character(unique(df$situacao)[1]), levels = levels(df$situacao)),
    renda = 0
  )
# newdata =
#   tidyr::crossing(
#     renda = seq(-.8, 1, length.out = 100),
#     nome_mesoreg = levels(df$nome_mesoreg)
#   ) |>
#   mutate(
#     situacao = factor(as.character(unique(df$situacao)[1]), levels = levels(df$situacao)),
#     prop_brancos = 50
#   )


newx = model.matrix(
  ~ situacao + (prop_brancos + renda) * nome_mesoreg,
  data = newdata
)[, -1, drop = FALSE]

newx = newx[, colnames(x), drop = FALSE]

newdata$pred = drop(predict(
  mod,
  newx = newx,
  s = "lambda.1se",
  type = "response"
))

# ggplot(newdata, aes(x = renda, y = pred)) +
ggplot(newdata, aes(x = prop_brancos, y = pred)) +
  geom_line() +
  facet_wrap(~ nome_mesoreg) +
  labs(
    x = "Proporção de brancos",
    # x = "renda padronizada",
    y = "Prob(esgoto adequado)"
  ) +
  # scale_y_continuous(limits = c(0.5, 1))
  scale_y_continuous(limits = c(0.85, 1))

