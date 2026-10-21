set.seed(12345)

df_2010 = read.csv("2010/tratado_2010.csv") |> as_tibble()
df_2022 = read.csv("2022/tratado_2022.csv") |> as_tibble()

df = df_2010

K = 10
fold = sample(rep(1:K, length.out = nrow(df)))

resultados = vector("list", K)

for (k in 1:K) {
  treino = df[fold != k, ]
  teste  = df[fold == k, ]
  
  mod = glm(
    cbind(dom_com_esgoto_adeq, dom_tot-dom_com_esgoto_adeq) ~ situacao + prop_brancos * nome_mesoreg,
    # cbind(dom_com_esgoto_adeq, dom_tot-dom_com_esgoto_adeq) ~ situacao + renda * nome_mesoreg,
    family = binomial(),
    data = treino
  )
  
  teste$prop_estimada = predict(
    mod,
    newdata = teste,
    type = "response"
  )
  
  teste$prop_real = teste$dom_com_esgoto_adeq / teste$dom_tot
  
  w = teste$dom_tot
  
  erro = teste$prop_estimada - teste$prop_real
  
  resultados[[k]] = data.frame(
    fold = k,
    n = nrow(teste),
    rmse = sqrt(sum(w * erro^2) / sum(w)),
    mae = sum(w * abs(erro)) / sum(w)
  )
}

resultados = do.call(rbind, resultados)

resultados
resultados |> colMeans()
