gerar_dados_snis = function(ano) {
  load("outros_bds/dados_snis.rda")
  for (k in 0:10) {
    ano_temp = ano - k
    df_temp = dados_snis[[as.character(ano_temp)]]$df
    if(is.null(df_temp)) next
    df_temp = 
      df_temp |>
      filter(`tipo de serviço` != "Esgoto") |>
      select(
        `código do município`, município, `natureza jurídica`,
        `código da região intermediária`, `região intermediária`
      ) |>
      rename(
        cod_municipio = `código do município`,
        nome_municipio = município,
        natureza_juridica = `natureza jurídica`,
        cod_mesoreg = `código da região intermediária`,
        nome_mesoreg = `região intermediária`
      ) |>
      mutate(
        natureza_juridica = forcats::fct_recode(
          natureza_juridica,
          "Economia mista" = "Sociedade de economia mista com administração pública"
        )
      ) |>
      distinct(cod_municipio, .keep_all = TRUE)
    
    if (k == 0) df_snis = df_temp
    cod_faltantes = setdiff(df_temp$cod_municipio, df_snis$cod_municipio)
    if (length(cod_faltantes)) {
      df_snis = bind_rows(
        df_snis,
        df_temp |> filter(cod_municipio %in% cod_faltantes)
      )
    }
  }
  rm(dados_snis)
  df_snis
}
