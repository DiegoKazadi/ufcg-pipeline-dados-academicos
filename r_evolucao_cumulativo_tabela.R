# EVASÃO CUMULATIVA POR PERÍODO

evasao_cumulativa_periodo <- base_analitica %>%
  filter(
    janela_1999_p1 | janela_2017_p1
  ) %>%
  mutate(
    periodo_max = case_when(
      janela_1999_p4 | janela_2017_p4 ~ 4,
      janela_1999_p3 | janela_2017_p3 ~ 3,
      janela_1999_p2 | janela_2017_p2 ~ 2,
      janela_1999_p1 | janela_2017_p1 ~ 1
    ),
    evadido = situacao_final == "EVADIDO"
  ) %>%
  group_by(grupo_curricular, periodo_max) %>%
  summarise(
    total = n(),
    evadidos = sum(evadido),
    .groups = "drop"
  ) %>%
  arrange(grupo_curricular, periodo_max) %>%
  group_by(grupo_curricular) %>%
  mutate(
    evadidos_acumulados = cumsum(evadidos),
    total_base = max(total),
    taxa_cumulativa = round((evadidos_acumulados / total_base) * 100, 2)
  )

evasao_cumulativa_periodo
