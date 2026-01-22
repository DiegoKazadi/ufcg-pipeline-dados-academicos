# TABELA DE EVASÃO CUMULATIVA POR CURRÍCULO

tabela_evasao_cumulativa <- base_analitica %>%
  filter(
    (janela_1999_p1 | janela_2017_p1)
  ) %>%
  mutate(
    evadido = situacao_final == "EVADIDO"
  ) %>%
  group_by(grupo_curricular) %>%
  summarise(
    total_estudantes = n(),
    total_evadidos   = sum(evadido),
    intensidade_evasao = round((total_evadidos / total_estudantes) * 100, 2),
    .groups = "drop"
  )

tabela_evasao_cumulativa
