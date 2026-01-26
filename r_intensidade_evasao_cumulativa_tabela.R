# INTENSIDADE FINAL DA EVASÃO CUMULATIVA

tabela_intensidade_cumulativa <- evasao_cumulativa_periodo %>%
  filter(periodo_max == 4) %>%
  select(
    grupo_curricular,
    total_base,
    evadidos_acumulados,
    taxa_cumulativa
  ) %>%
  rename(
    total_estudantes = total_base,
    total_evadidos = evadidos_acumulados,
    intensidade_evasao_cumulativa = taxa_cumulativa
  )

tabela_intensidade_cumulativa

#  RITMO DE ACUMULAÇÃO DA EVASÃO

ritmo_evasao <- evasao_cumulativa_periodo %>%
  group_by(grupo_curricular) %>%
  arrange(periodo_max) %>%
  mutate(
    variacao_periodo = taxa_cumulativa - lag(taxa_cumulativa)
  ) %>%
  ungroup()

ritmo_evasao

# GRÁFICO DO RITMO DE EVASÃO

ggplot(ritmo_evasao,
       aes(x = periodo_max,
           y = variacao_periodo,
           fill = grupo_curricular)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = 1:4) +
  labs(
    title = "Ritmo de Acumulação da Evasão por Período",
    x = "Período",
    y = "Variação da Taxa Cumulativa (%)",
    fill = "Currículo"
  ) +
  theme_minimal()
