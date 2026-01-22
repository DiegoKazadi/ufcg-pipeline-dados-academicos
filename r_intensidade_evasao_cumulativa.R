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

# GRÁFICO DA EVASÃO CUMULATIVA

ggplot(tabela_evasao_cumulativa,
       aes(x = grupo_curricular,
           y = intensidade_evasao,
           fill = grupo_curricular)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(intensidade_evasao, "%")),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Comparação da Intensidade da Evasão Cumulativa",
    subtitle = "Currículos Pré-Reforma (1999) e Pós-Reforma (2017)",
    x = "Grupo Curricular",
    y = "Intensidade da Evasão (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )
