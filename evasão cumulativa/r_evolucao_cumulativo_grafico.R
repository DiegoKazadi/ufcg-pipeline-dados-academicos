# BLOCO 13 — GRÁFICO DA EVASÃO CUMULATIVA

ggplot(evasao_cumulativa_periodo,
       aes(x = periodo_max,
           y = taxa_cumulativa,
           color = grupo_curricular,
           group = grupo_curricular)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:4) +
  labs(
    title = "Evolução Cumulativa da Evasão nos Períodos Iniciais",
    x = "Período",
    y = "Taxa de Evasão Cumulativa (%)",
    color = "Currículo"
  ) +
  theme_minimal()
