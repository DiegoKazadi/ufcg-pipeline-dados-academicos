# ======================================================
# TAXAS DE EVASÃO POR SEXO — P1 A P4
# ======================================================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

# ======================================================
# 1 — Lista estruturada das janelas
# ======================================================

janelas <- list(
  list(coluna = "janela_1999_p1", periodo = "P1"),
  list(coluna = "janela_1999_p2", periodo = "P2"),
  list(coluna = "janela_1999_p3", periodo = "P3"),
  list(coluna = "janela_1999_p4", periodo = "P4"),
  list(coluna = "janela_2017_p1", periodo = "P1"),
  list(coluna = "janela_2017_p2", periodo = "P2"),
  list(coluna = "janela_2017_p3", periodo = "P3"),
  list(coluna = "janela_2017_p4", periodo = "P4")
)

# ======================================================
# 2 — Função de cálculo por sexo
# ======================================================

calcular_taxa_sexo <- function(janela_info) {
  
  coluna  <- janela_info$coluna
  periodo <- janela_info$periodo
  
  base_analitica %>%
    filter(.data[[coluna]] == TRUE) %>%
    group_by(grupo_curricular, sexo) %>%
    summarise(
      total_alunos   = n(),
      total_evadidos = sum(situacao_final == "EVADIDO"),
      taxa_evasao    = total_evadidos / total_alunos,
      .groups        = "drop"
    ) %>%
    mutate(periodo = periodo)
}

# ======================================================
# 3 — Aplicação nas janelas
# ======================================================

taxas_sexo <- map_dfr(janelas, calcular_taxa_sexo) %>%
  mutate(periodo = factor(periodo, levels = c("P1","P2","P3","P4")))

# ======================================================
# 4 — Tabela consolidada
# ======================================================

print(taxas_sexo)

# ======================================================
# 5 — Tabela wide (útil para dissertação)
# ======================================================

tabela_sexo_wide <- taxas_sexo %>%
  select(grupo_curricular, sexo, periodo, taxa_evasao) %>%
  pivot_wider(names_from = periodo, values_from = taxa_evasao)

print(tabela_sexo_wide)

# ======================================================
# 6 — Gráfico comparativo
# ======================================================

ggplot(taxas_sexo,
       aes(x = periodo,
           y = taxa_evasao,
           color = sexo,
           group = sexo)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~grupo_curricular) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Evolução das taxas de evasão por sexo\nCurrículos 1999 e 2017",
    x = "Período do Curso",
    y = "Taxa de Evasão",
    color = "Sexo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
