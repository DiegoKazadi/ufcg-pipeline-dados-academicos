# ======================================================
# SCRIPT COMPLETO — MÉDIAS DE EVASÃO E GRÁFICO
# ======================================================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

# ======================================================
# 1 — Definição das janelas (coluna real + período)
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
# 2 — Função para calcular taxa de evasão
# ======================================================
calcular_taxa <- function(janela_info) {
  coluna <- janela_info$coluna
  periodo <- janela_info$periodo
  
  base_analitica %>%
    filter(.data[[coluna]] == TRUE) %>%
    group_by(grupo_curricular) %>%
    summarise(
      total_alunos   = n(),
      total_evadidos = sum(situacao_final == "EVADIDO"),
      taxa_evasao    = total_evadidos / total_alunos,
      .groups        = "drop"
    ) %>%
    mutate(periodo = periodo)
}

# ======================================================
# 3 — Aplicar função para todas as janelas
# ======================================================
taxas_evolucao <- map_dfr(janelas, calcular_taxa) %>%
  mutate(periodo = factor(periodo, levels = c("P1","P2","P3","P4")))

# ======================================================
# 4 — Criar tabela wide para visualização
# ======================================================
taxas_wide <- taxas_evolucao %>%
  select(grupo_curricular, periodo, taxa_evasao) %>%
  pivot_wider(names_from = periodo, values_from = taxa_evasao)

print(taxas_wide)

# ======================================================
# 5 — Gráfico de evolução das médias por grupo curricular
# ======================================================
ggplot(taxas_evolucao,
       aes(x = periodo,
           y = taxa_evasao,
           color = grupo_curricular,
           group = grupo_curricular)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Evolução das médias das taxas de evasão\nCurrículos 1999 e 2017",
    x = "Período do Curso",
    y = "Taxa Média de Evasão",
    color = "Grupo Curricular"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
