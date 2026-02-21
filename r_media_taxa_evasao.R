# ======================================================
# BLOCO 10 — TAXAS MÉDIAS DE EVASÃO (P1 A P4)
# ======================================================

# Função para calcular taxa de evasão em uma janela
calcular_taxa <- function(df, var_janela, periodo_label) {
  df %>%
    filter(.data[[var_janela]] == TRUE) %>%
    group_by(grupo_curricular) %>%
    summarise(
      total_alunos = n(),
      total_evadidos = sum(situacao_final == "EVADIDO"),
      taxa_evasao = total_evadidos / total_alunos,
      .groups = "drop"
    ) %>%
    mutate(periodo = periodo_label)
}

# Currículo 1999
taxa_1999_p1 <- calcular_taxa(base_analitica, "janela_1999_p1", "P1")
taxa_1999_p2 <- calcular_taxa(base_analitica, "janela_1999_p2", "P2")
taxa_1999_p3 <- calcular_taxa(base_analitica, "janela_1999_p3", "P3")
taxa_1999_p4 <- calcular_taxa(base_analitica, "janela_1999_p4", "P4")

# Currículo 2017
taxa_2017_p1 <- calcular_taxa(base_analitica, "janela_2017_p1", "P1")
taxa_2017_p2 <- calcular_taxa(base_analitica, "janela_2017_p2", "P2")
taxa_2017_p3 <- calcular_taxa(base_analitica, "janela_2017_p3", "P3")
taxa_2017_p4 <- calcular_taxa(base_analitica, "janela_2017_p4", "P4")

# Consolidar base final
taxas_evolucao <- bind_rows(
  taxa_1999_p1, taxa_1999_p2, taxa_1999_p3, taxa_1999_p4,
  taxa_2017_p1, taxa_2017_p2, taxa_2017_p3, taxa_2017_p4
) %>%
  mutate(
    periodo = factor(periodo, levels = c("P1","P2","P3","P4"))
  )


# ======================================================
# BLOCO 11 — FIGURA 5.3
# Evolução das médias das taxas de evasão
# ======================================================

library(ggplot2)

ggplot(taxas_evolucao, aes(x = periodo,
                           y = taxa_evasao,
                           color = grupo_curricular,
                           group = grupo_curricular)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
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
