# ======================================================
# BLOCO 12 — TAXA DE EVASÃO POR SEXO
# Figura 5.4
# ======================================================

calcular_taxa_sexo <- function(df, var_janela, periodo_label) {
  df %>%
    filter(.data[[var_janela]] == TRUE) %>%
    group_by(sexo) %>%
    summarise(
      total_alunos = n(),
      total_evadidos = sum(situacao_final == "EVADIDO"),
      taxa_evasao = total_evadidos / total_alunos,
      .groups = "drop"
    ) %>%
    mutate(periodo = periodo_label)
}

# Períodos
sexo_p1 <- calcular_taxa_sexo(base_analitica, "janela_1999_p1", "P1")
sexo_p2 <- calcular_taxa_sexo(base_analitica, "janela_1999_p2", "P2")
sexo_p3 <- calcular_taxa_sexo(base_analitica, "janela_1999_p3", "P3")
sexo_p4 <- calcular_taxa_sexo(base_analitica, "janela_1999_p4", "P4")

# Consolidar
evasao_sexo <- bind_rows(sexo_p1, sexo_p2, sexo_p3, sexo_p4) %>%
  mutate(
    periodo = factor(periodo, levels = c("P1","P2","P3","P4")),
    sexo = case_when(
      sexo %in% c("M","MASCULINO") ~ "Masculino",
      sexo %in% c("F","FEMININO")  ~ "Feminino",
      TRUE ~ as.character(sexo)
    )
  )

# ======================================================
# BLOCO 13 — FIGURA 5.4
# Taxa de evasão por sexo nos quatro períodos
# ======================================================

library(ggplot2)

ggplot(evasao_sexo,
       aes(x = periodo,
           y = taxa_evasao,
           color = sexo,
           group = sexo)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Taxa de evasão por sexo ",
    x = "Período do Curso",
    y = "Taxa de Evasão",
    color = "Sexo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


# ======================================================
# BLOCO 14 — FIGURA 5.4 (VERSÃO BARRAS)
# Taxa de evasão por sexo nos quatro períodos
# ======================================================

library(ggplot2)

ggplot(evasao_sexo,
       aes(x = periodo,
           y = taxa_evasao,
           fill = sexo)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Figura 5.4 — Taxa de evasão por sexo nos quatro primeiros períodos",
    x = "Período do Curso",
    y = "Taxa de Evasão",
    fill = "Sexo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# ======================================================
# BLOCO 15 — ALUNOS ATIVOS POR PERÍODO E CURRÍCULO
# ======================================================

ativos_por_ingresso_curriculo <- base_completa %>%
  filter(situacao_final == "ATIVO",
         grupo_curricular %in% c("PRE_REFORMA","POS_REFORMA")) %>%
  group_by(periodo_ingresso, grupo_curricular) %>%
  summarise(
    total_ativos = n(),
    .groups = "drop"
  ) %>%
  arrange(periodo_ingresso)

glimpse(ativos_por_ingresso_curriculo)

# ======================================================
# GRÁFICO — ATIVOS POR INGRESSO E CURRÍCULO
# ======================================================

ggplot(ativos_por_ingresso_curriculo,
       aes(x = periodo_ingresso,
           y = total_ativos,
           color = grupo_curricular,
           group = grupo_curricular)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Evolução do Número de Alunos Ativos por Período de Ingresso",
    x = "Período de Ingresso",
    y = "Número de Alunos Ativos",
    color = "Currículo"
  ) +
  scale_color_manual(
    values = c("PRE_REFORMA" = "#1F77B4",
               "POS_REFORMA" = "#D62728"),
    labels = c("PRE_REFORMA" = "Currículo 1999",
               "POS_REFORMA" = "Currículo 2017")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

