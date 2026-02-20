# PARTE II — TAXAS ACUMULADAS E VISUALIZAÇÕES

# 1. PACOTES
library(tidyverse)
library(DT)
library(scales)

# 2. PRESSUPOSTO DE ENTRADA
# =========================================================
# Espera-se que a base abaixo JÁ EXISTA no ambiente:
# alunos_final
#
# Colunas mínimas esperadas:
# - grupo_curricular  ("PRE_REFORMA", "POS_REFORMA")
# - periodo           (1, 2, 3, 4 ou equivalente)
# - situacao_final    ("ATIVO", "EVADIDO", "CONCLUIDO")

pasta_dados   <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"
base_objetiva <- "alunos-final"

# =========================================================
# 3. PADRONIZAÇÃO DO PERÍODO
# =========================================================
base_objetiva <- base_objetiva %>%
  mutate(
    periodo = as.integer(periodo),
    periodo_rotulo = case_when(
      periodo == 1 ~ "1º Período",
      periodo == 2 ~ "2º Período",
      periodo == 3 ~ "3º Período",
      periodo == 4 ~ "4º Período",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(periodo_rotulo))

# =========================================================
# 4. BASE DE CONTROLE (TOTAL POR CURRÍCULO)
# =========================================================
base_total <- alunos_final %>%
  group_by(grupo_curricular) %>%
  summarise(
    total_alunos = n(),
    .groups = "drop"
  )

# =========================================================
# 5. CONTAGEM POR PERÍODO E SITUAÇÃO
# =========================================================
contagem_periodo <- alunos_final %>%
  group_by(grupo_curricular, periodo, periodo_rotulo, situacao_final) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )

# =========================================================
# 6. CÁLCULO DAS TAXAS ACUMULADAS
# =========================================================
tabela_evasao_cumulativa <- contagem_periodo %>%
  left_join(base_total, by = "grupo_curricular") %>%
  mutate(
    evasao = if_else(situacao_final == "EVADIDO", n, 0),
    conclusao = if_else(situacao_final == "CONCLUIDO", n, 0)
  ) %>%
  group_by(grupo_curricular, periodo, periodo_rotulo, total_alunos) %>%
  summarise(
    evasao_periodo = sum(evasao),
    conclusao_periodo = sum(conclusao),
    .groups = "drop"
  ) %>%
  arrange(grupo_curricular, periodo) %>%
  group_by(grupo_curricular) %>%
  mutate(
    evasao_acumulada = cumsum(evasao_periodo),
    conclusao_acumulada = cumsum(conclusao_periodo),
    taxa_evasao_cumulativa = evasao_acumulada / total_alunos,
    taxa_conclusao_cumulativa = conclusao_acumulada / total_alunos
  ) %>%
  ungroup()

# =========================================================
# 7. TABELA FINAL (FORMATO PARA DISSERTAÇÃO)
# =========================================================
tabela_final <- tabela_evasao_cumulativa %>%
  select(
    grupo_curricular,
    periodo = periodo_rotulo,
    total_alunos,
    evasao_acumulada,
    taxa_evasao_cumulativa,
    conclusao_acumulada,
    taxa_conclusao_cumulativa
  )

# Visualização interativa
datatable(
  tabela_final,
  rownames = FALSE,
  options = list(pageLength = 8)
)

# =========================================================
# 8. GRÁFICO — TAXA DE EVASÃO ACUMULADA
# =========================================================
ggplot(
  tabela_final,
  aes(
    x = periodo,
    y = taxa_evasao_cumulativa,
    group = grupo_curricular,
    color = grupo_curricular
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "PRE_REFORMA" = "#1f4e79",  # azul escuro
      "POS_REFORMA" = "#ed7d31"   # laranja
    )
  ) +
  labs(
    title = "Taxa de Evasão Acumulada por Período",
    x = "Período do Curso",
    y = "Taxa de Evasão Acumulada",
    color = "Currículo"
  ) +
  theme_minimal()

# =========================================================
# 9. GRÁFICO — TAXA DE CONCLUSÃO ACUMULADA
# =========================================================
ggplot(
  tabela_final,
  aes(
    x = periodo,
    y = taxa_conclusao_cumulativa,
    group = grupo_curricular,
    color = grupo_curricular
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "PRE_REFORMA" = "#548235",  # verde
      "POS_REFORMA" = "#ed7d31"   # laranja
    )
  ) +
  labs(
    title = "Taxa de Conclusão Acumulada por Período",
    x = "Período do Curso",
    y = "Taxa de Conclusão Acumulada",
    color = "Currículo"
  ) +
  theme_minimal()

# =========================================================
# FIM DA PARTE II
# =========================================================
