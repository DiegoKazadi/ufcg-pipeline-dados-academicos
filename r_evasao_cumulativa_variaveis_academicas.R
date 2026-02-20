# Verificar nome da coluna antes
names(base_analitica)
# ==========================================================
# 5.10.2 — EVASÃO CUMULATIVA POR VARIÁVEIS ACADÊMICAS

# =========================
# 1) PACOTES
# =========================
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# =========================
# 2) PADRONIZACAO DOS TIPOS
# =========================
base_analitica <- base_analitica %>%
  mutate(
    tipo_evasao_macro = case_when(
      tipo_de_evasao == "GRADUADO" ~ "Concluído",
      tipo_de_evasao == "REGULAR" ~ "Ativo",
      str_detect(tipo_de_evasao, "ABANDONO") ~ "Abandono",
      str_detect(tipo_de_evasao, "SOLICITACAO") ~ "Desistência Formal",
      str_detect(tipo_de_evasao, "REPROV") ~ "Desligamento Acadêmico",
      str_detect(tipo_de_evasao, "MUDANCA|TRANSFERIDO|NOVO INGRESSO") ~ "Mobilidade",
      TRUE ~ "Outros"
    )
  )

# =========================
# 3) FILTRO – 1º AO 4º PERIODO
# =========================
evasao_4p <- base_analitica %>%
  filter(periodo <= 4) %>%
  filter(!tipo_evasao_macro %in% c("Ativo", "Concluído"))

# =========================
# 4) TABELA – TIPO DE EVASAO
# =========================
tabela_tipo <- evasao_4p %>%
  group_by(grupo_curricular, tipo_evasao_macro) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(grupo_curricular) %>%
  mutate(
    Percentual = round(100 * n / sum(n), 2)
  ) %>%
  arrange(grupo_curricular, desc(n))

print(tabela_tipo)

# =========================
# 5) GRAFICO – TIPO DE EVASAO
# =========================
grafico_tipo <- ggplot(tabela_tipo,
                       aes(x = reorder(tipo_evasao_macro, Percentual),
                           y = Percentual,
                           fill = grupo_curricular)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Distribuição dos Tipos de Evasão (1º ao 4º Período)",
    x = "Tipo de Evasão",
    y = "Percentual (%)",
    fill = "Currículo"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )

print(grafico_tipo)

# =========================
# 6) TABELA – PERIODO DE EVASAO
# =========================
tabela_periodo <- evasao_4p %>%
  group_by(grupo_curricular, periodo) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(grupo_curricular) %>%
  mutate(
    Percentual = round(100 * n / sum(n), 2),
    Acumulado = round(cumsum(Percentual), 2)
  )

print(tabela_periodo)

# =========================
# 7) GRAFICO – EVASAO ACUMULADA POR PERIODO
# =========================
grafico_periodo <- ggplot(tabela_periodo,
                          aes(x = periodo,
                              y = Acumulado,
                              color = grupo_curricular)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:4) +
  labs(
    title = "Evasão Acumulada por Período (1º ao 4º)",
    x = "Período",
    y = "Percentual Acumulado (%)",
    color = "Currículo"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )

print(grafico_periodo)

# =========================
# 8) HEATMAP – PERIODO x TIPO
# =========================
heatmap_data <- evasao_4p %>%
  group_by(grupo_curricular, periodo, tipo_evasao_macro) %>%
  summarise(n = n(), .groups = "drop")

grafico_heatmap <- ggplot(heatmap_data,
                          aes(x = periodo,
                              y = tipo_evasao_macro,
                              fill = n)) +
  geom_tile() +
  facet_wrap(~grupo_curricular) +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(
    title = "Intensidade de Evasão por Período e Tipo",
    x = "Período",
    y = "Tipo de Evasão"
  ) +
  theme_minimal()

print(grafico_heatmap)

