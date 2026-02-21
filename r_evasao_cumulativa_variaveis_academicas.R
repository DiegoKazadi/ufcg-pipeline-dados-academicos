# ==========================================================
# 5.10.2 — COMPOSIÇÃO DOS TIPOS DE EVASÃO ATÉ O 4º PERÍODO
# ==========================================================
library(tidyverse)
library(scales)

# 1) Criar categorias macro de evasão (caso não existam)
base_analitica <- base_analitica %>%
  mutate(
    tipo_evasao_macro = case_when(
      str_detect(tipo_de_evasao, "ABANDONO") ~ "Abandono",
      str_detect(tipo_de_evasao, "SOLICITACAO") ~ "Desistência Formal",
      str_detect(tipo_de_evasao, "REPROV") ~ "Desligamento Acadêmico",
      str_detect(tipo_de_evasao, "MUDANCA|TRANSFERIDO|NOVO INGRESSO") ~ "Mobilidade",
      TRUE ~ tipo_de_evasao  # mantém outras categorias se houver
    )
  )

# 2) Filtrar apenas evasões ocorridas até o 4º período
#    (exclui graduados e regulares)
evasao_4p <- base_analitica %>%
  filter(
    tipo_de_evasao != "GRADUADO",
    tipo_de_evasao != "REGULAR",
    !is.na(periodo_evasao_relativo),
    periodo_evasao_relativo <= 4,
    !is.na(grupo_curricular)
  )

# 3) Tabela de composição por currículo e tipo macro
tabela_tipo <- evasao_4p %>%
  count(Curriculo = grupo_curricular, 
        Tipo = tipo_evasao_macro) %>%
  group_by(Curriculo) %>%
  mutate(
    Percentual = round(n / sum(n) * 100, 2),
    # Formatação para exibição
    `N (evadidos)` = n,
    `%` = Percentual
  ) %>%
  ungroup() %>%
  select(Curriculo, Tipo, `N (evadidos)`, `%`) %>%
  arrange(Curriculo, desc(`%`))

# Exibir a tabela completa
print(tabela_tipo, n = Inf)

# 4) Tabela em formato wide (se preferir comparar os percentuais entre currículos)
tabela_tipo_wide <- tabela_tipo %>%
  select(Curriculo, Tipo, `%`) %>%
  pivot_wider(
    names_from = Curriculo,
    values_from = `%`,
    values_fill = 0
  ) %>%
  rename(`Tipo de Evasão` = Tipo)

print(tabela_tipo_wide)

# 5) Gráfico de barras comparativo
grafico_tipo <- tabela_tipo %>%
  ggplot(aes(x = reorder(Tipo, `%`), 
             y = `%`, 
             fill = Curriculo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Currículo 1999" = "#1f77b4", 
                               "Currículo 2017" = "#ff7f0e")) +
  labs(
    title = "Composição dos Tipos de Evasão",
    subtitle = "Distribuição percentual dentro de cada currículo",
    x = NULL,
    y = "Percentual (%)",
    fill = "Currículo"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(grafico_tipo)

# Salvar (opcional)
# ggsave("grafico_tipos_evasao.png", grafico_tipo, width = 8, height = 5, dpi = 300)
# write.csv2(tabela_tipo, "tabela_tipos_evasao.csv", row.names = FALSE)

# 5) Gráfico de linhas comparativo
grafico_tipo <- tabela_tipo %>%
  ggplot(aes(x = Tipo, 
             y = `%`, 
             group = Curriculo, 
             color = Curriculo)) +
  geom_line(size = 1.1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Currículo 1999" = "#1f77b4", 
                                "Currículo 2017" = "#ff7f0e")) +
  labs(
    title = "Composição dos Tipos de Evasão",
    subtitle = "Distribuição percentual até o 4º período",
    x = "Tipo de Evasão",
    y = "Percentual (%)",
    color = "Currículo"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(grafico_tipo)
