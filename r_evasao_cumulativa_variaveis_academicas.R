# Verificar nome da coluna antes
names(base_analitica)
# ===============================
# 5.10.2 — EVASÃO CUMULATIVA POR VARIÁVEIS ACADÊMICAS
# ===============================

library(tidyverse)
library(scales)
library(tidyr)

# ==========================================================
# FUNÇÃO GERAL (MESMA UTILIZADA NA SEÇÃO 5.10.1)
# ==========================================================

calcular_evasao_cumulativa_grupo <- function(base, variavel, periodo_max = 4) {
  
  base_filtrada <- base %>%
    filter(!is.na(.data[[variavel]]),
           !is.na(grupo_curricular),
           !is.na(periodo_evasao_relativo))
  
  # Total de ingressantes por grupo dentro do currículo
  ingressantes <- base_filtrada %>%
    group_by(Curriculo = grupo_curricular,
             Grupo = .data[[variavel]]) %>%
    summarise(total_ingressantes = n_distinct(matricula),
              .groups = "drop")
  
  # Estrutura completa
  estrutura <- crossing(
    Curriculo = unique(base_filtrada$grupo_curricular),
    Grupo = unique(base_filtrada[[variavel]]),
    periodo = 1:periodo_max
  )
  
  # Contagem de evasões por período
  evasao <- base_filtrada %>%
    distinct(matricula, periodo_evasao_relativo, .keep_all = TRUE) %>%
    group_by(Curriculo = grupo_curricular,
             Grupo = .data[[variavel]],
             periodo = periodo_evasao_relativo) %>%
    summarise(evades = n(),
              .groups = "drop")
  
  estrutura %>%
    left_join(ingressantes,
              by = c("Curriculo", "Grupo")) %>%
    left_join(evasao,
              by = c("Curriculo", "Grupo", "periodo")) %>%
    mutate(evades = replace_na(evades, 0)) %>%
    group_by(Curriculo, Grupo) %>%
    arrange(periodo) %>%
    mutate(
      evadidos_acumulados = cumsum(evades),
      ativos = total_ingressantes - evadidos_acumulados,
      taxa_evasao_cumulativa =
        evadidos_acumulados / total_ingressantes
    ) %>%
    ungroup()
}

# ==========================================================
# 1) EVASÃO CUMULATIVA POR TIPO DE EVASÃO
# ==========================================================

evasao_tipo <- calcular_evasao_cumulativa_grupo(
  base = base_analitica,
  variavel = "tipo_de_evasao"
)

# -------- TABELA --------

tabela_evasao_tipo <- evasao_tipo %>%
  mutate(
    Taxa_Evasao_Cumulativa_percent =
      round(taxa_evasao_cumulativa * 100, 2)
  ) %>%
  select(
    Curriculo,
    Tipo_de_Evasao = Grupo,
    Periodo = periodo,
    Evadidos_Acumulados = evadidos_acumulados,
    Ativos = ativos,
    Total_Ingressantes = total_ingressantes,
    Taxa_Evasao_Cumulativa_percent
  )

print(tabela_evasao_tipo, n = Inf)

# -------- TABELA FORMATO WIDE --------

tabela_tipo_wide <- tabela_evasao_tipo %>%
  select(Curriculo,
         Tipo_de_Evasao,
         Periodo,
         Taxa = Taxa_Evasao_Cumulativa_percent,
         Total_Ingressantes) %>%
  pivot_wider(
    names_from = Periodo,
    values_from = Taxa,
    names_prefix = "Período "
  ) %>%
  arrange(Curriculo, Tipo_de_Evasao)

print(tabela_tipo_wide)

# -------- GRÁFICO --------

grafico_tipo <- evasao_tipo %>%
  ggplot(aes(x = periodo,
             y = taxa_evasao_cumulativa,
             color = Grupo,
             group = Grupo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  scale_x_continuous(breaks = 1:4) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Evasão Cumulativa por Tipo de Evasão",
    x = "Período",
    y = "Taxa Cumulativa de Evasão",
    color = "Tipo de Evasão"
  ) +
  facet_wrap(~ Curriculo) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold")
  )

print(grafico_tipo)

# ==========================================================
# 2) EVASÃO CUMULATIVA POR PERÍODO DE EVASÃO
# ==========================================================

evasao_periodo <- base_analitica %>%
  filter(!is.na(periodo_evasao_relativo),
         !is.na(grupo_curricular)) %>%
  distinct(matricula,
           periodo_evasao_relativo,
           grupo_curricular) %>%
  count(Curriculo = grupo_curricular,
        periodo = periodo_evasao_relativo) %>%
  group_by(Curriculo) %>%
  arrange(periodo) %>%
  mutate(
    evadidos_acumulados = cumsum(n),
    total_evadidos = sum(n),
    taxa_evasao_cumulativa =
      evadidos_acumulados / total_evadidos
  ) %>%
  ungroup()

# -------- TABELA --------

tabela_evasao_periodo <- evasao_periodo %>%
  mutate(
    Taxa_Evasao_Cumulativa_percent =
      round(taxa_evasao_cumulativa * 100, 2)
  ) %>%
  select(
    Curriculo,
    Periodo = periodo,
    Evadidos_Acumulados = evadidos_acumulados,
    Total_Evadidos = total_evadidos,
    Taxa_Evasao_Cumulativa_percent
  )

print(tabela_evasao_periodo, n = Inf)

# -------- GRÁFICO --------

grafico_periodo <- evasao_periodo %>%
  ggplot(aes(x = periodo,
             y = taxa_evasao_cumulativa,
             color = Curriculo,
             group = Curriculo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  scale_x_continuous(breaks = 1:4) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Evasão Cumulativa por Período de Ocorrência",
    x = "Período de Evasão",
    y = "Proporção Acumulada das Evasões",
    color = "Currículo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(grafico_periodo)

# ==========================================================
# (Opcional) SALVAR GRÁFICOS
# ==========================================================

ggsave("grafico_evasao_tipo.png",
       grafico_tipo,
       width = 9, height = 5,
       dpi = 300)

ggsave("grafico_evasao_periodo.png",
       grafico_periodo,
       width = 8, height = 5,
       dpi = 300)

