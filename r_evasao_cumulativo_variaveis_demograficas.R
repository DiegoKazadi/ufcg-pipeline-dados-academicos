# ===============================
# 5.10.1 — EVASÃO CUMULATIVA POR VARIÁVEIS DEMOGRÁFICAS
# ===============================

library(tidyverse)
library(scales)
library(tidyr)

# CURRÍCULO

base_analitica <- base_analitica %>% 
  mutate(
    grupo_curricular = case_when(
      grupo_curricular == "PRE_REFORMA" ~ "Currículo 1999",
      grupo_curricular == "POS_REFORMA" ~ "Currículo 2017",
      TRUE ~ grupo_curricular
    )
  )

# ===============================
# AJUSTA FAIXA ETÁRIA
# ===============================

if (!"faixa_etaria_ingresso" %in% names(base_analitica)) {
  base_analitica <- base_analitica %>%
    mutate(
      faixa_etaria_ingresso = case_when(
        idade_aproximada_no_ingresso < 20 ~ "< 20",
        idade_aproximada_no_ingresso <= 24 ~ "20–24",
        idade_aproximada_no_ingresso <= 29 ~ "25–29",
        idade_aproximada_no_ingresso >= 30 ~ "30+",
        TRUE ~ NA_character_
      )
    )
}

# ===============================
# FUNÇÃO GERAL — EVASÃO CUMULATIVA POR GRUPO DENTRO DO CURRÍCULO
# ===============================

calcular_evasao_cumulativa_grupo <- function(base, variavel, periodo_max = 4) {
  
  base_filtrada <- base %>%
    filter(!is.na(.data[[variavel]]), !is.na(grupo_curricular))
  
  # Total de ingressantes por grupo
  ingressantes <- base_filtrada %>%
    group_by(Curriculo = grupo_curricular, Grupo = .data[[variavel]]) %>%
    summarise(total_ingressantes = n(), .groups = "drop")
  
  # Estrutura completa (crossing garante todas combinações)
  estrutura <- crossing(
    Curriculo = unique(base_filtrada$grupo_curricular),
    Grupo = unique(base_filtrada[[variavel]]),
    periodo = 1:periodo_max
  )
  
  # Contagem de evasões por período
  evasao <- base_filtrada %>%
    distinct(matricula, periodo_evasao_relativo, .keep_all = TRUE) %>%
    group_by(Curriculo = grupo_curricular, Grupo = .data[[variavel]], periodo_evasao_relativo) %>%
    summarise(evades = n(), .groups = "drop") %>%
    rename(periodo = periodo_evasao_relativo)
  
  # Combina estrutura completa com ingressantes e evasões
  estrutura %>%
    left_join(ingressantes, by = c("Curriculo", "Grupo")) %>%
    left_join(evasao, by = c("Curriculo", "Grupo", "periodo")) %>%
    tidyr::complete(
      Curriculo, Grupo, periodo = 1:periodo_max,
      fill = list(evades = 0, total_ingressantes = 0)
    ) %>%
    group_by(Curriculo, Grupo, periodo, total_ingressantes) %>%
    summarise(
      evadidos_acumulados = sum(evades, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ativos = total_ingressantes - evadidos_acumulados,
      taxa_evasao_cumulativa = evadidos_acumulados / total_ingressantes
    )
}

# ===============================
# 1) EVASÃO CUMULATIVA POR SEXO
# ===============================

evasao_sexo <- calcular_evasao_cumulativa_grupo(
  base = base_analitica,
  variavel = "sexo"
)

# ------ TABELA ------
tabela_evasao_sexo <- evasao_sexo %>%
  mutate(
    Taxa_Evasao_Cumulativa_percent = round(taxa_evasao_cumulativa * 100, 2)
  ) %>%
  select(
    Curriculo,
    Sexo = Grupo,
    Periodo = periodo,
    Evadidos_Acumulados = evadidos_acumulados,
    Ativos = ativos,
    Total_Ingressantes = total_ingressantes,
    Taxa_Evasao_Cumulativa_percent
  )

print(tabela_evasao_sexo)

# ------ GRÁFICO ------
grafico_sexo <- ggplot(
  evasao_sexo,
  aes(
    x = periodo,
    y = taxa_evasao_cumulativa,
    color = Grupo,
    group = Grupo
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = 1:4) +
  labs(
    title = "Evasão Cumulativa por Sexo (dentro de cada Currículo)",
    x = "Período",
    y = "Taxa Cumulativa de Evasão",
    color = "Sexo"
  ) +
  facet_wrap(~ Curriculo) +
  theme_minimal()

print(grafico_sexo)

# ===============================
# 2) EVASÃO CUMULATIVA POR FAIXA ETÁRIA
# ===============================

evasao_faixa <- calcular_evasao_cumulativa_grupo(
  base = base_analitica,
  variavel = "faixa_etaria_ingresso"
)

# ------ TABELA ------
tabela_evasao_faixa <- evasao_faixa %>%
  mutate(
    Taxa_Evasao_Cumulativa_percent = round(taxa_evasao_cumulativa * 100, 2)
  ) %>%
  select(
    Curriculo,
    Faixa_Etaria = Grupo,
    Periodo = periodo,
    Evadidos_Acumulados = evadidos_acumulados,
    Ativos = ativos,
    Total_Ingressantes = total_ingressantes,
    Taxa_Evasao_Cumulativa_percent
  )

print(tabela_evasao_faixa)

# ------ GRÁFICO ------
grafico_faixa <- ggplot(
  evasao_faixa,
  aes(
    x = periodo,
    y = taxa_evasao_cumulativa,
    color = Grupo,
    group = Grupo
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = 1:4) +
  labs(
    title = "Evasão Cumulativa por Faixa Etária (dentro de cada Currículo)",
    x = "Período",
    y = "Taxa Cumulativa de Evasão",
    color = "Faixa Etária"
  ) +
  facet_wrap(~ Curriculo) +
  theme_minimal()

print(grafico_faixa)


base_analitica %>%
  filter(grupo_curricular == "Currículo 2017") %>%
  count(faixa_etaria_ingresso)

# Mostrar todas as linhas no console
print(tabela_evasao_faixa, n = Inf)

# Ou abrir a tabela em uma visualização interativa
View(tabela_evasao_faixa)

### Formatar as tabelas
library(tidyr)
library(dplyr)

# Supondo que sua tabela de faixa etária se chame 'tabela_evasao_faixa'
tabela_faixa_wide <- tabela_evasao_faixa %>%
  select(Curriculo, Faixa_Etaria, Periodo, 
         Taxa = Taxa_Evasao_Cumulativa_percent, 
         Total_Ingressantes) %>%
  pivot_wider(
    names_from = Periodo,
    values_from = Taxa,
    names_prefix = "Período "
  ) %>%
  # A coluna Total_Ingressantes é constante para cada grupo, então será mantida
  arrange(Curriculo, Faixa_Etaria)

# Visualizar a tabela
print(tabela_faixa_wide)

# Gerar graficos

# Gráfico de evasão cumulativa por faixa etária
library(ggplot2)
library(scales)

# No objeto evasao_faixa, a coluna 'taxa_evasao_cumulativa' já está em proporção.
# Vamos usá-la.

grafico_faixa <- ggplot(evasao_faixa, 
                        aes(x = periodo, y = taxa_evasao_cumulativa, 
                            color = Grupo, group = Grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 1:4) +
  labs(
    title = "Evasão Cumulativa por Faixa Etária",
    subtitle = "Comparação entre Currículos 1999 e 2017",
    x = "Período",
    y = "Taxa Cumulativa de Evasão",
    color = "Faixa Etária"
  ) +
  facet_wrap(~ Curriculo) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold")
  )

print(grafico_faixa)

# Salvar em alta resolução
ggsave("evasao_faixa_etaria.png", plot = grafico_faixa, 
       width = 10, height = 6, dpi = 300, bg = "white")


library(tidyr)
library(dplyr)

# Transformar a tabela longa em wide com os percentuais por período
tabela_sexo_wide <- tabela_evasao_sexo %>%
  select(Curriculo, Sexo, Periodo, 
         Taxa = Taxa_Evasao_Cumulativa_percent, 
         Total_Ingressantes) %>%
  pivot_wider(
    names_from = Periodo,
    values_from = Taxa,
    names_prefix = "Período "
  ) %>%
  arrange(Curriculo, Sexo)

# Visualizar
print(tabela_sexo_wide)

library(ggplot2)
library(scales)
library(dplyr)  # necessário para o pipe e manipulação

# Garantir que a coluna Curriculo seja fator, se desejar ordenar (opcional)
# tabela_evasao_sexo <- tabela_evasao_sexo %>%
#   mutate(Curriculo = factor(Curriculo, levels = c("Currículo 1999", "Currículo 2017")))

grafico_sexo <- tabela_evasao_sexo %>%
  ggplot(aes(x = Periodo, y = Taxa_Evasao_Cumulativa_percent, 
             color = Sexo, group = Sexo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  scale_x_continuous(breaks = 1:4) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Evasão Cumulativa por Sexo",
    x = "Período",
    y = "Taxa de Evasão Cumulativa (%)",
    color = "Sexo"
  ) +
  facet_wrap(~ Curriculo) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    # Caixa cinza no título do facet
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold")
  )

# Exibir o gráfico
print(grafico_sexo)

# Salvar (opcional)
ggsave("grafico_evasao_sexo.png", grafico_sexo, width = 8, height = 5, dpi = 300)
