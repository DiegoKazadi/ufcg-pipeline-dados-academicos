# ===============================
# 5.10.1 — EVASÃO CUMULATIVA POR VARIÁVEIS DEMOGRÁFICAS
# ===============================

library(tidyverse)
library(scales)
library(tidyr)

# ===============================
# AJUSTA LABEL DE CURRÍCULO
# ===============================

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
