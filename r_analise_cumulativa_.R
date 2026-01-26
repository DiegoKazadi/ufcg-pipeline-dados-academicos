library(tidyverse)
library(scales)
library(DT)
#

base_analitica %>%
  count(grupo_curricular, situacao_final)

# 
calcular_evasao_cumulativa <- function(base, filtro_janela, periodo_label, janela_label) {
  base %>%
    filter({{ filtro_janela }}) %>%
    group_by(grupo_curricular) %>%
    summarise(
      periodo = periodo_label,
      janela  = janela_label,
      ingressantes = n(),
      evadidos_acumulados = sum(situacao_final == "EVADIDO"),
      taxa_evasao = evadidos_acumulados / ingressantes,
      .groups = "drop"
    )
}

# EVASÃO CUMULATIVA (1º AO 4º PERÍODO)

evasao_cumulativa <- bind_rows(
  
  # 1º período
  calcular_evasao_cumulativa(
    base_analitica,
    janela_1999_p1 | janela_2017_p1,
    "1º Período",
    "P1"
  ),
  
  # 2º período
  calcular_evasao_cumulativa(
    base_analitica,
    janela_1999_p2 | janela_2017_p2,
    "2º Período",
    "P2"
  ),
  
  # 3º período
  calcular_evasao_cumulativa(
    base_analitica,
    janela_1999_p3 | janela_2017_p3,
    "3º Período",
    "P3"
  ),
  
  # 4º período
  calcular_evasao_cumulativa(
    base_analitica,
    janela_1999_p4 | janela_2017_p4,
    "4º Período",
    "P4"
  )
)

# TABELA FINAL DA PARTE II

tabela_evasao_cumulativa <- evasao_cumulativa %>%
  mutate(
    taxa_percentual = percent(taxa_evasao, accuracy = 0.1),
    grupo_curricular = recode(
      grupo_curricular,
      "PRE_REFORMA" = "Currículo 1999",
      "POS_REFORMA" = "Currículo 2017"
    )
  ) %>%
  arrange(grupo_curricular, periodo)

tabela_evasao_cumulativa



