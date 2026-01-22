# =========================================================
# PACOTES
# =========================================================

library(tidyverse)
library(stringr)
library(scales)

# BLOCO 5 — DEFINIÇÃO DA SITUAÇÃO FINAL DO ESTUDANTE
# Critério:
# - EVADIDO: status == "INATIVO" e tipo_de_evasao != "GRADUADO"
# - CONCLUIDO: tipo_de_evasao == "GRADUADO"
# - ATIVO: status == "ATIVO"
# - OUTROS: casos residuais

alunos_recorte <- alunos_padrao %>%
  mutate(
    situacao_final = case_when(
      status == "ATIVO" ~ "ATIVO",
      status == "INATIVO" & tipo_de_evasao == "GRADUADO" ~ "CONCLUIDO",
      status == "INATIVO" & tipo_de_evasao != "GRADUADO" ~ "EVADIDO",
      TRUE ~ "OUTROS"
    )
  )

# Checkpoint
count(alunos_recorte, situacao_final)

# Verificação cruzada: status x tipo_de_evasao
alunos_recorte %>%
  count(status, tipo_de_evasao, situacao_final) %>%
  arrange(status, situacao_final)

# BLOCO 6 — PADRONIZAÇÃO DO GRUPO CURRICULAR

base_analitica <- base_analitica %>%
  mutate(
    grupo_curricular = case_when(
      grupo_curricular == "PRE_REFORMA" ~ "Currículo 1999",
      grupo_curricular == "POS_REFORMA" ~ "Currículo 2017",
      TRUE ~ grupo_curricular
    )
  )

# Checkpoint
count(base_analitica, grupo_curricular)

# BLOCO 7 — PERÍODO RELATIVO DE EVASÃO

base_analitica <- base_analitica %>%
  mutate(
    periodo_evasao_relativo = case_when(
      situacao_final != "EVADIDO" ~ NA_integer_,
      TRUE ~
        (as.integer(str_sub(periodo_de_evasao, 1, 4)) - ano_ingresso) * 2 +
        (as.integer(str_sub(periodo_de_evasao, 6, 6)) - semestre_ingresso) + 1
    )
  )

# Checkpoint
count(base_analitica, periodo_evasao_relativo)

# BLOCO 11 — FUNÇÃO PARA CÁLCULO DA EVASÃO CUMULATIVA
# (com inclusão da coluna ATIVOS)

calcular_evasao_cumulativa <- function(base, grupo, periodo_max = 4) {
  
  base_grupo <- base %>%
    filter(grupo_curricular == grupo)
  
  total_ingressantes <- nrow(base_grupo)
  
  tibble(
    periodo = 1:periodo_max,
    evadidos_acumulados = map_int(
      1:periodo_max,
      ~ sum(base_grupo$periodo_evasao_relativo <= .x, na.rm = TRUE)
    )
  ) %>%
    mutate(
      grupo_curricular = grupo,
      total_ingressantes = total_ingressantes,
      ativos = total_ingressantes - evadidos_acumulados,
      taxa_evasao_cumulativa = evadidos_acumulados / total_ingressantes
    )
}

# BLOCO 12 — BASE FINAL DE EVASÃO CUMULATIVA

evasao_cumulativa <- bind_rows(
  calcular_evasao_cumulativa(base_analitica, "Currículo 1999", 4),
  calcular_evasao_cumulativa(base_analitica, "Currículo 2017", 4)
)

evasao_cumulativa

# BLOCO 13 — TABELA FINAL (FORMATO DISSERTAÇÃO)

tabela_evasao_cumulativa <- evasao_cumulativa %>%
  mutate(
    taxa_evasao_cumulativa = round(taxa_evasao_cumulativa * 100, 2)
  ) %>%
  select(
    Currículo = grupo_curricular,
    Período = periodo,
    Evadidos_Acumulados = evadidos_acumulados,
    Ativos = ativos,
    Total_Ingressantes = total_ingressantes,
    Taxa_Evasão_Cumulativa_Percentual = taxa_evasao_cumulativa
  )

tabela_evasao_cumulativa

# BLOCO 14 — GRÁFICO: EVOLUÇÃO CUMULATIVA DA EVASÃO

ggplot(
  evasao_cumulativa,
  aes(
    x = periodo,
    y = taxa_evasao_cumulativa,
    group = grupo_curricular,
    color = grupo_curricular
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:4) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Evolução Cumulativa da Evasão nos Períodos Iniciais",
    subtitle = "Comparação entre o Currículo de 1999 e o Currículo de 2017",
    x = "Período do Curso",
    y = "Taxa de Evasão Cumulativa",
    color = "Currículo"
  ) +
  theme_minimal()

