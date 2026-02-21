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

# Verificação cruzada: status x tipo de evasão
alunos_recorte %>%
  count(status, tipo_de_evasao, situacao_final) %>%
  arrange(status, situacao_final)

base_analitica <- base_analitica %>%
  mutate(
    periodo_evasao_relativo = case_when(
      situacao_final != "EVADIDO" ~ NA_integer_,
      TRUE ~
        (as.integer(str_sub(periodo_de_evasao, 1, 4)) - ano_ingresso) * 2 +
        (as.integer(str_sub(periodo_de_evasao, 6, 6)) - semestre_ingresso) + 1
    )
  )


# PERÍODO RELATIVO DE EVASÃO

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

# FUNÇÃO PARA CÁLCULO DA EVASÃO CUMULATIVA

# FUNÇÃO DE EVASÃO CUMULATIVA

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
      taxa_evasao_cumulativa = evadidos_acumulados / total_ingressantes
    )
}

#  BASE CUMULATIVA FINAL

evasao_cumulativa <- bind_rows(
  calcular_evasao_cumulativa(base_analitica, "PRE_REFORMA", 4),
  calcular_evasao_cumulativa(base_analitica, "POS_REFORMA", 4)
)

evasao_cumulativa

#  TABELA DE EVASÃO CUMULATIVA

tabela_evasao_cumulativa <- evasao_cumulativa %>%
  mutate(
    taxa_evasao_cumulativa = round(taxa_evasao_cumulativa * 100, 2)
  ) %>%
  select(
    grupo_curricular,
    periodo,
    evadidos_acumulados,
    total_ingressantes,
    taxa_evasao_cumulativa
  )

tabela_evasao_cumulativa

# GRÁFICO: EVOLUÇÃO CUMULATIVA DA EVASÃO

# GRÁFICO DE EVASÃO CUMULATIVA

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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Evolução Cumulativa da Evasão nos Períodos Iniciais",
    x = "Período do Curso",
    y = "Taxa de Evasão Cumulativa",
    color = "Grupo Curricular"
  ) +
  theme_minimal()

