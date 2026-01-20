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
