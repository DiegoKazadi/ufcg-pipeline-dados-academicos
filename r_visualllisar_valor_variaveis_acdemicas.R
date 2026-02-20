# ===============================
# DIAGNÓSTICO DAS VARIÁVEIS ACADÊMICAS
# ===============================

library(dplyr)

# ---------------------------------
# 1) VISUALIZAR TIPOS DE EVASÃO
# ---------------------------------

cat("\n============================\n")
cat("TIPO DE EVASÃO — Valores únicos\n")
cat("============================\n")

unique(base_analitica$tipo_de_evasao)

cat("\nFrequência absoluta:\n")

base_analitica %>%
  count(tipo_de_evasao, sort = TRUE) %>%
  print(n = Inf)

cat("\nDistribuição por currículo:\n")

base_analitica %>%
  count(grupo_curricular, tipo_de_evasao) %>%
  arrange(grupo_curricular, desc(n)) %>%
  print(n = Inf)



# ---------------------------------
# 2) PERÍODO DE EVASÃO (original)
# ---------------------------------

cat("\n============================\n")
cat("PERÍODO DE EVASÃO (original)\n")
cat("============================\n")

summary(base_analitica$periodo_de_evasao)

base_analitica %>%
  count(periodo_de_evasao, sort = TRUE) %>%
  print(n = Inf)



# ---------------------------------
# 3) PERÍODO DE EVASÃO RELATIVO
# ---------------------------------

cat("\n============================\n")
cat("PERÍODO DE EVASÃO RELATIVO\n")
cat("============================\n")

summary(base_analitica$periodo_evasao_relativo)

base_analitica %>%
  count(periodo_evasao_relativo, sort = FALSE) %>%
  arrange(periodo_evasao_relativo) %>%
  print(n = Inf)



# ---------------------------------
# 4) PERÍODO RELATIVO POR CURRÍCULO
# ---------------------------------

cat("\n============================\n")
cat("PERÍODO RELATIVO POR CURRÍCULO\n")
cat("============================\n")

base_analitica %>%
  filter(!is.na(periodo_evasao_relativo)) %>%
  count(grupo_curricular, periodo_evasao_relativo) %>%
  arrange(grupo_curricular, periodo_evasao_relativo) %>%
  print(n = Inf)



# ---------------------------------
# 5) CHECAR INCONSISTÊNCIAS
# ---------------------------------

cat("\n============================\n")
cat("Casos com tipo de evasão mas sem período relativo\n")
cat("============================\n")

base_analitica %>%
  filter(!is.na(tipo_de_evasao) &
           is.na(periodo_evasao_relativo)) %>%
  select(matricula, tipo_de_evasao, periodo_de_evasao) %>%
  print(n = 20)

cat("\n============================\n")
cat("Casos com período relativo mas sem tipo\n")
cat("============================\n")

base_analitica %>%
  filter(is.na(tipo_de_evasao) &
           !is.na(periodo_evasao_relativo)) %>%
  select(matricula, periodo_evasao_relativo) %>%
  print(n = 20)

