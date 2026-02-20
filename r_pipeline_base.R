# ======================================================
# BLOCO 1 — CONFIGURAÇÃO E LEITURA DA BASE
# ======================================================

library(tidyverse)
library(janitor)
library(readr)
library(stringr)

pasta_dados   <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"
base_objetiva <- "alunos-final"

caminho_base <- file.path(pasta_dados, paste0(base_objetiva, ".csv"))

alunos_raw <- read_delim(
  caminho_base,
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  trim_ws = TRUE,
  show_col_types = FALSE
)

glimpse(alunos_raw)

# ======================================================
# BLOCO 2 — PADRONIZAÇÃO ESTRUTURAL
# ======================================================

alunos_padrao <- alunos_raw %>%
  clean_names() %>%
  rename_with(~ str_replace_all(.x, "__", "_")) %>%
  mutate(across(where(is.character), str_trim))

# ======================================================
# BLOCO 3 — INSPEÇÃO FORMAL DAS VARIÁVEIS
# ======================================================

resumo_variaveis <- tibble(
  variavel = names(alunos_padrao),
  tipo     = map_chr(alunos_padrao, ~ class(.x)[1]),
  n_na     = map_int(alunos_padrao, ~ sum(is.na(.x))),
  perc_na  = map_dbl(alunos_padrao, ~ mean(is.na(.x))*100)
)

resumo_variaveis %>% arrange(desc(perc_na))

# ======================================================
# BLOCO 4 — EXPLORAÇÃO DOS DOMÍNIOS
# ======================================================

vars_dom <- c("periodo_de_ingresso","curriculo","status","tipo_de_evasao","sexo","cor")
map(vars_dom, ~ count(alunos_padrao, .data[[.x]], sort = TRUE))

# ======================================================
# BLOCO 5 — SITUAÇÃO FINAL DO ESTUDANTE
# ======================================================

alunos_recorte <- alunos_padrao %>%
  mutate(
    situacao_final = case_when(
      status == "ATIVO"                          ~ "ATIVO",
      tipo_de_evasao == "GRADUADO"               ~ "CONCLUIDO",
      tipo_de_evasao %in% c(
        "CANCELAMENTO POR ABANDONO",
        "CANCELAMENTO P SOLICITACAO ALUNO"
      )                                          ~ "EVADIDO",
      TRUE                                       ~ "OUTROS"
    )
  )

# ======================================================
# BLOCO 6 — TRATAMENTO DO PERÍODO DE INGRESSO
# ======================================================

alunos_recorte <- alunos_recorte %>%
  mutate(
    periodo_ingresso   = as.character(periodo_de_ingresso),
    ano_ingresso       = as.integer(str_sub(periodo_ingresso, 1, 4)),
    semestre_ingresso  = as.integer(str_sub(periodo_ingresso, 6, 6))
  )

# ======================================================
# BLOCO 7 — GRUPO CURRICULAR
# ======================================================

alunos_recorte <- alunos_recorte %>%
  mutate(
    grupo_curricular = case_when(
      curriculo == "1999" ~ "PRE_REFORMA",
      curriculo == "2017" ~ "POS_REFORMA",
      TRUE                ~ NA_character_
    )
  )

# BLOCO 8 — DEFINIÇÃO DAS JANELAS TEMPORAIS
# ======================================================

## Função para criar janelas
criar_janela <- function(df, nome, grupo, ini, fim) {
  df %>% mutate(
    !!nome := grupo_curricular == grupo &
      periodo_ingresso >= ini &
      periodo_ingresso <= fim
  )
}

# Janelas 1999
alunos_recorte <- alunos_recorte %>%
  criar_janela("janela_1999_p1", "PRE_REFORMA", "2011.1", "2017.2") %>%
  criar_janela("janela_1999_p2", "PRE_REFORMA", "2011.1", "2016.1") %>%
  criar_janela("janela_1999_p3", "PRE_REFORMA", "2011.1", "2015.2") %>%
  criar_janela("janela_1999_p4", "PRE_REFORMA", "2011.1", "2014.2")

# Janelas 2017
alunos_recorte <- alunos_recorte %>%
  criar_janela("janela_2017_p1", "POS_REFORMA", "2018.1", "2023.1") %>%
  criar_janela("janela_2017_p2", "POS_REFORMA", "2018.1", "2022.2") %>%
  criar_janela("janela_2017_p3", "POS_REFORMA", "2018.1", "2022.1") %>%
  criar_janela("janela_2017_p4", "POS_REFORMA", "2018.1", "2021.2")

# ======================================================
# BLOCO 9 — CHECKPOINT FINAL
# ======================================================

count(alunos_recorte, grupo_curricular, situacao_final)

# Resolver os NA como "FORA_RECORTE"
alunos_recorte <- alunos_recorte %>%
  mutate(grupo_curricular = replace_na(grupo_curricular, "FORA_RECORTE"))

# Bases finais
base_analitica <- alunos_recorte %>%
  filter(grupo_curricular %in% c("PRE_REFORMA","POS_REFORMA"))

base_completa  <- alunos_recorte

nrow(base_analitica)
nrow(base_completa)
