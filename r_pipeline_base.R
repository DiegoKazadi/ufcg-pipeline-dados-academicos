# BLOCO 1 — CONFIGURAÇÃO E LEITURA DA BASE
# Garantir que a base seja lida corretamente desde o início.

# Pacotes essenciais
library(tidyverse)
library(janitor)
library(readr)
library(stringr)

# Diretório e base objetiva
pasta_dados   <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"
base_objetiva <- "alunos-final"

# Caminho completo do arquivo
caminho_base <- file.path(pasta_dados, paste0(base_objetiva, ".csv"))

# Leitura correta do CSV (separador ;)
alunos_raw <- read_delim(
  caminho_base,
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  trim_ws = TRUE,
  show_col_types = FALSE
)

# Checkpoint estrutural
dim(alunos_raw)
glimpse(alunos_raw)

# PADRONIZAÇÃO ESTRUTURAL DAS VARIÁVEIS
# Tornar os nomes utilizáveis e consistentes, sem alterar conteúdo.

# BLOCO 2 — PADRONIZAÇÃO DOS NOMES E TEXTOS

alunos_padrao <- alunos_raw %>%
  clean_names() %>%
  rename_with(~ str_replace_all(.x, "__", "_")) %>%
  mutate(across(where(is.character), str_trim))

# Verificação
names(alunos_padrao)
glimpse(alunos_padrao)

# BLOCO 3 — INSPEÇÃO FORMAL DAS VARIÁVEIS
# Conhecer a base antes de qualquer decisão analítica.


# Lista de variáveis
nomes_variaveis <- names(alunos_padrao)
nomes_variaveis
length(nomes_variaveis)

# Resumo estrutural (tipo + NA)
resumo_variaveis <- tibble(
  variavel = names(alunos_padrao),
  tipo     = map_chr(alunos_padrao, ~ class(.x)[1]),
  n_na     = map_int(alunos_padrao, ~ sum(is.na(.x))),
  perc_na  = map_dbl(alunos_padrao, ~ mean(is.na(.x)) * 100)
)

resumo_variaveis %>%
  arrange(desc(perc_na))

# BLOCO 4 — EXPLORAÇÃO DOS DOMÍNIOS (SANITY CHECK)
# Entender os valores reais antes de limpar ou recodificar.

# Período de ingresso
count(alunos_padrao, periodo_de_ingresso, sort = TRUE)

# Currículo
count(alunos_padrao, curriculo)

# Status acadêmico
count(alunos_padrao, status)

# Tipo de evasão
count(alunos_padrao, tipo_de_evasao)

# Sexo
count(alunos_padrao, sexo)

# Cor / Raça
count(alunos_padrao, cor)

# RECORTE ANALÍTICO DA BASE PARA EVASÃO
# BLOCO 5 — DEFINIÇÃO DA SITUAÇÃO FINAL DO ESTUDANTE

alunos_recorte <- alunos_padrao %>%
  mutate(
    situacao_final = case_when(
      status == "ATIVO" ~ "ATIVO",
      tipo_de_evasao == "GRADUADO" ~ "CONCLUIDO",
      tipo_de_evasao %in% c(
        "CANCELAMENTO POR ABANDONO",
        "CANCELAMENTO P SOLICITACAO ALUNO"
      ) ~ "EVADIDO",
      TRUE ~ "OUTROS"
    )
  )

# Verificação
count(alunos_recorte, situacao_final)


