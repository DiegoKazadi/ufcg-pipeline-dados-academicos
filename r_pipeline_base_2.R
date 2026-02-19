# 0 Instalação

install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")


# 1. PACOTES
library(tidyverse)
library(janitor)
library(lubridate)
library(readr)
library(dplyr)

# 2. PARÂMETROS GERAIS DO ESTUDO

# Currículo 1999 (pré-reforma)
curriculo_1999 <- list(
  nome = "1999",
  inicio = c(2011, 1),
  fim     = c(2017, 2)
)

# Currículo 2017 (pós-reforma)
curriculo_2017 <- list(
  nome = "2017",
  inicio = c(2018, 1),
  fim     = c(2023, 1)
)

# 3. FUNÇÕES AUXILIARES

# Criar identificador ano.periodo (ex: 2018.1)
criar_periodo <- function(ano, periodo) {
  as.numeric(paste0(ano, ".", periodo))
}

# Classificar currículo com base no ingresso
classificar_curriculo <- function(ano, periodo) {
  periodo_num <- criar_periodo(ano, periodo)
  
  case_when(
    periodo_num >= 2011.1 & periodo_num <= 2017.2 ~ "1999",
    periodo_num >= 2018.1 & periodo_num <= 2023.1 ~ "2017",
    TRUE ~ NA_character_
  )
}

# 4. CARREGAMENTO DA BASE

alunos_raw <- read_csv(
  "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas/alunos-final.csv",
  show_col_types = FALSE
)

# 4. CARREGAMENTO DA BASE linux
caminho_base <- "/home/diego/Documentos/Tabelas"

alunos_raw <- read_csv2(
  file.path(caminho_base, "alunos-final.csv"),
  show_col_types = FALSE
)

# INSPEÇÃO INICIAL DA BASE 

dim(alunos_raw)
glimpse(alunos_raw)
names(alunos_raw)


# 5. PADRONIZAÇÃO DA BASE 

alunos_base <- alunos_raw %>%
  clean_names() %>%
  separate(periodo_de_ingresso,
           into = c("ano_ingresso", "periodo_ingresso"),
           sep = "\\.",
           convert = TRUE) %>%
  mutate(
    periodo_ingresso_num = criar_periodo(ano_ingresso, periodo_ingresso),
    curriculo = classificar_curriculo(ano_ingresso, periodo_ingresso)
  ) %>%
  filter(!is.na(curriculo))


# 6. DEFINIÇÃO DOS CORTES POR PERÍODO (1º ao 4º)

definir_janela_periodo <- function(curriculo, periodo_n) {
  
  if (curriculo == "1999") {
    limites <- tibble(
      periodo = 1:4,
      fim = c(2017.2, 2016.1, 2015.2, 2014.2)
    )
    inicio <- 2011.1
  } else {
    limites <- tibble(
      periodo = 1:4,
      fim = c(2023.1, 2022.2, 2022.1, 2021.2)
    )
    inicio <- 2018.1
  }
  
  limites %>%
    filter(periodo == periodo_n) %>%
    transmute(
      inicio = inicio,
      fim = fim
    )
}


# 7. BASES PRONTAS PARA ANÁLISE (LISTA)

bases_analise <- list()

for (curr in c("1999", "2017")) {
  for (p in 1:4) {
    
    janela <- definir_janela_periodo(curr, p)
    
    bases_analise[[paste0("curr_", curr, "_p", p)]] <-
      alunos_base %>%
      filter(
        curriculo == curr,
        periodo_ingresso_num >= janela$inicio,
        periodo_ingresso_num <= janela$fim
      ) %>%
      mutate(
        periodo_analise = p,
        curriculo = curr
      )
  }
}



# Dimensão
dim(alunos_base)

# Estrutura das variáveis
glimpse(alunos_base)

# Tipos das variáveis
sapply(alunos_base, class)

# Frequência por currículo
table(alunos_base$curriculo)

# Distribuição percentual
prop.table(table(alunos_base$curriculo)) * 100

summary(alunos_base$periodo_ingresso_num)

# Mínimo e máximo por currículo
alunos_base %>%
  group_by(curriculo) %>%
  summarise(
    min_periodo = min(periodo_ingresso_num),
    max_periodo = max(periodo_ingresso_num),
    n = n()
  )
table(alunos_base$status)

alunos_base %>%
  count(curriculo, status) %>%
  group_by(curriculo) %>%
  mutate(percentual = round(n/sum(n)*100, 2))

names(bases_analise)

# Exemplo: visualizar uma base específica
glimpse(bases_analise$curr_1999_p1)

# Tamanho de cada base
sapply(bases_analise, nrow)

