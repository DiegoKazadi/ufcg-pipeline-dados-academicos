# ======================================================
# CRIAÇÃO DA VARIÁVEL FAIXA ETÁRIA
# ======================================================

base_analitica <- base_analitica %>%
  mutate(
    faixa_etaria = case_when(
      idade_aproximada_no_ingresso <= 20 ~ "Até 20 anos",
      idade_aproximada_no_ingresso <= 24 ~ "21–24 anos",
      idade_aproximada_no_ingresso <= 29 ~ "25–29 anos",
      idade_aproximada_no_ingresso >= 30 ~ "30 anos ou mais",
      TRUE ~ NA_character_
    )
  )

# ======================================================
# TAXAS DE EVASÃO POR FAIXA ETÁRIA
# ======================================================

library(dplyr)
library(purrr)

janelas <- list(
  list(coluna = "janela_1999_p1", periodo = "P1"),
  list(coluna = "janela_1999_p2", periodo = "P2"),
  list(coluna = "janela_1999_p3", periodo = "P3"),
  list(coluna = "janela_1999_p4", periodo = "P4"),
  list(coluna = "janela_2017_p1", periodo = "P1"),
  list(coluna = "janela_2017_p2", periodo = "P2"),
  list(coluna = "janela_2017_p3", periodo = "P3"),
  list(coluna = "janela_2017_p4", periodo = "P4")
)

calcular_taxa_idade <- function(janela_info) {
  
  coluna  <- janela_info$coluna
  periodo <- janela_info$periodo
  
  base_analitica %>%
    filter(.data[[coluna]] == TRUE) %>%
    group_by(grupo_curricular, faixa_etaria) %>%
    summarise(
      total_alunos   = n(),
      total_evadidos = sum(situacao_final == "EVADIDO"),
      taxa_evasao    = total_evadidos / total_alunos,
      .groups        = "drop"
    ) %>%
    mutate(periodo = periodo)
}

taxas_idade <- map_dfr(janelas, calcular_taxa_idade) %>%
  mutate(periodo = factor(periodo, levels = c("P1","P2","P3","P4")))

library(ggplot2)
library(scales)

ggplot(taxas_idade,
       aes(x = periodo,
           y = taxa_evasao,
           color = faixa_etaria,
           group = faixa_etaria)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  facet_wrap(~grupo_curricular, nrow = 1) +
  
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  
  labs(
    title = "Evolução das taxas de evasão por faixa etária",
    x = "Período do Curso",
    y = "Taxa de Evasão (%)",
    color = "Faixa Etária"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.background = element_rect(fill = "grey85", color = "grey40"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggplot(base_analitica %>%
         filter(grupo_curricular %in% c("PRE_REFORMA","POS_REFORMA")),
       aes(x = faixa_etaria,
           y = as.numeric(situacao_final == "EVADIDO"),
           fill = grupo_curricular)) +
  
  geom_boxplot(alpha = 0.6) +
  
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  
  labs(
    title = "Distribuição da evasão por faixa etária",
    x = "Faixa Etária",
    y = "Indicador de Evasão (0/1)",
    fill = "Grupo Curricular"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
