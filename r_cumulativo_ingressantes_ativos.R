# ======================================================
# BLOCO 10 — FUNÇÕES PARA TAXA CUMULATIVA
# ======================================================

# 1. Função para computar números agregados (ingressantes, ativos, evasão acumulada)
calcular_cumulativo <- function(df, janela_logica, nome_periodo) {
  
  dados <- df %>% filter(!!sym(janela_logica))
  
  ingressantes <- nrow(dados)
  
  ativos <- dados %>% filter(situacao_final == "ATIVO") %>% nrow()
  
  evadidos <- dados %>% filter(situacao_final == "EVADIDO") %>% nrow()
  
  taxa_evasao <- ifelse(ingressantes == 0, 0, evadidos / ingressantes)
  
  tibble(
    periodo_curso = nome_periodo,
    ingressantes = ingressantes,
    ativos = ativos,
    evadidos_acumulados = evadidos,
    taxa_evasao_cumulativa = round(taxa_evasao * 100, 2)
  )
}


# ======================================================
# BLOCO 11 — EXECUÇÃO PARA AMBOS CURRÍCULOS
# ======================================================

## Currículo 1999
result_1999 <- bind_rows(
  calcular_cumulativo(base_analitica, "janela_1999_p1", "1º Período"),
  calcular_cumulativo(base_analitica, "janela_1999_p2", "2º Período"),
  calcular_cumulativo(base_analitica, "janela_1999_p3", "3º Período"),
  calcular_cumulativo(base_analitica, "janela_1999_p4", "4º Período")
) %>% mutate(curriculo = "1999")


## Currículo 2017
result_2017 <- bind_rows(
  calcular_cumulativo(base_analitica, "janela_2017_p1", "1º Período"),
  calcular_cumulativo(base_analitica, "janela_2017_p2", "2º Período"),
  calcular_cumulativo(base_analitica, "janela_2017_p3", "3º Período"),
  calcular_cumulativo(base_analitica, "janela_2017_p4", "4º Período")
) %>% mutate(curriculo = "2017")


# ======================================================
# BLOCO 12 — TABELA FINAL COMPARATIVA
# ======================================================

tabela_cumulativa <- bind_rows(result_1999, result_2017) %>%
  select(curriculo, everything())

tabela_cumulativa

# ======================================================
# BLOCO 13 — PLOT (PRONTO PARA USAR)
# ======================================================

library(ggplot2)

ggplot(tabela_cumulativa,
       aes(x = periodo_curso,
           y = taxa_evasao_cumulativa,
           group = curriculo,
           color = curriculo)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Comparação da Taxa de Evasão Cumulativa (1999 vs 2017)",
    x = "Período do Curso",
    y = "Taxa de Evasão Cumulativa (%)"
  ) +
  theme_minimal()