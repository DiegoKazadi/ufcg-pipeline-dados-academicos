evadidos_exato <- c(59, 29)
totais_exato   <- c(639, 620)

prop.test(evadidos_exato, totais_exato, correct = FALSE)

matriz <- matrix(
  c(179, 616,
    97, 944),
  nrow = 2,
  byrow = TRUE
)

chisq.test(matriz)

evadidos <- c(179, 97)
totais   <- c(795, 1041)

prop.test(evadidos, totais, correct = FALSE)

# Dados
exata_1999 <- c(11/68, 2/85, 16/72, 3/81, 8/70, 5/84, 13/61, 1/59)
exata_2017 <- c(3/88, 3/83, 8/86, 0/92, 8/79, 1/72, 6/59, 0/32)

# Teste de Mann-Whitney
wilcox.test(exata_1999, exata_2017)
