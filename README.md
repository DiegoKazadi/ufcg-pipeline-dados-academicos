Pipeline de Análise de Impacto da Reforma Curricular na Evasão

# Visão Geral
Este projeto implementa um pipeline completo para análise do impacto da reforma curricular na evasão estudantil em uma universidade. O pipeline compara dados de duas coortes: Currículo 1999 (pré-reforma) e Currículo 2017 (pós-reforma), utilizando abordagens estatísticas robustas para inferência causal.

# Objetivo
Avaliar estatisticamente se a reforma curricular implementada em 2017 causou redução significativa nas taxas de evasão estudantil, controlando para fatores de confusão e tendências temporais.

Tabelas_Analise/
├── data/
│   ├── raw/                    # Dados brutos
│   ├── processed/              # Dados processados
│   └── results/                # Resultados das análises
├── scripts/
│   ├── 01_data_loading.R       # Carregamento e limpeza inicial
│   ├── 02_data_processing.R    # Processamento e engenharia de features
│   ├── 03_analysis_did.R       # Análise Diferenças-em-Diferenças
│   ├── 04_survival_analysis.R  # Análise de sobrevivência
│   ├── 05_visualization.R      # Visualizações e gráficos
│   └── 06_report_generation.R  # Geração de relatórios
├── config/
│   └── parameters.yaml         # Parâmetros do pipeline
├── outputs/
│   ├── tables/                 # Tabelas de resultados
│   ├── figures/                # Gráficos e visualizações
│   └── reports/                # Relatórios em PDF/HTML
└── README.md                   # Este arquivo

Configuração das Coortes
Currículo 1999 (Pré-Reforma)
Período	Ingresso Inicial	Ingresso Final	Períodos Completos
1º	2011.1	2017.2	11
2º	2011.1	2016.1	10
3º	2011.1	2015.2	9
4º	2011.1	2014.2	8
Currículo 2017 (Pós-Reforma)
Período	Ingresso Inicial	Ingresso Final	Períodos Completos
1º	2018.1	2023.1	11
2º	2018.1	2022.2	10
3º	2018.1	2022.1	9
4º	2018.1	2021.2	8
