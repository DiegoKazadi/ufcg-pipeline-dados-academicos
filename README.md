📊 Pipeline de Análise do Impacto da Reforma Curricular na Evasão
📌 Visão Geral

Este repositório implementa um pipeline completo de preparação, análise e visualização de dados acadêmicos, com foco na avaliação do impacto da reforma curricular sobre a evasão estudantil em uma universidade federal.

O pipeline compara duas grandes coortes de estudantes:

Currículo 1999 (Pré-Reforma)

Currículo 2017 (Pós-Reforma)

São utilizadas abordagens estatísticas robustas, incluindo análise de Diferenças-em-Diferenças e técnicas de sobrevivência, visando apoiar inferências causais consistentes.

🎯 Objetivo do Projeto

Avaliar estatisticamente se a reforma curricular implementada em 2017 resultou em uma redução significativa nas taxas de evasão estudantil, controlando para:

tendências temporais;

efeitos de coorte;

fatores acadêmicos e demográficos observáveis.

Este projeto está diretamente vinculado a uma dissertação de mestrado, seguindo princípios de reprodutibilidade e rigor metodológico.

## 🗂️ Estrutura do Repositório

```text
Tabelas_Analise/
├── data/
│   ├── raw/              # Dados brutos (sem tratamento)
│   ├── processed/        # Dados tratados e padronizados
│   └── results/          # Resultados intermediários das análises
│
├── scripts/
│   ├── 01_data_loading.R         # Carga e inspeção inicial da base
│   ├── 02_data_processing.R      # Limpeza e engenharia de variáveis
│   ├── 03_analysis_did.R         # Análise Diferenças-em-Diferenças
│   ├── 04_survival_analysis.R    # Análise de sobrevivência
│   ├── 05_visualization.R        # Gráficos e visualizações
│   └── 06_report_generation.R    # Geração de relatórios
│
├── config/
│   └── parameters.yaml           # Parâmetros gerais do pipeline
│
├── outputs/
│   ├── tables/                   # Tabelas finais de resultados
│   ├── figures/                  # Figuras e gráficos
│   └── reports/                  # Relatórios (PDF / HTML)
│
└── README.md                     # Documentação do projeto

🧪 Configuração das Coortes de Análise

A definição das janelas temporais respeita o critério de períodos completos observáveis, garantindo comparabilidade entre currículos.

📘 Currículo 1999 (Pré-Reforma)
Período de Análise	Ingresso Inicial	Ingresso Final	Períodos Completos
1º período	2011.1	2017.2	11
2º período	2011.1	2016.1	10
3º período	2011.1	2015.2	9
4º período	2011.1	2014.2	8
📗 Currículo 2017 (Pós-Reforma)
Período de Análise	Ingresso Inicial	Ingresso Final	Períodos Completos
1º período	2018.1	2023.1	11
2º período	2018.1	2022.2	10
3º período	2018.1	2022.1	9
4º período	2018.1	2021.2	8
⚙️ Metodologia Resumida

O pipeline segue as seguintes etapas principais:

Carga e inspeção dos dados

Padronização estrutural das variáveis

Construção das janelas temporais

Classificação por currículo

Criação de indicadores de evasão

Análise estatística (DiD e sobrevivência)

Visualização e geração de relatórios

Cada etapa é implementada em scripts independentes para facilitar auditoria, manutenção e reprodutibilidade.
